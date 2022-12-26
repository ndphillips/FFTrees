#' Grow fast-and-frugal trees (FFTs) using the \code{fan} algorithms
#'
#' @description \code{fftrees_grow_fan} is called by \code{\link{fftrees_define}}
#' to create new FFTs by applying the \code{fan} algorithms
#' (specifically, either \code{ifan} or \code{dfan}) to data.
#'
#' @param x An \code{FFTrees} object.
#'
#' @param repeat.cues Can cues be considered/used repeatedly (as logical)?
#' Default: \code{repeat.cues = TRUE}, but only relevant when using the \code{dfan} algorithm.
#'
#' @seealso
#' \code{\link{fftrees_create}} for creating \code{FFTrees} objects;
#' \code{\link{fftrees_define}} for defining FFTs;
#' \code{\link{fftrees_grow_fan}} for creating FFTs by applying algorithms to data;
#' \code{\link{fftrees_wordstofftrees}} for creating FFTs from verbal descriptions;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @importFrom stats as.formula glm predict var
#' @importFrom dplyr near

fftrees_grow_fan <- function(x,
                             repeat.cues = TRUE) {

  # Prepare: ------

  # Provide user feedback:
  if (!x$params$quiet) {
    msg <- paste0("Aiming to create FFTs with '", x$params$algorithm, "' algorithm:\n")
    cat(u_f_ini(msg))
  }

  # Global variables which can be changed later:
  exit_method <- "fixed"
  correction  <- .25

  # Extract key variables:
  criterion_name <- x$criterion_name
  criterion_v <- x$data$train[[criterion_name]]
  cues_n  <- length(x$cue_names)
  cases_n <- nrow(x$data$train)
  cue_df  <- x$data$train[, names(x$data$train) != criterion_name]


  # Initial training of cue accuracies: ------

  x <- fftrees_cuerank(x,
                       newdata = x$data$train,  # data (as df) vs.
                       data = "train"           # data type
  )


  # GROW TREES: ------

  # Setup trees: ----

  # [tree_dm, tree_stats_ls, level_stats_ls]
  {
    if (x$params$max.levels > 1) {
      expand_ls <- lapply(1:(x$params$max.levels - 1),
                          FUN = function(x) {
                            return(c(0, 1))
                          }
      )

      expand_ls[[length(expand_ls) + 1]] <- .5
      names(expand_ls) <- c(
        paste("exit.", 1:(x$params$max.levels - 1), sep = ""),
        paste("exit.", x$params$max.levels, sep = "")
      )

      tree_dm <- expand.grid(
        expand_ls,
        stringsAsFactors = FALSE
      )
    }

    if (isTRUE(all.equal(x$params$max.levels, 1))) {
      tree_dm <- data.frame("exit.1" = .5)
    }

    tree_dm$tree.num <- 1:nrow(tree_dm)
    tree_n <- nrow(tree_dm)

    # Set up tree_stats_ls:
    #  A list containing dataframes representing
    #  one-row-per-case, one-column-per-tree statistics

    tree_table_names <- c("decision", "levelout")

    tree_stats_ls <- lapply(1:length(tree_table_names), FUN = function(x) {

      output <- as.data.frame(matrix(NA, nrow = cases_n, ncol = tree_n))

      names(output) <- paste("tree", 1:tree_n, sep = ".")

      output
    })

    names(tree_stats_ls) <- tree_table_names

    level_stats_ls <- vector("list", length = tree_n)
  }

  # Loop over trees: ----
  for (tree_i in 1:tree_n) {

    # data:
    data_current   <- x$data$train
    cue_df_current <- x$cues$stats$train

    # Determine exits for tree_i:
    exits_i <- unlist(tree_dm[tree_i, grepl("exit.", names(tree_dm))])
    level_n <- length(exits_i)

    ## Set up placeholders:
    cue_best_df_original <- x$cues$stats$train  # Note: Must contain current goal.chase parameter!

    # Decisions, levelout, and cost vectors:
    decision_v <- rep(NA, cases_n)
    levelout_v <- rep(NA, cases_n)
    cuecost_v  <- rep(0, cases_n)
    outcomecost_v <- rep(NA, cases_n)
    totalcost_v <- rep(0, cases_n)

    hi_v <- rep(NA, cases_n)
    fa_v <- rep(NA, cases_n)
    mi_v <- rep(NA, cases_n)
    cr_v <- rep(NA, cases_n)

    # level_stats_i shows cumulative classification decisions statistics at each level:
    level_stats_i <- data.frame(
      "level" = NA,
      "cue" = NA,
      "cost_cue" = NA,
      "cost_cue_cum" = NA,
      "cost_dec" = NA,
      "class" = NA,
      "threshold" = NA,
      "direction" = NA,
      "exit" = NA
    )

    level_stat_names <- setdiff(names(fftrees_threshold_factor_grid()), c("threshold", "direction"))
    level_stats_i[level_stat_names] <- NA

    # asif_stats stores cumulative classification statistics AS IF all exemplars were
    #            classified at the current level (i.e., if the tree stopped here/at current level):

    asif_stats <- data.frame(
      "level" = 1:level_n,
      "sens" = NA,
      "spec" = NA,
      "acc" = NA,
      "bacc" = NA,
      "wacc" = NA,
      "dprime" = NA,
      "cost" = NA,
      "goal_change" = NA  # change in goal value
    )

    # Starting values:
    grow_tree <- TRUE
    level_current <- 0

    # GROW THE TREE: ------

    while (grow_tree == TRUE) {

      level_current <- level_current + 1
      exit_current  <- exits_i[level_current]
      cases_remaining <- is.na(decision_v)

      # Step 1: Determine a cue for the current level: ------
      {
        # A. ifan algorithm:
        if (x$params$algorithm == "ifan") {

          # Get accuracies of un-used cues:
          cue_best_df_current <- cue_best_df_original[(cue_best_df_original$cue %in% level_stats_i$cue) == FALSE, ]

        } # if algorithm == "ifan".

        # B. dfan algorithm:
        if (x$params$algorithm == "dfan") {

          data_current <- x$data$train[cases_remaining, ]

          # If cues may NOT be repeated, then remove old cues as well:
          if (repeat.cues == FALSE) {
            remaining_cues_ix <- (names(cue_df) %in% level_stats_i$cue) == FALSE
            remaining_cues <- names(cue_df)[remaining_cues_ix]
            data_current <- data_current[, c(criterion_name, remaining_cues)]
          }

          # If there is no variance in the criterion, then stop growth!
          if (all(duplicated(data_current)[-1L])) {
            grow_tree <- FALSE
            break
          }

          # Create a new "dynamic" cue range:
          x <- fftrees_cuerank(x,
                               newdata = data_current,  # data (as df) vs.
                               data = "dynamic"         # data type
          )

          # Calculate cue accuracies with remaining exemplars:
          cue_best_df_current <- x$cues$stats$dynamic  # Note: Must contain current goal.chase parameter!

        } # if algorithm == "dfan".

        # Get next cue based on maximizing goal (goal.chase):
        performance_max <- max(cue_best_df_current[[x$params$goal.chase]], na.rm = TRUE)
        cue_best_i <- which(dplyr::near(cue_best_df_current[[x$params$goal.chase]], performance_max))

        # If there is a tie, take the first:
        if (length(cue_best_i) > 1) {
          cue_best_i <- cue_best_i[1]
        }

        cues_name_new <- cue_best_df_current$cue[cue_best_i]
        cue_stats_new <- cue_best_df_current$cue[cue_best_i]
        cue_cost_new <- x$params$cost.cues[[cues_name_new]]
        cue_class_new <- cue_best_df_current$class[cue_best_i]
        cue_threshold_new <- cue_best_df_current$threshold[cue_best_i]
        cue_direction_new <- cue_best_df_current$direction[cue_best_i]

        # Add cue costs to cuecost_v:
        cuecost_v[is.na(decision_v)] <- cuecost_v[is.na(decision_v)] + cue_cost_new

        # ADD CUE INFO TO LEVEL.STATS:
        level_stats_i$level[level_current] <- level_current
        level_stats_i$cue[level_current] <- cues_name_new
        level_stats_i$cost_cue[level_current] <- cue_cost_new
        level_stats_i$cost_cue_cum[level_current] <- sum(level_stats_i$cost_cue[1:level_current])
        level_stats_i$class[level_current] <- cue_class_new
        level_stats_i$threshold[level_current] <- cue_threshold_new
        level_stats_i$direction[level_current] <- cue_direction_new
        level_stats_i$exit[level_current] <- exit_current

      } # Step 1.


      # Step 2: Determine how classifications would look if all remaining exemplars were classified (asif classification): ------
      {

        # Get decisions for current cue:
        cue_decisions <- apply_break(

          direction = cue_direction_new,
          threshold.val = cue_threshold_new,
          cue.v = x$data$train[[cues_name_new]],
          cue.class = cue_class_new

        )

        # How would classifications look if all remaining exemplars
        #   were classified at the current level?

        asif_decision_v <- decision_v
        asif_levelout_v <- levelout_v
        asif_cuecost_v  <- cuecost_v

        asif_decision_v[cases_remaining] <- cue_decisions[cases_remaining]
        asif_levelout_v[cases_remaining] <- level_current
        asif_cuecost_v[cases_remaining]  <- cue_cost_new

        # Calculate asif classification results:
        asif_results <- classtable(
          prediction_v = asif_decision_v,
          criterion_v  = criterion_v,
          sens.w = x$params$sens.w
        )


        # Add key stats to asif_stats:
        # Note: Horizontal/by-row measures (ppv, npv) are currently NOT recorded:  +++ here now +++
        asif_stats[level_current,
                   c("sens", "spec", "dprime", "acc", "bacc", "wacc", "cost")] <- c(#
                     asif_results$sens, asif_results$spec,
                     asif_results$dprime,                                     # ADDED on 2022-09-23
                     asif_results$acc, asif_results$bacc, asif_results$wacc,
                     asif_results$cost
                   )


        # If ASIF classification is perfect, then stop:
        if (x$params$goal.chase != "cost") {

          if (dplyr::near(asif_stats[[x$params$goal.chase]][level_current], 1)) { # perfect 1:
            grow_tree <- FALSE
          }

        } else { # chasing "cost":

          if (dplyr::near(asif_stats[[x$params$goal.chase]][level_current], 0)) { # perfect 0:
            grow_tree <- FALSE
          }

        }
        # ToDo: What would be a "perfect" value for x$params$goal.chase != "dprime"?  +++ here now +++


        # Calculate goal change value:
        {
          if (level_current == 1) {
            asif_stats$goal_change[1] <- asif_stats[[x$params$goal]][1]
          }

          if (level_current > 1) {
            goal_change <- asif_stats[[x$params$goal.chase]][level_current] - asif_stats[[x$params$goal.chase]][level_current - 1]  # difference
            asif_stats$goal_change[level_current] <- goal_change
          }
        }

      } # Step 2.


      # Step 3: Classify exemplars in current level: ------
      {
        if (dplyr::near(exit_current, 1) | dplyr::near(exit_current, .50)) {

          decide_1_index <- cases_remaining & cue_decisions == TRUE

          decision_v[decide_1_index] <- TRUE
          levelout_v[decide_1_index] <- level_current
        }

        if (exit_current == 0 | dplyr::near(exit_current, .50)) {

          decide_0_index <- is.na(decision_v) & cue_decisions == FALSE

          decision_v[decide_0_index] <- FALSE
          levelout_v[decide_0_index] <- level_current
        }

        # Update cost vectors:
        hi_v <- decision_v == TRUE  & criterion_v == TRUE
        mi_v <- decision_v == FALSE & criterion_v == TRUE
        fa_v <- decision_v == TRUE  & criterion_v == FALSE
        cr_v <- decision_v == FALSE & criterion_v == FALSE

        outcomecost_v[hi_v == TRUE] <- x$params$cost.outcomes$hi
        outcomecost_v[mi_v == TRUE] <- x$params$cost.outcomes$mi
        outcomecost_v[fa_v == TRUE] <- x$params$cost.outcomes$fa
        outcomecost_v[cr_v == TRUE] <- x$params$cost.outcomes$cr

      } # Step 3.


      # Step 4: Update results: ------
      {
        cases_remaining <- is.na(decision_v)

        # ToDo: NEED TO FIX THIS BELOW TO INCORPORATE ALL COSTS.

        # Get cumulative stats of exemplars currently classified:

        results_cum <- classtable(
          prediction_v = decision_v[cases_remaining == FALSE],
          criterion_v = criterion_v[cases_remaining == FALSE],
          sens.w = x$params$sens.w,
          cost.v = cuecost_v[cases_remaining == FALSE],
          cost.outcomes = x$params$cost.outcomes
        )

        # Update level stats:
        level_stats_i[level_current, ]         <- NA
        level_stats_i$level[level_current]     <- level_current
        level_stats_i$cue[level_current]       <- cues_name_new
        level_stats_i$class[level_current]     <- cue_class_new
        level_stats_i$threshold[level_current] <- cue_threshold_new
        level_stats_i$direction[level_current] <- cue_direction_new
        level_stats_i$exit[level_current]      <- exit_current

        # Current level stats:
        # Note: Horizontal/by-row measures (ppv, npv) are currently NOT recorded:  +++ here now +++
        level_stats_v <- c("hi", "fa", "mi", "cr",
                           "sens", "spec",
                           "dprime",                   # ADDED on 2022-09-23
                           "bacc", "acc", "wacc",
                           "cost_dec", "cost")
        level_stats_i[level_current, level_stats_v] <- results_cum[, level_stats_v]

      } # Step 4.


      # Step 5: Continue growing tree? ------
      {
        cases_remaining_n <- sum(cases_remaining)

        if (cases_remaining_n > 0 & level_current != cues_n & exit_method == "fixed") {
          if (level_current < level_n) {
            grow_tree <- TRUE
          }

          if (level_current == level_n) {
            grow_tree <- FALSE
            break
          }
        }

        if (cases_remaining_n == 0 | level_current == cues_n) {
          break
        }

        if (x$params$stopping.rule == "exemplars" & cases_remaining_n < x$params$stopping.par * nrow(cue_df)) {
          break
        }

        if (x$params$stopping.rule == "levels" & level_current == x$params$stopping.par) {
          break
        }

        if (x$params$algorithm == "dfan" & sd(criterion_v[cases_remaining]) == 0) {
          break
        }

        # Set up next level stats:
        level_stats_i[level_current + 1, ] <- NA

      } # Step 5.

    } # STOP while(grow_tree) loop.


    # Step 6: No more growth. Make sure that last level is bi-directional: ------
    {
      last_level_nr <- max(level_stats_i$level)
      last_cue <- level_stats_i$cue[last_level_nr]
      # cost_cue <- x$params$cost.cues[[last_cue]]  # never used???

      last_exit_direction <- level_stats_i$exit[level_stats_i$level == last_level_nr]

      if (last_exit_direction != .5) {

        decision_v[levelout_v == last_level_nr] <- NA

        last_cue_stats <- cue_best_df_current[cue_best_df_current$cue == last_cue, ]

        decision_index <- is.na(decision_v)

        # Step B: Determine accuracy of negative and positive classification: ----

        current_decisions <- apply_break(
          direction = last_cue_stats$direction,
          threshold.val = last_cue_stats$threshold,
          cue.v = x$data$train[[last_cue]],
          cue.class = last_cue_stats$class
        )

        decide_0_index <- decision_index == TRUE & current_decisions == FALSE
        decide_1_index <- decision_index == TRUE & current_decisions == TRUE

        decision_v[decide_0_index] <- FALSE
        decision_v[decide_1_index] <- TRUE

        levelout_v[decide_0_index] <- level_current
        levelout_v[decide_1_index] <- level_current

        # update classification results:
        last_classtable <- classtable(
          prediction_v = as.logical(decision_v),
          criterion_v = as.logical(criterion_v),
          sens.w = x$params$sens.w,
          cost.v = cuecost_v,
          cost.outcomes = x$params$cost.outcomes
        )

        level_stats_i$exit[last_level_nr] <- .5


        # Note: Why not use same stats as in level_stats_v above? (Here: "dprime" and "cost" missing): +++ here now +++
        # level_stats_i[last_level_nr, c("hi", "fa", "mi", "cr",
        #                                "sens", "spec", "bacc", "acc", "wacc", "cost_dec")] <- last_classtable[, c("hi", "fa", "mi", "cr", "sens", "spec", "bacc", "acc", "wacc", "cost_dec")]

        # NEW (using same level_stats_v as above) on 2022-09-23:
        level_stats_i[last_level_nr, level_stats_v] <- last_classtable[, level_stats_v]

      } # if (last_exit_direction != .5).

    } # Step 6.


    # Tree is finished! ----


    # Set up final output: ----

    tree_stats_ls$decision[, tree_i] <- decision_v
    tree_stats_ls$levelout[, tree_i] <- levelout_v

    level_stats_i$tree <- tree_i

    level_stats_ls[[tree_i]] <- level_stats_i
  }


  # Summarize tree definitions and statistics: ------
  # (as df of tree_definitions, 1 line per FFT):

  {

    # Summarize tree definitions:
    tree_definitions <- as.data.frame(matrix(NA, nrow = tree_n, ncol = 7))
    names(tree_definitions) <- c("tree", "nodes", "classes", "cues", "directions", "thresholds", "exits")

    for (tree_i in 1:tree_n) {

      level_stats_i <- level_stats_ls[[tree_i]]

      # Store tree definition (each FFT as 1 line of df):
      tree_definitions$tree[tree_i]       <- tree_i
      tree_definitions$cues[tree_i]       <- paste(level_stats_i$cue, collapse = ";")
      tree_definitions$nodes[tree_i]      <- length(level_stats_i$cue)
      tree_definitions$classes[tree_i]    <- paste(substr(level_stats_i$class, 1, 1), collapse = ";")
      tree_definitions$exits[tree_i]      <- paste(level_stats_i$exit, collapse = ";")
      tree_definitions$thresholds[tree_i] <- paste(level_stats_i$threshold, collapse = ";")
      tree_definitions$directions[tree_i] <- paste(level_stats_i$direction, collapse = ";")

    } # for (tree).

    # Remove duplicate trees:
    duplicate_trees  <- duplicated(tree_definitions[c("cues", "exits", "thresholds", "directions")])
    tree_definitions <- tree_definitions[duplicate_trees == FALSE, ]

    # Adjust names:
    rownames(tree_definitions) <- 1:nrow(tree_definitions)
    tree_definitions$tree <- 1:nrow(tree_definitions)
    tree_definitions <- tree_definitions[, c(which(names(tree_definitions) == "tree"), which(names(tree_definitions) != "tree"))]

  }

  # Add tree_definitions to x:
  x$trees$definitions <- tree_definitions
  x$trees$n <- nrow(tree_definitions)


  # Provide user feedback:
  if (!x$params$quiet) {
    msg <- paste0("Successfully created ", x$trees$n, " FFTs with '", x$params$algorithm, "' algorithm.\n")
    cat(u_f_fin(msg))
  }


  # Output: ----

  return(x)


} # fftrees_grow_fan().

# eof.
