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
  if (!x$params$quiet$ini) {

    cur_algorithm  <- x$params$algorithm
    cur_goal.chase <- x$params$goal.chase

    # msg <- paste0("Aiming to create FFTs with '", cur_algorithm, "' algorithm (chasing '", cur_goal.chase, "'):\n")
    # cat(u_f_ini(msg))

    cli::cli_alert("Create FFTs with '{cur_algorithm}' algorithm (chasing '{cur_goal.chase}'):",
                   class = "alert-start")
  }

  # Global variables which can be changed later:
  exit_method <- "fixed"
  correction  <- .25

  # Extract key variables:
  criterion_name <- x$criterion_name
  criterion_v    <- x$data$train[[criterion_name]]

  cues_n  <- length(x$cue_names)
  cases_n <- nrow(x$data$train)
  cue_df  <- x$data$train[, names(x$data$train) != criterion_name]

  my_goal     <- x$params$my.goal      # (only ONCE)
  my_goal_fun <- x$params$my.goal.fun  # (only ONCE)


  # Initial training of cue accuracies: ------

  x <- fftrees_cuerank(x,
                       newdata = x$data$train,  # data (as df) vs.
                       data = "train"           # data type
  )


  # GROW TREES: ------

  # Setup trees: ----

  # [tree_dm, tree_stats_ls, level_stats_ls]
  # ToDo: Use exit_types (global constant).
  {
    if (x$params$max.levels > 1) {
      expand_ls <- lapply(1:(x$params$max.levels - 1),
                          FUN = function(x) {
                            return(exit_types[1:2]) # return(c(0, 1))
                          }
      )

      expand_ls[[length(expand_ls) + 1]] <- exit_types[3] # .5
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
      tree_dm <- data.frame("exit.1" = exit_types[3]) # .5)
    }

    tree_dm$tree.num <- 1:nrow(tree_dm)
    tree_n <- nrow(tree_dm)
    # print(paste0("\u2014 Growing tree_n = ", tree_n, " FFTs."))  # 4debugging


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


  # Define the set of stats names (only once, before loop): ------

  # A. Define the set of ASIF stats [asif_stats_name_v]: ----
  if (!is.null(my_goal)){ # include my.goal (name and value):

    asif_stats_name_v <- c("sens", "spec",
                           "dprime",
                           "acc", "bacc", "wacc",
                           my_goal,        # my.goal (name)
                           "cost")

  } else { # default set of ASIF stats:

    asif_stats_name_v <- c("sens", "spec",
                           "dprime",
                           "acc", "bacc", "wacc",
                           # my_goal,      # my.goal (name)
                           "cost")

  } # if (my.goal).
  # Note: Horizontal/by-row measures (ppv, npv) are currently NOT recorded in asif_stats.


  # B. Define the set of level stats names [level_stats_name_v]: ----
  if (!is.null(my_goal)){ # include my.goal (name and value):

    level_stats_name_v <- c("hi", "fa", "mi", "cr",
                            "sens", "spec",
                            "dprime",
                            "acc", "bacc", "wacc",
                            my_goal,        # my.goal (name)
                            "cost_dec", "cost")

  } else { # default set of level stats:

    level_stats_name_v <- c("hi", "fa", "mi", "cr",
                            "sens", "spec",
                            "dprime",
                            "acc", "bacc", "wacc",
                            # my_goal,        # my.goal (name)
                            "cost_dec", "cost")

  } # if (my.goal).
  # Note: Horizontal/by-row measures (ppv, npv) are currently NOT recorded in level_stats_i.


  # LOOP (over trees): ----

  for (tree_i in 1:tree_n) {

    # Data:
    data_current   <- x$data$train
    cue_df_current <- x$cues$stats$train

    # Determine exits for tree_i:
    exits_i <- unlist(tree_dm[tree_i, grepl("exit.", names(tree_dm))])
    level_n <- length(exits_i)

    ## Set up placeholders:
    cue_best_df_original <- x$cues$stats$train  # Note: Must contain current goal.chase parameter!

    # Decisions, levelout, and cost vectors: ----

    decision_v <- rep(NA, cases_n)
    levelout_v <- rep(NA, cases_n)
    cuecost_v  <- rep(0, cases_n)
    outcomecost_v <- rep(NA, cases_n)
    totalcost_v   <- rep(0,  cases_n)  # is NOT used anywhere?

    hi_v <- rep(NA, cases_n)
    fa_v <- rep(NA, cases_n)
    mi_v <- rep(NA, cases_n)
    cr_v <- rep(NA, cases_n)


    # level_stats_i (as df): ----
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

    # HACK: Get names by calling fftrees_threshold_factor_grid()
    threshold_factor_grid_names <- names(fftrees_threshold_factor_grid())
    # threshold_factor_grid_names

    if (!is.null(my_goal)){ # add my.goal (name):
      threshold_factor_grid_names <- c(threshold_factor_grid_names, my_goal)
    }

    # level_stat_names (remove 2 names from threshold_factor_grid_names):
    level_stat_names <- setdiff(threshold_factor_grid_names, c("threshold", "direction"))
    level_stats_i[level_stat_names] <- NA  # initialize


    # asif_stats (as df): ----
    # asif_stats stores cumulative classification statistics AS IF all exemplars were
    #            classified at the current level (i.e., if the tree stopped here/at current level):

    # Define the default set of ASIF stats:
    asif_stats <- data.frame("level" = 1:level_n,
                             "sens" = NA, "spec" = NA,
                             "dprime" = NA,
                             "acc" = NA, "bacc" = NA, "wacc" = NA,
                             # my_goal = NA,    #  my.goal (name)
                             "cost" = NA,
                             "goal_change" = NA)

    if (!is.null(my_goal)){ # include my.goal (name and value):

      asif_stats[[my_goal]] <- NA

    }

    # print(asif_stats)  # 4debugging

    # Starting values:
    grow_tree <- TRUE
    level_current <- 0


    # GROW THE TREE: ------

    while (grow_tree == TRUE) {

      level_current <- level_current + 1
      exit_current  <- exits_i[level_current]
      ix_case_remaining <- is.na(decision_v)

      # Step 1: Determine a cue for the current level: ------
      {
        # A. ifan algorithm: ----

        if (x$params$algorithm == "ifan") {

          # Get accuracies of un-used cues:
          cue_best_df_current <- cue_best_df_original[(cue_best_df_original$cue %in% level_stats_i$cue) == FALSE, ]

        } # if algorithm == "ifan".


        # B. dfan algorithm: ----

        if (x$params$algorithm == "dfan") {

          data_current <- x$data$train[ix_case_remaining, ]

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

          # Create a new/special "dynamic" cue range:
          x <- fftrees_cuerank(x,
                               newdata = data_current,  # data (as df) vs.
                               data = "dynamic"         # special data type
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
        cue_cost_new  <- x$params$cost.cues[[cues_name_new]]
        cue_class_new <- cue_best_df_current$class[cue_best_i]
        cue_threshold_new <- cue_best_df_current$threshold[cue_best_i]
        cue_direction_new <- cue_best_df_current$direction[cue_best_i]

        # Add cue costs to cuecost_v:
        cuecost_v[is.na(decision_v)] <- cuecost_v[is.na(decision_v)] + cue_cost_new

        # ADD CUE INFO TO LEVEL.STATS:
        level_stats_i$level[level_current]    <- level_current
        level_stats_i$cue[level_current]      <- cues_name_new
        level_stats_i$cost_cue[level_current] <- cue_cost_new
        level_stats_i$cost_cue_cum[level_current] <- sum(level_stats_i$cost_cue[1:level_current])
        level_stats_i$class[level_current]     <- cue_class_new
        level_stats_i$threshold[level_current] <- cue_threshold_new
        level_stats_i$direction[level_current] <- cue_direction_new
        level_stats_i$exit[level_current]      <- exit_current

      } # Step 1.


      # Step 2: Look-ahead (using "ASIF" classification): ------

      # Rationale: Determine ASIF stats: How classification results and stats would look
      #            IF all remaining exemplars WERE classified at the current level.

      {

        # Get ASIF decisions for current cue:
        cue_decisions <- apply_break(

          direction = cue_direction_new,
          threshold.val = cue_threshold_new,
          cue.v = x$data$train[[cues_name_new]],
          cue.class = cue_class_new

        )

        asif_decision_v <- decision_v
        asif_levelout_v <- levelout_v
        asif_cuecost_v  <- cuecost_v

        asif_decision_v[ix_case_remaining] <- cue_decisions[ix_case_remaining]
        asif_levelout_v[ix_case_remaining] <- level_current
        asif_cuecost_v[ix_case_remaining]  <- cue_cost_new

        # ToDo: Pass level_current and cues_name_new to classtable() helper (to handle NA values in utility fn)
        #    OR move NA handling code to calling functions (to diagnose what happens here)?   +++ here now +++

        # Get results for ASIF classifications:
        asif_results <- classtable(
          prediction_v = asif_decision_v,
          criterion_v  = criterion_v,
          #
          sens.w = x$params$sens.w,
          #
          cost.outcomes = x$params$cost.outcomes,  # add outcome cost
          cost_v = asif_cuecost_v,                 # add cue cost
          #
          my.goal = my_goal,
          my.goal.fun = my_goal_fun,
          #
          quiet_mis = x$params$quiet$mis  # passed to hide/show NA user feedback
        )
        # Note: The 2 cost arguments cost.outcomes and cost_v were NOT being used to compute asif_results.
        # DONE: ADDED asif_cuecost_v to call to classtable() here (on 2023-01-19)     +++ here now +++

        # print(asif_results)  # 4debugging


        # Define and add the set of key ASIF stats (to asif_stats): ----

        { # HACKY code start: ------

          # if (!is.null(my_goal)){ # include my.goal (name and value):
          #
          #   asif_stats[level_current,
          #              c("sens", "spec",
          #                "acc", "bacc", "wacc",
          #                "dprime",
          #                my_goal,        # my.goal (name)
          #                "cost")] <- c(#
          #                  asif_results$sens, asif_results$spec,
          #                  asif_results$acc, asif_results$bacc, asif_results$wacc,
          #                  asif_results$dprime,
          #                  asif_results[[my_goal]],  # my.goal (value)
          #                  asif_results$cost
          #                )
          #
          # } else { # default set of ASIF stats:
          #
          #   asif_stats[level_current,
          #              c("sens", "spec",
          #                "acc", "bacc", "wacc",
          #                "dprime",
          #                # my_goal,        # my.goal (name)
          #                "cost")] <- c(#
          #                  asif_results$sens, asif_results$spec,
          #                  asif_results$acc, asif_results$bacc, asif_results$wacc,
          #                  asif_results$dprime,
          #                  # asif_results[[my.goal]],  # my.goal (value)
          #                  asif_results$cost
          #                )
          #
          # } # if (my.goal).
          # # Note: Horizontal/by-row measures (ppv, npv) are currently NOT recorded in asif_stats.
          #
          # # print(asif_stats)          # 4debugging
          # asif_stats_m1 <- asif_stats  # 4checking

        } # HACKY code end.


        # CLEANER code start: ------

        # # Define the set of ASIF stats [asif_stats_name_v]: ----
        # if (!is.null(my_goal)){ # include my.goal (name and value):
        #
        #   asif_stats_name_v <- c("sens", "spec",
        #                          "acc", "bacc", "wacc",
        #                          "dprime",
        #                          my_goal,        # my.goal (name)
        #                          "cost")
        #
        # } else { # default set of ASIF stats:
        #
        #   asif_stats_name_v <- c("sens", "spec",
        #                          "acc", "bacc", "wacc",
        #                          "dprime",
        #                          # my_goal,      # my.goal (name)
        #                          "cost")
        #
        # } # if (my.goal).
        # # Note: Horizontal/by-row measures (ppv, npv) are currently NOT recorded in asif_stats.


        # Update row and columns of asif_stats (df) with elements of asif_results (vector): ----
        # # (Assuming asif_stats_name_v (defined above, outside of loop):

        asif_stats[level_current, asif_stats_name_v] <- asif_results[asif_stats_name_v]

        # print(asif_stats)  # 4debugging

        # CLEANER code end. ------


        # # Verify that HACKY and CLEANER codes yield same result:
        # asif_stats_m2 <- asif_stats  # 4checking
        #
        # if (all.equal(asif_stats_m1, asif_stats_m2)){
        #   # print("Ok: Both asif_stats methods yield the same result, qed.")
        # } else {
        #   print("Caveat: Both asif_stats methods yield DIFFERENT results.")
        # }


        # If ASIF classification is perfect/ideal, set grow_tree to FALSE: ----

        asif_goal_chase_value <- asif_stats[[x$params$goal.chase]][level_current]
        # print(asif_goal_chase_value)  # 4debugging

        # Identify perfect/ideal FFTs:

        if (x$params$goal.chase %in% c("acc", "bacc", "wacc")) { # A. chasing an accuracy measure:

          if (dplyr::near(asif_goal_chase_value, 1)) { # perfect/ideal acc = 1
            grow_tree <- FALSE
          }

        } else if (x$params$goal.chase == "cost") { # B. chasing "cost" measure:

          if (dplyr::near(asif_goal_chase_value, 0)) { # perfect/ideal cost = 0  # ToDo: Or is best cost -1?
            grow_tree <- FALSE
          }

        } else if (x$params$goal.chase == "dprime") { # C. chasing "dprime" measure:

          # What would be a "perfect" value for x$params$goal.chase == "dprime"?
          #
          # "The highest possible d' (greatest sensitivity) is 6.93, the effective limit (using .99 and .01) 4.65,
          #  typical values are up to 2.0, and 69% correct for both different and same trials corresponds to a d' of 1.0."
          #  Source: <http://phonetics.linguistics.ucla.edu/facilities/statistics/dprime.htm>

          max_dprime <- 4.65  # effective limit (using .99 and .01)

          if (asif_goal_chase_value >= max_dprime){
            grow_tree <- FALSE
          }

        } else if (x$params$goal.chase == my_goal){

          # ToDo: What if goal.chase == my_goal?

          if (any(sapply(x$params$quiet, isFALSE))) { # Provide user feedback:

            msg <- paste0("A limit for growing FFTs with goal.chase = '", x$params$goal.chase, "' is unknown.")
            cat(u_f_hig("\u2014 ", msg, "\n"))

            # OR: cli::cli_alert_warning(msg)
          }


        } else { # note an unknown/invalid goal.chase value:

          # Current set of valid goals (for FFT selection):
          if (!is.null(my_goal)){
            valid_goal <- c(goal_options, my_goal)  # add my.goal (name) to default
          } else { # default:
            valid_goal <- goal_options  # use (global constant)
          }

          valid_goal_str <- paste(valid_goal, collapse = ", ")

          stop(paste0("The current goal.chase value '", x$params$goal.chase, "' is not in '", valid_goal_str, "'"))

        } # If perfect/ideal tree: Set grow_tree to FALSE.

        # print(paste0("1. grow_tree = ", grow_tree))  # 4debugging


        if (!grow_tree){ # A perfect/ideal tree_i (based on current goal.chase) was found:

          # if (any(sapply(x$params$quiet, isFALSE))) { # Provide user feedback:
          if (debug){ # Provide debugging feedback:

            cli::cli_alert_info("Found a perfect tree (i = {tree_i}): {x$params$goal.chase} = {asif_goal_chase_value}")

          } # if (debug).

        }


        # Calculate the current goal_change value: ----
        {

          if (level_current == 1) { # initialize:

            goal_change <- asif_stats[[x$params$goal.chase]][1]  # changed from $goal to $goal.chase on 2023-03-09.

          } else { # compute change (current level - previous level):

            goal_change <- asif_stats[[x$params$goal.chase]][level_current] - asif_stats[[x$params$goal.chase]][level_current - 1]  # difference

          }

          asif_stats$goal_change[level_current] <- goal_change

          if (debug){ # Provide debugging feedback:

            # Report goal_change value:
            goal_change_rnd <- round(asif_stats$goal_change[level_current], 3)
            cli::cli_alert_info("Tree {tree_i}, level {level_current}: goal_change = {goal_change_rnd} (chasing '{x$params$goal.chase}').")

          } # if (debug).

        }

      } # Step 2.


      # Step 3: Classify exemplars at current level: ------

      {
        if (dplyr::near(exit_current, exit_types[2]) | dplyr::near(exit_current, exit_types[3])) {
          # if (dplyr::near(exit_current, 1) | dplyr::near(exit_current, .50)) {

          decide_1_index <- ix_case_remaining & cue_decisions == TRUE

          decision_v[decide_1_index] <- TRUE
          levelout_v[decide_1_index] <- level_current

        }

        if (exit_current == exit_types[1] | dplyr::near(exit_current, exit_types[3])) {
          # if (exit_current == 0 | dplyr::near(exit_current, .50)) {

          decide_0_index <- is.na(decision_v) & cue_decisions == FALSE

          decision_v[decide_0_index] <- FALSE
          levelout_v[decide_0_index] <- level_current

        }

        # # Update cost vectors:
        # hi_v <- (decision_v == TRUE)  & (criterion_v == TRUE)
        # fa_v <- (decision_v == TRUE)  & (criterion_v == FALSE)
        # mi_v <- (decision_v == FALSE) & (criterion_v == TRUE)
        # cr_v <- (decision_v == FALSE) & (criterion_v == FALSE)

        # outcomecost_v[hi_v == TRUE] <- x$params$cost.outcomes$hi  # is NOT used anywhere?
        # outcomecost_v[fa_v == TRUE] <- x$params$cost.outcomes$fa  # is NOT used anywhere?
        # outcomecost_v[mi_v == TRUE] <- x$params$cost.outcomes$mi  # is NOT used anywhere?
        # outcomecost_v[cr_v == TRUE] <- x$params$cost.outcomes$cr  # is NOT used anywhere?

        # ToDo: NEED TO FIX THIS BELOW TO INCORPORATE ALL COSTS.

      } # Step 3.


      # Step 4: Update results: ------
      {
        ix_case_remaining <- is.na(decision_v)

        # Get cumulative stats of exemplars currently classified:

        results_cum <- classtable(
          prediction_v = decision_v[ix_case_remaining  == FALSE],
          criterion_v  = criterion_v[ix_case_remaining == FALSE],
          #
          sens.w = x$params$sens.w,
          #
          cost.outcomes = x$params$cost.outcomes,
          cost_v = cuecost_v[ix_case_remaining == FALSE],
          #
          my.goal = my_goal,
          my.goal.fun = my_goal_fun,
          #
          quiet_mis = x$params$quiet$mis  # passed to hide/show NA user feedback
        )

        # Update level stats:
        level_stats_i[level_current, ]         <- NA
        level_stats_i$level[level_current]     <- level_current
        level_stats_i$cue[level_current]       <- cues_name_new
        level_stats_i$class[level_current]     <- cue_class_new
        level_stats_i$threshold[level_current] <- cue_threshold_new
        level_stats_i$direction[level_current] <- cue_direction_new
        level_stats_i$exit[level_current]      <- exit_current


        # # Define the set of level stats names [level_stats_name_v]: ----
        # if (!is.null(my_goal)){ # include my.goal (name and value):
        #
        #   level_stats_name_v <- c("hi", "fa", "mi", "cr",
        #                           "sens", "spec",
        #                           "dprime",
        #                           "bacc", "acc", "wacc",
        #                           my_goal,        # my.goal (name)
        #                           "cost_dec", "cost")
        #
        # } else { # default set of level stats:
        #
        #   level_stats_name_v <- c("hi", "fa", "mi", "cr",
        #                           "sens", "spec",
        #                           "dprime",
        #                           "bacc", "acc", "wacc",
        #                           # my_goal,        # my.goal (name)
        #                           "cost_dec", "cost")
        #
        # } # if (my.goal).
        # # Note: Horizontal/by-row measures (ppv, npv) are currently NOT recorded in level_stats_i.

        # Select level stats (variables):
        # # (Assuming level_stats_name_v (defined above, outside of loop):

        level_stats_i[level_current, level_stats_name_v] <- results_cum[ , level_stats_name_v]

      } # Step 4.


      # Step 5: Continue growing tree? ------
      {

        if (!grow_tree){ # grow_tree has been set to FALSE above:

          break

        } # else:

        n_case_remaining <- sum(ix_case_remaining)
        # print(n_case_remaining)  # 4debugging

        if ((n_case_remaining > 0) & (level_current != cues_n) & (exit_method == "fixed")) {

          if (level_current < level_n) {
            grow_tree <- TRUE
          } else {
            grow_tree <- FALSE
            break
          }

        }

        if ((n_case_remaining == 0) | (level_current == cues_n)) {
          grow_tree <- FALSE
          break
        }

        if ((x$params$stopping.rule == "exemplars") & (n_case_remaining < x$params$stopping.par * nrow(cue_df))) {
          grow_tree <- FALSE
          break
        }

        if ((x$params$stopping.rule == "levels") & (level_current == x$params$stopping.par)) {
          grow_tree <- FALSE
          break
        }

        if (x$params$stopping.rule == "statdelta") {

          if (x$params$goal.chase == "cost"){ # Special case of chasing COST:

            # 1. only evaluate increments AFTER the 1st level (as 1st level usually incurs the largest increment in cost)
            # 2. stop if cost increase EXCEEDS stopping.par (i.e., aim to MINimize cost increase):

            if ((level_current > 1) & (asif_stats$goal_change[level_current] > x$params$stopping.par) ) { # stop when ABOVE:

              # print("Stop by 'stopping.rule = statdelta' for 'goal.chase = cost': 'goal_change > stopping.par' in asif_stats")  # 4debugging

              grow_tree <- FALSE
              break

            }

          } else { # all ACC measures (i.e., x$params$goal.chase != "cost"):

            if (asif_stats$goal_change[level_current] < x$params$stopping.par) { # stop when BELOW:

              # print("Stop by 'stopping.rule = statdelta' for an ACC measure: 'goal_change < stopping.par' in asif_stats")  # 4debugging

              grow_tree <- FALSE
              break

            }

          }

          # Limitation: Currently still keeps/includes the CURRENT level, i.e., the first level failing the criterion.
          # Note that some stats do NOT grow monotonically. Hence, this hill-climbing heuristic may prevent finding better solutions!

        } # if stopping.rule == "statdelta".


        if ((x$params$algorithm == "dfan") & sd(criterion_v[ix_case_remaining]) == 0) {
          grow_tree <- FALSE
          break
        }

        # print(paste0("2. grow_tree = ", grow_tree))  # 4debugging


        # Set up next level stats:
        level_stats_i[level_current + 1, ] <- NA


      } # Step 5.

    } # STOP while(grow_tree) loop.


    # Step 6: No more growth. Make sure that the last level is bi-directional: ------
    {
      last_level_nr <- max(level_stats_i$level)
      last_cue      <- level_stats_i$cue[last_level_nr]
      # cost_cue    <- x$params$cost.cues[[last_cue]]  # never used???

      last_exit_direction <- level_stats_i$exit[level_stats_i$level == last_level_nr]

      if (last_exit_direction != exit_types[3]) {  # != .5:

        decision_v[levelout_v == last_level_nr] <- NA

        last_cue_stats <- cue_best_df_current[cue_best_df_current$cue == last_cue, ]

        decision_index <- is.na(decision_v)


        # Determine the accuracy of negative and positive classification: ----

        current_decisions <- apply_break(
          direction = last_cue_stats$direction,
          threshold.val = last_cue_stats$threshold,
          cue.v = x$data$train[[last_cue]],
          cue.class = last_cue_stats$class
        )

        decide_0_index <- (decision_index == TRUE) & (current_decisions == FALSE)
        decide_1_index <- (decision_index == TRUE) & (current_decisions == TRUE)

        decision_v[decide_0_index] <- FALSE
        decision_v[decide_1_index] <- TRUE

        levelout_v[decide_0_index] <- level_current
        levelout_v[decide_1_index] <- level_current

        # Update classification results:
        last_classtable <- classtable(
          prediction_v = as.logical(decision_v),
          criterion_v = as.logical(criterion_v),
          #
          sens.w = x$params$sens.w,
          #
          cost.outcomes = x$params$cost.outcomes,
          cost_v = cuecost_v,
          #
          my.goal = my_goal,
          my.goal.fun = my_goal_fun,
          #
          quiet_mis = x$params$quiet$mis  # passed to hide/show NA user feedback
        )

        level_stats_i$exit[last_level_nr] <- exit_types[3]  # .5


        # Note: Why not use same stats as in level_stats_name_v above? (Here: "dprime" and "cost" missing): +++ here now +++
        # level_stats_i[last_level_nr, c("hi", "fa", "mi", "cr",
        #                                "sens", "spec", "bacc", "acc", "wacc", "cost_dec")] <- last_classtable[, c("hi", "fa", "mi", "cr", "sens", "spec", "bacc", "acc", "wacc", "cost_dec")]

        # NEW (using same level_stats_name_v as above) on 2022-09-23:
        level_stats_i[last_level_nr, level_stats_name_v] <- last_classtable[ , level_stats_name_v]

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
  # (as a df of tree_definitions, 1 line per FFT):

  {

    # Collect and summarize tree definitions:
    tree_definitions <- as.data.frame(matrix(NA, nrow = tree_n, ncol = 7))
    names(tree_definitions) <- c("tree",  "nodes",  "classes", "cues", "directions", "thresholds", "exits")  # (mostly plural)

    tree_definitions_o <- tree_definitions # (copy 4debugging below)

    for (tree_i in 1:tree_n) { # Loop (over trees):

      # Get level_stats_i (as df):
      level_stats_i <- level_stats_ls[[tree_i]]
      # print(level_stats_i)  # 4debugging

      # NEW code start: ----

      # Select variables of cur_tree_df from level_stats_i:
      req_tree_vars <- c("class", "cue", "direction", "threshold", "exit")  # [all singular]
      cur_tree_df <- level_stats_i[ , req_tree_vars]
      # print(cur_tree_df)  # 4debugging

      tree_definitions[tree_i, ] <- write_fft_df(fft = cur_tree_df, tree = tree_i)
      # print(tree_definitions[tree_i, ])  # 4debugging

      # NEW code end. ----

      # +++ here now +++

      # OLD code start: ----

      # # Store OLD tree definition ("_o") using level_stats_i (each FFT as 1 line of df):
      # tree_definitions_o$tree[tree_i]       <- tree_i  # counter & ID
      # tree_definitions_o$nodes[tree_i]      <- length(level_stats_i$cue)
      # tree_definitions_o$classes[tree_i]    <- paste(substr(level_stats_i$class, 1, 1), collapse = fft_node_sep)
      # tree_definitions_o$cues[tree_i]       <- paste(level_stats_i$cue,                 collapse = fft_node_sep)
      # tree_definitions_o$directions[tree_i] <- paste(level_stats_i$direction,           collapse = fft_node_sep)
      # tree_definitions_o$thresholds[tree_i] <- paste(level_stats_i$threshold,           collapse = fft_node_sep)
      # tree_definitions_o$exits[tree_i]      <- paste(level_stats_i$exit,                collapse = fft_node_sep)
      #
      # # print(tree_definitions_o)  # 4debugging

      # OLD code end. ----


    } # loop (over trees).

    # # Check: Verify equality of OLD and NEW code results:
    # if (!all.equal(tree_definitions, tree_definitions_o)) { stop("OLD vs. NEW: tree_definitions diff") }


    # Remove duplicate trees (rows):
    duplicate_trees  <- duplicated(tree_definitions[c("cues", "exits", "thresholds", "directions")])
    tree_definitions <- tree_definitions[duplicate_trees == FALSE, ]

    # Adjust names (of df):
    rownames(tree_definitions) <- 1:nrow(tree_definitions)  # assign rownames
    tree_definitions$tree      <- 1:nrow(tree_definitions)  # re-assign tree IDs
    tree_definitions <- tree_definitions[ , c(which(names(tree_definitions) == "tree"), which(names(tree_definitions) != "tree"))] # var "tree" first

  }

  # Add tree_definitions to x:
  x$trees$definitions <- tree_definitions
  x$trees$n <- nrow(tree_definitions)


  # Provide user feedback:
  if (!x$params$quiet$fin) {

    n_trees <- x$trees$n
    cur_algorithm  <- x$params$algorithm
    cur_goal.chase <- x$params$goal.chase

    # msg <- paste0("Successfully created ", n_trees, " FFTs with '", cur_algorithm, "' algorithm.\n")
    # cat(u_f_fin(msg))

    cli::cli_alert_success("Created {n_trees} FFT{?s} with '{cur_algorithm}' algorithm (chasing '{cur_goal.chase}').")

  }



  # Output: ----

  return(x)

} # fftrees_grow_fan().


# ToDo: ------

# - implement stopping.rule = "statdelta"

# eof.
