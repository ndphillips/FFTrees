#' Apply an FFT to data and generate accuracy statistics
#'
#' @description \code{fftrees_apply} applies a fast-and-frugal tree (FFT, as an \code{FFTrees} object)
#' to a dataset (of type \code{mydata}) and generates corresponding accuracy statistics
#' (on cue levels and for trees).
#'
#' \code{fftrees_apply} is called internally by the main \code{\link{FFTrees}} function
#' (with \code{mydata = "train"} and --- if test data exists --- \code{mydata = "test"}).
#' Alternatively, \code{fftrees_apply} is called when predicting outcomes for new data
#' by \code{\link{predict.FFTrees}}.
#'
#' @param x An object with FFT definitions which are to be applied to current data (as an \code{FFTrees} object).
#' @param mydata The type of data to which the FFT should be applied (as character, either \code{"train"} or \code{"test"}).
#' @param newdata New data to which an FFT should be applied (as a data frame).
#'
#' @param fin_NA_pred What outcome should be predicted if the \emph{final} node in a tree has a cue value of \code{NA}
#' (as character)? Valid options are:
#' \describe{
#'   \item{'noise'}{predict \code{FALSE} (0/left/signal) for all corresponding cases}
#'   \item{'signal'}{predict \code{TRUE} (1/right/noise) for all corresponding cases}
#'   \item{'majority'}{predict the more common criterion value (i.e., \code{TRUE} if base rate \code{p(TRUE) > .50} in 'train' data) for all corresponding cases}
#'   \item{'baseline'}{flip a random coin that is biased by the criterion baseline \code{p(TRUE)} (in 'train' data) for all corresponding cases}
#'   \item{'dnk'}{yet ToDo: abstain from classifying / decide to 'do not know' / defer (i.e., tertium datur)}
#'   }
#' Default: \code{fin_NA_pred = "majority"}.
#'
#' @return A modified \code{FFTrees} object (with lists in \code{x$trees} containing information on FFT decisions and statistics).
#'
#' @keywords internal
#'
#' @seealso
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @importFrom testthat expect_true
#' @importFrom tibble as_tibble tibble
#'
#' @export

fftrees_apply <- function(x,
                          mydata = NULL,   # data type (either "train" or "test")
                          newdata = NULL,
                          #
                          fin_NA_pred = "majority"  # Options available: c("noise", "signal", "baseline", "majority")
) {

  # Prepare: ------

  # Verify inputs: ----

  testthat::expect_s3_class(x, class = "FFTrees")
  testthat::expect_true(mydata %in% c("train", "test"))


  # Provide user feedback: ----

  if (!x$params$quiet$ini) {

    # msg <- paste0("Aiming to apply FFTs to '", mydata, "' data:\n")
    # cat(u_f_ini(msg))

    n_trees <- x$trees$n
    cli::cli_alert("Apply {n_trees} FFT{?s} to '{mydata}' data:", class = "alert-start")
  }


  # Get data (corresponding to mydata and newdata): ----

  if (mydata == "train") {

    data <- x$data$train

  } else if (mydata == "test") {

    if (is.null(newdata)) {

      testthat::expect_true(!is.null(x$data$test))

    } else {

      # Replace existing test data in x by newdata:

      x$data$test <- newdata

    }

    verify_train_test_data(train_data = x$data$train, test_data = x$data$test)  # verify (without consequences)

    data <- x$data$test

  }

  # Current criterion values from data (as df):
  criterion_v <- data[[x$criterion_name]]
  criterion_n <- length(criterion_v)

  # Simplify: ----

  # Extract key parts from FFTrees object x:
  n_trees <- x$trees$n
  # tree_defs <- x$trees$definition  # df (from object x)
  tree_defs <- get_fft_df(x = x)  # df (using helper fn)
  # print(tree_defs)  # 4debugging


  # Setup outputs: ------

  #  1. [decisions_ls]: ----
  #     A list containing tibbles, with 1 element per tree and 1 row per case:

  decisions_ls <- lapply(1:n_trees, FUN = function(i) {

    tibble::tibble(
      criterion = criterion_v,
      decision = rep(NA, criterion_n),
      levelout = rep(NA, criterion_n),
      cost_cue = rep(NA, criterion_n),
      cost_dec = rep(NA, criterion_n),
      cost = rep(NA, criterion_n),
      current_decision = rep(NA, criterion_n)
    )

  })

  names(decisions_ls) <- paste0("tree_", 1:n_trees)

  #  2. [level_stats_ls]: ----
  #     A list with 1 element per tree, each containing cumulative level statistics:

  level_stats_ls <- vector("list", length = n_trees)


  # 3. Critical stats [critical_stats_v]: ----
  #    Define the set of critical stats (as vector):

  if (!is.null(x$params$my.goal)){ # include my.goal:

    critical_stats_v <- c(
      # freq:
      "n",  "hi", "fa", "mi", "cr",
      # cond prob:
      "sens", "spec",  "far",  "ppv", "npv",
      # from prob:
      "dprime",
      # accuracy:
      "acc", "bacc", "wacc",
      # my.goal:
      x$params$my.goal,       # include my.goal (name and value)
      # costs:
      "cost_dec"  # Note: "cost_cue" and "cost" are added below.
    )

  } else { # set critical stats default:

    critical_stats_v <- c(
      # freq:
      "n",  "hi", "fa", "mi", "cr",
      # cond prob:
      "sens", "spec",  "far",  "ppv", "npv",
      # from prob:
      "dprime",
      # accuracy:
      "acc", "bacc", "wacc",
      # my.goal:                    (NO my.goal here)
      # costs:
      "cost_dec"  # Note: "cost_cue" and "cost" are added below.
    )

  }


  # Handle NA values: ------

  if ( allow_NA_pred | allow_NA_crit ){

    if (any(is.na(data))){ # IFF there are NA cases in data:

      # Compute only ONCE (before loop):
      if ( (fin_NA_pred == "baseline") | (fin_NA_pred == "majority") ){

        criterion_name <- x$criterion_name

        # Compute criterion baseline/base rate (for "train" data ONLY):
        if (allow_NA_crit){
          crit_br <- mean(x$data[["train"]][[criterion_name]], na.rm = TRUE)
        } else { # default:
          crit_br <- mean(x$data[["train"]][[criterion_name]])  # (from logical, i.e., proportion of TRUE values)
        }

        crit_br <- round(crit_br, 3)  # rounding

      } # if fin_NA_pred().

    } # if (any(is.na(data))).

    # Initialize some counters:
    nr_NA_lvl <- 0

    NA_hi <- 0
    NA_fa <- 0
    NA_mi <- 0
    NA_cr <- 0

  } # Handle NA: if ( allow_NA_pred | allow_NA_crit ).



  # LOOPs: ------

  #  Loop 1 (over trees): ----

  for (tree_i in 1:n_trees) {

    # print(paste0("tree ", tree_i, ":"))  # 4debugging

    # NEW code start: ----

    # Get ID of tree_defs$tree for tree_i value (to consider all trees in turn):
    tree_i_id <- tree_defs$tree[tree_i]
    # print(paste0("\u2014 Current tree_i = ", tree_i, " corresponds to tree_i_id = ", tree_i_id)) # 4debugging

    # print(tree_defs)  # 4debugging

    # Read FFT definition (with 1 row per tree) into df (with 1 row per node):
    cur_fft_df <- read_fft_df(ffts_df = tree_defs, tree = tree_i_id)
    # print(cur_fft_df)  # 4debugging

    # Get variables of cur_fft_df (as vectors):
    class_v     <- cur_fft_df$class
    cue_v       <- cur_fft_df$cue
    direction_v <- cur_fft_df$direction
    threshold_v <- cur_fft_df$threshold
    exit_v      <- cur_fft_df$exit

    # NEW code end. ----

    # +++ here now +++

    # # OLD code start: ----
    #
    # # Extract definition of current tree:
    # class_o     <- trimws(unlist(strsplit(tree_defs$classes[tree_i], ";")))
    # cue_o       <- trimws(unlist(strsplit(tree_defs$cues[tree_i], ";")))
    # direction_o <- trimws(unlist(strsplit(tree_defs$directions[tree_i], ";")))
    # threshold_o <- trimws(unlist(strsplit(tree_defs$thresholds[tree_i], ";")))
    # exit_o      <- trimws(unlist(strsplit(tree_defs$exits[tree_i], ";")))
    #
    # # Check: Verify equality of OLD and NEW code results:
    # if (!all.equal(class_o, class_v)) { stop("OLD vs. NEW: class diff") }
    # if (!all.equal(cue_o, cue_v)) { stop("OLD vs. NEW: cue diff") }
    # if (!all.equal(direction_o, direction_v)) { stop("OLD vs. NEW: direction diff") }
    # if (!all.equal(threshold_o, threshold_v)) { stop("OLD vs. NEW: threshold diff") }
    # if (!all.equal(exit_o, exit_v)) { stop("OLD vs. NEW: exit diff") }
    #
    # # OLD code end. ----


    # Verify current tree definition:
    verify_all_cues_in_data(cue_v, data)  # Do all cues occur (as names) in current data?

    level_n <- as.integer(tree_defs$nodes[tree_i])
    # print(paste0("- level_n = ", level_n))  # 4debugging

    decisions_df <- decisions_ls[[tree_i]]

    # costs:
    cost_cue_level <- sapply(cue_v, FUN = function(cue_i) {

      if (cue_i %in% names(x$params$cost.cues)) {
        cost_cue_i <- x$params$cost.cues[[cue_i]]
      } else {
        cost_cue_i <- 0
      }

    }
    )
    # print(cost_cue_level)  # 4debugging

    cost_cue_level_cum <- cumsum(cost_cue_level)
    # print(paste0("- cost_cue_level_cum = ", cost_cue_level_cum))  # 4debugging

    # as df (NOT USED anwywhere???):
    cue_cost_cum_level <- data.frame(
      level = 1:level_n,
      cue_cost_cum = cost_cue_level_cum
    )


    # Prepare data structures: ------

    # level_stats_i collect cumulative level statistics: ----
    level_stats_i <- data.frame(
      tree = tree_i,     # NOTE: Assumes that current tree ID tree_i_id == counter variable tree_i.
      # tree = tree_i_id,  # NOTE: The current tree ID is tree_i_id, NOT the counter variable tree_i.
      level = 1:level_n,
      cue = cue_v,
      class = class_v,
      threshold = threshold_v,
      direction = direction_v,
      exit = exit_v,
      stringsAsFactors = FALSE
    )

    # Add stats names to level_stats_i: ----
    level_stats_i[critical_stats_v] <- NA
    level_stats_i$cost_cue <- NA


    # Loop 2 (over levels): ----

    for (level_i in 1:level_n) {

      # Get tree definition at current level:
      cue_i       <- cue_v[level_i]
      class_i     <- class_v[level_i]
      direction_i <- direction_v[level_i]
      exit_i      <- as.numeric(exit_v[level_i])
      threshold_i <- threshold_v[level_i]

      # Current cue values from data (as df):
      cue_values <- as.vector(data[[cue_i]])  # as.vector() turns "matrix" "array" into (numeric) vector
      cur_class  <- substr(class(cue_values), 1, 1)

      # print(paste0("class_i = ", class_i))  # 4debugging
      # print(paste0("cur_class = ", cur_class))

      if (cur_class != class_i){
        warning(paste0("Mismatch: class_i = ", class_i, "; cur_class = ", cur_class))
      }

      decisions_df$current_cue_values <- cue_values


      # threshold_i: ----

      if (is.character(threshold_i)) {
        threshold_i <- unlist(strsplit(threshold_i, ","))
      }

      if (substr(class_i, 1, 1) %in% c("n", "i")) {
        threshold_i <- as.numeric(threshold_i)
      }


      # current_decision: ----

      if (direction_i == "!=") {
        decisions_df$current_decision <- (decisions_df$current_cue_values %in% threshold_i) == FALSE
      }
      if (direction_i == "=") {
        decisions_df$current_decision <- decisions_df$current_cue_values %in% threshold_i
      }
      if (direction_i == "<") {
        decisions_df$current_decision <- decisions_df$current_cue_values < threshold_i
      }
      if (direction_i == "<=") {
        decisions_df$current_decision <- decisions_df$current_cue_values <= threshold_i
      }
      if (direction_i == ">") {
        decisions_df$current_decision <- decisions_df$current_cue_values > threshold_i
      }
      if (direction_i == ">=") {
        decisions_df$current_decision <- decisions_df$current_cue_values >= threshold_i
      }


      # classify_now: ----

      if (isTRUE(all.equal(exit_i, exit_types[1]))) { # 1: exit_i 0:
        classify_now <- (decisions_df$current_decision == FALSE) & is.na(decisions_df$decision) # FALSE or NA
      }
      if (isTRUE(all.equal(exit_i, exit_types[2]))) { # 2: exit_i 1:
        classify_now <- (decisions_df$current_decision == TRUE) & is.na(decisions_df$decision)  # FALSE or NA
      }
      if (isTRUE(all.equal(exit_i, exit_types[3]))) { # 3: exit_i .5:
        classify_now <- is.na(decisions_df$decision)  # TRUE for NA only
      }


      # Handle NA values: ------

      if ( allow_NA_pred | allow_NA_crit ){

        # Goal: Deal with NA cases at a tree node.

        # 1. If this is an intermediate / NOT the final node, then don't classify NA cases:
        if (exit_i %in% exit_types[1:2]) {  # exit_types in c(0, 1)

          # NAs on current level (based on classify_now):
          ix_NA_classify_now <- is.na(classify_now)
          nr_NA_lvl <- sum(ix_NA_classify_now)

          if (any(ix_NA_classify_now)){ # IFF there ARE NA cases:

            # Assign:
            classify_now[ix_NA_classify_now] <- FALSE  # Do NOT classify NA cases (which differs from "classify as FALSE")!

            # Classify and count outcomes for NA cases (i.e., sub-2x2 matrix for NA cases):
            NA_hi <- NA  # not classified = no outcome
            NA_fa <- NA  # not classified = no outcome
            NA_mi <- NA  # not classified = no outcome
            NA_cr <- NA  # not classified = no outcome

            if (!x$params$quiet$mis) { # Provide user feedback:

              cli::cli_alert_warning("Tree {tree_i} node {level_i}: Seeing {nr_NA_lvl} NA value{?s} in intermediate cue '{cue_i}' and proceed.")

            }

          } # if any(ix_NA_classify_now).

        } # if (intermediate exit).


        # 2. If this IS the final / terminal node, then classify all NA cases according to fin_NA_pred:
        if (exit_i %in% exit_types[3]) {  # exit_types = .5:

          # NAs on current level (based on current_decision):
          ix_NA_current_decision <- is.na(decisions_df$current_decision)
          nr_NA_lvl <- sum(ix_NA_current_decision)

          if (any(ix_NA_current_decision)){ # IFF there ARE NA cases:

            # Classify NA cases (in final node):

            fin_NA_decisions <- rep(NA, nr_NA_lvl)  # initialize NA decisions

            if (fin_NA_pred == "noise"){

              fin_NA_decisions <- rep(FALSE, nr_NA_lvl)  # all FALSE

            } else if (fin_NA_pred == "signal"){

              fin_NA_decisions <- rep(TRUE, nr_NA_lvl)  # all TRUE

            } else if (fin_NA_pred == "baseline"){

              # Flip baseline coin:
              fin_NA_decisions <- sample(x = c(TRUE, FALSE), size = nr_NA_lvl, replace = TRUE, prob = c(crit_br, 1 - crit_br))

            } else if (fin_NA_pred == "majority"){

              if (crit_br > .50){
                fin_NA_decisions <- rep(TRUE, nr_NA_lvl)
              } else {
                fin_NA_decisions <- rep(FALSE, nr_NA_lvl)
              }

            } else { # note unknown option:

              fin_NA_opt_s <- paste0(fin_NA_options, collapse = ", ")

              stop(paste0("The value of fin_NA_pred must be in c('", fin_NA_opt_s, "')."))

            } # if fin_NA_pred.

            # Assign final NA decisions (only ONCE):
            decisions_df$current_decision[ix_NA_current_decision] <- fin_NA_decisions

            # Get corresponding criterion values:
            fin_NA_criteria <- decisions_df$criterion[ix_NA_current_decision]

            # Classify and count outcomes for NA cases (i.e., sub-2x2 matrix for NA cases):
            NA_hi <- sum((fin_NA_criteria == TRUE)  & (fin_NA_decisions == TRUE))
            NA_fa <- sum((fin_NA_criteria == FALSE) & (fin_NA_decisions == TRUE))
            NA_mi <- sum((fin_NA_criteria == TRUE)  & (fin_NA_decisions == FALSE))
            NA_cr <- sum((fin_NA_criteria == FALSE) & (fin_NA_decisions == FALSE))

            if (!x$params$quiet$mis) { # Provide user feedback:

              if ( (fin_NA_pred == "baseline") | (fin_NA_pred == "majority") ){ # crit_br is relevant:

                cli::cli_alert_warning("Tree {tree_i} node {level_i}: Making {nr_NA_lvl} {fin_NA_pred} prediction{?s} (with a 'train' base rate p(TRUE) = {crit_br}): {fin_NA_decisions}.")

              } else { # default:

                cli::cli_alert_warning("Tree {tree_i} node {level_i}: Making {nr_NA_lvl} {fin_NA_pred} prediction{?s}: {fin_NA_decisions}.")

              }

              if (debug) { # Provide debugging feedback:

                NA_mx <- paste0(c(NA_hi, NA_fa, NA_mi, NA_cr), collapse = ", ")

                cli::cli_alert_info("Tree {tree_i} node {level_i}: {nr_NA_lvl} corresponding criterion value{?s}: {fin_NA_criteria} => (hi fa mi cr) for NA cases is ({NA_mx})")

              } # if (debug).

            } # if (!x$params$quiet$mis).

          } # if (any(ix_NA_current_decision)).

        } # if (final exit).

        # +++ here now +++

        # Done:
        #
        # - When a final cue is NA: Create the 2x2 matrix for NA cases (true criterion values x decisions made):
        #   1. hi among NA cases
        #   2. fa among NA cases
        #   3. mi among NA cases
        #   4. cr among NA cases
        #
        # Among NA cases:
        #               Criterion
        # Decision      TRUE      FALSE
        # 'true'        hi        fa
        # 'false'       mi        cr

        # ToDo:
        #
        # - When allowing for a 3rd category ("dnk" / abstention / suspension):
        #   2 new errors (as criterion still IS binary / non-contingent / knowable in principle):
        #   5. (fa): deciding for "dnk" when criterion is FALSE / missing a true FALSE
        #   6. (mi): deciding for "dnk" when criterion is TRUE
        #
        #               Criterion
        # Decision      TRUE      FALSE
        #   'true'       hi        fa
        #   'dnk'       (mi)      (fa)
        #   'false'      mi        cr

        # - Consider alternative policies for indecision / doxastic abstention:
        #   - predict the most common category OF NA CASES in training data (rather than OVERALL baseline or baseline at this level)
        #   - predict a 3rd category (tertium datur: abstention / dnk: "do not know" / NA decision)
        #   Corresponding results will depend on the costs of errors.

      } # Handle NA: if ( allow_NA_pred | allow_NA_crit ).


      # Define critical values for current decisions: ----

      decisions_df$decision[classify_now] <- decisions_df$current_decision[classify_now]
      decisions_df$levelout[classify_now] <- level_i
      decisions_df$cost_cue[classify_now] <- cost_cue_level_cum[level_i]

      decisions_df$cost_dec[decisions_df$criterion == TRUE  & decisions_df$decision == TRUE]  <- x$params$cost.outcomes$hi
      decisions_df$cost_dec[decisions_df$criterion == FALSE & decisions_df$decision == TRUE]  <- x$params$cost.outcomes$fa
      decisions_df$cost_dec[decisions_df$criterion == TRUE  & decisions_df$decision == FALSE] <- x$params$cost.outcomes$mi
      decisions_df$cost_dec[decisions_df$criterion == FALSE & decisions_df$decision == FALSE] <- x$params$cost.outcomes$cr

      decisions_df$cost <- decisions_df$cost_cue + decisions_df$cost_dec


      # Handle NA values: ------

      if ( allow_NA_pred | allow_NA_crit ){

        # Goal: Create index vector ix_non_NA_deci (to constrain the call to classtable() below).

        # Detect NA values: ----

        ix_NA_deci <- is.na(decisions_df$decision)   # 1. NA in decision
        # ix_NA_crit <- is.na(decisions_df$criterion)  # 2. NA in criterion

        nr_NA_dec <- sum(ix_NA_deci)
        # cli::cli_alert_info("Tree {tree_i}, level {level_i}: nr_NA_dec = {nr_NA_dec}, decision = {decisions_df$decision}")

        # Non-NA values:
        ix_non_NA_deci <- !ix_NA_deci

        # ToDo:
        # - Add user feedback
        # - Handle NA in criterion?

      } else { # no NA handling:

        nr_NA_dec <- NA  # (as NA handling not enabled)

        ix_non_NA_deci <- rep(TRUE, length(decisions_df$decision))  # use ALL elements

      } # Handle NA: if ( allow_NA_pred | allow_NA_crit ).


      # Get cumulative level stats: ------

      my_level_stats_i <- classtable(
        prediction_v = decisions_df$decision[ix_non_NA_deci],
        criterion_v  = decisions_df$criterion[ix_non_NA_deci],
        #
        sens.w = x$params$sens.w,
        #
        cost.outcomes = x$params$cost.outcomes,          # outcome cost (per outcome type)
        cost_v = decisions_df$cost_cue[ix_non_NA_deci],  # cue cost (per decision at level)
        #
        my.goal = x$params$my.goal,
        my.goal.fun = x$params$my.goal.fun,
        #
        quiet_mis = x$params$quiet$mis  # passed to hide/show NA user feedback
      )

      # level_stats_i$costc <- sum(cost_cue[,tree_i], na.rm = TRUE)
      level_stats_i[level_i, critical_stats_v] <- my_level_stats_i[ , critical_stats_v]


      # Add cue cost and total cost: ----

      level_stats_i$cost_cue[level_i] <- mean(decisions_df$cost_cue[ix_non_NA_deci])
      level_stats_i$cost[level_i]     <- level_stats_i$cost_cue[level_i] + level_stats_i$cost_dec[level_i]


      # Handle NA values: ------

      if ( allow_NA_pred | allow_NA_crit ){

        # Goal: Add NA values to level stats.

        # Total NA cases in level_stats_i:
        level_stats_i$NA_cue[level_i] <- nr_NA_lvl  # nr. of NA in cue values on the current level_i

        # Details: Decision outcomes for NA cases:
        level_stats_i$NA_hi[level_i] <- NA_hi
        level_stats_i$NA_fa[level_i] <- NA_fa
        level_stats_i$NA_mi[level_i] <- NA_mi
        level_stats_i$NA_cr[level_i] <- NA_cr

        # Total of undecided cases (= data N - n):
        level_stats_i$NA_dec[level_i] <- nr_NA_dec  # nr. of NA values in decisions / indecisions on level_i

      } # Handle NA: if ( allow_NA_pred | allow_NA_crit ).


    } # Loop 2: level_i.


    # Add final tree results to level_stats_ls and decisions_ls: ----

    level_stats_ls[[tree_i]] <- level_stats_i

    decisions_ls[[tree_i]] <- decisions_df[ , names(decisions_df) %in% c("current_decision", "current_cue_values") == FALSE]

  } # Loop 1: tree_i.


  # Aggregate results: ----

  # Combine all level_stats into one data.frame:
  level_stats <- do.call("rbind", args = level_stats_ls)

  #  3. Cumulative tree stats [tree_stats]: ----
  #  One row per tree definitions and statistics:

  helper  <- paste(level_stats$tree, level_stats$level, sep = ".")
  maxlevs <- paste(rownames(tapply(level_stats$level, level_stats$tree, FUN = which.max)), tapply(level_stats$level, level_stats$tree, FUN = which.max), sep = ".")
  tree_stats <- cbind(tree_defs[ , c("tree")], level_stats[helper %in% maxlevs, c(critical_stats_v, "cost_cue", "cost")])
  names(tree_stats)[1] <- "tree"
  rownames(tree_stats) <- 1:nrow(tree_stats)


  # Compute pci and mcu: ----

  for (tree_i in 1:n_trees) {

    max_lookups <- nrow(data) * length(x$cue_names)
    n_lookups   <- sum(decisions_ls[[tree_i]]$levelout)

    tree_stats$pci[tree_i] <- 1 - (n_lookups / max_lookups)
    tree_stats$mcu[tree_i] <- mean(decisions_ls[[tree_i]]$levelout)

  }


  # Add results to x$trees (given mydata type): ----

  x$trees$stats[[mydata]]       <- tibble::as_tibble(tree_stats)
  x$trees$level_stats[[mydata]] <- tibble::as_tibble(level_stats)
  x$trees$decisions[[mydata]]   <- decisions_ls


  # Update best tree IDs: ----

  if (mydata == "train"){
    x$trees$best$train <- get_best_tree(x, data = mydata, goal = x$params$goal)
  } else if (mydata == "test"){
    x$trees$best$test <- get_best_tree(x, data = mydata, goal = x$params$goal)
  }


  # Provide user feedback: ----

  if (!x$params$quiet$fin) {

    # msg <- paste0("Successfully applied FFTs to '", mydata, "' data.\n")
    # cat(u_f_fin(msg))

    n_trees <- x$trees$n
    cli::cli_alert_success("Applied {n_trees} FFT{?s} to '{mydata}' data.")

  }


  # Output: ------

  return(x)

} # fftrees_apply().

# eof.
