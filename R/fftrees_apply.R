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
#' @param allNA.pred What should be predicted if all cue values in tree are \code{NA} (as logical)?
#' Default: \code{allNA.pred = FALSE}.
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
                          allNA.pred = FALSE) {

  # Prepare: ------

  # Verify inputs: ----

  testthat::expect_s3_class(x, class = "FFTrees")
  testthat::expect_true(mydata %in% c("train", "test"))


  # Provide user feedback: ----

  if (!x$params$quiet) {
    msg <- paste0("Aiming to apply FFTs to '", mydata, "' data:\n")
    cat(u_f_ini(msg))
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
  tree_defs <- get_fft_definitions(x = x)  # df (using helper fn)
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
      cost_decision = rep(NA, criterion_n),
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
      cue_values <- data[[cue_i]]

      cur_class <- substr(class(cue_values), 1, 1)

      # print(paste0("class_i = ", class_i, "; cur_class = ", cur_class)) # 4debugging

      if (cur_class != class_i){
        warning(paste0("Mismatch: class_i = ", class_i, "; cur_class = ", cur_class))
      }

      decisions_df$current_cue_values <- cue_values


      # threshold_i:

      if (is.character(threshold_i)) {
        threshold_i <- unlist(strsplit(threshold_i, ","))
      }

      if (substr(class_i, 1, 1) %in% c("n", "i")) {
        threshold_i <- as.numeric(threshold_i)
      }


      # current_decision:

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


      # classify_now:

      if (isTRUE(all.equal(exit_i, 0))) {
        classify_now <- decisions_df$current_decision == FALSE & is.na(decisions_df$decision)
      }
      if (isTRUE(all.equal(exit_i, 1))) {
        classify_now <- decisions_df$current_decision == TRUE & is.na(decisions_df$decision)
      }
      if (isTRUE(all.equal(exit_i, .5))) {
        classify_now <- is.na(decisions_df$decision)
      }


      # Handle NAs: ----

      # If this is NOT the final node, then don't classify NA cases:
      if (exit_i %in% c(0, 1)) {
        classify_now[is.na(classify_now)] <- FALSE
      }

      # [was:] If this IS the final node, then classify NA cases into the most common class [?: seems not done here]

      # If this IS the final node, then classify NA cases according to allNA.pred value:
      if (exit_i %in% .5) {
        decisions_df$current_decision[is.na(decisions_df$current_decision)] <- allNA.pred
      }

      # ToDo: Examine alternative policies for indecision / doxastic abstention:
      # - predict either TRUE or FALSE (according to allNA.pred)
      # - predict the most common category (overall baseline or baseline at this level)
      # - predict a 3rd category (tertium datur: abstention / "don't know" / NA decision)
      # Results will depend on costs of errors.


      # Define critical values for current decisions: ----

      decisions_df$decision[classify_now] <- decisions_df$current_decision[classify_now]
      decisions_df$levelout[classify_now] <- level_i
      decisions_df$cost_cue[classify_now] <- cost_cue_level_cum[level_i]

      decisions_df$cost_decision[decisions_df$criterion == TRUE  & decisions_df$decision == TRUE]  <- x$params$cost.outcomes$hi
      decisions_df$cost_decision[decisions_df$criterion == FALSE & decisions_df$decision == TRUE]  <- x$params$cost.outcomes$fa
      decisions_df$cost_decision[decisions_df$criterion == TRUE  & decisions_df$decision == FALSE] <- x$params$cost.outcomes$mi
      decisions_df$cost_decision[decisions_df$criterion == FALSE & decisions_df$decision == FALSE] <- x$params$cost.outcomes$cr

      decisions_df$cost <- decisions_df$cost_cue + decisions_df$cost_decision


      # Get cumulative level stats: ----

      non_na_decision_ix <- !is.na(decisions_df$decision)

      my_level_stats_i <- classtable(
        prediction_v = decisions_df$decision[non_na_decision_ix],
        criterion_v = decisions_df$criterion[non_na_decision_ix],
        #
        sens.w = x$params$sens.w,
        #
        cost.outcomes = x$params$cost.outcomes,              # outcome cost (per outcome type)
        cost_v = decisions_df$cost_cue[non_na_decision_ix],  # cue cost (per decision at level)
        #
        my.goal = x$params$my.goal,
        my.goal.fun = x$params$my.goal.fun
      )

      # level_stats_i$costc <- sum(cost_cue[,tree_i], na.rm = TRUE)
      level_stats_i[level_i, critical_stats_v] <- my_level_stats_i[ , critical_stats_v]


      # Add cue cost and total cost: ----

      level_stats_i$cost_cue[level_i] <- mean(decisions_df$cost_cue[non_na_decision_ix])
      level_stats_i$cost[level_i]     <- level_stats_i$cost_cue[level_i] + level_stats_i$cost_dec[level_i]

    } # Loop 2: level_i.


    # Add final tree results to level_stats_ls and decisions_ls: ----

    level_stats_ls[[tree_i]] <- level_stats_i

    decisions_ls[[tree_i]] <- decisions_df[, names(decisions_df) %in% c("current_decision", "current_cue_values") == FALSE]

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

  if (!x$params$quiet) {
    msg <- paste0("Successfully applied FFTs to '", mydata, "' data.\n")
    cat(u_f_fin(msg))
  }


  # Output: ------

  return(x)

} # fftrees_apply().

# eof.
