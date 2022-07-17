#' Applies a fast-and-frugal tree to a dataset and generates several accuracy statistics
#'
#' @param x FFTrees.
#' @param mydata dataframe.
#' @param newdata dataframe.
#' @param allNA.pred logical. What should be predicted if all cue values in tree are NA? Default is FALSE
#' @importFrom testthat expect_true
#' @return A list of length 4 containing
#'
#' @export
#' @keywords internal
#'
#'

fftrees_apply <- function(x,
                          mydata = NULL,
                          newdata = NULL,
                          allNA.pred = FALSE) {
  testthat::expect_true(mydata %in% c("train", "test"))

  if (mydata == "train") {
    data <- x$data$train
  } else if (mydata == "test") {
    if (is.null(newdata)) {
      testthat::expect_true(!is.null(x$data$test))
    } else {
      x$data$test <- newdata
    }

    data <- x$data$test
  }

  criterion_v <- data[[x$criterion_name]]
  criterion_n <- length(criterion_v)

  # Setup outputs

  #  [decisions_ls]
  #    A list containing dataframes with one row per case, and one column per tree

  decisions_ls <- lapply(1:x$trees$n, FUN = function(i) {
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

  names(decisions_ls) <- paste0("tree_", 1:x$trees$n)

  # [level_stats_ls]
  #   A list with one element per tree, each containing cumulative level statistics

  level_stats_ls <- vector("list", length = x$trees$n)

  # LOOP
  #  Loop over trees

  for (tree_i in 1:x$trees$n) {

    # Extract defintions for current tree
    cue_v <- unlist(strsplit(x$trees$definitions$cues[tree_i], ";"))
    class_v <- unlist(strsplit(x$trees$definitions$classes[tree_i], ";"))
    exit_v <- unlist(strsplit(x$trees$definitions$exits[tree_i], ";"))
    threshold_v <- unlist(strsplit(x$trees$definitions$thresholds[tree_i], ";"))
    direction_v <- unlist(strsplit(x$trees$definitions$directions[tree_i], ";"))
    level_n <- x$trees$definitions$nodes[tree_i]

    decisions_df <- decisions_ls[[tree_i]]


    costc.level <- sapply(cue_v, FUN = function(cue_i) {
      if (cue_i %in% names(x$params$cost.cues)) {
        cost.cue_i <- x$params$cost.cues[[cue_i]]
      } else {
        cost.cue_i <- 0
      }
    })

    costc.level.cum <- cumsum(costc.level)

    cue_cost_cum_level <- data.frame(
      level = 1:level_n,
      cue_cost_cum = costc.level.cum
    )


    # level_stats_i contains cumulative level statistics
    level_stats_i <- data.frame(
      tree = tree_i,
      level = 1:level_n,
      cue = cue_v,
      class = class_v,
      threshold = threshold_v,
      direction = direction_v,
      exit = exit_v,
      stringsAsFactors = FALSE
    )

    critical_stats_v <- c("n", "hi", "fa", "mi", "cr", "sens", "spec", "far", "ppv", "npv", "acc", "bacc", "wacc", "cost_decisions")

    # Add stat names to level_stats_i
    level_stats_i[critical_stats_v] <- NA
    level_stats_i$cost_cues <- NA

    # Loop over levels
    for (level_i in 1:level_n) {

      # Get definitions for current level
      cue_i <- cue_v[level_i]
      class_i <- class_v[level_i]
      direction_i <- direction_v[level_i]
      exit_i <- as.numeric(exit_v[level_i])
      threshold_i <- threshold_v[level_i]

      cue_values <- data[[cue_i]]

      decisions_df$current_cue_values <- cue_values


      if (is.character(threshold_i)) {
        threshold_i <- unlist(strsplit(threshold_i, ","))
      }

      if (substr(class_i, 1, 1) %in% c("n", "i")) {
        threshold_i <- as.numeric(threshold_i)
      }

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

      if (isTRUE(all.equal(exit_i, 0))) {
        classify.now <- decisions_df$current_decision == FALSE & is.na(decisions_df$decision)
      }
      if (isTRUE(all.equal(exit_i,  1))) {
        classify.now <- decisions_df$current_decision == TRUE & is.na(decisions_df$decision)
      }
      if (isTRUE(all.equal(exit_i, .5))) {
        classify.now <- is.na(decisions_df$decision)
      }

      # Convert NAs

      # If it is not the final node, then don't classify NA cases
      if (exit_i %in% c(0, 1)) {
        classify.now[is.na(classify.now)] <- FALSE
      }


      # If it is the final node, then classify NA cases according to most common class
      if (exit_i %in% .5) {
        decisions_df$current_decision[is.na(decisions_df$current_decision)] <- allNA.pred
      }


      # Define critical values for current decisions

      decisions_df$decision[classify.now] <- decisions_df$current_decision[classify.now]
      decisions_df$levelout[classify.now] <- level_i
      decisions_df$cost_cue[classify.now] <- costc.level.cum[level_i]

      decisions_df$cost_decision[decisions_df$criterion == TRUE & decisions_df$decision == TRUE] <- x$params$cost.outcomes$hi
      decisions_df$cost_decision[decisions_df$criterion == TRUE & decisions_df$decision == FALSE] <- x$params$cost.outcomes$mi
      decisions_df$cost_decision[decisions_df$criterion == FALSE & decisions_df$decision == TRUE] <- x$params$cost.outcomes$fa
      decisions_df$cost_decision[decisions_df$criterion == FALSE & decisions_df$decision == FALSE] <- x$params$cost.outcomes$cr


      decisions_df$cost <- decisions_df$cost_cue + decisions_df$cost_decision

      # Get cumulative level stats

      my_level_stats_i <- classtable(
        prediction_v = decisions_df$decision[!is.na(decisions_df$decision)],
        criterion_v = decisions_df$criterion[!is.na(decisions_df$decision)],
        sens.w = x$params$sens.w,
        cost.v = decisions_df$cost_cue[!is.na(decisions_df$decision)],
        cost.outcomes = x$params$cost.outcomes
      )


      # level_stats_i$costc <- sum(cost_cues[,tree_i], na.rm = TRUE)
      level_stats_i[level_i, critical_stats_v] <- my_level_stats_i[, critical_stats_v]

      # Add cue cost and cost

      level_stats_i$cost_cues[level_i] <- mean(decisions_df$cost_cue[!is.na(decisions_df$decision)])
      level_stats_i$cost[level_i] <- level_stats_i$cost_cues[level_i] + level_stats_i$cost_decisions[level_i]
    }

    # Add final tree results to level_stats_ls and decisions_ls

    level_stats_ls[[tree_i]] <- level_stats_i

    decisions_ls[[tree_i]] <- decisions_df[, names(decisions_df) %in% c("current_decision", "current_cue_values") == FALSE]
  }

  # Aggregate results
  {

    # Combine all levelstats into one dataframe
    level_stats <- do.call("rbind", args = level_stats_ls)

    # [tree_stats]
    #  One row per tree definitions and statistics
    # CUMULATIVE TREE STATS

    helper <- paste(level_stats$tree, level_stats$level, sep = ".")
    maxlevs <- paste(rownames(tapply(level_stats$level, level_stats$tree, FUN = which.max)), tapply(level_stats$level, level_stats$tree, FUN = which.max), sep = ".")
    tree_stats <- cbind(x$trees$definitions[, c("tree")], level_stats[helper %in% maxlevs, c(critical_stats_v, "cost_cues", "cost")])
    names(tree_stats)[1] <- "tree"
    rownames(tree_stats) <- 1:nrow(tree_stats)


    # Calculate pci and mcu

    for (tree_i in 1:x$trees$n) {
      max.lookups <- nrow(data) * length(x$cue_names)
      n.lookups <- sum(decisions_ls[[tree_i]]$levelout)

      tree_stats$pci[tree_i] <- 1 - n.lookups / max.lookups
      tree_stats$mcu[tree_i] <- mean(decisions_ls[[tree_i]]$levelout)
    }
  }

  # Add results to x -------------------------

  x$trees$stats[[mydata]] <- tree_stats
  x$trees$level_stats[[mydata]] <- level_stats
  x$trees$decisions[[mydata]] <- decisions_ls

  return(x)
}
