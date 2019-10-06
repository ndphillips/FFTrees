#' Applies a fast-and-frugal tree to a dataset and generates several accuracy statistics
#'
#' @param x FFTrees.
#' @param allNA.pred logical. What should be predicted if all cue values in tree are NA? Default is FALSE
#' @importFrom testthat expect_true
#' @return A list of length 4 containing
#'
#'

fftrees_apply <- function(x,
                          mydata = NULL,
                          newdata = NULL,
                          allNA.pred = FALSE
) {


  testthat::expect_true(mydata %in% c("train", "test"))

  if(mydata == "train") {

    data <- x$data$train

  } else if (mydata == "test") {

    if(is.null(newdata)) {

    testthat::expect_true(!is.null(x$data$test))

    } else {

      x$data$test <- newdata

    }

    data <- x$data$test

  }

  criterion_v <- data[[x$metadata$criterion_name]]

  # Setup outputs

  #  [output_ls]
  #    A list containing dataframes with one row per case, and one column per tree

  output_names <- c("decision", "levelout", "cost_decisions", "cost_cues", "cost")

  output_ls <- lapply(1:length(output_names), FUN = function(i) {

    output <- as.data.frame(matrix(NA,
                                   nrow = nrow(data),
                                   ncol = x$trees$n))

    names(output) <- paste("tree", 1:x$trees$n, sep = ".")

    output <- tibble::as_tibble(output)

    output

  })

  names(output_ls) <- output_names

  # [level_stats_ls]
  #   A list with one element per tree, each containing cumulative level statistics

  level_stats_ls <- vector("list", length = x$trees$n)

  # LOOP
  #  Loop over trees

  for(tree_i in 1:x$trees$n) {

    # Extract defintions for current tree
    cue_v <- unlist(strsplit(x$trees$definitions$cues[tree_i], ";"))
    class_v <- unlist(strsplit(x$trees$definitions$classes[tree_i], ";"))
    exit_v <- unlist(strsplit(x$trees$definitions$exits[tree_i], ";"))
    threshold_v <- unlist(strsplit(x$trees$definitions$thresholds[tree_i], ";"))
    direction_v <-  unlist(strsplit(x$trees$definitions$directions[tree_i], ";"))
    level_n <- x$trees$definitions$nodes[tree_i]


    costc.level <- sapply(cue_v, FUN = function(cue_i) {

      if(cue_i %in% names(x$params$cost.cues)) {cost.cue_i <- x$params$cost.cues[[cue_i]]} else {

        cost.cue_i <- 0}})

    costc.level.cum <- cumsum(costc.level)

    cue_cost_cum_level <- data.frame(level = 1:level_n,
                                     cue_cost_cum = costc.level.cum)

    # Define vectors of critical outputs for each case

    decision_v <- rep(NA, nrow(data))
    levelout_v <- rep(NA, nrow(data))
    cost_cues_v <-  rep(NA, nrow(data))
    cost_decisions_v <-  rep(NA, nrow(data))
    cost_v <-  rep(NA, nrow(data))

    # level_stats_i contains cumulative level statistics
    level_stats_i <- data.frame(tree = tree_i,
                                level = 1:level_n,
                                cue = cue_v,
                                class = class_v,
                                threshold = threshold_v,
                                direction = direction_v,
                                exit = exit_v,
                                stringsAsFactors = FALSE)

    critical_stats_v <- c("n", "hi", "fa", "mi", "cr", "sens", "spec", "far", "ppv", "npv", "acc", "bacc", "wacc", "cost_decisions")

    # Add stat names to level_stats_i
    level_stats_i[critical_stats_v] <- NA
    level_stats_i$cost_cues <- NA


    # Loop over levels
    for(level_i in 1:level_n) {

      # Get definitions for current level
      cue_i <- cue_v[level_i]
      class_i <- class_v[level_i]
      direction_i <- direction_v[level_i]
      exit_i <- as.numeric(exit_v[level_i])
      threshold_i <- threshold_v[level_i]


      # Determine which cases are classified / unclassified
      unclassified.cases <- which(is.na(decision_v))
      classified.cases <- which(is.na(decision_v) == FALSE)

      cue_values <- data[[cue_i]]

      if(is.character(threshold_i)) {threshold_i <- unlist(strsplit(threshold_i, ","))}

      if(substr(class_i, 1, 1) %in% c("n", "i")) {threshold_i <- as.numeric(threshold_i)}

      if(direction_i == "!=") {current.decisions <- (cue_values %in% threshold_i) == FALSE}
      if(direction_i == "=") {current.decisions <- cue_values %in% threshold_i}
      if(direction_i == "<") {current.decisions <- cue_values < threshold_i}
      if(direction_i == "<=") {current.decisions <- cue_values <= threshold_i}
      if(direction_i == ">") {current.decisions <- cue_values > threshold_i}
      if(direction_i == ">=") {current.decisions <- cue_values >= threshold_i}

      if(exit_i == 0) {classify.now <- current.decisions == FALSE & is.na(decision_v)}
      if(exit_i == 1) {classify.now <- current.decisions == TRUE & is.na(decision_v)}
      if(exit_i == .5) {classify.now <- is.na(decision_v)}

      # Convert NAs

      # If it is not the final node, then don't classify NA cases
      if(exit_i %in% c(0, 1)) {classify.now[is.na(classify.now)] <- FALSE}


      #If it is the final node, then classify NA cases according to most common class
      if(exit_i %in% .5) {

        current.decisions[is.na(current.decisions)] <- allNA.pred

      }


      # Define critical values for current decisions

      decision_v[classify.now] <- current.decisions[classify.now]
      levelout_v[classify.now] <- level_i
      cost_cues_v[classify.now] <- costc.level.cum[level_i]

      cost_decisions_v[current.decisions[classify.now] == TRUE & criterion_v[classify.now] == TRUE] <- x$params$cost.outcomes$hi
      cost_decisions_v[current.decisions[classify.now] == TRUE & criterion_v[classify.now] == FALSE] <- x$params$cost.outcomes$fa
      cost_decisions_v[current.decisions[classify.now] == FALSE & criterion_v[classify.now] == TRUE] <- x$params$cost.outcomes$mi
      cost_decisions_v[current.decisions[classify.now] == FALSE & criterion_v[classify.now] == FALSE] <- x$params$cost.outcomes$cr

      cost_v[classify.now] <- cost_cues_v[classify.now] + cost_decisions_v[classify.now]


      # Get cumulative level stats

      my_level_stats_i <- FFTrees:::classtable(prediction_v = decision_v[levelout_v <= level_i & is.finite(levelout_v)],
                                               criterion_v = criterion_v[levelout_v <= level_i & is.finite(levelout_v)],
                                               target = x$metadata$target,
                                               sens.w = x$params$sens.w,
                                               cost.v = cost_cues_v[levelout_v <= level_i & is.finite(levelout_v)],
                                               cost.outcomes = x$params$cost.outcomes)


      # level_stats_i$costc <- sum(cost_cues[,tree_i], na.rm = TRUE)
      level_stats_i[level_i, critical_stats_v] <- my_level_stats_i[,critical_stats_v]

      # Add cue cost and cost

      level_stats_i$cost_cues[level_i] <- mean(cost_cues_v[!is.na(cost_cues_v)])
      level_stats_i$cost[level_i] <-level_stats_i$cost_cues[level_i]  + level_stats_i$cost_decisions[level_i]

    }

    # Add final tree results to level_stats_ls and output_ls

    level_stats_ls[[tree_i]] <- level_stats_i

    output_ls$decision[,tree_i] <- decision_v
    output_ls$levelout[,tree_i] <- levelout_v
    output_ls$cost_cues[,tree_i] <- cost_cues_v
    output_ls$cost_decisions[,tree_i] <- cost_decisions_v
    output_ls$cost[,tree_i] <- cost_v


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
    tree_stats <- cbind(x$trees$definitions[,c("tree")], level_stats[helper %in% maxlevs, c(critical_stats_v, "cost_cues", "cost")])
    names(tree_stats)[1] <- "tree"
    rownames(tree_stats) <- 1:nrow(tree_stats)

    # Add pci to treestats
    #   pci is the number of cues looked up for each case divided by the maximum possible

    n.lookups <- colSums(output_ls$levelout)
    max.lookups <- nrow(data) * ncol(data)

    tree_stats$pci <- 1 - n.lookups / max.lookups

    # Add mean cues per case (mcu)
    tree_stats$mcu <- colMeans(output_ls$levelout)

  }

  # Add results to x -------------------------

  x$trees$results[[mydata]]$stats <- tree_stats
  x$trees$results[[mydata]]$level_stats <- level_stats
  x$trees$results[[mydata]]$decisions <- output_ls$decision
  x$trees$results[[mydata]]$levelout <- output_ls$levelout
  x$trees$results[[mydata]]$cost_decisions <- output_ls$cost_decisions
  x$trees$results[[mydata]]$cost_cues <- output_ls$cost_cues
  x$trees$results[[mydata]]$cost <- output_ls$cost

  return(x)

}
