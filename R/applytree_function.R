#' Applies a fast and frugal tree to a dataset.
#'
#' @param formula A formula
#' @param data dataframe. A model training dataset. An m x n dataframe containing n cue values for each of the m exemplars.
#' @param tree.definitions dataframe. Definitions of one or more trees. The dataframe must contain the columns: cues, classes, thresholds, directions, exits.
#' @param sens.w numeric.  A number from 0 to 1 indicating how to weight sensitivity relative to specificity. Only used for calculating wacc values.
#' @return A list of length 4 containing
#' @export
#' @examples
#'
#'
#'   tree.definitions <- data.frame("tree" = 1,
#'                                  "cues" = "sex;age",
#'                                  "thresholds" = "male;adult",
#'                                  "directions" = "=;="
#'                                  )
#'
#'

apply.tree <- function(data,
                       formula,
                       tree.definitions,
                       sens.w = .5
) {

  #
  # data = data.train
  # formula = formula
  # tree.definitions = tree.definitions
  #
  criterion.v <- model.frame(formula = formula,
                             data = data,
                             na.action = NULL)[,1]

  n.exemplars <- nrow(data)
  n.trees <- nrow(tree.definitions)

  levelout <- matrix(NA, nrow = n.exemplars, ncol = n.trees)
  decision <- matrix(NA, nrow = n.exemplars, ncol = n.trees)

  level.stats.ls <- vector("list", length = n.trees)

  for(tree.i in 1:n.trees) {

    cue.v <- unlist(strsplit(tree.definitions$cue[tree.i], ";"))
    class.v <- unlist(strsplit(tree.definitions$classes[tree.i], ";"))
    exit.v <- unlist(strsplit(tree.definitions$exits[tree.i], ";"))
    threshold.v <- unlist(strsplit(tree.definitions$thresholds[tree.i], ";"))
    direction.v <-  unlist(strsplit(tree.definitions$directions[tree.i], ";"))

    n.levels <- length(cue.v)

    level.stats.df.i <- data.frame(tree = tree.i,
                                   level = 1:n.levels,
                                   cue = cue.v,
                                   class = class.v,
                                   threshold = threshold.v,
                                   direction = direction.v,
                                   exit = exit.v)

    level.stats.df.i[names(classtable(1, 1))] <- NA

    for(level.i in 1:n.levels) {

      cue.i <- cue.v[level.i]
      class.i <- class.v[level.i]
      direction.i <- direction.v[level.i]
      exit.i <- as.numeric(exit.v[level.i])
      threshold.i <- threshold.v[level.i]

      cue.values <- data[[cue.i]]

      unclassified.cases <- which(is.na(decision[,tree.i]))
      classified.cases <- which(is.na(decision[,tree.i]) == FALSE)

      if(is.character(threshold.i)) {threshold.i <- unlist(strsplit(threshold.i, ","))}

      if(substr(class.i, 1, 1) %in% c("n", "i")) {threshold.i <- as.numeric(threshold.i)}

      if(direction.i == "!=") {current.decisions <- (cue.values %in% threshold.i) == FALSE}
      if(direction.i == "=") {current.decisions <- cue.values %in% threshold.i}
      if(direction.i == "<") {current.decisions <- cue.values < threshold.i}
      if(direction.i == "<=") {current.decisions <- cue.values <= threshold.i}
      if(direction.i == ">") {current.decisions <- cue.values > threshold.i}
      if(direction.i == ">=") {current.decisions <- cue.values >= threshold.i}

      if(exit.i == 0) {classify.now <- current.decisions == FALSE & is.na(decision[,tree.i])}
      if(exit.i == 1) {classify.now <- current.decisions == TRUE & is.na(decision[,tree.i])}
      if(exit.i == .5) {classify.now <- is.na(decision[,tree.i])}


      decision[classify.now, tree.i] <- current.decisions[classify.now]
      levelout[classify.now, tree.i] <- level.i

      # Get level stats

      level.i.stats <- classtable(prediction.v = decision[levelout[,tree.i] <= level.i & is.finite(levelout[,tree.i]), tree.i],
                                  criterion.v = criterion.v[levelout[,tree.i] <= level.i & is.finite(levelout[,tree.i])],
                                  sens.w = sens.w)

      level.stats.df.i[level.i, names(level.i.stats)] <- level.i.stats



    }

    level.stats.ls[[tree.i]] <- level.stats.df.i


  }




  levelstats <- do.call("rbind", args = level.stats.ls)

  # CUMULATIVE TREE STATS

  treestats <- tree.definitions
  helper <- paste(levelstats$tree, levelstats$level, sep = ".")
  maxlevs <- paste(rownames(tapply(levelstats$level, levelstats$tree, FUN = which.max)), tapply(levelstats$level, levelstats$tree, FUN = which.max), sep = ".")
  treestats <- cbind(tree.definitions, levelstats[helper %in% maxlevs, names(level.i.stats)])
  rownames(treestats) <- 1:nrow(treestats)


  # Add pci to treestats
  #   pci is the number of cues looked up for each case divided by the maximum possible

  n.lookups <- colSums(levelout)
  max.lookups <- nrow(data) * ncol(data)

  treestats$pci <- 1 - n.lookups / max.lookups

  # Add mean cues per case (mcu)

  treestats$mcu <- colMeans(levelout)

  return(list("decision" = decision,
              "levelout" = levelout,
              "levelstats" = levelstats,
              "treestats" = treestats
  ))

}
