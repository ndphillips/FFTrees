#' Applies a fast-and-frugal tree to a dataset and generates several accuracy statistics
#'
#' @param formula A formula
#' @param data dataframe. A model training dataset. An m x n dataframe containing n cue values for each of the m exemplars.
#' @param tree.definitions dataframe. Definitions of one or more trees. The dataframe must contain the columns: cues, classes, thresholds, directions, exits.
#' @param sens.w numeric.  A number from 0 to 1 indicating how to weight sensitivity relative to specificity. Only used for calculating wacc values.
#' @param cost.outcomes numeric. A vector of length 4 specifying the costs of a hit, false alarm, miss, and correct rejection rspectively. E.g.; \code{cost.outcomes = c(0, 10, 20, 0)} means that a false alarm and miss cost 10 and 20 respectively while correct decisions have no cost.
#' @param cost.cues dataframe. A dataframe with two columns specifying the cost of each cue. The first column should be a vector of cue names, and the second column should be a numeric vector of costs. Cues in the dataset not present in \code{cost.cues} are assume to have 0 cost.
#' @param allNA.pred logical. What should be predicted if all cue values in tree are NA? Default is FALSE

#' @return A list of length 4 containing
#' @export
#'
#'

apply.tree <- function(data,
                       formula,
                       tree.definitions,
                       sens.w = .5,
                       cost.outcomes = c(0, 1, 1, 0),
                       cost.cues = NULL,
                       allNA.pred = FALSE
) {

# Step 0: Validation and Setup

criterion.v <- model.frame(formula = formula,
                           data = data,
                           na.action = NULL)[,1]

n.exemplars <- nrow(data)
n.trees <- nrow(tree.definitions)


# decision: The decision for each case
decision          <- matrix(NA, nrow = n.exemplars, ncol = n.trees)

# levelout: The level at which each case is classified
levelout          <- matrix(NA, nrow = n.exemplars, ncol = n.trees)

# costcue: The cue cost for each case
costcue          <- matrix(NA, nrow = n.exemplars, ncol = n.trees)

# level.stats.ls: Cumulative level statistics
level.stats.ls <- vector("list", length = n.trees)

# Loop over trees
for(tree.i in 1:n.trees) {

# Extract node definitions
cue.v <- unlist(strsplit(tree.definitions$cues[tree.i], ";"))
class.v <- unlist(strsplit(tree.definitions$classes[tree.i], ";"))
exit.v <- unlist(strsplit(tree.definitions$exits[tree.i], ";"))
threshold.v <- unlist(strsplit(tree.definitions$thresholds[tree.i], ";"))
direction.v <-  unlist(strsplit(tree.definitions$directions[tree.i], ";"))

n.levels <- length(cue.v)

# level.stats.df.i contains cumulative level statistics
level.stats.df.i <- data.frame(tree = tree.i,
                               level = 1:n.levels,
                               cue = cue.v,
                               class = class.v,
                               threshold = threshold.v,
                               direction = direction.v,
                               exit = exit.v, stringsAsFactors = FALSE)

level.stats.df.i[names(classtable(1, 1))] <- NA
# level.stats.df.i$costc <- NA
# Calculate cumulative cue cost for each level

if(is.null(cost.cues) == FALSE) {

  costc.level <- sapply(cue.v, FUN = function(cue.i) {

    if(cue.i %in% cost.cues[,1]) {cost.cue.i <- cost.cues[cost.cues[,1] == cue.i, 2]} else {

      cost.cue.i <- 0}})


  costc.level.cum <- cumsum(costc.level)

} else {costc.level.cum <- rep(0, n.levels)}

# Loop over levels
for(level.i in 1:n.levels) {

# Get definitions for current level
cue.i <- cue.v[level.i]
class.i <- class.v[level.i]
direction.i <- direction.v[level.i]
exit.i <- as.numeric(exit.v[level.i])
threshold.i <- threshold.v[level.i]


# Determine which cases are classified / unclassified
unclassified.cases <- which(is.na(decision[,tree.i]))
classified.cases <- which(is.na(decision[,tree.i]) == FALSE)

cue.values <- data[[cue.i]]

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

# Convert NAs

# If it is not the final node, then don't classify NA cases
if(exit.i %in% c(0, 1)) {classify.now[is.na(classify.now)] <- FALSE}


#If it is the final node, then classify NA cases according to most common class
if(exit.i %in% .5) {

  current.decisions[is.na(current.decisions)] <- allNA.pred

  }


decision[classify.now, tree.i] <- current.decisions[classify.now]
levelout[classify.now, tree.i] <- level.i
costcue[classify.now, tree.i] <- costc.level.cum[level.i]

# Get level stats

level.i.stats <- classtable(prediction.v = decision[levelout[,tree.i] <= level.i & is.finite(levelout[,tree.i]), tree.i],
                            criterion.v = criterion.v[levelout[,tree.i] <= level.i & is.finite(levelout[,tree.i])],
                            sens.w = sens.w,
                            cost.v = costcue[levelout[,tree.i] <= level.i & is.finite(levelout[,tree.i]), tree.i],
                            cost.outcomes = cost.outcomes)


# level.i.stats$costc <- sum(costcue[,tree.i], na.rm = TRUE)
level.stats.df.i[level.i, names(level.i.stats)] <- level.i.stats

}

# Add costt

  level.stats.ls[[tree.i]] <- level.stats.df.i

}

# Combine all levelstats into one dataframe
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

# Calculate outcome costs
costoutcomes.t <- sapply(1:n.trees, FUN = function(tree.i) {

  # which cases are hits
  hi.log <- criterion.v == 1 & decision[,tree.i] == 1

  # which cases are false alarms
  fa.log <- criterion.v == 0 & decision[,tree.i] == 1

  # which cases are misses
  mi.log <- criterion.v == 1 & decision[,tree.i] == 0

  # which cases are correct rejections
  cr.log <- criterion.v == 0 & decision[,tree.i] == 0

cost.v <- hi.log * cost.outcomes[1] + fa.log * cost.outcomes[2] + mi.log * cost.outcomes[3] + cr.log * cost.outcomes[4]

  return(cost.v)

})

# Calculate cue costs
costcues.t <- sapply(1:n.trees, FUN = function(tree.i) {

  # Determine cues in tree:
  cues.in.tree <- unlist(strsplit(tree.definitions$cues[tree.i], ";"))

  # Determine cost of each cue in tree
  cue.cost.in.tree <- sapply(cues.in.tree, FUN = function(x) {

    if(x %in% cost.cues$cue) {
    return(cost.cues$cost[cost.cues$cue == x])} else {return(0)}

  })

  # Get cumulative cost for each level
  cost.per.level <- cumsum(cue.cost.in.tree)

  # Get node for each case for current tree
  cost.level.v <- cost.per.level[levelout[,tree.i]]


  return(cost.level.v)

})

# Calculate total costs (outcomes + cues)
costtotal.t <- costoutcomes.t + costcues.t

cost.ls <- list("outcomes" = costoutcomes.t,
                "cues" = costcues.t,
                "total" = costtotal.t)

# Add mean cost per case (mcc)
# treestats$costt <- colMeans(costtotal.t)
# treestats$costc <- colMeans(costcues.t)

return(list("decision" = decision,
            "levelout" = levelout,
            "levelstats" = levelstats,
            "treestats" = treestats,
            "treecost" = cost.ls))

}
