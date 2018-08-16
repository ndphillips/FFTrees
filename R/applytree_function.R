#' Applies a fast-and-frugal tree to a dataset and generates several accuracy statistics
#'
#' @param formula A formula
#' @param data dataframe. A model training dataset. An m x n dataframe containing n cue values for each of the m exemplars.
#' @param tree.definitions dataframe. Definitions of one or more trees. The dataframe must contain the columns: cues, classes, thresholds, directions, exits.
#' @param sens.w numeric.  A number from 0 to 1 indicating how to weight sensitivity relative to specificity. Only used for calculating wacc values.
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying the costs of a hit, false alarm, miss, and correct rejection rspectively. E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a false alarm and miss cost 10 and 20 respectively while correct decisions have no cost.
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
                       cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0),
                       cost.cues = NULL,
                       allNA.pred = FALSE
) {

# Step 0: Validation and Setup

criterion_v <- model.frame(formula = formula,
                           data = data,
                           na.action = NULL)[,1]

case_n <- nrow(data)
tree_n <- nrow(tree.definitions)


output_names <- c("decision", "levelout", "costcue", "costout")

output_ls <- lapply(1:length(output_names), FUN = function(x) {

  output <- as.data.frame(matrix(NA,
                                 nrow = case_n,
                                 ncol = tree_n))

  names(output) <- paste("tree", 1:tree_n, sep = ".")

  output

})

names(output_ls) <- output_names



# level.stats.ls: Cumulative level statistics
level.stats.ls <- vector("list", length = tree_n)

# Loop over trees
for(tree_i in 1:tree_n) {

# Extract node definitions
cue_v <- unlist(strsplit(tree.definitions$cues[tree_i], ";"))
class_v <- unlist(strsplit(tree.definitions$classes[tree_i], ";"))
exit_v <- unlist(strsplit(tree.definitions$exits[tree_i], ";"))
threshold_v <- unlist(strsplit(tree.definitions$thresholds[tree_i], ";"))
direction_v <-  unlist(strsplit(tree.definitions$directions[tree_i], ";"))

level_n <- length(cue_v)

# level_stats_i contains cumulative level statistics
level_stats_i <- data.frame(tree = tree_i,
                            level = 1:level_n,
                            cue = cue_v,
                            class = class_v,
                            threshold = threshold_v,
                            direction = direction_v,
                            exit = exit_v,
                            stringsAsFactors = FALSE)

# Add stat names to level_stats_i
level_stats_i[c("hi", "fa", "mi", "cr", "sens", "spec", "acc", "bacc", "wacc")] <- NA


# Calculate cumulative cue cost for each level

if(is.null(cost.cues) == FALSE) {

  costc.level <- sapply(cue_v, FUN = function(cue_i) {

    if(cue_i %in% cost.cues[,1]) {cost.cue_i <- cost.cues[cost.cues[,1] == cue_i, 2]} else {

      cost.cue_i <- 0}})


  costc.level.cum <- cumsum(costc.level)

} else {costc.level.cum <- rep(0, level_n)}

# Loop over levels
for(level_i in 1:level_n) {

# Get definitions for current level
cue_i <- cue_v[level_i]
class_i <- class_v[level_i]
direction_i <- direction_v[level_i]
exit_i <- as.numeric(exit_v[level_i])
threshold_i <- threshold_v[level_i]


# Determine which cases are classified / unclassified
unclassified.cases <- which(is.na(decision[,tree_i]))
classified.cases <- which(is.na(decision[,tree_i]) == FALSE)

cue_values <- data[[cue_i]]

if(is.character(threshold_i)) {threshold_i <- unlist(strsplit(threshold_i, ","))}

if(substr(class_i, 1, 1) %in% c("n", "i")) {threshold_i <- as.numeric(threshold_i)}

if(direction_i == "!=") {current.decisions <- (cue_values %in% threshold_i) == FALSE}
if(direction_i == "=") {current.decisions <- cue_values %in% threshold_i}
if(direction_i == "<") {current.decisions <- cue_values < threshold_i}
if(direction_i == "<=") {current.decisions <- cue_values <= threshold_i}
if(direction_i == ">") {current.decisions <- cue_values > threshold_i}
if(direction_i == ">=") {current.decisions <- cue_values >= threshold_i}

if(exit_i == 0) {classify.now <- current.decisions == FALSE & is.na(decision[,tree_i])}
if(exit_i == 1) {classify.now <- current.decisions == TRUE & is.na(decision[,tree_i])}
if(exit_i == .5) {classify.now <- is.na(decision[,tree_i])}

# Convert NAs

# If it is not the final node, then don't classify NA cases
if(exit_i %in% c(0, 1)) {classify.now[is.na(classify.now)] <- FALSE}


#If it is the final node, then classify NA cases according to most common class
if(exit_i %in% .5) {

  current.decisions[is.na(current.decisions)] <- allNA.pred

  }


output_ls$decision[classify.now, tree_i] <- current.decisions[classify.now]
output_ls$levelout[classify.now, tree_i] <- level_i
output_ls$costcue[classify.now, tree_i] <- costc.level.cum[level_i]

# Get level stats

level_i.stats <- classtable(prediction_v = output_ls$decision[levelout[,tree_i] <= level_i & is.finite(output_ls$levelout[,tree_i]), tree_i],
                            criterion_v = criterion_v[output_ls$levelout[,tree_i] <= level_i & is.finite(output_ls$levelout[,tree_i])],
                            sens.w = sens.w,
                            cost_v = output_ls$costcue[output_ls$levelout[,tree_i] <= level_i & is.finite(output_ls$levelout[,tree_i]), tree_i],
                            cost.outcomes = cost.outcomes)


# level_i.stats$costc <- sum(costcue[,tree_i], na.rm = TRUE)
level_stats_i[level_i, names(level_i.stats)] <- level_i.stats

}

# Add costt

  level.stats.ls[[tree_i]] <- level_stats_i

}

# Combine all levelstats into one dataframe
levelstats <- do.call("rbind", args = level.stats.ls)

# CUMULATIVE TREE STATS

treestats <- tree.definitions
helper <- paste(levelstats$tree, levelstats$level, sep = ".")
maxlevs <- paste(rownames(tapply(levelstats$level, levelstats$tree, FUN = which.max)), tapply(levelstats$level, levelstats$tree, FUN = which.max), sep = ".")
treestats <- cbind(tree.definitions, levelstats[helper %in% maxlevs, names(level_i.stats)])
rownames(treestats) <- 1:nrow(treestats)


# Add pci to treestats
#   pci is the number of cues looked up for each case divided by the maximum possible

n.lookups <- colSums(levelout)
max.lookups <- nrow(data) * ncol(data)

treestats$pci <- 1 - n.lookups / max.lookups

# Add mean cues per case (mcu)

treestats$mcu <- colMeans(levelout)

# Calculate outcome costs
costoutcomes.t <- sapply(1:tree_n, FUN = function(tree_i) {

  # which cases are hits
  hi.log <- criterion_v == 1 & decision[,tree_i] == 1

  # which cases are false alarms
  fa.log <- criterion_v == 0 & decision[,tree_i] == 1

  # which cases are misses
  mi.log <- criterion_v == 1 & decision[,tree_i] == 0

  # which cases are correct rejections
  cr.log <- criterion_v == 0 & decision[,tree_i] == 0

cost_v <- hi.log * cost.outcomes[1] + fa.log * cost.outcomes[2] + mi.log * cost.outcomes[3] + cr.log * cost.outcomes[4]

  return(cost_v)

})

# Calculate cue costs
costcues.t <- sapply(1:tree_n, FUN = function(tree_i) {

  # Determine cues in tree:
  cues_in.tree <- unlist(strsplit(tree.definitions$cues[tree_i], ";"))

  # Determine cost of each cue in tree
  cue.cost_in.tree <- sapply(cues_in.tree, FUN = function(x) {

    if(x %in% cost.cues$cue) {
    return(cost.cues$cost[cost.cues$cue == x])} else {return(0)}

  })

  # Get cumulative cost for each level
  cost.per.level <- cumsum(cue.cost_in.tree)

  # Get node for each case for current tree
  cost.level_v <- cost.per.level[levelout[,tree_i]]


  return(cost.level_v)

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
