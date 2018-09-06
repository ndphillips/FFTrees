#' Applies a fast-and-frugal tree to a dataset and generates several accuracy statistics
#'
#' @param formula A formula
#' @param data dataframe. A model training dataset. An m x n dataframe containing n cue values for each of the m exemplars.
#' @param tree.definitions dataframe. Definitions of one or more trees. The dataframe must contain the columns: cues, classes, thresholds, directions, exits.
#' @param sens.w numeric.  A number from 0 to 1 indicating how to weight sensitivity relative to specificity. Only used for calculating wacc values.
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying the costs of a hit, false alarm, miss, and correct rejection rspectively. E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a false alarm and miss cost 10 and 20 respectively while correct decisions have no cost.
#' @param cost.cues dataframe. A dataframe with two columns specifying the cost of each cue. The first column should be a vector of cue names, and the second column should be a numeric vector of costs. Cues in the dataset not present in \code{cost.cues} are assume to have 0 cost.
#' @param allNA.pred logical. What should be predicted if all cue values in tree are NA? Default is FALSE
#'
#' @return A list of length 4 containing
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

  # data = data.train
  # formula = formula
  # tree.definitions = tree.definitions
  # sens.w = sens.w
  # cost.cues = cost.cues
  # cost.outcomes = cost.outcomes
  #  allNA.pred = FALSE

# Step 0: Validation and Setup
{


# Data Details
{
case_n <- nrow(data)

criterion_name <- paste(formula)[2]

if(criterion_name %in% names(data)) {

  criterion_v <- model.frame(formula = formula,
                             data = data,
                             na.action = NULL)[,1]

  if(class(criterion_v) != "logical") {criterion_v <- as.logical(criterion_v)}


} else {criterion_v <- rep(NA, case_n)}
}

# Tree details
{
tree_n <- nrow(tree.definitions)

  }

# Setup outputs

#  [output_ls]
#    A list containing dataframes with one row per case, and one column per tree

output_names <- c("decision", "levelout", "costout", "costcue", "cost")

output_ls <- lapply(1:length(output_names), FUN = function(x) {

  output <- as.data.frame(matrix(NA,
                                 nrow = case_n,
                                 ncol = tree_n))

  names(output) <- paste("tree", 1:tree_n, sep = ".")

  output

})
names(output_ls) <- output_names


# [level_stats_ls]
#   A list with one element per tree, each containing cumulative level statistics

level_stats_ls <- vector("list", length = tree_n)

}

# LOOP
#  Loop over trees

for(tree_i in 1:tree_n) {

# Extract defintions for current tree
cue_v <- unlist(strsplit(tree.definitions$cues[tree_i], ";"))
class_v <- unlist(strsplit(tree.definitions$classes[tree_i], ";"))
exit_v <- unlist(strsplit(tree.definitions$exits[tree_i], ";"))
threshold_v <- unlist(strsplit(tree.definitions$thresholds[tree_i], ";"))
direction_v <-  unlist(strsplit(tree.definitions$directions[tree_i], ";"))
level_n <- tree.definitions$nodes[tree_i]

#  [cue_cost_cum_level]
# Calculate cumulative cue cost for each level based on cue costs

cost.cues <- cost.cues.append(formula = formula,
                              data = data,
                              cost.cues)


costc.level <- sapply(cue_v, FUN = function(cue_i) {

    if(cue_i %in% names(cost.cues)) {cost.cue_i <- cost.cues[[cue_i]]} else {

      cost.cue_i <- 0}})

costc.level.cum <- cumsum(costc.level)

cue_cost_cum_level <- data.frame(level = 1:level_n,
                                 cue_cost_cum = costc.level.cum)

# Define vectors of critical outputs for each case

decision_v <- rep(NA, case_n)
levelout_v <- rep(NA, case_n)
costcue_v <-  rep(NA, case_n)
costout_v <-  rep(NA, case_n)
cost_v <-  rep(NA, case_n)

# level_stats_i contains cumulative level statistics
level_stats_i <- data.frame(tree = tree_i,
                            level = 1:level_n,
                            cue = cue_v,
                            class = class_v,
                            threshold = threshold_v,
                            direction = direction_v,
                            exit = exit_v,
                            stringsAsFactors = FALSE)

critical_stats_v <- c("n", "hi", "fa", "mi", "cr", "sens", "spec", "far", "ppv", "npv", "acc", "bacc", "wacc", "costout")

# Add stat names to level_stats_i
level_stats_i[critical_stats_v] <- NA
level_stats_i$costcue <- NA


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
costcue_v[classify.now] <- costc.level.cum[level_i]

costout_v[current.decisions[classify.now] == TRUE & criterion_v[classify.now] == TRUE] <- cost.outcomes$hi
costout_v[current.decisions[classify.now] == TRUE & criterion_v[classify.now] == FALSE] <- cost.outcomes$fa
costout_v[current.decisions[classify.now] == FALSE & criterion_v[classify.now] == TRUE] <- cost.outcomes$mi
costout_v[current.decisions[classify.now] == FALSE & criterion_v[classify.now] == FALSE] <- cost.outcomes$cr

cost_v[classify.now] <- costcue_v[classify.now] + costout_v[classify.now]


# Get cumulative level stats

my_level_stats_i <- classtable(prediction.v = decision_v[levelout_v <= level_i & is.finite(levelout_v)],
                               criterion.v = criterion_v[levelout_v <= level_i & is.finite(levelout_v)],
                               sens.w = sens.w,
                               cost.v = costcue_v[levelout_v <= level_i & is.finite(levelout_v)],
                               cost.outcomes = cost.outcomes)


# level_stats_i$costc <- sum(costcue[,tree_i], na.rm = TRUE)
level_stats_i[level_i, critical_stats_v] <- my_level_stats_i[,critical_stats_v]

# Add cue cost and cost

level_stats_i$costcue[level_i] <- mean(costcue_v[!is.na(costcue_v)])
level_stats_i$cost[level_i] <-level_stats_i$costcue[level_i]  + level_stats_i$costout[level_i]

}

# Add final tree results to level_stats_ls and output_ls

  level_stats_ls[[tree_i]] <- level_stats_i

  output_ls$decision[,tree_i] <- decision_v
  output_ls$levelout[,tree_i] <- levelout_v
  output_ls$costcue[,tree_i] <- costcue_v
  output_ls$costout[,tree_i] <- costout_v
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
tree_stats <- cbind(tree.definitions[,c("tree")], level_stats[helper %in% maxlevs, c(critical_stats_v, "costcue", "cost")])
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

# Define output
output <- list("tree.definitions" = tree.definitions,
               "treestats" = tree_stats,
               "levelstats" = level_stats,
               "decision" = output_ls$decision,
               "levelout" = output_ls$levelout,
               "costout" = output_ls$costout,
               "costcue" = output_ls$costcue,
               "cost" = output_ls$cost)

return(output)

}
