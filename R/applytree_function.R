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



# level_stats_ls: Cumulative level statistics
level_stats_ls <- vector("list", length = tree_n)

# Loop over trees
for(tree_i in 1:tree_n) {

decision_v <- rep(NA, case_n)
levelout_v <- rep(NA, case_n)
costcue_v <- rep(NA, case_n)

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

critical_stats_v <- c("n", "hi", "fa", "mi", "cr", "sens", "spec", "ppv", "npv", "acc", "bacc", "wacc", "costout")

# Add stat names to level_stats_i
level_stats_i[critical_stats_v] <- NA


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

decision_v[classify.now] <- current.decisions[classify.now]
levelout_v[classify.now] <- level_i
costcue_v[classify.now] <- costc.level.cum[level_i]




# Get level stats

my_level_stats_i <- classtable(prediction.v = decision_v[levelout_v <= level_i & is.finite(levelout_v)],
                               criterion.v = criterion_v[levelout_v <= level_i & is.finite(levelout_v)],
                               sens.w = sens.w,
                               cost.v = costcue_v[levelout_v <= level_i & is.finite(levelout_v)],
                               cost.outcomes = cost.outcomes)


# level_stats_i$costc <- sum(costcue[,tree_i], na.rm = TRUE)
level_stats_i[level_i, critical_stats_v] <- my_level_stats_i[,critical_stats_v]


}

# Add costt

  level_stats_ls[[tree_i]] <- level_stats_i

  output_ls$decision[,tree_i] <- decision_v
  output_ls$levelout[,tree_i] <- levelout_v
  output_ls$costcue[,tree_i] <- costcue_v


}

# Combine all levelstats into one dataframe
level_stats <- do.call("rbind", args = level_stats_ls)

# CUMULATIVE TREE STATS

helper <- paste(level_stats$tree, level_stats$level, sep = ".")
maxlevs <- paste(rownames(tapply(level_stats$level, level_stats$tree, FUN = which.max)), tapply(level_stats$level, level_stats$tree, FUN = which.max), sep = ".")
tree_stats <- cbind(tree.definitions, level_stats[helper %in% maxlevs, critical_stats_v])
rownames(tree_stats) <- 1:nrow(tree_stats)


# Add pci to treestats
#   pci is the number of cues looked up for each case divided by the maximum possible

n.lookups <- colSums(output_ls$levelout)
max.lookups <- nrow(data) * ncol(data)

tree_stats$pci <- 1 - n.lookups / max.lookups

# Add mean cues per case (mcu)

tree_stats$mcu <- colMeans(output_ls$levelout)

# Calculate outcome costs
costoutcomes.t <- sapply(1:tree_n, FUN = function(tree_i) {

  # which cases are hits
  hi.log <- criterion_v == 1 & output_ls$decision[,tree_i] == 1

  # which cases are false alarms
  fa.log <- criterion_v == 0 & output_ls$decision[,tree_i] == 1

  # which cases are misses
  mi.log <- criterion_v == 1 & output_ls$decision[,tree_i] == 0

  # which cases are correct rejections
  cr.log <- criterion_v == 0 & output_ls$decision[,tree_i] == 0

cost_v <- hi.log * cost.outcomes$hi + fa.log * cost.outcomes$fa + mi.log * cost.outcomes$mi + cr.log * cost.outcomes$cr

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
  cost.level_v <- cost.per.level[output_ls$levelout[,tree_i]]


  return(cost.level_v)

})

# Calculate total costs (outcomes + cues)
costtotal.t <- costoutcomes.t + costcues.t

cost.ls <- list("out" = costoutcomes.t,
                "cue" = costcues.t,
                "tot" = costtotal.t)

# Add mean cost per case (mcc)
# treestats$costt <- colMeans(costtotal.t)
# treestats$costc <- colMeans(costcues.t)

output <- list("decision" = output_ls$decision,
               "levelout" = output_ls$levelout,
               "levelstats" = level_stats,
               "treestats" = tree_stats,
               "treecost" = cost.ls)

return(output)

}
