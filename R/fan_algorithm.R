#' Grows fast-and-frugal trees using the fan algorithm
#'
#' @param formula formula. A formula
#' @param data dataframe. A dataset
#' @param max.levels integer. The maximum number of levels in the tree(s)
#' @param algorithm character. A string indicating how to rank cues during tree construction. "ifan"  (independent fan) means that cues will only be ranked once with the entire training dataset "dfan" (dependent fan) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars.
#' @param goal character. A string indicating the statistic to maximize: "acc" = overall accuracy, "bacc" = balanced accuracy, "wacc" = weighted accuracy, "bacc" = balanced accuracy
#' @param goal.chase character. A string indicating the statistic to maximize when constructing trees: "acc" = overall accuracy, "wacc" = weighted accuracy, "bacc" = balanced accuracy
#' @param goal.threshold character. A string indicating the statistic to maximize when calculting cue thresholds: "acc" = overall accuracy, "wacc" = weighted accuracy, "bacc" = balanced accuracy
#' @param sens.w numeric. A number from 0 to 1 indicating how to weight sensitivity relative to specificity.
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying the costs of a hit, false alarm, miss, and correct rejection rspectively. E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a false alarm and miss cost 10 and 20 respectively while correct decisions have no cost.
#' @param cost.cues dataframe. A dataframe with two columns specifying the cost of each cue. The first column should be a vector of cue names, and the second column should be a numeric vector of costs. Cues in the dataset not present in \code{cost.cues} are assume to have 0 cost.
#' @param numthresh.method character. How should thresholds for numeric cues be determined? \code{"o"} will optimize thresholds, while \code{"m"} will always use the median.
#' @param numthresh.n numeric. Number of threshold values to consider.
#' @param stopping.rule character. A string indicating the method to stop growing trees. \code{"levels"} means the tree grows until a certain level, \code{"exemplars"} means the tree grows until a certain number of unclassified exemplars remain. \code{"statdelta"} means the tree grows until the change in the criterion statistic is less than a specified level.
#' @param stopping.par numeric. A number indicating the parameter for the stopping rule. For stopping.rule == \code{"levels"}, this is the number of levels. For stopping rule \code{"exemplars"}, this is the smallest percentage of examplars allowed in the last level.
#' @param rounding integer. How much should threshold parameters be rounded? Default is
#' @param quiet logical. Should tree growing progress be displayed?
#' @param repeat.cues logical. Can cues occur multiple times within a tree?
#' @param ... Currently ignored
#' @importFrom stats anova predict glm as.formula var
#'
#' @return A definition of an FFT
#'
fan.algorithm <- function(formula,
                          data,
                          max.levels = 5,
                          algorithm = "ifan",
                          goal = "wacc",
                          goal.chase = "bacc",
                          goal.threshold = "bacc",
                          sens.w = .5,
                          cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0),
                          cost.cues = NULL,
                          numthresh.method = "o",
                          numthresh.n = 20,
                          stopping.rule = "exemplars",
                          stopping.par = .1,
                          rounding = NULL,
                          quiet = TRUE,
                          repeat.cues = TRUE) {
#
  # formula = formula
  # data = data.mf
  # max.levels = max.levels
  # algorithm = algorithm
  # goal = goal
  # goal.chase = goal.chase
  # sens.w = sens.w
  # cost.outcomes = cost.outcomes
  # cost.cues = cost.cues
  # numthresh.method = numthresh.method
  # stopping.rule = stopping.rule
  # stopping.par = stopping.par
  # progress = progress
  # repeat.cues = repeat.cues



# Some global variables which could be changed later.

exit.method <- "fixed"
correction <- .25

# Define key objects
data.mf <- model.frame(formula, data, na.action = NULL)

if(class(data.mf[,1]) != "logical") {data.mf[,1] <- as.logical(data.mf[,1])}


criterion_name <- names(data.mf)[1]
criterion.v <- data.mf[,1]
cue_n <- ncol(data.mf) - 1
case_n <- nrow(data.mf)
cue_df <- data.mf[,2:ncol(data.mf)]
cue_names <- names(cue_df)

if(is.null(cost.cues)) {cost.cues <- cost.cues.append(formula, data)}


# ----------
# INITIAL TRAINING CUE ACCURACIES
# ----------

cue_best_df <- cuerank(formula = formula,
                       data = data,
                       goal.threshold = goal.threshold,
                       numthresh.method = numthresh.method,
                       numthresh.n = numthresh.n,
                       rounding = rounding,
                       quiet = quiet,
                       sens.w = sens.w,
                       cost.outcomes = cost.outcomes,
                       cost.cues = cost.cues)

# ----------
# GROW TREES
# ----------
{

# SETUP TREES
# [tree_dm, tree_stats_ls, level_stats_ls]
{
if(max.levels > 1) {

  expand.ls <- lapply(1:(max.levels - 1),
                      FUN = function(x) {return(c(0, 1))})

  expand.ls[[length(expand.ls) + 1]] <- .5
  names(expand.ls) <- c(paste("exit.", 1:(max.levels - 1), sep = ""),
                        paste("exit.", max.levels, sep = ""))

  tree_dm <- expand.grid(
    expand.ls,
    stringsAsFactors = FALSE)

}

if(max.levels == 1) {

  tree_dm <- data.frame("exit.1" = .5)

}

tree_dm$tree.num <- 1:nrow(tree_dm)
tree_n <- nrow(tree_dm)

# Set up tree_stats_ls
#  A list containing dataframes representing
#   one-row-per-case, one-column-per-tree statistics

tree_table_names <- c("decision", "levelout")

tree_stats_ls <- lapply(1:length(tree_table_names), FUN = function(x) {

  output <- as.data.frame(matrix(NA, nrow = case_n, ncol = tree_n))

  names(output) <- paste("tree", 1:tree_n, sep = ".")

  output

})

names(tree_stats_ls) <- tree_table_names

level_stats_ls <- vector("list", length = tree_n)

}

  # Loop over trees
for(tree_i in 1:tree_n) {

  ## Determine exits for tree_i

  exits_i <- unlist(tree_dm[tree_i, grepl("exit.", names(tree_dm))])
  level_n <- length(exits_i)

  ## Set up placeholders
  cue_best_df.original <- cue_best_df

  # Decisions, levelout, and cost vectors
  decision_v <- rep(NA, case_n)
  levelout_v <- rep(NA, case_n)
  cuecost_v <- rep(0, case_n)
  outcomecost_v <- rep(NA, case_n)
  totalcost_v <- rep(0, case_n)
  hi_v <- rep(NA, case_n)
  fa_v <- rep(NA, case_n)
  mi_v <- rep(NA, case_n)
  cr_v <- rep(NA, case_n)

  ## level_stats_i shows cumulative classification decisions statistics at each level
  level_stats_i <- data.frame("level" = NA,
                           "cue" = NA,
                           "costcue" = NA,
                           "costcue.cum" = NA,
                           "costout"= NA,
                           "class" = NA,
                           "threshold" = NA,
                           "direction" = NA,
                           "exit" = NA)

  level_stat_names <- setdiff(names(threshold_factor_grid()), c("threshold", "direction"))
  level_stats_i[level_stat_names] <- NA

  ## asif.stats shows cumulative classification statistics as if all exemplars were
  #   classified at the current level (i.e; if the tree stopped here)

  asif.stats <- data.frame("level" = 1:level_n,
                           "sens" = NA,
                           "spec" = NA,
                           "acc" = NA,
                           "bacc" = NA,
                           "wacc" = NA,
                           "dprime" = NA,
                           "cost" = NA,
                           "goal.change" = NA)

  # Starting values
  grow.tree <- TRUE
  level_current <- 0

    # ------------------
    # Grow Tree!
    # --------------------

while(grow.tree == TRUE) {

level_current <- level_current + 1
exit_current <- exits_i[level_current]
cases_remaining <- is.na(decision_v)

# Step 1) Determine cue for current level
{
# ifan algorithm
if(algorithm == "ifan") {

  # Get accuracies of un-used cues
  cue_best_df_current <- cue_best_df.original[(cue_best_df.original$cue %in% level_stats_i$cue) == FALSE,]

}

# dfan algorithm
if(algorithm == "dfan") {

  data.mf.r <- data.mf[cases_remaining, ]

  # If cues can NOT be repeated, then remove old cues as well
  if(repeat.cues == FALSE) {

    remaining.cues.index <- (names(cue_df) %in% level_stats_i$cue) == FALSE
    remaining.cues <- names(cue_df)[remaining.cues.index]
    data.mf.r <- data.mf.r[, c(criterion_name, remaining.cues)]

  }

  # If there is no variance in the criterion, then stop growth!
  if(var(data.mf.r[,1]) == 0) {grow.tree <- FALSE ; break}

  # Calculate cue accuracies with remaining exemplars
  cue_best_df_current <-  cuerank(formula = formula,
                                  data = data.mf.r,
                                  goal.threshold = goal.threshold,
                                  numthresh.method = numthresh.method,
                                  numthresh.n = numthresh.n,
                                  rounding = rounding,
                                  sens.w = sens.w,
                                  quiet = quiet,
                                  cost.outcomes = cost.outcomes,
                                  cost.cues = cost.cues)

}

# Get next cue based on maximizing goal
cue_best_i <- which(cue_best_df_current[[goal.chase]] == max(cue_best_df_current[[goal.chase]], na.rm = TRUE))

# If there is a tie, take the first
if(length(cue_best_i) > 1) {cue_best_i <- cue_best_i[1]}

cue_name_new <- cue_best_df_current$cue[cue_best_i]
cue_stats_new <- cue_best_df_current$cue[cue_best_i]
cue_cost_new <- cost.cues[[cue_name_new]]
cue_class_new <- cue_best_df_current$class[cue_best_i]
cue_threshold_new <- cue_best_df_current$threshold[cue_best_i]
cue_direction_new <- cue_best_df_current$direction[cue_best_i]

# Add cue costs to cuecost_v
cuecost_v[is.na(decision_v)] <- cuecost_v[is.na(decision_v)] + cue_cost_new

# ADD CUE INFO TO LEVEL.STATS

level_stats_i$level[level_current] <- level_current
level_stats_i$cue[level_current] <- cue_name_new
level_stats_i$costcue[level_current] <- cue_cost_new
level_stats_i$costcue.cum[level_current] <- sum(level_stats_i$costcue[1:level_current])
level_stats_i$class[level_current] <- cue_class_new
level_stats_i$threshold[level_current] <- cue_threshold_new
level_stats_i$direction[level_current] <- cue_direction_new
level_stats_i$exit[level_current] <- exit_current
}

# Step 2) Determine how classifications would look if
#  all remaining exemplars were classified
{

# Get decisions for current cue
cue.decisions <- apply.break(direction = cue_direction_new,
                             threshold.val = cue_threshold_new,
                             cue.v = data.mf[[cue_name_new]],
                             cue.class = cue_class_new)

# How would classifications look if all remaining exemplars
#   were classified at the current level?

asif.decision_v <- decision_v
asif.levelout_v <- levelout_v
asif.cuecost_v <- cuecost_v

asif.decision_v[cases_remaining] <- cue.decisions[cases_remaining]
asif.levelout_v[cases_remaining] <- level_current
asif.cuecost_v[cases_remaining] <- cue_cost_new

# Calculate asif_cm

asif_results <- classtable(prediction.v = asif.decision_v,
                           criterion.v = criterion.v)


# Add key stats to asif.stats

asif.stats[level_current, c("sens", "spec", "acc", "bacc", "wacc", "dprime", "cost")] <-  c(asif_results$sens, asif_results$spec, asif_results$acc, asif_results$bacc, asif_results$wacc, asif_results$dprime, asif_results$cost)


  # If ASIF classification is perfect, then stop!

  if(goal.chase != "cost") {

  if(asif.stats[[goal.chase]][level_current] == 1) {grow.tree <- FALSE}

  } else {

    if(asif.stats[[goal.chase]][level_current] == 0) {grow.tree <- FALSE}

  }

# Calculate goal change
{
  if(level_current == 1) {

    asif.stats$goal.change[1] <- asif.stats[[goal]][1]

  }

  if(level_current > 1) {

    goal.change <- asif.stats[[goal.chase]][level_current] - asif.stats[[goal.chase]][level_current - 1]
    asif.stats$goal.change[level_current] <- goal.change

  }
}
}

# Step 3) Classify exemplars in current level
{

    if(exit_current == 1 | exit_current == .5) {

      decide.1.index <- cases_remaining & cue.decisions == TRUE

      decision_v[decide.1.index] <- TRUE
      levelout_v[decide.1.index] <- level_current

    }

    if(exit_current == 0 | exit_current == .5) {

      decide.0.index <- is.na(decision_v) & cue.decisions == FALSE

      decision_v[decide.0.index] <- FALSE
      levelout_v[decide.0.index] <- level_current
    }

    # Update cost vectors

    hi_v <- decision_v == TRUE & criterion.v == TRUE
    mi_v <- decision_v == FALSE & criterion.v == TRUE
    fa_v <- decision_v == TRUE & criterion.v == FALSE
    cr_v <- decision_v == FALSE & criterion.v == FALSE

    outcomecost_v[hi_v == TRUE] <- cost.outcomes$hi
    outcomecost_v[mi_v == TRUE] <- cost.outcomes$mi
    outcomecost_v[fa_v == TRUE] <- cost.outcomes$fa
    outcomecost_v[cr_v == TRUE] <- cost.outcomes$cr

  }

# Step 4) Update Results
{
  cases_remaining <- is.na(decision_v)

  # NEED TO FIX THIS BELOW TO INCORPORATE ALL COSTS

  # Get cumulative stats of examplars currently classified

  results_cum <- classtable(prediction.v = decision_v[cases_remaining == FALSE],
                            criterion.v = criterion.v[cases_remaining == FALSE],
                            sens.w = sens.w,
                            cost.v = cuecost_v[cases_remaining == FALSE],
                            cost.outcomes = cost.outcomes)


  # Update level stats
  level_stats_i[level_current,] <- NA
  level_stats_i$level[level_current] <- level_current
  level_stats_i$cue[level_current] <- cue_name_new
  level_stats_i$class[level_current] <- cue_class_new
  level_stats_i$threshold[level_current] <- cue_threshold_new
  level_stats_i$direction[level_current] <- cue_direction_new
  level_stats_i$exit[level_current] <- exit_current


  level_stats_i[level_current, c("hi", "fa", "mi", "cr", "sens", "spec", "bacc", "acc", "wacc", "costout", "cost")] <- results_cum[,c("hi", "fa", "mi", "cr", "sens", "spec", "bacc", "acc", "wacc", "costout", "cost")]

}

# Step 5) Continue growing tree?
{

  cases_remaining_n <- sum(cases_remaining)

  if(cases_remaining_n > 0 & level_current != cue_n & exit.method == "fixed") {

    if(level_current < level_n) {grow.tree <- TRUE}
    if(level_current == level_n) {grow.tree <- FALSE ; break}

  }
  if(cases_remaining_n == 0 | level_current == cue_n) {break}
  if(stopping.rule == "exemplars" & cases_remaining_n < stopping.par * nrow(cue_df)) {break}
  if(stopping.rule == "levels" & level_current == stopping.par) {break}

  if(algorithm == "dfan" & sd(criterion.v[cases_remaining]) == 0) {break}

  # Set up next level stats
  level_stats_i[level_current + 1,] <- NA

}

}  # STOP while(grow.tree) Loop

# Step 6) No more growth. Make sure last level is bidirectional
{

  last.level <- max(level_stats_i$level)
  last.cue <- level_stats_i$cue[last.level]
  cost.cue <- cost.cues[[last.cue]]


  last.exitdirection <- level_stats_i$exit[level_stats_i$level == last.level]

  if(last.exitdirection != .5) {

    decision_v[levelout_v == last.level] <- NA

    last_cue_stats <- cue_best_df_current[cue_best_df_current$cue == last.cue,]

    decision.index <- is.na(decision_v)

    # Step 2) Determine accuracy of negative and positive classification

    current.decisions <- apply.break(direction = last_cue_stats$direction,
                                     threshold.val = last_cue_stats$threshold,
                                     cue.v = data.mf[[last.cue]],
                                     cue.class = last_cue_stats$class)

    decide.0.index <- decision.index == TRUE & current.decisions == FALSE
    decide.1.index <- decision.index == TRUE & current.decisions == TRUE

    decision_v[decide.0.index] <- FALSE
    decision_v[decide.1.index] <- TRUE

    levelout_v[decide.0.index] <- level_current
    levelout_v[decide.1.index] <- level_current

    # up

    last.classtable <- classtable(prediction.v = as.logical(decision_v),
                                  criterion.v = as.logical(criterion.v),
                                  sens.w = sens.w,
                                  cost.v = cuecost_v,
                                  cost.outcomes = cost.outcomes)

    level_stats_i$exit[last.level] <- .5


    level_stats_i[last.level,  c("hi", "fa", "mi", "cr", "sens", "spec", "bacc", "acc", "wacc", "costout")] <- last.classtable[,c("hi", "fa", "mi", "cr", "sens", "spec", "bacc", "acc", "wacc", "costout")]

  }

}

# ------------------
# Tree is finished!
# --------------------

# Set up final output

tree_stats_ls$decision[,tree_i] <- decision_v
tree_stats_ls$levelout[,tree_i] <- levelout_v

level_stats_i$tree <- tree_i

level_stats_ls[[tree_i]] <- level_stats_i

  }

}

# -------------------------
# SUMMARISE TREE DEFINITIONS AND STATISTICS
#   tree.definitions
# -------------------------
{
  # Summarise tree definitions

  tree.definitions <- as.data.frame(matrix(NA, nrow = tree_n, ncol = 7))
  names(tree.definitions) <- c("tree", "nodes", "classes", "cues", "directions", "thresholds", "exits")

  for(tree_i in 1:tree_n) {

    level_stats_i <- level_stats_ls[[tree_i]]

    tree.definitions$tree[tree_i] <- tree_i
    tree.definitions$cues[tree_i] <- paste(level_stats_i$cue, collapse = ";")
    tree.definitions$nodes[tree_i] <- length(level_stats_i$cue)
    tree.definitions$classes[tree_i] <- paste(substr(level_stats_i$class, 1, 1), collapse = ";")
    tree.definitions$exits[tree_i] <- paste(level_stats_i$exit, collapse = ";")
    tree.definitions$thresholds[tree_i] <- paste(level_stats_i$threshold, collapse = ";")
    tree.definitions$directions[tree_i] <- paste(level_stats_i$direction, collapse = ";")

  }

  duplicate.trees <- duplicated(tree.definitions[c("cues", "exits", "thresholds", "directions")])
  tree.definitions <- tree.definitions[duplicate.trees == FALSE,]
  rownames(tree.definitions) <- 1:nrow(tree.definitions)
  tree.definitions$tree <- 1:nrow(tree.definitions)
  tree.definitions <- tree.definitions[,c(which(names(tree.definitions) == "tree"), which(names(tree.definitions) != "tree"))]

}

# Order tree definitions by wacc:

my.applytree <- apply.tree(data = data,
                           formula = formula,
                           tree.definitions = tree.definitions,
                           sens.w = sens.w,
                           cost.outcomes = cost.outcomes,
                           cost.cues = cost.cues)

if(goal != "cost") {

tree.order <- rank(-1 * my.applytree$treestats[[goal]], ties.method = "first")

} else {

  tree.order <- rank(my.applytree$treestats[[goal]], ties.method = "first")

}

tree.definitions$rank <- tree.order
tree.definitions <- tree.definitions[order(tree.definitions$rank),]
tree.definitions$tree <- 1:nrow(tree.definitions)
tree.definitions <- tree.definitions[,names(tree.definitions) != "rank"]
rownames(tree.definitions) <- 1:nrow(tree.definitions)

return(list(tree.definitions = tree.definitions,
            cue.accuracies = cue_best_df,
            cue_best_df = cue_best_df))

}
