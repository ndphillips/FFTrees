#' Grows fast and frugal trees
#' @param formula a formula
#' @param data A dataset
#' @param max.levels The maximum number of levels in the tree(s)
#' @param verbose A logical value indicating whether or not to display progress
#' @param rank.method A string indicating how to rank cues during tree construction. "m" (for marginal) means that cues will only be ranked once with the entire training dataset. "c" (conditional) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars.
#' @param repeat.cues A logical value indicating whether or not to allow repeated cues in the tree. Only relevant when `rank.method = 'c'.
#' @param hr.weight A value between 0 and 1 indicating how much weight to give to maximizing hit rates versus minimizing false alarm rates. Used for ranking cues in the tree.
#' @param stopping.rule A string indicating the method to stop growing trees. "levels" means the tree grows until a certain level. "exemplars" means the tree grows until a certain number of unclassified exemplars remain. "statdelta" means the tree grows until the change in the tree.criterion statistic is less than a specified level.
#' @param stopping.par A number indicating the parameter for the stopping rule. For stopping.rule == "levels", this is the number of levels. For stopping rule == "exemplars", this is the smallest percentage of examplars allowed in the last level.
#' @importFrom stats anova predict glm as.formula
#' @return A list of length 4. tree.definitions contains definitions of the tree(s). tree.stats contains classification statistics for the tree(s). levelout shows which level in the tree(s) each exemplar is classified. Finally, decision shows the classification decision for each tree for each exemplar
#' @export
#' @examples
#'
#'  titanic.trees <- grow.FFTrees(formula = survived ~.,
#'                                    data = titanic)
#'
#' # Tree definitions are stored in tree.definitions
#'
#' titanic.trees$tree.definitions
#'
#' # Tree classification statistics are in tree.stats
#'
#' titanic.trees$tree.stats
#'
#' # The level at which each exemplar is classified for each tree is in levelout
#'
#' titanic.trees$levelout
#'
#' # The decision for each exemplar for each tree is in decision
#'
#' titanic.trees$decision
#'
#'
#'


grow.FFTrees <- function(formula,
                         data,
                         rank.method = "m",
                         repeat.cues = TRUE,
                         hr.weight = .5,
                         max.levels = 4,
                         stopping.rule = "exemplars",
                         stopping.par = .1,
                         verbose = F
) {
#

numthresh.method <- "o"
tree.criterion <- "v"
exit.method <- "fixed"
correction <- .25
rounding <- 2



# Set up dataframes

data.o <- data

data <- model.frame(formula = formula, data = data)
cue.df <- data[,2:ncol(data)]

if(ncol(data) == 2) {

  cue.df <- data.frame(cue.df)
  names(cue.df) <- names(data)[2]

}

criterion.v <- data[,1]
crit.name <- names(data)[1]
n.cues <- ncol(cue.df)

# ----------
# INITIAL TRAINING CUE ACCURACIES
# ----------

cue.accuracies <- cuerank(formula = formula,
                        data = data,
                        tree.criterion = tree.criterion,
                        numthresh.method = numthresh.method,
                        rounding = rounding,
                        verbose = verbose)

# ----------
# GROW TREES
# ----------
{

  if(verbose) {print("Growing trees..")}

# SETUP TREES
# create tree.dm (exit values and n.levels)

if(max.levels > 1) {

  expand.ls <- lapply(1:(max.levels - 1), FUN = function(x) {return(c(0, 1))})
  expand.ls[[length(expand.ls) + 1]] <- .5
  names(expand.ls) <- c(paste("exit.", 1:(max.levels - 1), sep = ""),
                      paste("exit.", max.levels, sep = ""))

  tree.dm <- expand.grid(
  expand.ls,
  stringsAsFactors = F)
}

if(max.levels == 1) {
  tree.dm <- data.frame("exit.1" = .5)
}


tree.dm$tree.num <- 1:nrow(tree.dm)
n.trees <- nrow(tree.dm)

# Set up decision.df and levelout.df

decision <- as.data.frame(matrix(NA,
                               nrow = length(criterion.v),
                               ncol = n.trees))

levelout <- as.data.frame(matrix(NA,
                               nrow = length(criterion.v),
                               ncol = n.trees))

names(decision) <- paste("tree", 1:n.trees, sep = ".")
names(levelout) <- paste("tree", 1:n.trees, sep = ".")

# Loop over trees
for(tree.i in 1:n.trees) {

## Determine exits for tree.i

level.exits.v.i <- unlist(tree.dm[tree.i, grepl("exit.", names(tree.dm))])
n.levels <- length(level.exits.v.i)

## Set up placeholders

cue.accuracies.original <- cue.accuracies
decision.v <- rep(NA, length(criterion.v))
levelout.v <- rep(NA, length(criterion.v))

## level.stats shows cumulative classification decisions statistics at each level
level.stats = data.frame("level" = NA,
                         "cue" = NA,
                         "class" = NA,
                         "threshold" = NA,
                         "direction" = NA,
                         "exit" = NA)

level.stat.names <- names(classtable(1, 1))
level.stats[level.stat.names] <- NA

## asif.stats shows cumulative classification statistics as if all exemplars were
#   classified at the current level (i.e; if the tree stopped here)

asif.stats <- data.frame("level" = 1:n.levels,
                         "hr" = NA,
                         "far" = NA,
                         "v" = NA,
                         "v.change" = NA)

# Starting values
grow.tree <- T
current.level <- 0

# ------------------
# Grow Tree!
# --------------------

while(grow.tree == T) {

current.level <- current.level + 1
current.exit <- level.exits.v.i[current.level]
remaining.exemplars <- is.na(decision.v)

# Step 1) Determine cue

if(rank.method == "m") {

cue.accuracies.current <- cue.accuracies.original[(cue.accuracies.original$cue %in% level.stats$cue) == F,]

}

if(rank.method == "c") {

data.r <- data[remaining.exemplars, ]

# If cues canNOT be repeated, then remove old cues as well
if(repeat.cues == FALSE) {

remaining.cues.index <- (names(cue.df) %in% level.stats$cue) == F
remaining.cues <- names(cue.df)[remaining.cues.index]
data.r <- data.r[, c(crit.name, remaining.cues)]

}

# Calculate new cue accuracies
cue.accuracies.current <-  cuerank(formula = formula,
                                   data = data.r,
                                   tree.criterion = tree.criterion,
                                   numthresh.method = numthresh.method,
                                   rounding = rounding)

}

# GET NEXT CUE BASED ON WEIGHTED HR AND FAR

hr.vec <- cue.accuracies.current$hr
far.vec <- cue.accuracies.current$far

if(tree.criterion == "v") {

  weighted.v.vec <- hr.vec * hr.weight - far.vec * (1 - hr.weight)
  best.cue.index <- which(weighted.v.vec == max(weighted.v.vec))

}

if(substr(tree.criterion, 1, 1) == "d") {

  weighted.d.vec <- qnorm(hr.vec) * hr.weight - qnorm(far.vec) * (1 - hr.weight)
  best.cue.index <- which(weighted.d.vec == max(weighted.d.vec))

}

new.cue <- cue.accuracies.current$cue[best.cue.index]
if(length(new.cue) > 1) {new.cue <- new.cue[sample(1:length(new.cue), size = 1)]}

new.cue.stats <- cue.accuracies.current[cue.accuracies.current$cue == new.cue,]

new.class <- new.cue.stats$class
new.threshold <- new.cue.stats$threshold
new.direction <- new.cue.stats$direction

# ADD CUE INFO TO LEVEL.STATS

level.stats$level[current.level] <- current.level
level.stats$cue[current.level] <- new.cue
level.stats$class[current.level] <- new.class
level.stats$threshold[current.level] <- new.threshold
level.stats$direction[current.level] <- new.direction
level.stats$exit[current.level] <- current.exit

# CUE DECISIONS

cue.decisions <- apply.break(direction = new.direction,
                                 threshold.val = new.threshold,
                                 cue.v = data[[new.cue]],
                                 cue.class = new.cue.stats$class)

cue.classtable <- classtable(prediction.v = cue.decisions,
                                 criterion.v = criterion.v)

# ASIF DECISIONS

as.if.decision.v <- decision.v
as.if.decision.v[remaining.exemplars] <- cue.decisions[remaining.exemplars]

asif.classtable <- classtable(prediction.v = as.if.decision.v,
                              criterion.v = criterion.v)

asif.stats[current.level, c("hr", "far", "v")] <-  asif.classtable[1, c("hr", "far", "v")]

# If ASIF classification is perfect, then stop!
if(asif.stats$v[current.level] == 1) {break}

if(current.level == 1) {
  asif.stats$v.change[1] <- asif.classtable$v
}

if(current.level > 1) {

  v.change <- asif.stats$v[current.level] - asif.stats$v[current.level - 1]
  asif.stats$v.change[current.level] <- v.change

}

# Step 3) Classify participants in current level
{

if(current.exit == 1 | current.exit == .5) {

  decide.1.index <- remaining.exemplars & cue.decisions == T

  decision.v[decide.1.index] <- 1
  levelout.v[decide.1.index] <- current.level

}


if(current.exit == 0 | current.exit == .5) {

  decide.0.index <- is.na(decision.v) & cue.decisions == F

  decision.v[decide.0.index] <- 0
  levelout.v[decide.0.index] <- current.level


}
}

remaining.exemplars <- is.na(decision.v)

# Step X) Update Results
{

# Get cumulative stats of examplars currently classified

cum.classtable <- classtable(
  prediction.v = decision.v[remaining.exemplars == F],
  criterion.v = criterion.v[remaining.exemplars == F])

# Update level stats

level.stats[current.level, c("level", "cue", "class", "threshold", "direction", "exit")] <- c(
  current.level, new.cue.stats[c("cue", "class", "threshold", "direction")], current.exit)

level.stats[current.level, names(cum.classtable)] <- cum.classtable

}

# Step 4) Continue growing tree?
{

n.remaining <- sum(remaining.exemplars)

if(n.remaining > 0 & current.level != n.cues & exit.method == "fixed") {

  if(current.level < n.levels) {grow.tree <- T}
  if(current.level == n.levels) {grow.tree <- F}

}
if(n.remaining == 0 | current.level == n.cues) {break}
if(stopping.rule == "exemplars" & n.remaining < stopping.par * nrow(cue.df)) {break}
if(stopping.rule == "levels" & current.level == stopping.par) {break}

  level.stats[current.level + 1,] <- NA

}

}  # STOP while(grow.tree) Loop

# Step 5) No more growth. Turn last level into a bi-directional one
{

last.level <- max(level.stats$level)
last.cue <- level.stats$cue[last.level]

last.exitdirection <- level.stats$exit[level.stats$level == last.level]

if(last.exitdirection != .5) {

decision.v[levelout.v == last.level] <- NA

new.cue.stats <- cue.accuracies.current[cue.accuracies.current$cue == last.cue,]

decision.index <- is.na(decision.v)

# Step 2) Determine accuracy of negative and positive classification

current.decisions <- apply.break(direction = new.direction,
                                 threshold.val = new.threshold,
                                 cue.v = data[[last.cue]],
                                 cue.class = new.class
)

decide.0.index <- decision.index == T & current.decisions == 0
decide.1.index <- decision.index == T & current.decisions == 1

decision.v[decide.0.index] <- 0
decision.v[decide.1.index] <- 1

levelout.v[decide.0.index] <- current.level
levelout.v[decide.1.index] <- current.level

# up

last.classtable <- classtable(prediction.v = decision.v,
                              criterion.v = criterion.v)

level.stats$exit[last.level] <- .5
level.stats[last.level, names(last.classtable)] <- last.classtable

}

}

# ------------------
# Tree is finished!
# --------------------

# Set up final output

decision[,tree.i] <- decision.v
levelout[,tree.i] <- levelout.v

level.stats$tree <- tree.i

if(tree.i == 1) {level.stats.df <- level.stats}
if(tree.i > 1) {level.stats.df <- rbind(level.stats.df, level.stats)}

  }

}

# -------------------------
# SUMMARISE TREE DEFINITIONS AND STATISTICS
#   trees
# -------------------------
{
  # Summarise tree definitions

  stat.names <- names(classtable(1, 1))

  trees <- as.data.frame(matrix(NA, nrow = n.trees, ncol = 7))
  names(trees) <- c("tree", "cues", "nodes", "classes", "exits", "thresholds", "directions")

  for(i in 1:n.trees) {
    trees$tree[i] <- i
    trees$cues[i] <- paste(level.stats.df$cue[level.stats.df$tree == i], collapse = ";")
    trees$nodes[i] <- length(level.stats.df$cue[level.stats.df$tree == i])
    trees$classes[i] <- paste(level.stats.df$class[level.stats.df$tree == i], collapse = ";")
    trees$exits[i] <- paste(level.stats.df$exit[level.stats.df$tree == i], collapse = ";")
    trees$thresholds[i] <- paste(level.stats.df$threshold[level.stats.df$tree == i], collapse = ";")
    trees$directions[i] <- paste(level.stats.df$direction[level.stats.df$tree == i], collapse = ";")

  }

  # Determine final tree train statistics

  for(tree.i in 1:n.trees) {

    tree.i.stats <- classtable(prediction.v = decision[,tree.i],
                                     criterion.v = criterion.v)

    if(tree.i == 1) {tree.stats <- tree.i.stats}
    if(tree.i != 1) {tree.stats <- rbind(tree.stats,
                                         tree.i.stats)}

  }

  trees <- cbind(trees, tree.stats)


  # Remove duplicate trees...

  duplicate.trees <- duplicated(trees[c("cues", "exits", "thresholds", "directions")])
  trees <- trees[duplicate.trees == F,]
  rownames(trees) <- 1:nrow(trees)
  trees$tree <- 1:nrow(trees)
  trees <- trees[,c(which(names(trees) == "tree"), which(names(trees) != "tree"))]

  levelout <- levelout[,duplicate.trees == F]
  decision <- decision[,duplicate.trees == F]

if(sum(duplicate.trees == F) > 1) {
  # sort trees by far

  tree.far.order <- order(trees$far)

  trees <- trees[tree.far.order, ]
  levelout <- levelout[,tree.far.order]
  decision <- decision[,tree.far.order]
  colnames(levelout) <- paste("tree.", 1:ncol(levelout), sep = "")
  colnames(decision) <- paste("tree.", 1:ncol(decision), sep = "")
}

  trees$tree <- 1:nrow(trees)


}

# setup output

tree.definitions <- trees[,c("tree", "cues", "nodes", "classes", "exits", "thresholds", "directions")]
tree.stats <- trees[,c("tree", names(classtable(1, 1)))]


output <- list(tree.definitions = tree.definitions,
               tree.stats = tree.stats,
               levelout = levelout,
               decision = decision)

return(output)

}
