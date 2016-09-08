#' Grows fast and frugal trees
#' @param formula a formula
#' @param data.mf A training dataset
#' @param data.test A testing dataset
#' @param max.levels The maximum number of levels in the tree(s)
#' @param verbose A logical value indicating whether or not to display progress
#' @param numthresh.method A string indicating how to calculate cue splitting thresholds. "m" = median split, "o" = split that maximizes the tree criterion.
#' @param rank.method A string indicating how to rank cues during tree construction. "m" (for marginal) means that cues will only be ranked once with the entire training dataset. "c" (conditional) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars.
#' @param stopping.rule A string indicating the method to stop growing trees. "levels" means the tree grows until a certain level. "exemplars" means the tree grows until a certain number of unclassified exemplars remain. "statdelta" means the tree grows until the change in the tree.criterion statistic is less than a specified level.
#' @param stopping.par A number indicating the parameter for the stopping rule. For stopping.rule == "levels", this is the number of levels. For stopping rule == "exemplars", this is the smallest percentage of examplars allowed in the last level.
#' @importFrom stats anova predict glm as.formula
#' @return A list of length 3. The first element "tree.acc" is a dataframe containing the final statistics of all trees. The second element "cue.accuracies" shows the accuracies of all cues. The third element "tree.class.ls" is a list with n.trees elements, where each element shows the final decisions for each tree for each exemplar.
#' @export
#'


grow.FFTrees <- function(formula,
                         data,
                         rank.method = "m",
                         numthresh.method = "o",
                         max.levels = 4,
                         stopping.rule = "exemplars",
                         stopping.par = .1,
                         verbose = F
) {


tree.criterion <- "v"
exit.method <- "fixed"
correction <- .25
rounding <- 2
hr.weight <- .5


# Set up dataframes

data.o <- data

data <- model.frame(formula = formula, data = data)
cue.df <- data[,2:ncol(data)]
crit <- data[,1]
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
                          verbose = verbose
)

# ----------
# GROW TREES
# ----------
{

  if(verbose) {print("Growing trees..")}

# SETUP TREES
# create tree.dm (exit values and n.levels)

expand.ls <- lapply(1:(max.levels - 1), FUN = function(x) {return(c(0, 1))})
expand.ls[[length(expand.ls) + 1]] <- .5
names(expand.ls) <- c(paste("exit.", 1:(max.levels - 1), sep = ""),
                    paste("exit.", max.levels, sep = "")
)

tree.dm <- expand.grid(
expand.ls,
stringsAsFactors = F
)

tree.dm$tree.num <- 1:nrow(tree.dm)
n.trees <- nrow(tree.dm)

# Set up decision.df and levelout.df

decision <- as.data.frame(matrix(NA,
                               nrow = length(crit),
                               ncol = n.trees))

levelout <- as.data.frame(matrix(NA,
                               nrow = length(crit),
                               ncol = n.trees))

names(decision) <- paste("tree", 1:n.trees, sep = ".")
names(levelout) <- paste("tree", 1:n.trees, sep = ".")

# Loop over trees
for(tree.i in 1:n.trees) {

# ----------
# Step 3: Grow tree.i
# ----------

## Determine exits for tree.i

level.exits.v.i <- unlist(tree.dm[tree.i, grepl("exit.", names(tree.dm))])
n.levels <- length(level.exits.v.i)

## Set up placeholders

data.i <- data

cue.df.i <- cue.df
criterion.v.i <- crit

cue.accuracies.original <- cue.accuracies
decision.v <- rep(NA, length(crit))
levelout.v <- rep(NA, length(crit))

level.stats = data.frame("cue.order" = NA,
                       "cue" = NA,
                       "class" = NA,
                       "threshold" = NA,
                       "direction" = NA,
                       "exit" = NA
)

level.stat.names <- names(classtable(1, 1))
level.stats[level.stat.names] <- NA

# Starting values
grow.tree <- T
current.level <- 0

# Apply break function
apply.break <- function(direction,
                      threshold.val,
                      cue.v,
                      cue.class
) {


if(is.character(threshold.val)) {threshold.val <- unlist(strsplit(threshold.val, ","))}

if(cue.class %in% c("numeric", "integer")) {threshold.val <- as.numeric(threshold.val)}


if(direction == "!=") {output <- (cue.v %in% threshold.val) == F}
if(direction == "=") {output <- cue.v %in% threshold.val}
if(direction == "<") {output <- cue.v < threshold.val}
if(direction == "<=") {output <- cue.v <= threshold.val}
if(direction == ">") {output <- cue.v > threshold.val}
if(direction == ">=") {output <- cue.v >= threshold.val}


return(output)

}

# ------------------
# Grow Tree!
# --------------------

while(grow.tree == T) {

current.level <- current.level + 1

# Step 1) Determine cue

if(rank.method == "m") {

cue.accuracies.current <- cue.accuracies.original[(cue.accuracies.original$cue %in% level.stats$cue) == F,]

}

if(rank.method == "c") {

remaining.exemplars <- is.na(decision.v)
remaining.cues.index <- (names(cue.df.i) %in% level.stats$cue) == F
remaining.cues <- names(cue.df.i)[remaining.cues.index]

# REDUCED DATASET WITH REMAINING EXEMPLARS AND CUES
data.r <- data[remaining.exemplars, c(crit.name, remaining.cues)]

cue.accuracies.current <-  cuerank(formula = formula,
                                   data = data.r,
                                   tree.criterion = tree.criterion,
                                   numthresh.method = numthresh.method,
                                   rounding = rounding
)

}

best.cue.df.index <- which(cue.accuracies.current[tree.criterion] == max(cue.accuracies.current[tree.criterion], na.rm = T))

new.cue <- cue.accuracies.current$cue[best.cue.df.index]

if(length(new.cue) > 1) {new.cue <- new.cue[sample(1:length(new.cue), size = 1)]}

new.cue.stats <- cue.accuracies.current[cue.accuracies.current$cue == new.cue,]

# Step 2) Make decisions If all remaining exemplars were classified

current.decisions <- apply.break(direction = new.cue.stats$direction,
                               threshold.val = new.cue.stats$threshold,
                               cue.v = data.i[, new.cue],
                               cue.class = new.cue.stats$class
)

current.classtable <- classtable(prediction.v = current.decisions[is.na(decision.v)],
                                     criterion.v = criterion.v.i[is.na(decision.v)])


current.exit <- level.exits.v.i[current.level]


# Step 3) Classify participants in current level
{

if(current.exit == 1 | current.exit == .5) {

  decide.1.index <- is.na(decision.v) & current.decisions == T

  decision.v[decide.1.index] <- 1
  levelout.v[decide.1.index] <- current.level

}


if(current.exit == 0 | current.exit == .5) {

  decide.0.index <- is.na(decision.v) & current.decisions == F

  decision.v[decide.0.index] <- 0
  levelout.v[decide.0.index] <- current.level


}
}

# Step X) Update Results
{

# Get cumulative stats of examplars currently classified

cum.classtable <- classtable(
  prediction.v = decision.v[levelout.v <= current.level & is.na(levelout.v) == F],
  criterion.v = criterion.v.i[levelout.v <= current.level & is.na(levelout.v) == F])

if(current.level > 1) {

  level.stats[nrow(level.stats) + 1, ] <- NA

}

# Update level stats

level.stats[current.level, c("cue.order", "cue", "class", "threshold", "direction", "exit")] <- c(
  current.level, new.cue.stats[c("cue", "class", "threshold", "direction")], current.exit)


level.stats[current.level, names(cum.classtable)] <- cum.classtable

}

# Step 4) Continue growing tree?
{

n.remaining <- sum(is.na(decision.v))

if(n.remaining > 0 & current.level != n.cues & exit.method == "fixed") {

  if(current.level < n.levels) {grow.tree <- T}
  if(current.level == n.levels) {grow.tree <- F}

}
if(n.remaining == 0 | current.level == n.cues) {grow.tree <- F}
if(stopping.rule == "exemplars" & n.remaining < stopping.par * nrow(cue.df.i)) {grow.tree <- F}
if(stopping.rule == "levels" & current.level == stopping.par) {grow.tree <- F}

}

}  # STOP while(grow.tree) Loop

# Step 5) No more growth. Turn last level into a bi-directional one
{

last.level <- max(level.stats$cue.order)
last.cue <- level.stats$cue[last.level]

last.exitdirection <- level.stats$exit[level.stats$cue.order == last.level]

if(last.exitdirection != .5) {

decision.v[levelout.v == last.level] <- NA

new.cue.stats <- cue.accuracies.current[cue.accuracies.current$cue == last.cue,]

decision.index <- is.na(decision.v)

# Step 2) Determine accuracy of negative and positive classification

current.decisions <- apply.break(direction = new.cue.stats$direction,
                                 threshold.val = new.cue.stats$threshold,
                                 cue.v = data.i[[last.cue]],
                                 cue.class = new.cue.stats$class
)

decide.0.index <- decision.index == T & current.decisions == 0
decide.1.index <- decision.index == T & current.decisions == 1

decision.v[decide.0.index] <- 0
decision.v[decide.1.index] <- 1

levelout.v[decide.0.index] <- current.level
levelout.v[decide.1.index] <- current.level

# up

last.classtable <- classtable(prediction.v = decision.v,
                              criterion.v = crit)

level.stats$exit[last.level] <- .5
level.stats[last.level, names(current.classtable)] <- last.classtable

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
                                     criterion.v = crit)

    if(tree.i == 1) {tree.stats <- tree.i.stats}
    if(tree.i != 1) {tree.stats <- rbind(tree.stats,
                                               tree.i.stats)}

  }

  tree.stats$n <- nrow(data)


  tree.stats <- tree.stats[c(stat.names, setdiff(names(tree.stats), stat.names))]
  trees <- cbind(trees, tree.stats[, 1:8])


  # Remove duplicate trees...

  duplicate.trees <- duplicated(trees[c("cues", "exits", "thresholds", "directions")])
  trees <- trees[duplicate.trees == F,]
  rownames(trees) <- 1:nrow(trees)
  trees$tree <- 1:nrow(trees)
  trees <- trees[,c(which(names(trees) == "tree"), which(names(trees) != "tree"))]

  levelout <- levelout[,duplicate.trees == F]
  decision <- decision[,duplicate.trees == F]

  # sort trees by far

  tree.far.order <- order(trees$far)

  trees <- trees[tree.far.order, ]
  levelout <- levelout[,tree.far.order]
  decision <- decision[,tree.far.order]

  trees$tree <- 1:nrow(trees)
  colnames(levelout) <- paste("tree.", 1:ncol(levelout), sep = "")
  colnames(decision) <- paste("tree.", 1:ncol(decision), sep = "")

}


  # setup output

  output <- list(trees = trees,
                 levelout = levelout,
                 decision = decision
                 )

  return(output)

}
