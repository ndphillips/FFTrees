#' Grows fast-and-frugal trees using the fan algorithm
#'
#' @param formula formula. A formula
#' @param data dataframe. A dataset
#' @param max.levels integer. The maximum number of levels in the tree(s)
#' @param algorithm character. A string indicating how to rank cues during tree construction. "ifan"  (independent fan) means that cues will only be ranked once with the entire training dataset "dfan" (dependent fan) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars.
#' @param goal character. A string indicating the statistic to maximize: "acc" = overall accuracy, "bacc" = balanced accuracy, "wacc" = weighted accuracy, "bacc" = balanced accuracy
#' @param goal.chase character. A string indicating the statistic to maximize when constructing trees: "acc" = overall accuracy, "wacc" = weighted accuracy, "bacc" = balanced accuracy
#' @param sens.w numeric. A number from 0 to 1 indicating how to weight sensitivity relative to specificity.
#' @param cost.outcomes numeric. A vector of length 4 specifying the costs of a hit, false alarm, miss, and correct rejection rspectively. E.g.; \code{cost.outcomes = c(0, 10, 20, 0)} means that a false alarm and miss cost 10 and 20 respectively while correct decisions have no cost.
#' @param cost.cues dataframe. A dataframe with two columns specifying the cost of each cue. The first column should be a vector of cue names, and the second column should be a numeric vector of costs. Cues in the dataset not present in \code{cost.cues} are assume to have 0 cost.
#' @param numthresh.method character. How should thresholds for numeric cues be determined? \code{"o"} will optimize thresholds, while \code{"m"} will always use the median.
#' @param stopping.rule character. A string indicating the method to stop growing trees. \code{"levels"} means the tree grows until a certain level, \code{"exemplars"} means the tree grows until a certain number of unclassified exemplars remain. \code{"statdelta"} means the tree grows until the change in the criterion statistic is less than a specified level.
#' @param stopping.par numeric. A number indicating the parameter for the stopping rule. For stopping.rule == \code{"levels"}, this is the number of levels. For stopping rule \code{"exemplars"}, this is the smallest percentage of examplars allowed in the last level.
#' @param rounding integer. How much should threshold parameters be rounded? Default is
#' @param progress logical. Should tree growing progress be displayed?
#' @param ... Currently ignored
#' @importFrom stats anova predict glm as.formula var
#'
#' @return A definition of an FFT
#' @export
#'
fan.algorithm <- function(formula,
                          data,
                          max.levels = 5,
                          algorithm = "ifan",
                          goal = "wacc",
                          goal.chase = "bacc",
                          sens.w = .5,
                          cost.outcomes = c(0, 1, 1, 0),
                          cost.cues = NULL,
                          numthresh.method = "o",
                          stopping.rule = "exemplars",
                          stopping.par = .1,
                          rounding = NULL,
                          progress = TRUE) {

#
  # formula = diagnosis ~.
  # data = heart.train
  # data.test = heart.test
  # max.levels = 4
  # algorithm = "ifan"
  # goal = "cost"
  # goal.chase = "cost"
  # sens.w = .5
  # cost.outcomes = c(0, 1, 3, 0)
  # cost.cues = NULL
  # numthresh.method = "o"
  # stopping.rule = "exemplars"
  # stopping.par = .1
  # rounding = NULL
  # progress = TRUE

# # Some global variables which could be changed later.

repeat.cues <- TRUE
exit.method <- "fixed"
correction <- .25


# Start with criterion
data.mf <- model.frame(formula, data)
criterion.name <- names(data.mf)[1]
criterion.v <- data.mf[,1]
cues.n <- ncol(data.mf) - 1
cases.n <- nrow(data.mf)
cue.df <- data.mf[,2:ncol(data.mf)]
cue.names <- names(cue.df)

# Fill in emptry cost.cues
{
if(is.null(cost.cues)) {

  cost.cues <- data.frame("cue" = cue.names,
                          "cost" = rep(0, cues.n),
                          stringsAsFactors = FALSE)

} else {

  # Which cues are missing in cost.cues?

  missing.cues <- setdiff(cue.names, cost.cues[,1])

  if(length(missing.cues) > 0) {

    cost.cues.missing <- data.frame("cue" = missing.cues,
                                    "cost" = rep(0, length(missing.cues)))

    cost.cues <- rbind(cost.cues, cost.cues.missing)


  }

}
}

# ----------
# INITIAL TRAINING CUE ACCURACIES
# ----------

cue.accuracies <- cuerank(formula = formula,
                          data = data,
                          goal = goal.chase,
                          numthresh.method = numthresh.method,
                          rounding = rounding,
                          progress = progress,
                          sens.w = sens.w,
                          cost.outcomes = cost.outcomes,
                          cost.cues = cost.cues,
                          considerFALSE = TRUE,
                          cue.rules = NULL)

# ----------
# GROW TREES
# ----------
{

# SETUP TREES
# create tree.dm which contains exit values for max.levels

if(max.levels > 1) {

  expand.ls <- lapply(1:(max.levels - 1),
                      FUN = function(x) {return(c(0, 1))})

  expand.ls[[length(expand.ls) + 1]] <- .5
  names(expand.ls) <- c(paste("exit.", 1:(max.levels - 1), sep = ""),
                        paste("exit.", max.levels, sep = ""))

  tree.dm <- expand.grid(
    expand.ls,
    stringsAsFactors = FALSE)

}

if(max.levels == 1) {

  tree.dm <- data.frame("exit.1" = .5)

}

tree.dm$tree.num <- 1:nrow(tree.dm)
trees.n <- nrow(tree.dm)

# Set up decision.df and levelout.df
#  These contain each tree's decisions and the level at which
#   classifications are made

decision <- as.data.frame(matrix(NA,
                                 nrow = cases.n,
                                 ncol = trees.n))

levelout <- as.data.frame(matrix(NA,
                                 nrow = cases.n,
                                 ncol = trees.n))

cuecost <- as.data.frame(matrix(NA,
                             nrow = cases.n,
                             ncol = trees.n))

outcomecost <- as.data.frame(matrix(NA,
                                nrow = cases.n,
                                ncol = trees.n))

totalcost  <- as.data.frame(matrix(NA,
                                   nrow = cases.n,
                                   ncol = trees.n))

names(decision) <- paste("tree", 1:trees.n, sep = ".")
names(levelout) <- paste("tree", 1:trees.n, sep = ".")
names(cuecost) <- paste("tree", 1:trees.n, sep = ".")
names(totalcost) <- paste("tree", 1:trees.n, sep = ".")


  # Loop over trees
for(tree.i in 1:trees.n) {

  ## Determine exits for tree.i

  level.exits.v.i <- unlist(tree.dm[tree.i, grepl("exit.", names(tree.dm))])
  levels.n <- length(level.exits.v.i)

  ## Set up placeholders
  cue.accuracies.original <- cue.accuracies

  # Decisions, levelout, and cost vectors
  decision.v <- rep(NA, cases.n)
  levelout.v <- rep(NA, cases.n)
  cuecost.v <- rep(0, cases.n)
  outcomecost.v <- rep(NA, cases.n)
  totalcost.v <- rep(0, cases.n)

  ## level.stats shows cumulative classification decisions statistics at each level
  level.stats = data.frame("level" = NA,
                           "cue" = NA,
                           "cost.cue" = NA,
                           "cost.cue.cum" = NA,
                           "class" = NA,
                           "threshold" = NA,
                           "direction" = NA,
                           "exit" = NA)

  level.stat.names <- names(classtable(1, 1))
  level.stats[level.stat.names] <- NA

  ## asif.stats shows cumulative classification statistics as if all exemplars were
  #   classified at the current level (i.e; if the tree stopped here)

  asif.stats <- data.frame("level" = 1:levels.n,
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
  current.level <- 0

    # ------------------
    # Grow Tree!
    # --------------------

while(grow.tree == TRUE) {

current.level <- current.level + 1
current.exit <- level.exits.v.i[current.level]
remaining.exemplars <- is.na(decision.v)

# Step 1) Determine cue for current level

# ifan algorithm
if(algorithm == "ifan") {

  # Get accuracies of un-used cues
  cue.accuracies.current <- cue.accuracies.original[(cue.accuracies.original$cue %in% level.stats$cue) == FALSE,]

}

# Conditional algorithm
if(algorithm == "dfan") {

  data.mf.r <- data.mf[remaining.exemplars, ]

  # If cues can NOT be repeated, then remove old cues as well
  if(repeat.cues == FALSE) {

    remaining.cues.index <- (names(cue.df) %in% level.stats$cue) == FALSE
    remaining.cues <- names(cue.df)[remaining.cues.index]
    data.mf.r <- data.mf.r[, c(criterion.name, remaining.cues)]

  }

  # If there is no variance in the criterion, then stop growth!
  if(var(data.mf.r[,1]) == 0) {grow.tree <- FALSE ; break}

  # Calculate cue accuracies with remaining exemplars
  cue.accuracies.current <-  cuerank(formula = formula,
                                     data = data.mf.r,
                                     goal = goal.chase,
                                     numthresh.method = numthresh.method,
                                     rounding = rounding,
                                     sens.w = sens.w,
                                     cost.outcomes = cost.outcomes,
                                     cost.cues = cost.cues)

}

# Get next cue based on maximizing  (or minimizing) goal
if(grepl("cost", goal.chase)) {best.cue.index <- which(cue.accuracies.current[[goal.chase]] == min(cue.accuracies.current[[goal.chase]]))}
if(grepl("cost", goal.chase) == FALSE) {best.cue.index <- which(cue.accuracies.current[[goal.chase]] == max(cue.accuracies.current[[goal.chase]]))}

# If there is a tie, take the first
if(length(best.cue.index) > 1) {best.cue.index <- best.cue.index[1]}

new.cue <- cue.accuracies.current$cue[best.cue.index]
new.cue.stats <- cue.accuracies.current[cue.accuracies.current$cue == new.cue,]
new.cost.cue <- new.cue.stats$cost.cue
new.class <- new.cue.stats$class
new.threshold <- new.cue.stats$threshold
new.direction <- new.cue.stats$direction

# Add cue costs to cuecost.v
cuecost.v[is.na(decision.v)] <- cuecost.v[is.na(decision.v)] + new.cost.cue

# ADD CUE INFO TO LEVEL.STATS

level.stats$level[current.level] <- current.level
level.stats$cue[current.level] <- new.cue
level.stats$cost.cue[current.level] <- new.cost.cue
level.stats$cost.cue.cum[current.level] <- sum(level.stats$cost.cue[1:current.level])
level.stats$class[current.level] <- new.class
level.stats$threshold[current.level] <- new.threshold
level.stats$direction[current.level] <- new.direction
level.stats$exit[current.level] <- current.exit

# Get decisions for current cue
cue.decisions <- apply.break(direction = new.direction,
                             threshold.val = new.threshold,
                             cue.v = data.mf[[new.cue]],
                             cue.class = new.cue.stats$class)

# Statistics for current decisions
cue.classtable <- classtable(prediction.v = cue.decisions,
                             criterion.v = criterion.v,
                             sens.w = sens.w,
                             cost.v = cuecost.v,
                             cost.outcomes = cost.outcomes)

# How would classifications look if all remaining exemplars
#   were classified at the current level?
as.if.decision.v <- decision.v
as.if.levelout.v <- levelout.v
as.if.cuecost.v <- cuecost.v

as.if.decision.v[remaining.exemplars] <- cue.decisions[remaining.exemplars]
as.if.levelout.v[remaining.exemplars] <- current.level
as.if.cuecost.v[remaining.exemplars] <- new.cost.cue

asif.classtable <- classtable(prediction.v = as.if.decision.v,
                              criterion.v = criterion.v,
                              sens.w = sens.w,
                              cost.v = as.if.cuecost.v,
                              cost.outcomes = cost.outcomes)

  asif.stats[current.level, c("sens", "spec", "acc", "bacc", "wacc", "dprime", "cost")] <-  asif.classtable[1, c("sens", "spec", "acc", "bacc", "wacc", "dprime","cost")]


  # If ASIF classification is perfect, then stop!

  if(goal.chase != "cost") {

  if(asif.stats[[goal.chase]][current.level] == 1) {grow.tree <- FALSE}

  }

  if(current.level == 1) {

    asif.stats$goal.change[1] <- asif.stats[[goal]][1]

  }

  if(current.level > 1) {

    goal.change <- asif.stats[[goal.chase]][current.level] - asif.stats[[goal.chase]][current.level - 1]
    asif.stats$goal.change[current.level] <- goal.change

  }

  # Step 3) Classify exemplars in current level
  {

    if(current.exit == 1 | current.exit == .5) {

      decide.1.index <- remaining.exemplars & cue.decisions == TRUE

      decision.v[decide.1.index] <- 1
      levelout.v[decide.1.index] <- current.level

    }

    if(current.exit == 0 | current.exit == .5) {

      decide.0.index <- is.na(decision.v) & cue.decisions == FALSE

      decision.v[decide.0.index] <- 0
      levelout.v[decide.0.index] <- current.level
    }


    # Update cost vectors

    hi.v <- decision.v == 1 & criterion.v == 1
    mi.v <- decision.v == 0 & criterion.v == 1
    fa.v <- decision.v == 1 & criterion.v == 0
    cr.v <- decision.v == 0 & criterion.v == 0

    outcomecost.v[hi.v == TRUE] <- cost.outcomes[1]
    outcomecost.v[mi.v == TRUE] <- cost.outcomes[2]
    outcomecost.v[fa.v == TRUE] <- cost.outcomes[3]
    outcomecost.v[cr.v == TRUE] <- cost.outcomes[4]

  }

  remaining.exemplars <- is.na(decision.v)

  # Step 4) Update Results
  {

    # NEED TO FIX THIS BELOW TO INCORPORATE ALL COSTS

    # Get cumulative stats of examplars currently classified

    cum.classtable <- classtable(prediction.v = decision.v[remaining.exemplars == FALSE],
                                criterion.v = criterion.v[remaining.exemplars == FALSE],
                                sens.w = sens.w,
                                cost.v = cuecost.v[remaining.exemplars == FALSE],
                                cost.outcomes = cost.outcomes)


    # Update level stats

    level.stats[current.level, c("level", "cue", "class", "threshold", "direction", "exit")] <- c(
      current.level, new.cue.stats[c("cue", "class", "threshold", "direction")], current.exit)

    level.stats[current.level, names(cum.classtable)] <- cum.classtable

  }

  # Step 5) Continue growing tree?
  {

    n.remaining <- sum(remaining.exemplars)

    if(n.remaining > 0 & current.level != cues.n & exit.method == "fixed") {

      if(current.level < levels.n) {grow.tree <- TRUE}
      if(current.level == levels.n) {grow.tree <- FALSE ; break}

    }
    if(n.remaining == 0 | current.level == cues.n) {break}
    if(stopping.rule == "exemplars" & n.remaining < stopping.par * nrow(cue.df)) {break}
    if(stopping.rule == "levels" & current.level == stopping.par) {break}


    if(algorithm == "dfan" & sd(criterion.v[remaining.exemplars]) == 0) {break}

    # Set up next level stats
    level.stats[current.level + 1,] <- NA

  }

}  # STOP while(grow.tree) Loop

# Step 5) No more growth. Make sure last level is bidirectional
{

  last.level <- max(level.stats$level)
  last.cue <- level.stats$cue[last.level]
  if(last.cue %in% cost.cues[,1]) {cost.cue <- cost.cues[cost.cues[,1] == last.cue,2]} else {

    cost.cue <- 0

  }


  last.exitdirection <- level.stats$exit[level.stats$level == last.level]

  if(last.exitdirection != .5) {

    decision.v[levelout.v == last.level] <- NA

    new.cue.stats <- cue.accuracies.current[cue.accuracies.current$cue == last.cue,]

    decision.index <- is.na(decision.v)

    # Step 2) Determine accuracy of negative and positive classification

    current.decisions <- apply.break(direction = new.direction,
                                     threshold.val = new.threshold,
                                     cue.v = data.mf[[last.cue]],
                                     cue.class = new.class)

    decide.0.index <- decision.index == TRUE & current.decisions == 0
    decide.1.index <- decision.index == TRUE & current.decisions == 1

    decision.v[decide.0.index] <- 0
    decision.v[decide.1.index] <- 1

    levelout.v[decide.0.index] <- current.level
    levelout.v[decide.1.index] <- current.level

    # up

    last.classtable <- classtable(prediction.v = decision.v,
                                  criterion.v = criterion.v,
                                  sens.w = sens.w,
                                  cost.v = cuecost.v,
                                  cost.outcomes = cost.outcomes)

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

  tree.definitions <- as.data.frame(matrix(NA, nrow = trees.n, ncol = 7))
  names(tree.definitions) <- c("tree", "nodes", "classes", "cues", "directions", "thresholds", "exits")

  level.stats.df$class <- substr(level.stats.df$class, 1, 1)

  for(i in 1:trees.n) {

    tree.definitions$tree[i] <- i
    tree.definitions$cues[i] <- paste(level.stats.df$cue[level.stats.df$tree == i], collapse = ";")
    tree.definitions$nodes[i] <- length(level.stats.df$cue[level.stats.df$tree == i])
    tree.definitions$classes[i] <- paste(level.stats.df$class[level.stats.df$tree == i], collapse = ";")
    tree.definitions$exits[i] <- paste(level.stats.df$exit[level.stats.df$tree == i], collapse = ";")
    tree.definitions$thresholds[i] <- paste(level.stats.df$threshold[level.stats.df$tree == i], collapse = ";")
    tree.definitions$directions[i] <- paste(level.stats.df$direction[level.stats.df$tree == i], collapse = ";")

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
            cue.accuracies = cue.accuracies))

}
