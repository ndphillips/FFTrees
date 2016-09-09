#' Create Fast and Frugal Trees (FFTrees)
#'
#' @param formula A formula
#' @param data A model training dataset. An m x n dataframe containing n cue values for each of the m exemplars.
#' @param data.test (Optional) A model testing dataset (same format as data.train)
#' @param max.levels A number indicating the maximum number of levels considered for the tree.
#' @param train.p A number between 0 and 1 indicating what percentage of the data to use for training. This only applies when data.test is not specified by the user.
#' @param rank.method A string indicating how to rank cues during tree construction. "m" (for marginal) means that cues will only be ranked once with the entire training dataset. "c" (conditional) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars.
#' @param do.cart,do.lr logical values indicating whether or not to evaluate logistic regression and/or CART on the data for comparison.
#' @param verbose A logical value indicating whether or not to print progress reports. Can be helpful for diagnosis when the function is running slowly...
#' @param object An optional existing FFTrees object (do not specify by hand)
#' @importFrom stats anova predict glm as.formula formula
#' @return A list of length 3. The first element "tree.acc" is a dataframe containing the final statistics of all trees. The second element "cue.accuracies" shows the accuracies of all cues. The third element "tree.class.ls" is a list with n.trees elements, where each element shows the final decisions for each tree for each exemplar.
#' @export
#'

FFTrees <- function(
                formula = NULL,
                data = NULL,
                data.test = NULL,
                train.p = 1,
                rank.method = "m",
                verbose = F,
                max.levels = 4,
                do.cart = T,
                do.lr = T,
                object = NULL
) {

# Set some global parameters

tree.criterion <- "v"
stopping.rule <- "exemplars"
stopping.par <- .1
correction <- .25
numthresh.method <- "o"
rounding <- 2
exit.method <- "fixed"


# EXTRACT OBJECTS FROM EXISTING FFTrees OBJECT

# GET FORMULA
if(is.null(object) == F) {

  formula <- object$formula

}

# DEFINE TESTING AND TRAINING DATA
{

if(is.null(object) == F) {

  data.train.o <- object$data$train
  data.train <- model.frame(formula = formula,
                            data = data.train.o)

  cue.train <- data.train[,2:ncol(data.train)]
  crit.train <- data.train[,1]

  if(is.null(data.test) == F) {

    data.test.o <- data.test

    data.test <- model.frame(formula = formula,
                             data = data.test.o)

    cue.test <- data.test[,2:ncol(data.test)]
    crit.test <- data.test[,1]

  }

  if(is.null(data.test)) {

    data.test.o <- NULL
    data.test <- NULL
    cue.test <- NULL
    crit.test <- NULL

  }

}

if(is.null(object) == T & train.p == 1) {

  data.train.o <- data
  data.train <- model.frame(formula = formula,
                            data = data.train.o)
  cue.train <- data.train[,2:ncol(data.train)]
  crit.train <- data.train[,1]

 if(is.null(data.test) == F) {

   data.test.o <- data.test
   data.test <- model.frame(formula = formula,
                            data = data.test)
   cue.test <- data.test[,2:ncol(data.test)]
   crit.test <- data.test[,1]

 }

 if(is.null(data.test)) {

  data.test.o <- NULL
  data.test <- NULL
  cue.test <- NULL
  crit.test <- NULL

}


}

## TRAINING / TESTING RANDOM SPLIT
if(is.null(data.test) & train.p < 1) {

  if(is.null(object) == F) {

    data.train.o <- object$data$train
  }

  if(is.null(object) == T) {

    data.train.o <- data

  }

  data.train <- model.frame(formula = formula,
                            data = data.train.o)
  cue.train <- data.train[,2:ncol(data.train)]
  crit.train <- data.train[,1]

  # create 'training testing' sets of size train.p * nrow(data.train)

  continue <- T
  run.i <- 0

  # Create train and test set

  # Test set must not have new factor values
  # training set must have at least one positive and one negative case

  while(continue) {

    run.i <- run.i + 1

    n.train <- floor(train.p * nrow(data.train))
    train.exemplars.i <- sample(1:nrow(data.train), size = n.train)
    test.exemplars.i <- setdiff(1:nrow(data.train), train.exemplars.i)
    crit.train.i <- data.train[train.exemplars.i, 1]
    crit.test.i <- data.train[test.exemplars.i, 1]

    data.train.i <- data.train[train.exemplars.i,]
    data.test.i <- data.train[test.exemplars.i,]

    orig.vals.ls <- lapply(1:ncol(data.train.i), FUN = function(x) {unique(data.train.i[,x])})

    can.predict.mtx <- matrix(1, nrow = nrow(data.test.i), ncol = ncol(data.test.i))

    for(i in 1:ncol(can.predict.mtx)) {

      test.vals.i <- data.test.i[,i]

      if(is.numeric(test.vals.i)) {
        can.predict.mtx[,i] <- 1} else {

          can.predict.mtx[,i] <- paste(test.vals.i) %in% paste(orig.vals.ls[[i]])


        }
    }

    model.can.predict <- rowMeans(can.predict.mtx) == 1


    if(mean(crit.train.i) > 0 & mean(crit.train.i) < 1 &
       mean(crit.test.i > 0) & mean(crit.test.i < 1) & all(model.can.predict)) {continue <- F}

    if(run.i == 50) {print("I'm having a hard time coming up with valid training and test data.train.mfsets...You may need to stop the processor and create them manually")}

  }

  data.train.full <- data.train

  data.train <- data.train.full[train.exemplars.i,]
  cue.train <- data.train[,2:ncol(data.train)]
  crit.train <- data.train[,1]

  data.test <- data.train.full[test.exemplars.i,]
  cue.test <- data.test[,2:ncol(data.test)]
  crit.test <- data.test[,1]

}

}

# SETUP
{

# Convert factors to strings
{
fac.to.string <- function(x) {

  if(is.null(x) == F) {

    for(i in 1:ncol(x)) {

      x.i <- x[,i]

      if("factor" %in% class(x.i)) {x[,i] <- paste(x.i)}

    }
  }

  return(x)

}

data.test <- fac.to.string(data.test)
data.train <- fac.to.string(data.train)
cue.train <- fac.to.string(cue.train)
cue.test <- fac.to.string(cue.test)

}

# Check for missing or bad inputs
{
if(is.null(data.train) |
   "data.frame" %in% class(data) == F) {

  stop("Please specify a valid dataframe object in data")

}
}
# MAKE SURE TRAINING AND TEST DATAFRAMES ARE SIMILAR

if(is.null(data.test) == F) {


if(setequal(names(data.train), names(data.test)) == F) {

  stop("Your training (data) and test (data.test) dataframes do not appear to have the same column names. Please fix and try again.")

}

}


## VALIDITY CHECKS
{
# Non-binary DV
if(setequal(crit.train, c(0, 1)) == F) {

  stop("Warning! Your DV is not binary or logical. Convert to 0s and 1s (or FALSE and TRUE)")

}

}
}

# EXTRACT PARAMETERS
max.levels <- min(max.levels, ncol(data.train) - 1)

# CALCULATE CUE ACCURACIES
{

stat.names <- names(classtable(1, 1))

if(is.null(object)) {

cue.accuracies.train <- cuerank(formula = formula,
                                data = data.train,
                                tree.criterion = tree.criterion,
                                numthresh.method = numthresh.method,
                                rounding = rounding,
                                verbose = verbose
)

}

if(is.null(object) == F) {

cue.accuracies.train <- object$cue.accuracies$train

}

if(is.null(data.test) == F) {

cue.accuracies.test <- cuerank(formula = formula,
                                data = data.test,
                                tree.criterion = tree.criterion,
                                numthresh.method = numthresh.method,
                                rounding = rounding,
                                verbose = verbose,
                                cue.rules = cue.accuracies.train
)

}

if(is.null(data.test)) {

cue.accuracies.test <- NULL

}

cue.accuracies <- list("train" = cue.accuracies.train, "test" = cue.accuracies.test)

}

# GET TREE DEFINITIONS
{
if(is.null(object)) {

tree.growth <- grow.FFTrees(formula = formula,
                            data = data.train,
                            rank.method = rank.method,
                            numthresh.method = numthresh.method,
                            stopping.rule = stopping.rule,
                            stopping.par = stopping.par,
                            max.levels = max.levels)

tree.definitions <- tree.growth$tree.definitions

}

if(is.null(object) == F) {tree.definitions <- object$tree.definitions}
}

## CALCULATE TREE STATISTICS FROM DEFINITIONS
{
n.trees <- nrow(tree.definitions)

apply.tree <- function(data,
                       formula,
                       tree.definitions
                       ) {

  criterion.v <- model.frame(formula = formula, data = data)[,1]

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
      exit.i <- exit.v[level.i]
      threshold.i <- threshold.v[level.i]

      cue.values <- data[[cue.i]]

    unclassified.cases <- which(is.na(decision[,tree.i]))
    classified.cases <- which(is.na(decision[,tree.i]) == F)


    if(is.character(threshold.i)) {threshold.i <- unlist(strsplit(threshold.i, ","))}

    if(class.i %in% c("numeric", "integer")) {threshold.i <- as.numeric(threshold.i)}

    if(direction.i == "!=") {current.decisions <- (cue.values %in% threshold.i) == F}
    if(direction.i == "=") {current.decisions <- cue.values %in% threshold.i}
    if(direction.i == "<") {current.decisions <- cue.values < threshold.i}
    if(direction.i == "<=") {current.decisions <- cue.values <= threshold.i}
    if(direction.i == ">") {current.decisions <- cue.values > threshold.i}
    if(direction.i == ">=") {current.decisions <- cue.values >= threshold.i}


    if(exit.i == 0) {classify.now <- current.decisions == F & is.na(decision[,tree.i])}
    if(exit.i == 1) {classify.now <- current.decisions == T & is.na(decision[,tree.i])}
    if(exit.i == .5) {classify.now <- is.na(decision[,tree.i])}


    decision[classify.now, tree.i] <- current.decisions[classify.now]
    levelout[classify.now, tree.i] <- level.i

    # Get level stats

    level.i.stats <- classtable(prediction.v = decision[levelout[,tree.i] <= level.i & is.finite(levelout[,tree.i]), tree.i],
                                criterion.v = criterion.v[levelout[,tree.i] <= level.i & is.finite(levelout[,tree.i])]
                                )

    level.stats.df.i[level.i, names(level.i.stats)] <- level.i.stats



    }

  level.stats.ls[[tree.i]] <- level.stats.df.i

  }

  levelstats <- do.call("rbind", args = level.stats.ls)

  # CUMULATIVE TREE STATS

  treestats <- tree.definitions
  helper <- paste(levelstats$tree, levelstats$level, sep = ".")
  maxlevs <- paste(rownames(tapply(levelstats$level, levelstats$tree, FUN = which.max)), tapply(levelstats$level, levelstats$tree, FUN = which.max), sep = ".")
  treestats <- cbind(tree.definitions, levelstats[helper %in% maxlevs, stat.names])
  rownames(treestats) <- 1:nrow(treestats)



return(list("decision" = decision,
            "levelout" = levelout,
            "levelstats" = levelstats,
            "treestats" = treestats
            ))

}

if(is.null(data.train) == F) {

train.results <- apply.tree(data.train, formula, tree.definitions)

decision.train <- train.results$decision
levelout.train <- train.results$levelout
levelstats.train <- train.results$levelstats
treestats.train <- train.results$treestats

tree.auc.train <- auc(hr.v = train.results$treestats$hr, far.v = train.results$treestats$far)
}

if(is.null(data.train) == T) {

  decision.train <- NULL
  levelout.train <- NULL
  levelstats.train <- NULL
  treestats.train <- NULL
  tree.auc.train <- NA
}

if(is.null(data.test) == F) {

test.results <- apply.tree(data.test, formula, tree.definitions)

decision.test <- test.results$decision
levelout.test <- test.results$levelout
levelstats.test <- test.results$levelstats
treestats.test <- test.results$treestats
tree.auc.test <- auc(hr.v = test.results$treestats$hr, far.v = test.results$treestats$far)

}

if(is.null(data.test) == T) {

  decision.test <- NULL
  levelout.test <- NULL
  levelstats.test <- NULL
  treestats.test <- NULL
  tree.auc.test <- NA
}

decision <- list("train" = decision.train, "test" = decision.test)
levelout <- list("train" = levelout.train, "test" = levelout.test)
levelstats <- list("train" = levelstats.train, "test" = levelstats.test)
treestats <- list("train" = treestats.train, "test" = treestats.test)

tree.auc <- data.frame("FFTrees" = c(tree.auc.train, tree.auc.test))
rownames(tree.auc) = c("train", "test")

}

# LR
{
if(do.lr) {

lr.acc <- lr.pred(formula = formula,
               data.train = data.train,
               data.test = data.test
               )


lr.stats <- lr.acc$accuracy
lr.auc <- lr.acc$auc
lr.model <- lr.acc$model

}

if(do.lr == F) {

lr.acc <- NULL; lr.stats <- NULL ; lr.model <- NULL

lr.auc <- matrix(NA, nrow = 2, ncol =1)
rownames(lr.auc) <- c("train", "test")
colnames(lr.auc) <- "lr"

}

}

# CART
{

if(do.cart) {

cart.acc <- cart.pred(formula = formula,
                      data.train = data.train,
                      data.test = data.test
)

cart.stats <- cart.acc$accuracy
cart.auc <- cart.acc$auc
cart.model <- cart.acc$model

}

if(do.cart == F) {

  cart.acc <- NULL ; cart.stats <- NULL ; cart.model <- NULL

  cart.auc <- matrix(NA, nrow = 2, ncol =1)
  rownames(cart.auc) <- c("train", "test")
  colnames(cart.auc) <- "cart"

}
}

# Get AUC matrix

auc <- cbind(tree.auc, lr.auc, cart.auc)

output.fft <- list(
                  "formula" = formula,
                  "data" = list("train" = data.train, "test" = data.test),
                  "cue.accuracies" = cue.accuracies,
                  "tree.definitions" = tree.definitions,
                  "tree.stats" = treestats,
                  "level.stats" = levelstats,
                  "decision" = decision,
                  "levelout" = levelout,
                  "auc" = auc,
                  "lr" = list("model" = lr.model, "stats" = lr.stats),
                  "cart" = list("model" = cart.model, "stats" = cart.stats)
)

class(output.fft) <- "FFTrees"

return(output.fft)

}


