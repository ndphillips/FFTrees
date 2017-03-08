#' Creates a Fast and Frugal Trees (FFTrees) object.
#'
#' This is the workhorse function for the \code{FFTrees} package. It creates a set of fast and frugal decision trees trained on a training dataset and tested on an optional test dataset.
#'
#' @param formula formula. A formula specifying a logical criterion as a function of 1 or more predictors.
#' @param data dataframe. A training dataset.
#' @param data.test dataframe. An optional testing dataset with the same structure as data.
#' @param max.levels integer. The maximum number of levels considered for the trees. Because all permutations of exit structures are considered, the larger \code{max.levels} is, the more trees will be created.
#' @param train.p numeric. What percentage of the data to use for training when \code{data.test} is not specified? For example, \code{train.p = .5} will randomly split \code{data} into a 50\% training set and a 50\% test set. \code{train.p = 1}, the default, uses all data for training.
#' @param algorithm character. How should cues be ranked during tree construction. "m" (for marginal) means that cues will only be ranked once with the entire training dataset. "c" (conditional) means that cues will be re-ranked after each level in the tree with the remaining unclassified training exemplars. This also means that the same cue can be used multiple times in the trees. Note that the "c" method can take (much) longer and may be prone to overfitting.
#' @param goal character. A string indicating the statistic to maximize: "acc" = overall accuracy, "bacc" = balanced accuracy, "d" = d-prime
#' @param sens.weight numeric. A number between 0 and 1 indicating how much weight to give to maximizing hits versus minimizing false alarms when determining cue thresholds and ordering cues in trees (ignored when \code{goal = "c"})
#' @param tree.definitions dataframe. An optional hard-coded definition of trees (see details below). If specified, no new trees are created.
#' @param do.cart,do.lr,do.rf,do.svm logical. Should alternative algorithms be created for comparison? cart = regression trees, lr = logistic regression, rf = random forests, svm = support vector machines.
#' @param store.data logical. Should training / test data be stored in the object? Default is FALSE.
#' @param verbose logical. Should progress reports be printed? Can be helpful for diagnosis when the function is running slowly.
#' @param object FFTrees. An optional existing FFTrees object. When specified, no new trees are fitted and the existing trees are applied to \code{data} and \code{data.test}.
#' @param rank.method depricated arguments.
#' @importFrom stats anova predict glm as.formula formula sd
#' @return An \code{FFTrees} object with the following elements
#'
#' \describe{
#'   \item{data, data.test}{The original training and test data.}
#'   \item{cue.accuracies}{Marginal accuracies of each cue given a threshold that maximizes balanced accuracy for the training data. These are calculated using the \code{cuerank()} function.}
#'   \item{tree.definitions}{Definitions of each tree created by \code{FFTrees}. Each row corresponds to one tree. Different levels within a tree are separated by semi-colons. See above for more details.}
#'   \item{tree.stats}{Tree definitions and classification statistics. Training and test data are stored separately}
#'   \item{level.stats}{Cumulative classification statistics at each tree level. Training and test data are stored separately}
#'   \item{auc}{Area under the curve statistics}
#'   \item{decision}{Final classification decisions. Each row is a case and each column is a tree. For example, row 1 in column 2 is the classification decision of tree number 2 for the first case. Training and test data are stored separately.}
#'   \item{levelout}{The level at which each case is classified in each tree. Rows correspond to cases and columns correspond to trees. Training and test data are stored separately.}
#'   \item{params}{A list of control parameters (e.g.; \code{algorithm}, \code{goal})}
#'   \item{comp}{Models and classification statistics for competitive classification algorithms: Regularized logistic regression, CART, and random forest.}
#'   }
#'
#' @export
#' @details
#'
#' \code{tree.definitions} should be a dataframe defining trees with each row. At least 4 columns should be present: \code{cues}, the names of the cues, \code{thresholds}, thresholds determining cue splits, \code{directions}, directions pointing towards positive classifications, \code{classes}, classes of the cues, and \code{exits}, the exit directions where 0 means a negative exit, 1 means a positive exit, and .5 means a bi-directional exit. Different levels within a tree should be separated by semicolons.
#'
#' @examples
#'
#'  # Create ffts for heart disease
#'  heart.fft <- FFTrees(formula = diagnosis ~.,
#'                       data = heartdisease)
#'
#'  # Print the result for summary info
#'  heart.fft
#'
#'  # Plot the best tree
#'  plot(heart.fft)
#'
#'



FFTrees <- function(formula = NULL,
                    data = NULL,
                    data.test = NULL,
                    train.p = 1,
                    algorithm = "m",
                    goal = "bacc",
                    sens.weight = .5,
                    max.levels = 4,
                    tree.definitions = NULL,
                    verbose = FALSE,
                    do.cart = TRUE,
                    do.lr = TRUE,
                    do.rf = TRUE,
                    do.svm = TRUE,
                    store.data = FALSE,
                    object = NULL,
                    rank.method = NULL
) {


  # formula = poisonous ~.
  # data = mushrooms.train
  # data.test = mushrooms.test
  # train.p = 1
  # algorithm = "m"
  # goal = "bacc"
  # sens.weight = .5
  # max.levels = 4
  # tree.definitions = NULL
  # verbose = FALSE
  # do.cart = TRUE
  # do.lr = TRUE
  # do.rf = TRUE
  # do.svm = TRUE
  # store.data = FALSE
  # object = NULL
  # rank.method = NULL

if(is.null(rank.method) == FALSE) {

  warning("The argument rank.method is depricated. Use algorithm instead.")

  algorithm <- rank.method

}

if(is.null(tree.definitions) == FALSE) {

  for(i in 1:ncol(tree.definitions)) {

    if(class(tree.definitions[,i]) == "factor") {tree.definitions[,i] <- paste(tree.definitions[,i])}
  }


  # Check for invalid exits

  exit.vals <- unlist(strsplit(tree.definitions$exits, ";"))

  if(any(exit.vals %in% c("0", "1", "0.5", ".5") == FALSE)) {

    stop(paste0("You specified an invalid exit value of ", exit.vals[exit.vals %in% c("0", "1", "0.5", ".5") == FALSE], " in tree.definitions. Use only 0, 1 or .5"))

  }

}

# Set some global parameters

repeat.cues <- TRUE
stopping.rule <- "exemplars"
stopping.par <- .1
correction <- .25
numthresh.method <- "o"
rounding <- 2
exit.method <- "fixed"


# Is there training data?
if(is.null(data)) {stop("Please specify a dataframe in data")}

# Is there a valid formula?
if(is.null(formula) | class(formula) != "formula") {stop("Please specify a valid formula")}

# Does the named criterion exist?
crit.name <- paste(formula)[2]
if(crit.name %in% names(data) == FALSE) {

  stop(paste0("The criterion variable ", crit.name, " is not in the data."))}

# If there is an existing FFTrees object, then get the formula
if(is.null(object) == FALSE) {

  formula <- object$formula

}

# DEFINE TESTING AND TRAINING DATA
{

if(is.null(object) == FALSE) {

  data.train.o <- object$data$train
  data.train <- model.frame(formula = formula,
                            data = data.train.o)

  cue.train <- data.train[,2:ncol(data.train)]

  if(ncol(data.train) == 2) {

    cue.train <- data.frame(cue.train)

  }

  crit.train <- data.train[,1]

  crit.name <- names(data.train)[1]

  if(is.null(data.test) == FALSE) {

    data.test.o <- data.test

    if(setequal(names(data.train), names(data.test)) == FALSE) {

      stop("Your training (data) and test (data.test) dataframes do not appear to have the same column names. Please fix and try again.")

    }

    data.test <- model.frame(formula = formula,
                             data = data.test.o,
                             na.action = NULL)

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

if(is.null(object) == TRUE & (train.p == 1 | is.null(data.test) == FALSE)) {

  data.train.o <- data
  data.train <- model.frame(formula = formula,
                            data = data.train.o)

  cue.train <- data.train[,2:ncol(data.train)]
  crit.train <- data.train[,1]

  if(ncol(data.train) == 2) {

    cue.train <- data.frame(cue.train)

  }

  crit.train <- data.train[,1]

 if(is.null(data.test) == FALSE) {

   if(setequal(names(data.train.o), names(data.test)) == FALSE) {

     stop("Your training (data) and test (data.test) dataframes do not appear to have the same column names. Please fix and try again.")

   }

   data.test.o <- data.test
   data.test <- model.frame(formula = formula,
                            data = data.test,
                            na.action = NULL)

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


  # UPDATE

  # randomly sort the original data
  # loop over the rows. Assign the first row to the training data and the second row to the test data
  # Look at the next row, would it fill an empty factor slot for the training data? if yes, then assign to training. If no, then assign to test.
  # repeat until all slots are filled for both training and test.
  # assign the remaining cases proportionally to training and test


  data.train.o <- data


  data.train <- model.frame(formula = formula,
                            data = data.train.o)

  cue.train <- data.train[,2:ncol(data.train)]
  crit.train <- data.train[,1]

  # create 'training testing' sets of size train.p * nrow(data.train)

  continue <- TRUE
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

     ## FORCE TEST AND TRAINING SET TO HAVE SAME FACTOR VALUES
    force.factor <- FALSE

    if(force.factor == TRUE) {
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
    }

    if(force.factor == FALSE) {model.can.predict <- TRUE}

    # Do the training and test data valid?
    if(mean(crit.train.i) > 0 & mean(crit.train.i) < 1 &
       mean(crit.test.i > 0) & mean(crit.test.i < 1) & all(model.can.predict)) {continue <- FALSE}

    if(run.i == 100) {stop("I could not create valid training and test data sets. This is likely due to sparse data. You'll have to create them manually.")}

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

  if(is.null(x) == FALSE) {

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
   "data.frame" %in% class(data.train) == FALSE) {

  stop("Please specify a valid dataframe object in data")

}
}
# MAKE SURE TRAINING AND TEST DATAFRAMES ARE SIMILAR

if(is.null(data.test) == FALSE) {


if(setequal(names(data.train), names(data.test)) == FALSE) {

  stop("Your training (data) and test (data.test) dataframes do not appear to have the same column names. Please fix and try again.")

}

}


## VALIDITY CHECKS
{
# Non-binary DV
if(setequal(crit.train, c(0, 1)) == FALSE) {

  stop(paste0("The criterion ", crit.name, " is either not binary or logical, or does not have variance"))

}

  # Missing values
  if(any(is.finite(crit.train) == FALSE)) {

    stop(paste0("The criterion ", crit.name, " has missing values"))

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
                                goal = goal,
                                rounding = rounding,
                                verbose = verbose
)

}

if(is.null(object) == FALSE) {

cue.accuracies.train <- object$cue.accuracies$train

}

if(is.null(data.test) == FALSE & all(is.finite(crit.test)) & is.finite(sd(crit.test))) {

  if(sd(crit.test) > 0) {

cue.accuracies.test <- cuerank(formula = formula,
                                data = data.test,
                                goal = goal,
                                rounding = rounding,
                                verbose = verbose,
                                cue.rules = cue.accuracies.train
)
}

  if(sd(crit.test) == 0) {

    cue.accuracies.test <- NULL

  }

}

if(is.null(data.test) == TRUE | any(is.finite(crit.test)) == FALSE | is.finite(sd(crit.test)) == FALSE) {

cue.accuracies.test <- NULL

}

cue.accuracies <- list("train" = cue.accuracies.train, "test" = cue.accuracies.test)

}

# GET TREE DEFINITIONS
{
if(is.null(object) & is.null(tree.definitions)) {

tree.growth <- grow.FFTrees(formula = formula,
                            data = data.train,
                            algorithm = algorithm,
                            goal = goal,
                            repeat.cues = repeat.cues,
                            stopping.rule = stopping.rule,
                            stopping.par = stopping.par,
                            max.levels = max.levels,
                            sens.weight = sens.weight)

tree.definitions <- tree.growth$tree.definitions

}

if(is.null(object) == FALSE) {tree.definitions <- object$tree.definitions}

}

## CALCULATE TREE STATISTICS FROM DEFINITIONS
{
n.trees <- nrow(tree.definitions)

if(is.null(data.train) == FALSE) {

train.results <- apply.tree(data = data.train,
                            formula = formula,
                            tree.definitions = tree.definitions)

decision.train <- train.results$decision
levelout.train <- train.results$levelout
levelstats.train <- train.results$levelstats
treestats.train <- train.results$treestats

tree.auc.train <- FFTrees::auc(sens.v = train.results$treestats$sens,
                               spec.v = train.results$treestats$spec)
}

if(is.null(data.train) == TRUE) {

  decision.train <- NULL
  levelout.train <- NULL
  levelstats.train <- NULL
  treestats.train <- NULL
  tree.auc.train <- NA

}

if(is.null(data.test) == FALSE) {

test.results <- apply.tree(data = data.test,
                           formula = formula,
                           tree.definitions = tree.definitions)

decision.test <- test.results$decision
levelout.test <- test.results$levelout
levelstats.test <- test.results$levelstats
treestats.test <- test.results$treestats

if(any(is.na(test.results$treestats$sens)) == TRUE) {

  tree.auc.test <- NA

}

if(all(is.finite(test.results$treestats$sens))) {

tree.auc.test <- FFTrees::auc(sens.v = test.results$treestats$sens,
                              spec.v = test.results$treestats$spec)

}
}

if(is.null(data.test) == TRUE) {

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

lr.acc <- comp.pred(formula = formula,
                     data.train = data.train,
                     data.test = data.test,
                     algorithm = "lr")

lr.stats <- lr.acc$accuracy
lr.auc <- lr.acc$auc
lr.model <- lr.acc$model

}

if(do.lr == FALSE) {

lr.acc <- NULL
lr.stats <- NULL
lr.model <- NULL

lr.auc <- matrix(NA, nrow = 2, ncol =1)
rownames(lr.auc) <- c("train", "test")
colnames(lr.auc) <- "lr"

}

}

# CART
{

if(do.cart) {

cart.acc <- comp.pred(formula = formula,
                      data.train = data.train,
                      data.test = data.test,
                      algorithm = "cart"
)

cart.stats <- cart.acc$accuracy
cart.auc <- cart.acc$auc
cart.model <- cart.acc$model

}

if(do.cart == FALSE) {

  cart.acc <- NULL
  cart.stats <- NULL
  cart.model <- NULL

  cart.auc <- matrix(NA, nrow = 2, ncol =1)
  rownames(cart.auc) <- c("train", "test")
  colnames(cart.auc) <- "cart"

}
}

# rf
{

  if(do.rf) {

    rf.acc <- comp.pred(formula = formula,
                      data.train = data.train,
                      data.test = data.test,
                      algorithm = "rf")

    rf.stats <- rf.acc$accuracy
    rf.auc <- rf.acc$auc
    rf.model <- rf.acc$model

  }

  if(do.rf == FALSE) {

    rf.acc <- NULL
    rf.stats <- NULL
    rf.model <- NULL

    rf.auc <- matrix(NA, nrow = 2, ncol =1)
    rownames(rf.auc) <- c("train", "test")
    colnames(rf.auc) <- "rf"

  }
}

# svm
{

  if(do.svm) {

    svm.acc <- comp.pred(formula = formula,
                      data.train = data.train,
                      data.test = data.test,
                      algorithm = "svm")

    svm.stats <- svm.acc$accuracy
    svm.auc <- svm.acc$auc
    svm.model <- svm.acc$model
  }

  if(do.svm == FALSE) {

    svm.acc <- NULL
    svm.stats <- NULL
    svm.model <- NULL

    svm.auc <- matrix(NA, nrow = 2, ncol =1)
    rownames(svm.auc) <- c("train", "test")
    colnames(svm.auc) <- "svm"

  }
}


# Get AUC matrix
auc <- cbind(tree.auc, lr.auc, cart.auc, rf.auc, svm.auc)

# Store data?

if(store.data) {data.ls <- list("train" = data.train, "test" = data.test)} else {

  data.ls <- list("train", "test")

}

# Data descriptions

data.desc <- list("train" = data.frame("cases" = nrow(cue.train),
                                       "features" = ncol(cue.train),
                                       "n.pos" = sum(crit.train),
                                       "n.neg" = sum(crit.train == 0),
                                       "criterion.br" = mean(crit.train)),
                  "test" = data.frame("cases" = NA,
                                      "features" = NA,
                                      "n.pos" = NA,
                                      "n.neg" = NA,
                                      "criterion.br" = NA))

if(is.null(data.test) == FALSE) {

  data.desc[[2]] <- data.frame("cases" = nrow(cue.test),
                               "features" = ncol(cue.test),
                               "n.pos" = sum(crit.test),
                               "n.neg" = sum(crit.test == 0),
                               "criterion.br" = mean(crit.test))

}


output.fft <- list("formula" = formula,
                   "data.desc" = data.desc,
                  "cue.accuracies" = cue.accuracies,
                  "tree.definitions" = tree.definitions,
                  "tree.stats" = treestats,
                  "level.stats" = levelstats,
                  "decision" = decision,
                  "levelout" = levelout,
                  "auc" = auc,
                  "params" = list("algorithm" = algorithm, "goal" = goal, "sens.weight" = sens.weight, "max.levels" = max.levels),
                  "comp" = list("lr" = list("model" = lr.model, "stats" = lr.stats),
                                "cart" = list("model" = cart.model, "stats" = cart.stats),
                                "rf" = list("model" = rf.model, "stats" = rf.stats),
                                "svm" = list("model" = svm.model, "stats" = svm.stats)),
                  "data" = data.ls
)

class(output.fft) <- "FFTrees"

return(output.fft)

}


