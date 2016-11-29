#' Creates a Fast and Frugal Trees (FFTrees) object.
#'
#' This is the workhorse function for the FFTrees package.
#'
#' @param formula formula. A formula specifying a logical criterion as a function of 1 or more predictors.
#' @param data dataframe. A training dataset.
#' @param data.test dataframe. An optional testing dataset with the same structure as data.
#' @param max.levels integer. The maximum number of levels considered for the trees. Default is 5.
#' @param train.p numeric. What percentage of the data to use for training. This only applies when data.test is not specified by the user.
#' @param rank.method character. How should cues be ranked during tree construction. "m" (for marginal) means that cues will only be ranked once with the entire training dataset. "c" (conditional) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars. This also means that the same cue can be used multiple times in the trees. However, the "c" method will take longer and may be prone to overfitting.
#' @param hr.weight numeric. A number between 0 and 1 indicating how much weight to give to maximizing hits versus minimizing false alarms.
#' @param tree.definitions dataframe. An optional hard-coded definition of trees. See details.
#' @param do.cart,do.lr,do.rf logical. Should alternative algorithms be created for comparison? cart = regression trees, lr = logistic regression, rf = random forests.
#' @param verbose logical. Should progress reports be printed? Can be helpful for diagnosis when the function is running slowly...
#' @param object An optional existing FFTrees object (do not specify by hand)
#' @importFrom stats anova predict glm as.formula formula sd
#' @return A list, see details
#' @export
#' @details
#' Here are the main elements of the output:
#' \code{cue.accuracies}: a dataframe containing the marginal accuracies of each cue given a threshold that maximizes hr - far.
#' \code{tree.definitions}: a dataframe specifying the definitions of each tree created by \code{FFTrees}. Each row corresponds to one tree. Different levels within a tree are separated by semi-colons.

FFTrees <- function(formula = NULL,
                    data = NULL,
                    data.test = NULL,
                    train.p = 1,
                    rank.method = "m",
                    hr.weight = .5,
                    max.levels = 4,
                    tree.definitions = NULL,
                    verbose = FALSE,
                    do.cart = TRUE,
                    do.lr = TRUE,
                    do.rf = TRUE,
                    object = NULL
) {
#

  #
  #
  # formula = diagnosis ~.
  # data = heartdisease
  # data.test = NULL
  # train.p = 1
  # rank.method = "m"
  # hr.weight = .5
  # max.levels = 4
  # tree.definitions = NULL
  # verbose = FALSE
  # do.cart = TRUE
  # do.lr = TRUE
  # do.rf = TRUE
  # object = NULL

# Set some global parameters

repeat.cues <- TRUE
tree.criterion <- "v"
stopping.rule <- "exemplars"
stopping.par <- .1
correction <- .25
numthresh.method <- "o"
rounding <- 2
exit.method <- "fixed"


# Check for missing / invalid inputs

if(is.null(data)) {stop("Please specify a dataframe in data")}
if(is.null(formula) | class(formula) != "formula") {stop("Please specify a valid formula")}

crit.name <- paste(formula)[2]

if(crit.name %in% names(data) == FALSE) {

  stop(paste0("The criterion variable ", crit.name, " is not in the data."))}



# EXTRACT OBJECTS FROM EXISTING FFTrees OBJECT

# GET FORMULA
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

if(is.null(object) == TRUE & train.p == 1) {

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

  if(is.null(object) == FALSE) {

    data.train.o <- object$data$train
  }

  if(is.null(object) == TRUE) {

    data.train.o <- data

  }

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
                                tree.criterion = tree.criterion,
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
                            rank.method = rank.method,
                            repeat.cues = repeat.cues,
                            stopping.rule = stopping.rule,
                            stopping.par = stopping.par,
                            max.levels = max.levels,
                            hr.weight = hr.weight)

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

tree.auc.train <- auc(hr.v = train.results$treestats$hr,
                      far.v = train.results$treestats$far)
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

if(any(is.na(test.results$treestats$hr)) == TRUE) {

  tree.auc.test <- NA

}

if(all(is.finite(test.results$treestats$hr))) {

tree.auc.test <- auc(hr.v = test.results$treestats$hr, far.v = test.results$treestats$far)

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

lr.acc <- lr.pred(formula = formula,
                 data.train = data.train,
                 data.test = data.test)

lr.stats <- lr.acc$accuracy
lr.auc <- lr.acc$auc
lr.model <- lr.acc$model

}

if(do.lr == FALSE) {

lr.acc <- NULL; lr.stats <- NULL ; lr.model <- NULL

lr.auc <- matrix(NA, nrow = 2, ncol =1)
rownames(lr.auc) <- c("train", "test")
colnames(lr.auc) <- "lr"

}

}

# CART
{

if(do.cart) {

if(is.null(object)) {cart.model <- NULL}
if(is.null(object) == FALSE) {cart.model <- object$cart$model}

cart.acc <- cart.pred(formula = formula,
                      data.train = data.train,
                      data.test = data.test,
                      cart.model = cart.model
)

cart.stats <- cart.acc$accuracy
cart.auc <- cart.acc$auc
cart.model <- cart.acc$model

}

if(do.cart == FALSE) {

  cart.acc <- NULL ; cart.stats <- NULL ; cart.model <- NULL

  cart.auc <- matrix(NA, nrow = 2, ncol =1)
  rownames(cart.auc) <- c("train", "test")
  colnames(cart.auc) <- "cart"

}
}

# rf
{

  if(do.rf) {

    if(is.null(object)) {rf.model <- NULL}
    if(is.null(object) == FALSE) {rf.model <- object$rf$model}

    rf.acc <- rf.pred(formula = formula,
                      data.train = data.train,
                      data.test = data.test,
                      rf.model = rf.model)

    rf.stats <- rf.acc$accuracy
    rf.auc <- rf.acc$auc
    rf.model <- rf.acc$model

  }

  if(do.rf == FALSE) {

    rf.acc <- NULL ; rf.stats <- NULL ; rf.model <- NULL

    rf.auc <- matrix(NA, nrow = 2, ncol =1)
    rownames(rf.auc) <- c("train", "test")
    colnames(rf.auc) <- "rf"

  }
}

# Get AUC matrix

auc <- cbind(tree.auc, lr.auc, cart.auc, rf.auc)

output.fft <- list("formula" = formula,
                  "data" = list("train" = data.train, "test" = data.test),
                  "cue.accuracies" = cue.accuracies,
                  "tree.definitions" = tree.definitions,
                  "tree.stats" = treestats,
                  "level.stats" = levelstats,
                  "decision" = decision,
                  "levelout" = levelout,
                  "auc" = auc,
                  "lr" = list("model" = lr.model, "stats" = lr.stats),
                  "cart" = list("model" = cart.model, "stats" = cart.stats),
                  "rf" = list("model" = rf.model, "stats" = rf.stats)
)

class(output.fft) <- "FFTrees"

return(output.fft)

}


