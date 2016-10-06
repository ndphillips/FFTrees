#' Create Fast and Frugal Trees (FFTrees)
#'
#' @param formula A formula
#' @param data A model training dataset. An m x n dataframe containing n cue values for each of the m exemplars.
#' @param data.test (Optional) A model testing dataset (same format as data.train)
#' @param max.levels A number indicating the maximum number of levels considered for the tree.
#' @param train.p A number between 0 and 1 indicating what percentage of the data to use for training. This only applies when data.test is not specified by the user.
#' @param rank.method A string indicating how to rank cues during tree construction. "m" (for marginal) means that cues will only be ranked once with the entire training dataset. "c" (conditional) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars. This also means that the same cue can be used multiple times in the trees. However, the "c" method will take longer and may be prone to overfitting.
#' @param repeat.cues A logical value indicating whether or not to allow repeated cues in the tree. Only relevant when rank.method = 'c'.
#' @param hr.weight A number between 0 and 1 indicating how much weight to give to maximizing hits versus minimizing false alarms.
#' @param do.cart,do.lr logical values indicating whether or not to evaluate logistic regression and/or CART on the data for comparison.
#' @param verbose A logical value indicating whether or not to print progress reports. Can be helpful for diagnosis when the function is running slowly...
#' @param object An optional existing FFTrees object (do not specify by hand)
#' @importFrom stats anova predict glm as.formula formula sd
#' @return A list of length 3. The first element "tree.acc" is a dataframe containing the final statistics of all trees. The second element "cue.accuracies" shows the accuracies of all cues. The third element "tree.class.ls" is a list with n.trees elements, where each element shows the final decisions for each tree for each exemplar.
#' @export
#'

FFTrees <- function(formula = NULL,
                    data = NULL,
                    data.test = NULL,
                    train.p = 1,
                    rank.method = "m",
                    repeat.cues = TRUE,
                    hr.weight = .5,
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

  if(ncol(data.train) == 2) {

    cue.train <- data.frame(cue.train)

  }

  crit.train <- data.train[,1]

  crit.name <- names(data.train)[1]

  if(is.null(data.test) == F) {

    data.test.o <- data.test

    if(setequal(names(data.train), names(data.test)) == F) {

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

if(is.null(object) == T & train.p == 1) {

  data.train.o <- data
  data.train <- model.frame(formula = formula,
                            data = data.train.o)

  cue.train <- data.train[,2:ncol(data.train)]

  if(ncol(data.train) == 2) {

    cue.train <- data.frame(cue.train)

  }

  crit.train <- data.train[,1]

 if(is.null(data.test) == F) {

   if(setequal(names(data.train.o), names(data.test)) == F) {

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
   "data.frame" %in% class(data.train) == F) {

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

  stop("Warning! The dependent variable in your training data is either not binary or logical, or does not have variance. Convert to 0s and 1s (or FALSE and TRUE)")

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

if(is.null(object) == F) {

cue.accuracies.train <- object$cue.accuracies$train

}

if(is.null(data.test) == F & all(is.finite(crit.test)) & is.finite(sd(crit.test))) {

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

if(is.null(data.test) == T | any(is.finite(crit.test)) == F | is.finite(sd(crit.test)) == F) {

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
                            repeat.cues = repeat.cues,
                            stopping.rule = stopping.rule,
                            stopping.par = stopping.par,
                            max.levels = max.levels,
                            hr.weight = hr.weight)

tree.definitions <- tree.growth$tree.definitions

}

if(is.null(object) == F) {tree.definitions <- object$tree.definitions}
}

## CALCULATE TREE STATISTICS FROM DEFINITIONS
{
n.trees <- nrow(tree.definitions)

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

if(any(is.na(test.results$treestats$hr)) == T) {

  tree.auc.test <- NA

}

if(all(is.finite(test.results$treestats$hr))) {

tree.auc.test <- auc(hr.v = test.results$treestats$hr, far.v = test.results$treestats$far)

}
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
                 data.test = data.test)

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
                  "cart" = list("model" = cart.model, "stats" = cart.stats)
)

class(output.fft) <- "FFTrees"

return(output.fft)

}


