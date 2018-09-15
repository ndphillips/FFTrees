#' Creates a fast-and-frugal trees (FFTrees) object.
#'
#' This is the workhorse function for the \code{FFTrees} package. It creates (one or more) fast-and-frugal decision trees trained on a training dataset and tested on an optional test dataset.
#'
#' @param formula formula. A formula specifying a logical criterion as a function of 1 or more predictors.
#' @param data dataframe. A training dataset.
#' @param data.test dataframe. An optional testing dataset with the same structure as data.
#' @param algorithm character. The algorithm to create FFTs. Can be \code{'ifan'}, \code{'dfan'}, \code{'max'}, or \code{'zigzag'}.
#' @param max.levels integer. The maximum number of levels considered for the trees. Because all permutations of exit structures are considered, the larger \code{max.levels} is, the more trees will be created.
#' @param sens.w numeric. A number from 0 to 1 indicating how to weight sensitivity relative to specificity. Only relevant when \code{goal = 'wacc'}
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying the costs of a hit, false alarm, miss, and correct rejection rspectively. E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a false alarm and miss cost 10 and 20 respectively while correct decisions have no cost.
#' @param cost.cues list A list containing containing costs for each cue. Each element should have a name corresponding to a column in \code{data}, and each entry should be a single (positive) number. Cues not present in \code{cost.cues} are assume to have 0 cost.
#' @param stopping.rule character. A string indicating the method to stop growing trees. "levels" means the tree grows until a certain level. "exemplars" means the tree grows until a certain number of unclassified exemplars remain. "statdelta" means the tree grows until the change in the criterion statistic is less than a specified level.
#' @param stopping.par numeric. A number indicating the parameter for the stopping rule. For stopping.rule == "levels", this is the number of levels. For stopping rule == "exemplars", this is the smallest percentage of examplars allowed in the last level.
#' @param goal character. A string indicating the statistic to maximize when selecting final trees: "acc" = overall accuracy, "wacc" = weighted accuracy, "bacc" = balanced accuracy
#' @param goal.chase character. A string indicating the statistic to maximize when constructing trees: "acc" = overall accuracy, "wacc" = weighted accuracy, "bacc" = balanced accuracy, "cost" = cost.
#' @param goal.threshold character. A string indicating the statistic to maximize when calculting cue thresholds: "acc" = overall accuracy, "wacc" = weighted accuracy, "bacc" = balanced accuracy
#' @param numthresh.method character. How should thresholds for numeric cues be determined? \code{"o"} will optimize thresholds, while \code{"m"} will always use the median.
#' @param decision.labels string. A vector of strings of length 2 indicating labels for negative and positive cases. E.g.; \code{decision.labels = c("Healthy", "Diseased")}
#' @param main string. An optional label for the dataset. Passed on to other functions like \code{plot.FFTrees()}, and \code{print.FFTrees()}
#' @param train.p numeric. What percentage of the data to use for training when \code{data.test} is not specified? For example, \code{train.p = .5} will randomly split \code{data} into a 50\% training set and a 50\% test set. \code{train.p = 1}, the default, uses all data for training.
#' @param rounding integer. An integer indicating digit rounding for non-integer numeric cue thresholds. The default is NULL which means no rounding. A value of 0 rounds all possible thresholds to the nearest integer, 1 rounds to the nearest .1 (etc.).
#' @param quiet logical. Should progress reports be printed? Can be helpful for diagnosis when the function is running slowly.
#' @param repeat.cues logical. Can cues occur multiple times within a tree?
#' @param my.tree string. A string representing an FFT in words. For example, \code{my.tree = "If age > 20, predict TRUE. If sex = {m}, predict FALSE. Otherwise, predict TRUE"}
#' @param tree.definitions dataframe. An optional hard-coded definition of trees (see details below). If specified, no new trees are created.
#' @param do.comp,do.cart,do.lr,do.rf,do.svm logical. Should alternative algorithms be created for comparison? cart = regular (non-frugal) trees with \code{rpart}, lr = logistic regression with \code{glm}, rf = random forests with \code{randomForest}, svm = support vector machines with \code{e1071}. Setting \code{comp = FALSE} sets all these arguments to FALSE.
#' @param store.data logical. Should training / test data be stored in the object? Default is FALSE.
#' @param object FFTrees. An optional existing FFTrees object. When specified, no new trees are fitted and the existing trees are applied to \code{data} and \code{data.test}.
#' @param rank.method,verbose,comp depricated arguments.
#' @param force logical. If TRUE, forces some parameters (like goal) to be as specified by the user even when the algorithm thinks those specifications don't make sense.
#' @importFrom stats anova predict glm as.formula formula sd
#' @return An \code{FFTrees} object with the following elements
#'
#' \describe{
#'   \item{formula}{The formula specified when creating the FFTs.}
#'   \item{data.desc}{Descriptive statistics of the data}
#'   \item{cue.accuracies}{Marginal accuracies of each cue given a decision threshold calculated with the specified algorithm}
#'   \item{tree.definitions}{Definitions of each tree created by \code{FFTrees}. Each row corresponds to one tree. Different levels within a tree are separated by semi-colons. See above for more details.}
#'   \item{tree.stats}{Tree definitions and classification statistics. Training and test data are stored separately}
#'   \item{cost}{A list of cost information for each case in each tree.}
#'   \item{level.stats}{Cumulative classification statistics at each tree level. Training and test data are stored separately}
#'   \item{decision}{Final classification decisions. Each row is a case and each column is a tree. For example, row 1 in column 2 is the classification decision of tree number 2 for the first case. Training and test data are stored separately.}
#'   \item{levelout}{The level at which each case is classified in each tree. Rows correspond to cases and columns correspond to trees. Training and test data are stored separately.}
#'   \item{tree.max}{The index of the 'final' tree specified by the algorithm. For algorithms that only return a single tree, this value is always 1.}
#'   \item{inwords}{A verbal definition of \code{tree.max}.}
#'   \item{params}{A list of defined control parameters (e.g.; \code{algorithm}, \code{goal})}
#'   \item{comp}{Models and classification statistics for competitive classification algorithms: Regularized logistic regression, CART, and random forest.}
#'   \item{data}{The original training and test data (only included when \code{store.data = TRUE})}
#'   }
#'
#' @export
#' @examples
#'
#'  # Create fast-and-frugal trees for heart disease
#'  heart.fft <- FFTrees(formula = diagnosis ~.,
#'                       data = heart.train,
#'                       data.test = heart.test,
#'                       main = "Heart Disease",
#'                       decision.labels = c("Healthy", "Diseased"))
#'
#'  # Print the result for summary info
#'  heart.fft
#'
#'  # Plot the tree applied to training data
#'  plot(heart.fft, stats = FALSE)
#'  plot(heart.fft)
#'  plot(heart.fft, data = "test")  # Now for testing data
#'  plot(heart.fft, data = "test", tree = 2) # Look at tree number 2
#'
#'
#'  ## Predict classes and probabilities for new data
#'
#'  predict(heart.fft, newdata = heartdisease)
#'  predict(heart.fft, newdata = heartdisease, type = "prob")
#'
#'  ### Create your own custom tree with my.tree
#'
#'  custom.fft <- FFTrees(formula = diagnosis ~ .,
#'                        data = heartdisease,
#'                        my.tree = 'If chol > 300, predict True.
#'                                   If sex = {m}, predict False,
#'                                   If age > 70, predict True, otherwise predict False'
#'                                   )
#'
#'  # Plot the custom tree (it's pretty terrible)
#'  plot(custom.fft)
#'
#'
#'

FFTrees <- function(formula = NULL,
                    data = NULL,
                    data.test = NULL,
                    algorithm = "ifan",
                    max.levels = NULL,
                    sens.w = .5,
                    cost.outcomes = NULL,
                    cost.cues = NULL,
                    stopping.rule = "exemplars",
                    stopping.par = .1,
                    goal = NULL,
                    goal.chase = NULL,
                    goal.threshold = "bacc",
                    numthresh.method = "o",
                    decision.labels = c("False", "True"),
                    main = NULL,
                    train.p = 1,
                    rounding = NULL,
                    repeat.cues = TRUE,
                    my.tree = NULL,
                    tree.definitions = NULL,
                    do.comp = TRUE,
                    do.cart = TRUE,
                    do.lr = TRUE,
                    do.rf = TRUE,
                    do.svm = TRUE,
                    store.data = FALSE,
                    object = NULL,
                    rank.method = NULL,
                    force = FALSE,
                    verbose = NULL,
                    comp = NULL,
                    quiet = TRUE
) {
#
#
  # formula = diagnosis ~.
  # data = heart.train
  # data.test = heart.test
  # algorithm = "ifan"
  # max.levels = NULL
  # sens.w = .5
  # cost.outcomes = NULL
  # cost.cues = NULL
  # stopping.rule = "exemplars"
  # stopping.par = .1
  # goal = NULL
  # goal.chase = NULL
  # numthresh.method = "o"
  # decision.labels = c("False", "True")
  # main = NULL
  # train.p = 1
  # rounding = NULL
  # progress = TRUE
  # repeat.cues = TRUE
  # my.tree = NULL
  # tree.definitions = NULL
  # do.comp = TRUE
  # do.cart = TRUE
  # do.lr = TRUE
  # do.rf = TRUE
  # do.svm = TRUE
  # store.data = FALSE
  # object = NULL
  # rank.method = NULL
  # force = FALSE
  # verbose = NULL
  # comp = NULL

# Depricated arguments
{
  if(is.null(verbose) == FALSE) {

    warning("The argument verbose is depricated. Use progress instead.")

    progress <- verbose

  }

  if(is.null(rank.method) == FALSE) {

    warning("The argument rank.method is depricated. Use algorithm instead.")

    algorithm <- rank.method

  }

  if(is.null(comp) == FALSE) {

    warning("The argument comp is depricated. Use do.comp instead.")

    do.comp <- comp

  }

}

# # Input validation

# Make sure criterion is logical

criterion_name <- paste(formula)[2]

if(class(data[[criterion_name]]) != "logical") {

  if(sum(is.na(as.logical(data[[criterion_name]]))) == sum(is.na(data[[criterion_name]]))) {

    data[[criterion_name]] <- as.logical(data[[criterion_name]])

    message(criterion_name, " is now logical.")

  } else {

  stop("The criterion ", criterion_name, " does not appear to be logical (or easily converted to logical). Please fix.")

  }
}


# If no FFTrees object is specified
if(is.null(object)) {

if(!is.null(cost.outcomes)) {

  if(all(names(cost.outcomes) %in% c("hi", "mi", "fa", "cr")) == FALSE) {

    stop("cost.outcomes must be a list of length 4 with the names 'hi', 'mi', 'fa', and 'cr'")}

  # add any missing entries as 0
  for(lab in c("hi", "mi", "cr", "fa")) {

  if(lab %in% names(cost.outcomes) == FALSE) {

    cost.outcomes[[length(cost.outcomes) + 1]] <- 0
    names(cost.outcomes)[length(cost.outcomes)] <- lab
  }

  }


}


  # goal, goal.chase, goal.threshold

  {

    if(is.null(goal)) {

      if(!is.null(cost.outcomes) | !is.null(cost.cues)) {

        goal <- "cost"
        # message("Setting goal = 'cost'")

      } else {

        goal <- "wacc"
        cost.outcomes <- list(hi = 0, mi = 1, fa = 1, cr = 0)
        # message("Setting goal = 'wacc'")

      }
    } else {

      if(!(goal %in% c("bacc", "wacc", "dprime", "cost", "acc"))) {

        stop("goal must be in the set 'bacc', 'wacc', 'cost', 'acc'")

      }
    }


    if(goal == "cost" & is.null(goal.chase)) {

      goal.chase <- "cost"

    } else if (is.null(goal.chase)) {

      goal.chase <- "wacc"

    }
  }


# cost.cues
{

if(is.null(cost.cues) == FALSE) {

cost.cues <- cost.cues.append(formula, data, cost.cues = cost.cues)

}

if(is.null(cost.outcomes) & is.null(cost.cues) == FALSE) {

  cost.outcomes <- list(hi = 0, fa = 1, mi = 1, cr = 0)
  cost.cues <- cost.cues.append(formula, data)

  message("Warning: You specified cue costs but no outcome costs. Results could be misleading.")

} else {

  if(is.null(cost.outcomes) & is.null(cost.cues)) {

    cost.outcomes <- list(hi = 0, fa = 1, mi = 1, cr = 0)
    cost.cues <- cost.cues.append(formula, data)


  }

}

  if(is.null(cost.cues)) {

cost.cues <- cost.cues.append(formula, data)

  }
}


# tree.definitions
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

# algorithm
valid.algorithms <- c("ifan", "dfan", "max", "zigzag")

if(algorithm %in% valid.algorithms == FALSE) {

  if(algorithm %in% c("c", "m") == FALSE) {

    stop(paste0("The algorithm ", algorithm, " is invalid, please use one of the following: [", paste(valid.algorithms, collapse = ", "), "]"))

  }

  if(algorithm == "m") {

    message("Algorithm 'm' is depricated, using 'ifan' instead")
    algorithm <- "ifan"

  }

  if(algorithm == "c") {

    message("Algorithm 'c' is depricated, using 'dfan' instead")
    algorithm <- "dfan"

    }
}

if(force == FALSE) {

if(goal %in% c("acc") & sens.w != .5) {

  message(paste0("Note: Because sens.w != .5, I will set goal = 'wacc'. To prevent this, include force = TRUE"))

  goal <- "wacc"

}

}


if(algorithm %in% c("ifan", "dfan") & is.null(max.levels)) {

  max.levels <- 4

}

# Missing manditory arguments
{
if(is.null(data) & is.null(object)) {stop("Please specify training data")}

# Is there a valid formula?
if((is.null(formula) | class(formula) != "formula") & is.null(object)) {stop("Please specify a valid formula")}

# Does the named criterion exist?
crit.name <- paste(formula)[2]
if((crit.name %in% names(data) == FALSE) & is.null(object)) {

  stop(paste0("The criterion variable ", crit.name, " is not in the data."))}

}

# DEFINE TESTING AND TRAINING DATA [data.train, data.test]
{

if(is.null(object) == FALSE) {

  data.train.o <- NULL
  data.train <- NULL

  cue.train <- NULL
  cue.names <- object$cue.accuracies$train$cue
  n.cues <- length(cue.names)

  crit.train <- NULL
  crit.name <- NULL

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
                            data = data.train.o,
                            na.action = NULL)

  cue.train <- data.train[,2:ncol(data.train), drop = FALSE]
  cue.names <- names(cue.train)
  n.cues <- length(cue.names)

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
                            data = data.train.o,
                            na.action = NULL)

  cue.train <- data.train[,2:ncol(data.train)]
  crit.train <- data.train[,1]
  cue.names <- names(cue.train)
  n.cues <- length(cue.names)

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

# More validity checks
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

    if(is.null(object)) {

    data.test <- fac.to.string(data.test)
    data.train <- fac.to.string(data.train)
    cue.train <- fac.to.string(cue.train)
    cue.test <- fac.to.string(cue.test)
}

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

  if(setequal(crit.train, c(0, 1)) == FALSE |
     {"factor" %in% c(class(crit.train))} |
     {"character" %in% c(class(crit.train))}
     ) {

    stop(paste0("The criterion ", crit.name, " is either not binary or logical, or does not have variance"))

  }

  # Missing values
  if(any(is.finite(crit.train) == FALSE)) {

    stop(paste0("The criterion ", crit.name, " has missing values"))

  }

}

}

# GET TREE DEFINITIONS  with grow.FFTrees()
#  [tree.definitions, cue.accuracies.train]
{

  if(is.null(object) == FALSE) {

    tree.definitions <- object$tree.definitions
    cue.accuracies.train <- object$cue.accuracies$train

  }

  if(is.null(object) &
     is.null(tree.definitions) &
     is.null(my.tree)) {

    if(!quiet) {message(paste0("Growing FFTs with ", algorithm))}

    tree.growth <- grow.FFTrees(formula = formula,
                                data = data.train,
                                algorithm = algorithm,
                                goal = goal,
                                goal.chase = goal.chase,
                                goal.threshold = goal.threshold,
                                stopping.rule = stopping.rule,
                                stopping.par = stopping.par,
                                max.levels = max.levels,
                                sens.w = sens.w,
                                cost.outcomes = cost.outcomes,
                                cost.cues = cost.cues,
                                quiet = quiet,
                                repeat.cues = repeat.cues)

    tree.definitions <- tree.growth$tree.definitions
    cue.accuracies.train <- tree.growth$cue.accuracies

  } else {

    if(is.null(my.tree) == FALSE) {

      tree.definitions <- wordstoFFT(input = my.tree,
                                     cue.names = names(data.train),
                                     decision.labels = decision.labels)

      cue.accuracies.train <- NULL

    }

    if(is.null(tree.definitions) == FALSE) {

      cue.accuracies.train <- NULL

    }


  }



}

}

if(is.null(object) == FALSE) {

  if(class(object) != "FFTrees") {stop("The object you specified is not of class FFTrees")}

  formula <- object$formula
  tree.definitions <- object$tree.definitions
  algorithm <- object$params$algorithm
  goal <- object$params$goal
  goal.chase <- object$params$goal.chase
  goal.threshold <- object$params$goal.threshold
  sens.w <- object$params$sens.w
  max.levels <- object$params$max.levels
  cost.outcomes <- object$params$cost.outcomes
  cost.cues <- object$params$cost.cues
  decision.labels <- object$params$decision.labels
  main <- object$params$main
  data.desc.train <- object$data.desc$train
  cue.accuracies.train <- object$cue.accuracies$train
  treestats.train <- object$tree.stats$train
  levelstats.train <- object$level.stats$train
  decision.train <- object$decision$train
  levelout.train <- object$levelout$train
  treecost.train <- object$cost$train
  tree.max <- object$tree.max
  comp <- object$comp
  data.train <- NULL


  if((is.null(data) == FALSE) & is.null(data.test)) {

    warning("You specified an FFTrees object but not data.test. I will use data as data.test")

    data.test <- data
    data <- NULL

  }

  data.test.o <- data.test
  data.test <- model.frame(formula = formula,
                           data = data.test,
                           na.action = NULL)

  cue.test <- data.test[,2:ncol(data.test)]
  crit.test <- data.test[,1]

}

# CALCULATE TEST CUE ACCURACIES [cue.accuracies.test]
{


if(is.null(data.test) == FALSE & all(is.finite(crit.test)) & is.finite(sd(crit.test)) & is.null(cue.accuracies.train) == FALSE) {

  if(sd(crit.test) > 0) {

# if(progress) {message("Calculating cue test accuracies...")}

cue.accuracies.test <- cuerank(formula = formula,
                                data = data.test,
                                goal.threshold = goal.threshold,
                                rounding = rounding,
                                quiet = TRUE,
                                cue.rules = cue.accuracies.train,
                                sens.w = sens.w,
                                numthresh.method = numthresh.method)
}

} else {cue.accuracies.test <- NULL}

cue.accuracies <- list("train" = cue.accuracies.train, "test" = cue.accuracies.test)



}

## CALCULATE TREE STATISTICS FROM DEFINITIONS
{
n.trees <- nrow(tree.definitions)

if(is.null(data.train) == FALSE) {

train.results <- apply.tree(data = data.train,
                            formula = formula,
                            tree.definitions = tree.definitions,
                            sens.w = sens.w,
                            cost.cues = cost.cues,
                            cost.outcomes = cost.outcomes)

decision.train <- train.results$decision
levelout.train <- train.results$levelout
levelstats.train <- train.results$levelstats
treestats.train <- train.results$treestats
costout.train <- train.results$costout
costcue.train <- train.results$costcue
cost.train <- train.results$cost

}

if(is.null(data.train) == TRUE) {

  if(is.null(object)) {

  decision.train <- NULL
  levelout.train <- NULL
  levelstats.train <- NULL
  treestats.train <- NULL
  costout.train <- NULL
  costcue.train <- NULL
  cost.train <- NULL


  }
}

if(is.null(data.test) == FALSE) {

test.results <- apply.tree(data = data.test,
                           formula = formula,
                           tree.definitions = tree.definitions,
                           sens.w = sens.w,
                           cost.cues = cost.cues,
                           cost.outcomes = cost.outcomes)

decision.test <- test.results$decision
levelout.test <- test.results$levelout
levelstats.test <- test.results$levelstats
treestats.test <- test.results$treestats
costout.test <- train.results$costout
costcue.test <- train.results$costcue
cost.test <- train.results$cost


}

if(is.null(data.test) == TRUE) {

  decision.test <- NULL
  levelout.test <- NULL
  levelstats.test <- NULL
  treestats.test <- NULL
  treecost.test <- NULL
  costout.test <- NULL
  costcue.test <- NULL
  cost.test <- NULL
}

decision <- list("train" = decision.train, "test" = decision.test)
levelout <- list("train" = levelout.train, "test" = levelout.test)
levelstats <- list("train" = levelstats.train, "test" = levelstats.test)
treestats <- list("train" = treestats.train, "test" = treestats.test)
costout <- list("train" = costout.train, "test" = costout.test)
costcue <- list("train" = costcue.train, "test" = costcue.test)
cost <- list("train" = cost.train, "test" = cost.test)

}


# FIT COMPETITIVE ALGORITHMS
{

if(do.comp == FALSE) {

do.lr <- FALSE
do.cart <- FALSE
do.svm <- FALSE
do.rf <- FALSE

}
  if(do.lr | do.cart | do.rf | do.svm) {if(!quiet) {message("Fitting other algorithms for comparison (disable with do.comp = FALSE) ...")}}

  # LR
  {
    if(do.lr & ((is.null(object) == FALSE &  is.null(object$comp$lr$model) == FALSE) |
                is.null(data.train) == FALSE)) {

      if(is.null(object) == FALSE & is.null(object$comp$lr$model) == FALSE) {

        model <- object$comp$lr$model

      } else {model <- NULL}

      lr.acc <- comp.pred(formula = formula,
                          data.train = data.train,
                          data.test = data.test,
                          algorithm = "lr",
                          model = model)

      lr.stats <- lr.acc$accuracy
      lr.model <- lr.acc$model


      if(is.null(object) == FALSE) {

        lr.stats[,grepl(".train", names(lr.stats))] <- object$comp$lr$stats[,grepl(".train", names(object$comp$lr$stats))]

      }

    } else {

      lr.acc <- NULL
      lr.stats <- NULL
      lr.model <- NULL

    }

  }

  # CART
  {

    if(do.cart & ((is.null(object) == FALSE &  is.null(object$comp$cart$model) == FALSE) |
                is.null(data.train) == FALSE)) {

      if(is.null(object) == FALSE & is.null(object$comp$cart) == FALSE) {

        model <- object$comp$cart$model

      } else {model <- NULL}

      cart.acc <- comp.pred(formula = formula,
                            data.train = data.train,
                            data.test = data.test,
                            algorithm = "cart",
                            model = model)

      cart.stats <- cart.acc$accuracy
      cart.model <- cart.acc$model

      if(is.null(object) == FALSE) {

      cart.stats[,grepl(".train", names(cart.stats))] <- object$comp$cart$stats[,grepl(".train", names(object$comp$cart$stats))]

      }

    } else {

      cart.acc <- NULL
      cart.stats <- NULL
      cart.model <- NULL

    }
  }

  # rf
  {

    if(do.rf & ((is.null(object) == FALSE &  is.null(object$comp$rf$model) == FALSE) |
                is.null(data.train) == FALSE)){

      if(is.null(object) == FALSE & is.null(object$comp$rf) == FALSE) {

        model <- object$comp$rf$model

      } else {model <- NULL}

      rf.acc <- comp.pred(formula = formula,
                          data.train = data.train,
                          data.test = data.test,
                          algorithm = "rf",
                          model = model)

      rf.stats <- rf.acc$accuracy
      rf.model <- rf.acc$model

      if(is.null(object) == FALSE) {

        rf.stats[,grepl(".train", names(rf.stats))] <- object$comp$rf$stats[,grepl(".train", names(object$comp$rf$stats))]

      }

    } else {

      rf.acc <- NULL
      rf.stats <- NULL
      rf.model <- NULL


    }
  }

  # svm
  {

    if(do.lr & ((is.null(object) == FALSE &  is.null(object$comp$svm$model) == FALSE) |
                is.null(data.train) == FALSE)){

      if(is.null(object) == FALSE & is.null(object$comp$svm) == FALSE) {

        model <- object$comp$svm$model

      } else {model <- NULL}

      svm.acc <- comp.pred(formula = formula,
                           data.train = data.train,
                           data.test = data.test,
                           algorithm = "svm",
                           model = model)

      svm.stats <- svm.acc$accuracy
      svm.model <- svm.acc$model

      if(is.null(object) == FALSE) {

        svm.stats[,grepl(".train", names(svm.stats))] <- object$comp$svm$stats[,grepl(".train", names(object$comp$svm$stats))]

      }
    } else {

      svm.acc <- NULL
      svm.stats <- NULL
      svm.model <- NULL

    }
  }
}

# Set up additional outputs
{

# GET BEST TREE
# Note: Only matters when the algorithm produces multiple trees
if(is.null(object)) {
if(goal != "cost") {

tree.max <- which(treestats$train[[goal]] == max(treestats$train[[goal]]))

} else {

tree.max <- which(treestats$train[[goal]] == min(treestats$train[[goal]]))

}

if(length(tree.max) > 1) {tree.max <- tree.max[1]}
}


# Store data?

if(store.data) {data.ls <- list("train" = data.train, "test" = data.test)} else {

  data.ls <- list("train", "test")

}


# Data descriptions

if(is.null(object)) {

data.desc <- list("train" = data.frame("cases" = nrow(cue.train),
                                       "features" = ncol(cue.train),
                                       "n.pos" = sum(crit.train),
                                       "n.neg" = sum(crit.train == 0),
                                       "criterion.br" = mean(crit.train)),
                  "test" = data.frame("cases" = NA,
                                      "features" = NA,
                                      "n.pos" = NA,
                                      "n.neg" = NA,
                                      "criterion.br" = NA)) } else {

data.desc <- list("train" = data.frame("cases" = object$data.desc$train$cases,
                                       "features" = object$data.desc$train$features,
                                       "n.pos" = object$data.desc$train$n.pos,
                                       "n.neg" = object$data.desc$train$n.neg,
                                       "criterion.br" = object$data.desc$train$criterion.br),
                  "test" = data.frame("cases" = NA,
                                      "features" = NA,
                                      "n.pos" = NA,
                                      "n.neg" = NA,
                                      "criterion.br" = NA))
                                      }

if(is.null(data.test) == FALSE) {

  data.desc[[2]] <- data.frame("cases" = nrow(cue.test),
                               "features" = ncol(cue.test),
                               "n.pos" = sum(crit.test),
                               "n.neg" = sum(crit.test == 0),
                               "criterion.br" = mean(crit.test))

}


inwords.i <- inwords(tree = tree.max,
                     classes.v = unlist(strsplit(tree.definitions$classes[tree.max], ";")),
                     cues.v = unlist(strsplit(tree.definitions$cues[tree.max], ";")),
                     directions.v = unlist(strsplit(tree.definitions$directions[tree.max], ";")),
                     thresholds.v = unlist(strsplit(tree.definitions$thresholds[tree.max], ";")),
                     exits.v = unlist(strsplit(tree.definitions$exits[tree.max], ";")),
                     decision.labels = decision.labels
                     )


}

# Final output

x.FFTrees <- list("formula" = formula,
                   "data.desc" = data.desc,
                   "cue.accuracies" = cue.accuracies,
                   "tree.definitions" = tree.definitions,
                   "tree.stats" = treestats,
                   "cost" = list("cost" = cost, "outcomes" = costout, "cues" = costcue),
                   # "history" = history,
                   "level.stats" = levelstats,
                   "decision" = decision,
                   "levelout" = levelout,
                   "tree.max" = tree.max,
                   "inwords" = inwords.i,
                   "params" = list("algorithm" = algorithm,
                                   "goal" = goal,
                                   "goal.chase" = goal.chase,
                                   "goal.threshold" = goal.threshold,
                                   "sens.w" = sens.w,
                                   "max.levels" = max.levels,
                                   "cost.outcomes" = cost.outcomes,
                                   "cost.cues" = cost.cues,
                                   "decision.labels" = decision.labels,
                                   "main" = main,
                                   "repeat.cues" = repeat.cues),
                   "comp" = list("lr" = list("model" = lr.model, "stats" = lr.stats),
                                "cart" = list("model" = cart.model, "stats" = cart.stats),
                                "rf" = list("model" = rf.model, "stats" = rf.stats),
                                "svm" = list("model" = svm.model, "stats" = svm.stats)),
                   "data" = data.ls
)

class(x.FFTrees) <- "FFTrees"

return(x.FFTrees)

}


