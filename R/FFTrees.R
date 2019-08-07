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
#' @param numthresh.n integer. Number of numeric thresholds to try.
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
#' @param rank.method,verbose,comp deprecated arguments.
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
                    numthresh.n = 10,
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

  formula = NULL
  data = NULL
  data.test = NULL
  algorithm = "ifan"
  max.levels = NULL
  sens.w = .5
  cost.outcomes = NULL
  cost.cues = NULL
  stopping.rule = "exemplars"
  stopping.par = .1
  goal = NULL
  goal.chase = NULL
  goal.threshold = "bacc"
  numthresh.method = "o"
  numthresh.n = 10
  decision.labels = c("False", "True")
  main = NULL
  train.p = 1
  rounding = NULL
  repeat.cues = TRUE
  my.tree = NULL
  tree.definitions = NULL
  do.comp = TRUE
  do.cart = TRUE
  do.lr = TRUE
  do.rf = TRUE
  do.svm = TRUE
  store.data = FALSE
  object = NULL
  rank.method = NULL
  force = FALSE
  verbose = NULL
  comp = NULL
  quiet = TRUE


  formula = diagnosis ~.
  data = heartdisease
  train.p = .5    # Split data into 50\50 training \ test
  main = "Heartdisease"

  decision.labels = c("False", "True")
  # my.tree = "If age > 55, predict True.
  #           If cp = {a,b,np}, predict False, otherwise, predict True"


# Deprecated arguments -------------------------------------------------
{
  if(is.null(verbose) == FALSE) {

    warning("The argument verbose is deprecated. Use progress instead.")

    progress <- verbose

  }

  if(is.null(rank.method) == FALSE) {

    warning("The argument rank.method is deprecated. Use algorithm instead.")

    algorithm <- rank.method

  }

  if(is.null(comp) == FALSE) {

    warning("The argument comp is deprecated. Use do.comp instead.")

    do.comp <- comp

  }

}

# Create training and test split ---------------------------------------
if(train.p < 1 && is.null(data.test)) {

  # Save original data
  data_o <- data

  train_cases <- caret::createDataPartition(data_o[[paste(formula)[2]]],
                                            p = train.p)[[1]]

  data <- data_o[train_cases,]
  data.test <- data_o[-train_cases,]

  if(quiet == FALSE) {

    message("Splitting data into a ", scales::percent(train.p), " (N = ", scales::comma(nrow(data)), ") training and ",
            scales::percent(1 - train.p), " (N = ", scales::comma(nrow(data.test)), ") test set")
  }

}

# 0) CREATE AN FFTREES OBJECT --------------------------------------------

x <- FFTrees:::fftrees_create(data = data,
                              formula = formula,
                              goal = goal,
                              data.test = data.test,
                              algorithm = algorithm,
                              goal.chase = goal.chase,
                              goal.threshold = goal.threshold,
                              sens.w = sens.w,
                              max.levels = max.levels,
                              cost.outcomes = cost.outcomes,
                              cost.cues = cost.cues,
                              stopping.rule = stopping.rule,
                              stopping.par = stopping.par,
                              decision.labels = decision.labels,
                              main = main,
                              my.tree = my.tree,
                              repeat.cues = repeat.cues,
                              numthresh.method = numthresh.method,
                              numthresh.n = numthresh.n,
                              quiet = quiet)

# 1) GET FFTREES DEFINITIONS ----------------------------------------

# If object is specified, take the trees from that object

if(is.null(object) == FALSE) {

  expect_is(object, "FFTrees", info = "You specified object but it is not of class 'FFTrees'")

  x$trees <- object$trees

} else if(!is.null(my.tree)) {

  x <- FFTrees::fftrees_wordstofftrees(x, my.tree = my.tree)

} else if(is.null(object) && is.null(my.tree)) {


if(x$params$algorithm %in% c("ifan", "dfan")) {

  x <- FFTrees:::fftrees_grow_fan(x)

}

} else {

  stop("I don't know how to define your trees...")

}


# 2) APPLY TREES TO TRAINING DATA -------------------------------

# Training......

x <- FFTrees:::fftrees_apply(x, mydata = "train")

# Test.........

x <- FFTrees:::fftrees_apply(x, mydata = "test")


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

      lr.acc <- FFTrees:::comp.pred(formula = formula,
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

      cart.acc <- FFTrees:::comp.pred(formula = formula,
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

      rf.acc <- FFTrees:::comp.pred(formula = formula,
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

      svm.acc <- FFTrees:::comp.pred(formula = formula,
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
                                       "n.pos" = sum(crit.train == levels(crit.train)[1]),
                                       "n.neg" = sum(crit.train == levels(crit.train)[2]),
                                       "criterion.br" = mean(crit.train == levels(crit.train)[2])),
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
                               "n.pos" = sum(crit.test == levels(crit.test)[1]),
                               "n.neg" = sum(crit.test == levels(crit.test)[2]),
                               "criterion.br" = mean(crit.test == levels(crit.test)[2]))

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
                  "cost" = list("cost" = cost,
                                 "outcomes" = costout,
                                 "cues" = costcue),
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


