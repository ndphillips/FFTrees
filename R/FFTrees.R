#' Creates a fast-and-frugal trees (FFTrees) object.
#'
#' This is the workhorse function for the \code{FFTrees} package. It creates (one or more) fast-and-frugal decision trees trained on a training dataset and tested on an optional test dataset.
#'
#' @param formula formula. A formula specifying a logical criterion as a function of 1 or more predictors.
#' @param data dataframe. A training dataset.
#' @param data.test dataframe. An optional testing dataset with the same structure as data.
#' @param algorithm character. The algorithm to create FFTs. Can be \code{'ifan'}, \code{'dfan'}
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
#' # Create fast-and-frugal trees for heart disease
#' heart.fft <- FFTrees(
#'   formula = diagnosis ~ .,
#'   data = heart.train,
#'   data.test = heart.test,
#'   main = "Heart Disease",
#'   decision.labels = c("Healthy", "Diseased")
#' )
#'
#' # Print the result for summary info
#' heart.fft
#'
#' # Plot the tree applied to training data
#' plot(heart.fft, stats = FALSE)
#' plot(heart.fft)
#' plot(heart.fft, data = "test") # Now for testing data
#' plot(heart.fft, data = "test", tree = 2) # Look at tree number 2
#'
#'
#' ## Predict classes and probabilities for new data
#'
#' predict(heart.fft, newdata = heartdisease)
#' predict(heart.fft, newdata = heartdisease, type = "prob")
#'
#' ### Create your own custom tree with my.tree
#'
#' custom.fft <- FFTrees(
#'   formula = diagnosis ~ .,
#'   data = heartdisease,
#'   my.tree = "If chol > 300, predict True.
#'                                   If sex = {m}, predict False,
#'                                   If age > 70, predict True, otherwise predict False"
#' )
#'
#' # Plot the custom tree (it's pretty terrible)
#' plot(custom.fft)
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
                    quiet = FALSE) {


  # DEPRECATED ARGUMENTS -------------------------------------------------
  {
    if (is.null(verbose) == FALSE) {
      warning("The argument verbose is deprecated. Use progress instead.")

      progress <- verbose
    }

    if (is.null(rank.method) == FALSE) {
      warning("The argument rank.method is deprecated. Use algorithm instead.")

      algorithm <- rank.method
    }

    if (is.null(comp) == FALSE) {
      warning("The argument comp is deprecated. Use do.comp instead.")

      do.comp <- comp
    }


    if (any(c("zigzag", "max") %in% algorithm)) {
      stop("'zigzag' and 'max' are no longer supported algorithms")
    }
  }

  # Convert factor NA to new missing factor level

  # data <- data %>%
  #   dplyr::mutate_if(is.factor, addNA)   %>%
  #   dplyr::mutate_if(is.character, addNA)

  # TRAINING / TEST SPLIT ---------------------------------------
  if (train.p < 1 && is.null(data.test)) {

    # Save original data
    data_o <- data

    train_cases <- caret::createDataPartition(data_o[[paste(formula)[2]]],
      p = train.p
    )[[1]]

    data <- data_o[train_cases, ]
    data.test <- data_o[-train_cases, ]

    if (quiet == FALSE) {
      message(
        "Splitting data into a ", scales::percent(train.p), " (N = ", scales::comma(nrow(data)), ") training and ",
        scales::percent(1 - train.p), " (N = ", scales::comma(nrow(data.test)), ") test set"
      )
    }
  }


  # CREATE AN FFTREES OBJECT --------------------------------------------

  x <- fftrees_create(
    data = data,
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
    quiet = quiet,
    do.lr = do.lr,
    do.cart = do.cart,
    do.svm = do.svm,
    do.rf = do.rf,
    do.comp = do.comp
  )

  # 1) GET FFTREES DEFINITIONS ----------------------------------------

  x <- fftrees_define(x, object = object)

  # 2) APPLY TREES TO TRAINING DATA -------------------------------

  # Training......

  x <- fftrees_apply(x,
    mydata = "train"
  )

  # Rank trees by goal

  x <- fftrees_ranktrees(x)

  # Test.........

  if (!is.null(x$data$test)) {
    x <- fftrees_apply(x, mydata = "test")
  }

  ## 3) DEFINE TREES IN WORDS

  x <- fftrees_ffttowords(
    x = x,
    digits = 2
  )

  # FIT COMPETITIVE ALGORITHMS

  x <- fftrees_fitcomp(x = x)

  return(x)
}
