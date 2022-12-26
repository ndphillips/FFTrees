#' Main function to create and apply fast-and-frugal trees (FFTs)
#'
#' @description \code{FFTrees} is the workhorse function of the \strong{FFTrees} package for creating fast-and-frugal trees (FFTs).
#'
#' FFTs are decision algorithms for solving binary classification tasks, i.e., they predict the values of a binary criterion variable based on 1 or multiple predictor variables (cues).
#'
#' Using \code{FFTrees} on \code{data} usually generates a range of FFTs and corresponding summary statistics (as an \code{FFTrees} object)
#' that can then be printed, plotted, and examined further.
#'
#' The criterion and predictor variables are specified in \code{\link{formula}} notation.
#' Based on the settings of \code{data} and \code{data.test}, FFTs are trained on a (required) training dataset and tested on an (optional) test dataset.
#'
#' If an existing \code{FFTrees} object \code{object} or \code{tree.definitions} are provided as inputs,
#' no new FFTs are created.
#' When both arguments are provided, \code{tree.definitions} take priority over the FFTs in an existing \code{object}.
#' Specifically,
#'
#' \itemize{
#'
#'   \item{If \code{tree.definitions} are provided, these are assigned to the FFTs of \code{x}.}
#'
#'   \item{If no \code{tree.definitions} are provided, but an existing \code{FFTrees} object \code{object} is provided,
#'   the trees from \code{object} are assigned to the FFTs of \code{x}.}
#'
#' }
#'
#' @param formula formula. A \code{\link{formula}} specifying a binary criterion variable (as logical) as a function of 1 or more predictor variables (cues).
#' @param data dataframe. A training dataset.
#' @param data.test dataframe. An optional testing dataset with the same structure as data.
#' @param algorithm character. The algorithm used to create FFTs. Can be \code{'ifan'}, \code{'dfan'}.
#' @param max.levels integer. The maximum number of levels considered for the trees. Because all permutations of exit structures are considered, the larger \code{max.levels} is, the more trees will be created.
#' @param sens.w numeric. A number from 0 to 1 indicating how to weight sensitivity relative to specificity when \code{goal = 'wacc'}. Default: \code{sens.w = .50}.
#'
#' @param cost.outcomes A list of length 4 specifying the cost value for one of the 4 possible classification outcomes.
#' The list elements must have names \code{'hi'}, \code{'fa'}, \code{'mi'}, and \code{'cr'}
#' (for specifying the costs of a hit, false alarm, miss, and correct rejection, respectively) and provide a numeric cost value.
#' E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a false alarm and miss cost 10 and 20, respectively, while correct decisions have no costs.
#' @param cost.cues A list containing the cost of each cue (in some unit).
#' Each list element must have a name corresponding to a cue (i.e., a column in \code{data}), and should be a single (positive) number.
#' Cues not present in \code{cost.cues} are assumed to have no costs (i.e., a cost value of 0).
#'
#' @param stopping.rule character. A string indicating the method to stop growing trees.
#' \code{"levels"} means the tree grows until a certain level;
#' \code{"exemplars"} means the tree grows until a certain number of unclassified exemplars remain;
#' \code{"statdelta"} means the tree grows until the change in the criterion statistic is less than a specified level.
#' @param stopping.par numeric. A number indicating the parameter for the stopping rule.
#' For stopping.rule \code{"levels"}, this is the number of levels.
#' For stopping rule \code{"exemplars"}, this is the smallest percentage of exemplars allowed in the last level.
#'
#' @param goal character. A string indicating the statistic to maximize when selecting final trees: \code{"acc"} = overall accuracy, \code{"bacc"} = balanced accuracy, \code{"wacc"} = weighted accuracy.
#' @param goal.chase character. A string indicating the statistic to maximize when constructing trees: \code{"acc"} = overall accuracy, \code{"bacc"} = balanced accuracy, \code{"wacc"} = weighted accuracy, \code{"cost"} = cue costs.
#' @param goal.threshold character. A string indicating the statistic to maximize when calculating cue thresholds: \code{"acc"} = overall accuracy, \code{"bacc"} = balanced accuracy, \code{"wacc"} = weighted accuracy.
#' Default: \code{goal.threshold = "bacc"}.
#'
#' @param numthresh.method character. How should thresholds for numeric cues be determined? \code{"o"} will optimize thresholds, while \code{"m"} will always use the median.
#' @param numthresh.n integer. Number of numeric thresholds to try.
#' @param decision.labels string. A vector of strings of length 2 indicating labels for negative and positive cases. E.g.; \code{decision.labels = c("Healthy", "Diseased")}.
#' @param main string. An optional label for the dataset. Passed on to other functions, like \code{\link{plot.FFTrees}}, and \code{\link{print.FFTrees}}.
#' @param train.p numeric. What percentage of the data to use for training when \code{data.test} is not specified?
#' For example, \code{train.p = .50} will randomly split \code{data} into a 50\% training set and a 50\% test set.
#' The default of \code{train.p = 1} uses all data for training.
#' @param rounding integer. An integer indicating digit rounding for non-integer numeric cue thresholds.
#' The default of \code{rounding = NULL} implies no rounding.
#' A value of \code{0} rounds all possible thresholds to the nearest integer, \code{1} rounds to the nearest decade (.10), etc.
#' @param repeat.cues logical. May cues occur multiple times within a tree? Default: \code{repeat.cues = TRUE}.
#'
#' @param my.tree An optional character string. A a verbal description of an FFT, i.e., an FFT in words.
#' For example, \code{my.tree = "If age > 20, predict TRUE. If sex = {m}, predict FALSE. Otherwise, predict TRUE."}
#' @param object An optional existing \code{FFTrees} object.
#' When specified, no new FFTs are fitted, but existing trees are applied to \code{data} and \code{data.test}.
#' When \code{formula}, \code{data} or \code{data.test} are not specified, the current values of \code{object} are used.
#' @param tree.definitions An optional \code{data.frame} of hard-coded FFT definitions (in the format of \code{x$trees$definitions} of an \code{FFTrees} object \code{x}).
#' If specified, no new FFTs are fitted, but the tree definitions provided are used to re-evaluate the current \code{FFTrees} object.
#'
#' @param do.comp,do.cart,do.lr,do.rf,do.svm logical. Should alternative algorithms be used for comparison?
#' All options set to \code{TRUE} by default. Available options are:
#' \code{cart} = regular (non-frugal) trees with \strong{rpart};
#' \code{lr} = logistic regression with \strong{glm};
#' \code{rf} = random forests with \strong{randomForest};
#' \code{svm} = support vector machines with \strong{e1071}.
#' Specifying \code{do.comp = FALSE} sets all available options to \code{FALSE}.
#'
#' @param quiet logical. Should progress reports be suppressed? Setting \code{quiet = FALSE} is helpful for diagnosing errors.
#' Default: \code{quiet = FALSE} (i.e., show progress).
#'
#' @param comp,force,rank.method,store.data,verbose Deprecated arguments (unused or replaced, to be retired in future releases).
#'
#' @return An \code{FFTrees} object with the following elements:
#' \describe{
#'   \item{criterion_name}{The name of the binary criterion variable (as character).}
#'   \item{cue_names}{The names of all potential predictor variables (cues) in the data (as character).}
#'   \item{formula}{The \code{\link{formula}} specified when creating the FFTs.}
#'   \item{trees}{A list of FFTs created, with further details contained in \code{n}, \code{best}, \code{definitions}, \code{inwords}, \code{stats}, \code{level_stats}, and \code{decisions}.}
#'   \item{data}{The original training and test data (if available).}
#'   \item{params}{A list of defined control parameters (e.g.; \code{algorithm}, \code{goal}, \code{sens.w}, as well as various thresholds, stopping rule, and cost parameters).}
#'   \item{competition}{Models and classification statistics for competitive classification algorithms: Logistic regression, CART, random forests RF, and SVM.}
#'   \item{cues}{A list of cue information, with further details contained in \code{thresholds} and \code{stats}.}
#' }
#'
#' @examples
#'
#' # 1. Create fast-and-frugal trees (FFTs) for heart disease:
#' heart.fft <- FFTrees(formula = diagnosis ~ .,
#'                      data = heart.train,
#'                      data.test = heart.test,
#'                      main = "Heart Disease",
#'                      decision.labels = c("Healthy", "Diseased")
#'                      )
#'
#' # 2. Print a summary of the result:
#' heart.fft  # same as:
#' # print(heart.fft, data = "train", tree = "best.train")
#'
#' # 3. Plot an FFT applied to training data:
#' plot(heart.fft)  # same as:
#' # plot(heart.fft, what = "all", data = "train", tree = "best.train")
#'
#' # 4. Apply FFT to (new) testing data:
#' plot(heart.fft, data = "test")            # predict for Tree 1
#' plot(heart.fft, data = "test", tree = 2)  # predict for Tree 2
#'
#' # 5. Predict classes and probabilities for new data:
#' predict(heart.fft, newdata = heartdisease)
#' predict(heart.fft, newdata = heartdisease, type = "prob")
#'
#' # 6. Create a custom tree (from verbal description) with my.tree:
#' custom.fft <- FFTrees(
#'   formula = diagnosis ~ .,
#'   data = heartdisease,
#'   my.tree = "If age < 50, predict False.
#'              If sex = 1, predict True.
#'              If chol > 300, predict True, otherwise predict False.",
#'   main = "My custom FFT")
#'
#' # Plot the (pretty bad) custom tree:
#' plot(custom.fft)
#'
#' @seealso
#' \code{\link{print.FFTrees}} for printing FFTs;
#' \code{\link{plot.FFTrees}} for plotting FFTs;
#' \code{\link{summary.FFTrees}} for summarizing FFTs;
#' \code{\link{inwords}} for obtaining a verbal description of FFTs;
#' \code{\link{showcues}} for plotting cue accuracies.
#'
#' @importFrom stats as.formula formula glm predict sd
#'
#' @export

FFTrees <- function(formula = NULL,
                    data = NULL,
                    data.test = NULL,
                    #
                    algorithm = "ifan",
                    max.levels = NULL,
                    sens.w = .50,
                    cost.outcomes = NULL,
                    cost.cues = NULL,
                    stopping.rule = "exemplars",
                    stopping.par = .1,
                    #
                    goal = NULL,  # (default set in fftrees_create.R)
                    goal.chase = NULL,  # (default set in fftrees_create.R)
                    goal.threshold = "bacc",  # default
                    numthresh.method = "o",
                    numthresh.n = 10,
                    #
                    decision.labels = c("False", "True"),  # in 0:left/1:right order!
                    main = NULL,
                    train.p = 1,
                    rounding = NULL,
                    repeat.cues = TRUE,
                    #
                    my.tree = NULL,
                    object = NULL,
                    tree.definitions = NULL,
                    #
                    do.comp = TRUE,
                    do.cart = TRUE,
                    do.lr = TRUE,
                    do.rf = TRUE,
                    do.svm = TRUE,
                    #
                    quiet = FALSE,
                    #
                    # Deprecated args:   Use instead:
                    comp = NULL,         # do.comp
                    force = NULL,        # (none)
                    rank.method = NULL,  # algorithm
                    store.data = NULL,   # (none)
                    verbose = NULL       # progress
) {

  # Prepare: ------

  # A. Handle deprecated arguments and options: ----

  if (!is.null(comp)) {
    warning("The argument 'comp' is deprecated. Use 'do.comp' instead.")

    do.comp <- comp
  }

  if (!is.null(force)){
    warning("The argument 'force' is deprecated and ignored.")
  }

  if (!is.null(rank.method)) {
    warning("The argument 'rank.method' is deprecated. Use 'algorithm' instead.")

    algorithm <- rank.method
  }

  if (!is.null(store.data)) {
    warning("The argument 'store.data' is deprecated and ignored.")
  }

  if (!is.null(verbose)) {
    warning("The argument 'verbose' is deprecated. Use 'progress' instead.")

    progress <- verbose
  }

  if (any(c("max", "zigzag") %in% algorithm)) {
    stop("The 'max' and 'zigzag' algorithms are no longer supported.")
  }


  # B. Verify inputs: ----

  if (!is.null(object)) { # an FFTrees object is provided:

    # Verify:
    testthat::expect_s3_class(object, class = "FFTrees")

    # Fill in some missing defaults by current object values:
    if (is.null(formula)){ formula <- object$formula }
    if (is.null(data)) { data <- object$data$train }
    if (is.null(data.test) & (!is.null(object$data$test))) { data.test <- object$data$test }

    # Other candidates: goal and threshold parameters.

  }

  if (!is.null(tree.definitions)){

    testthat::expect_true(is.data.frame(tree.definitions), info = "Provided 'tree.definitions' are not a data.frame")

    # ToDo: Verify integrity of tree definitions:
    # 1. tree.definitions contains valid tree definitions (in appropriate format)
    # 2. tree.definitions fit to provided data (see verify_all_cues_in_data() in helper.R)

  }


  # C. Handle data: ----

  # # Convert factor NA to new missing factor level:
  #
  # data <- data %>%
  #   dplyr::mutate_if(is.factor, addNA)   %>%
  #   dplyr::mutate_if(is.character, addNA)


  # Split training / test data: ----

  if ((train.p < 1) && is.null(data.test)) {

    # Save original data:
    data_o <- data

    train_cases <- caret::createDataPartition(data_o[[paste(formula)[2]]],
                                              p = train.p)[[1]]

    data      <- data_o[train_cases, ]
    data.test <- data_o[-train_cases, ]


    # Provide user feedback: ----

    if (!quiet) {
      msg <- paste0("Successfully split data into a ", scales::percent(train.p), " (N = ", scales::comma(nrow(data)), ") training and ",
        scales::percent(1 - train.p), " (N = ", scales::comma(nrow(data.test)), ") test set.\n")
      cat(u_f_fin(msg))
    }

  }


  # Main: ------

  # 1. Create a new FFTrees object x: ----

  x <- fftrees_create(data = data,
                      formula = formula,
                      data.test = data.test,
                      algorithm = algorithm,
                      goal = goal,
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
                      do.lr   = do.lr,
                      do.cart = do.cart,
                      do.svm  = do.svm,
                      do.rf   = do.rf,
                      do.comp = do.comp,
                      quiet = quiet  # store in x$params$quiet
  )


  # 2. Create FFT definitions for x: ----

  x <- fftrees_define(x,
                      object = object,
                      tree.definitions = tree.definitions)


  # 3. Apply x to training data: ----

  x <- fftrees_apply(x, mydata = "train")  # apply and re-assign!


  # 4. Rank trees in x by goal: ----

  x <- fftrees_ranktrees(x)


  # 5. Apply x to test data: ----

  if (!is.null(x$data$test)) {
    x <- fftrees_apply(x, mydata = "test")  # apply and re-assign!
  }


  # 6. Express trees in x in words: ----

  x <- fftrees_ffttowords(x,
                          mydata = "train",  # data type: 'train'->'decide' or 'test'->'predict'
                          digits = 2)


  # 7. Fit competitive algorithms: ----

  x <- fftrees_fitcomp(x)


  # Output: ------

  return(x)

} # FFTrees().


# ToDo: ------

# - When providing a valid FFTrees object:
#   Fill in missing defaults (formula, data, ...) with corresponding object values.

# eof.
