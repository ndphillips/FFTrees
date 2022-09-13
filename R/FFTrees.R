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
#' @param formula formula. A \code{\link{formula}} specifying a binary criterion variable (as logical) as a function of 1 or more predictor variables (cues).
#' @param data dataframe. A training dataset.
#' @param data.test dataframe. An optional testing dataset with the same structure as data.
#' @param algorithm character. The algorithm used to create FFTs. Can be \code{'ifan'}, \code{'dfan'}.
#' @param max.levels integer. The maximum number of levels considered for the trees. Because all permutations of exit structures are considered, the larger \code{max.levels} is, the more trees will be created.
#' @param sens.w numeric. A number from 0 to 1 indicating how to weight sensitivity relative to specificity. Only relevant when \code{goal = 'wacc'}.
#'
#' @param cost.outcomes A list of length 4 with names \code{'hi'}, \code{'fa'}, \code{'mi'}, and \code{'cr'} specifying the costs of a hit, false alarm, miss, and correct rejection, respectively.
#' E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a false alarm and miss cost 10 and 20, respectively, while correct decisions have no costs.
#' @param cost.cues A list containing costs for each cue.
#' Each element should have a name corresponding to a column in \code{data}, and each entry should be a single (positive) number.
#' Cues not present in \code{cost.cues} are assumed to have no costs (i.e., a value of 0).
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
#' @param train.p numeric. What percentage of the data to use for training when \code{data.test} is not specified? For example, \code{train.p = .5} will randomly split \code{data} into a 50\% training set and a 50\% test set. \code{train.p = 1}, the default, uses all data for training.
#' @param rounding integer. An integer indicating digit rounding for non-integer numeric cue thresholds. The default is NULL which means no rounding. A value of 0 rounds all possible thresholds to the nearest integer, 1 rounds to the nearest .1 (etc.).
#' @param repeat.cues logical. Can cues occur multiple times within a tree?
#'
#' @param my.tree string. A string representing a verbal description of an FFT, i.e., an FFT in words.
#' For example, \code{my.tree = "If age > 20, predict TRUE. If sex = {m}, predict FALSE. Otherwise, predict TRUE."}
#' @param tree.definitions dataframe. An optional hard-coded definition of trees (see details below). If specified, no new trees are created.
#'
#' @param do.comp,do.cart,do.lr,do.rf,do.svm logical. Should alternative algorithms be created for comparison? All TRUE by default. Options are:
#' \code{cart} = regular (non-frugal) trees with \strong{rpart};
#' \code{lr} = logistic regression with \strong{glm};
#' \code{rf} = random forests with \strong{randomForest};
#' \code{svm} = support vector machines with \strong{e1071}.
#' Specifying \code{comp = FALSE} sets all these arguments to \code{FALSE}.
#'
#' @param object FFTrees. An optional existing \code{FFTrees} object. When specified, no new trees are fitted and the existing trees are applied to \code{data} and \code{data.test}.
#' @param force logical. Setting \code{force = TRUE} forces some parameters (like goal) to be as specified by the user even when the algorithm thinks those specifications don't make sense. Default is \code{force = FALSE}.
#' @param quiet logical. Should progress reports be printed? Can be helpful for diagnosis when the function is running slowly. Default is \code{quiet = FALSE} (i.e., show progress).
#'
#' @param comp,rank.method,store.data,verbose Deprecated arguments (unused or replaced, to be retired in future releases).
#'
#' @return An \code{FFTrees} object with the following elements:
#' \describe{
#'   \item{criterion_name}{The name of the binary criterion variable (as character).}
#'   \item{cue_names}{The names of all potential predictor variables (cues) in the data (as character).}
#'   \item{formula}{The \code{\link{formula}} specified when creating the FFTs.}
#'   \item{trees}{A list of FFTs created, with further details contained in \code{n}, \code{best}, \code{definitions}, \code{inwords}, \code{stats}, \code{level_stats}, and \code{decisions}.}
#'   \item{data}{The original training and test data (if available).}
#'   \item{params}{A list of defined control parameters (e.g.; \code{algorithm}, \code{goal}).}
#'   \item{competition}{Models and classification statistics for competitive classification algorithms: Regularized logistic regression, CART, and random forest.}
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
#' plot(heart.fft, data = "test")            # predictions for Tree 1
#' plot(heart.fft, data = "test", tree = 2)  # predictions for Tree 2
#'
#' # 5. Predict classes and probabilities for new data:
#' predict(heart.fft, newdata = heartdisease)
#' predict(heart.fft, newdata = heartdisease, type = "prob")
#'
#' # 6. Create custom trees (from verbal description) with my.tree:
#' custom.fft <- FFTrees(
#'   formula = diagnosis ~ .,
#'   data = heartdisease,
#'   my.tree = "If chol > 300, predict True.
#'              If sex = {m}, predict False,
#'              If age > 70, predict True, otherwise predict False."
#'              )
#'
#' # Plot the (pretty terrible) custom tree:
#' plot(custom.fft)
#'
#' @seealso
#' \code{\link{print.FFTrees}} for printing FFTs;
#' \code{\link{plot.FFTrees}} for plotting FFTs;
#' \code{\link{summary.FFTrees}} for summarizing FFTs;
#' \code{\link{inwords}} for obtaining a verbal description of FFTs;
#' \code{\link{showcues}} for plotting cue accuracies.
#'
#' @importFrom stats anova predict glm as.formula formula sd
#'
#' @export

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
                    goal = NULL,  # (default set in fftrees_create.R)
                    goal.chase = NULL,  # (default set in fftrees_create.R)
                    goal.threshold = "bacc",  # default
                    numthresh.method = "o",
                    numthresh.n = 10,
                    decision.labels = c("False", "True"), # in 0:left/1:right order!
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
                    object = NULL,
                    force = FALSE,
                    quiet = FALSE,
                    # Deprecated args:     Use instead:
                    comp = NULL,         # do.comp
                    rank.method = NULL,  # algorithm
                    store.data = NULL,   # (none)
                    verbose = NULL       # progress
) {

  # Preparation: ------

  # a. Handle deprecated arguments and options: ----
  {
    if (is.null(comp) == FALSE) {
      warning("The argument comp is deprecated. Use do.comp instead.")

      do.comp <- comp
    }

    if (is.null(rank.method) == FALSE) {
      warning("The argument rank.method is deprecated. Use algorithm instead.")

      algorithm <- rank.method
    }

    if (is.null(store.data) == FALSE) {
      warning("The argument store.data is deprecated and ignored.")
    }

    if (is.null(verbose) == FALSE) {
      warning("The argument verbose is deprecated. Use progress instead.")

      progress <- verbose
    }

    # Deprecated options:

    if (any(c("max", "zigzag") %in% algorithm)) {
      stop("The 'max' and 'zigzag' algorithms are no longer supported.")
    }
  }

  # Convert factor NA to new missing factor level:
  # data <- data %>%
  #   dplyr::mutate_if(is.factor, addNA)   %>%
  #   dplyr::mutate_if(is.character, addNA)


  # b. Training / Test split: ----

  if (train.p < 1 && is.null(data.test)) {

    # Save original data:
    data_o <- data

    train_cases <- caret::createDataPartition(data_o[[paste(formula)[2]]],
                                              p = train.p
    )[[1]]

    data <- data_o[train_cases, ]
    data.test <- data_o[-train_cases, ]

    if (quiet == FALSE) {
      message(
        "Splitting data into a ", scales::percent(train.p), " (N = ", scales::comma(nrow(data)), ") training and ",
        scales::percent(1 - train.p), " (N = ", scales::comma(nrow(data.test)), ") test set."
      )
    }
  }


  # 1. Create an FFTrees object: ------

  x <- fftrees_create(
    data = data,
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
    quiet = quiet
  )


  # 2. Get FFTrees definitions: ------

  x <- fftrees_define(x, object = object)


  # 3. Apply to training data:  ------

  x <- fftrees_apply(x, mydata = "train")


  # 4. Rank trees by goal: ------

  x <- fftrees_ranktrees(x)


  # 5. Apply to test data: ------

  if (!is.null(x$data$test)) {
    x <- fftrees_apply(x, mydata = "test")
  }


  # 6. Define trees in words: ------

  x <- fftrees_ffttowords(
    x = x,
    mydata = "train",  # either 'train':'decide' or 'test':'predict'
    digits = 2
  )


  # 7. Fit competitive algorithms: ------

  x <- fftrees_fitcomp(x = x)


  # Output: ------

  return(x)

} # FFTrees().


# ToDo: ------

# - Update list of elements (to new hiearchical structure of FFTrees object).
# - Is the store.data argument still being used? If not, remove...

# eof.
