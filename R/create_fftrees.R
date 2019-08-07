#' Create an FFTrees object
#'
#' @param data dataframe. Training data
#' @param formula formula. A formula
#' @param algorithm string.
#' @param goal string.
#' @param goal.chase string.
#' @param goal.threshold string.
#' @param numthresh.method string.
#' @param numthresh.n integer.
#' @param sens.w numeric.
#' @param max.levels integer.
#' @param cost.outcomes list.
#' @param cost.cues list.
#' @param stopping.rule string.
#' @param stopping.par numeric.
#' @param decision.labels string.
#' @param main string.
#' @param my.tree string.
#' @param data.test dataframe.
#' @param repeat.cues logical.
#' @param quiet logical
#'
#' @return An FFTrees object.
#'
fftrees_create <- function(data = NULL,
                           formula = NULL,
                           algorithm = NULL,
                           goal = NULL,
                           goal.chase = NULL,
                           goal.threshold = NULL,
                           numthresh.method = NULL,
                           numthresh.n = NULL,
                           sens.w = NULL,
                           max.levels = NULL,
                           cost.outcomes = NULL,
                           cost.cues = NULL,
                           stopping.rule = NULL,
                           stopping.par = NULL,
                           decision.labels = NULL,
                           main = NULL,
                           my.tree = NULL,
                           data.test = NULL,
                           repeat.cues = NULL,
                           quiet = NULL) {


# Validation tests ------------------------------------

## data ==============================================

expect_true(!is.null(data),
            info = "data is NULL")

expect_true(is.data.frame(data),
            info = "Object is not a dataframe")


## formula ============================================

expect_true(!is.null(formula),
            info = "formula is NULL")

expect_is(formula, "formula")


criterion_name <- paste(formula)[2]


## algorithm ==========================================

algorithm_valid <- c("ifan", "dfan", "max", "zigzag")

expect_true(!is.null(algorithm),
            info = "algorithm is NULL")

expect_true(algorithm %in% algorithm_valid)

## goal ================================================

goal_valid <- c("bacc", "wacc", "dprime", "cost", "acc")

if(is.null(goal)) {

  if(!is.null(cost.outcomes) | !is.null(cost.cues)) {

    goal <- "cost"

  if(quiet == FALSE) {message("Setting goal = 'cost'")}

  } else {

    goal <- "wacc"
    if(quiet == FALSE) {message("Setting goal = 'waccc'")}

  }
}

expect_true(!is.null(goal),
            info = "goal is NULL")

expect_true(goal %in% goal_valid)

## goal.chase ================================================

if(goal == "cost" & is.null(goal.chase)) {

  goal.chase <- "cost"

  if(quiet == FALSE) {message("Setting goal.chase = 'cost'")}

} else if (is.null(goal.chase)) {

  goal.chase <- "wacc"

  if(quiet == FALSE) {message("Setting goal.chase = 'waccc'")}


}

expect_true(!is.null(goal.chase),
            info = "goal.chase is NULL")

expect_true(goal.chase %in% goal_valid)

## goal.threshold ================================================

expect_true(!is.null(goal.threshold),
            info = "goal.threshold is NULL")

expect_true(goal.threshold %in% goal_valid)




## numthresh.method ==================================

numthresh.method_valid <- c("optimise", "median")

expect_true(substr(numthresh.method, 1, 1) %in% substr(numthresh.method_valid, 1, 1),
            info = paste0("numthresh.method is not valid\nTry one of the following: ",
            paste(numthresh.method_valid, collapse = ", ")))



## numthresh.n ==================================

numthresh.n_valid <- c(3:20)

expect_true(numthresh.n %in% numthresh.n_valid,
            info = paste0("numthresh.n is not valid\nTry one of the following: ",
                          paste(numthresh.n_valid, collapse = ", ")))


## sens.w ================================================

expect_true(!is.null(sens.w),
            info = "sens.w is NULL")

expect_lte(sens.w, expected = 1)

expect_gte(sens.w, expected = 0)

## max.levels =========================================

if(is.null(max.levels)) {

  max.levels <- 4

  if(quiet == FALSE) {"Setting max.levels = 4"}
}

expect_true(!is.null(max.levels),
            info = "max.levels is NULL")

expect_true(max.levels %in% 1:6,
            info = "max.levels must be an integer between 1 and 6")

## cost.outcomes =========================================

if(is.null(cost.outcomes)) {

cost.outcomes <- list(hi = 0, mi = 1, fa = 1, cr = 0)

if(quiet == FALSE) {

  message("Setting cost.outcomes = list(hi = 0, mi = 1, fa = 1, cr = 0)")

}

}

expect_true(!is.null(cost.outcomes),
            info = "cost.outcomes is NULL")

expect_is(cost.outcomes,
          class = "list")

expect_equal(names(cost.outcomes),
             expected = c("hi", "mi", "fa", "cr"))



## cost.cues =========================================

# Append cost.cues
cost.cues <- FFTrees:::cost.cues.append(formula,
                                        data,
                                        cost.cues = cost.cues)

expect_true(!is.null(cost.cues),
            info = "cost.cues is NULL")

expect_is(cost.cues,
          class = "list")

expect_true(all(names(cost.cues) %in% names(data)),
            info = "At least one of the values you specified in cost.cues is not in data")

## stopping.rule ====================================

stopping.rule_valid <- c("exemplars", "levels")

expect_true(stopping.rule %in% stopping.rule_valid)

## stopping.par ====================================

expect_gt(stopping.par, expected = 0)
expect_lt(stopping.par, expected = 1)

## decision.labels ===================================

expect_true(!is.null(decision.labels),
            info = "decision.labels is NULL")

expect_equal(length(decision.labels), 2)

## repeat.cues ============================================
expect_is(repeat.cues, "logical")

# Data quality checks ----------------------------------------------------------

## Criterion is in data ===================================

expect_true(criterion_name %in% names(data),
            info = paste("The criterion", criterion_name, "is not in your data object"))

## No missing criterion values ============================

expect_true(all(!is.na(data[[criterion_name]])),
            info = "At least one of the criterion values are missing. Please remove these cases and try again.")

## Criterion has two unique values

expect_equal(length(unique(data[[criterion_name]])),
             expected = 2,
             info = "Your criterion does not have exactly 2 unique values")

## Make criterion logical

if(class(data[[criterion_name]]) %in% c("character", "factor")) {

  # Save original values as decision.labels
  decision.labels <- unique(data[[criterion_name]])

  # Convert criterion to logical
  data[[criterion_name]] <- data[[criterion_name]] == decision.labels[2]

  if(quiet == FALSE) {

    message("Setting target to ", criterion_name, " == ", decision.labels[2])

    }

}



## Criterion is in data.test

if(!is.null(data.test)) {

expect_true(is.data.frame(data),
            info = "Object is not a dataframe")

expect_true(criterion_name %in% names(data.test),
            info = paste("The criterion", criterion_name, "is not in your data.test object"))

}

## Remove cues not specified in formula ========================

data <- model.frame(formula = formula,
                    data = data,
                    na.action = NULL)

## Convert factor columns to character

data <- data %>%
  dplyr::mutate_if(is.factor, paste)

# Get cue names
cue_names <- names(data)[2:ncol(data)]


# Convert data to tibble

data <- data %>%
  tibble::as_tibble()

# Create FFTrees object ------------------------------------------------

  x <- list(

            # Raw training data
            data = list(train = data,
                        test = data.test),

            # Formula
            formula = formula,                                  # Original formula

            # Tree info
            trees = list(n = NULL,
                         best = list(train = NULL,
                                     test = NULL),
                         definitions = NULL,
                         inwords = NULL,
                         results = list(train = list(stats = NULL,
                                                     decisions = NULL,
                                                     levelout = NULL,
                                                     cost = NULL,
                                                     cost_decisions = NULL,
                                                     cost_cues = NULL,
                                                     level_stats = NULL),

                                        test = list(stats = NULL,
                                                    decisions = NULL,
                                                    levelout = NULL,
                                                    cost = NULL,
                                                    cost_decisions = NULL,
                                                    cost_cues = NULL,
                                                    level_stats = NULL))),
            # model
            metadata = list(criterion_name = criterion_name,            # Name of the criterion
                            cue_names = cue_names,
                            cues_n = ncol(data) - 1,                 # Cue names
                            cases_n = nrow(data)),             # Number of cases
            # Parameters

            params = list(algorithm = algorithm,
                          goal = goal,
                          goal.chase = goal.chase,
                          goal.threshold = goal.threshold,
                          numthresh.method = numthresh.method,
                          numthresh.n = numthresh.n,
                          stopping.rule = stopping.rule,
                          stopping.par = stopping.par,
                          sens.w = sens.w,
                          max.levels = max.levels,
                          cost.outcomes = cost.outcomes,
                          cost.cues = cost.cues,
                          decision.labels = decision.labels,
                          main = main,
                          repeat.cues = repeat.cues,
                          quiet = quiet),

            # Competitive algorithms

            comp = list(lr = list(model = NULL, stats = NULL),
                        cart = list(model = NULL, stats = NULL),
                        rf = list(model = NULL, stats = NULL),
                        svm = list(model = NULL, stats = NULL)))

  class(x) <- "FFTrees"

  return(x)


}
