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
#' @param do.lr logical.
#' @param do.cart logical.
#' @param do.svm logical.
#' @param do.rf logical.
#' @param do.comp logical.
#'
#' @importFrom  magrittr "%>%"
#' @import testthat
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
                           quiet = NULL,
                           do.lr = TRUE,
                           do.svm = TRUE,
                           do.cart = TRUE,
                           do.rf = TRUE,
                           do.comp = TRUE) {

# Validation tests ------------------------------------

## data ==============================================

testthat::expect_true(!is.null(data),
            info = "data is NULL")

testthat::expect_true(is.data.frame(data),
            info = "Object is not a dataframe")


## formula ============================================

testthat::expect_true(!is.null(formula),
            info = "formula is NULL")

testthat::expect_is(formula, "formula")


criterion_name <- paste(formula)[2]


## algorithm ==========================================

algorithm_valid <- c("ifan", "dfan", "max", "zigzag")

testthat::expect_true(!is.null(algorithm),
            info = "algorithm is NULL")

testthat::expect_true(algorithm %in% algorithm_valid)

## goal ================================================

goal_valid <- c("bacc", "wacc", "dprime", "cost", "acc")

if(is.null(goal)) {

  if(!is.null(cost.outcomes) | !is.null(cost.cues)) {

    goal <- "cost"

  if(quiet == FALSE) {message("Setting goal = 'cost'")}

  } else {

    goal <- "wacc"
    if(quiet == FALSE) {message("Setting goal = 'wacc'")}

  }
}

testthat::expect_true(!is.null(goal),
            info = "goal is NULL")

testthat::expect_true(goal %in% goal_valid)

## goal.chase ================================================

if(goal == "cost" & is.null(goal.chase)) {

  goal.chase <- "cost"

  if(quiet == FALSE) {message("Setting goal.chase = 'cost'")}

} else if (is.null(goal.chase)) {

  goal.chase <- "wacc"

  if(quiet == FALSE) {message("Setting goal.chase = 'waccc'")}

}

testthat::expect_true(!is.null(goal.chase),
            info = "goal.chase is NULL")

testthat::expect_true(goal.chase %in% goal_valid)

## goal.threshold ================================================

testthat::expect_true(!is.null(goal.threshold),
            info = "goal.threshold is NULL")

testthat::expect_true(goal.threshold %in% goal_valid)


## numthresh.method ==================================

numthresh.method_valid <- c("optimise", "median")

testthat::expect_true(substr(numthresh.method, 1, 1) %in% substr(numthresh.method_valid, 1, 1),
            info = paste0("numthresh.method is not valid\nTry one of the following: ",
            paste(numthresh.method_valid, collapse = ", ")))

## numthresh.n ==================================

numthresh.n_valid <- c(3:20)

testthat::expect_true(numthresh.n %in% numthresh.n_valid,
            info = paste0("numthresh.n is not valid\nTry one of the following: ",
                          paste(numthresh.n_valid, collapse = ", ")))


## sens.w ================================================

testthat::expect_true(!is.null(sens.w),
            info = "sens.w is NULL")

testthat::expect_lte(sens.w, expected = 1)

testthat::expect_gte(sens.w, expected = 0)

## max.levels =========================================

if(is.null(max.levels)) {

  max.levels <- 4

  if(quiet == FALSE) {"Setting max.levels = 4"}
}

testthat::expect_true(!is.null(max.levels),
            info = "max.levels is NULL")

testthat::expect_true(max.levels %in% 1:6,
            info = "max.levels must be an integer between 1 and 6")

## cost.outcomes =========================================

if(!is.null(cost.outcomes) & goal != "cost") {

  message("Note: You specified cost.outcomes but goal = '", goal, "' (not 'cost'). Trees will ignore these costs during growth")

}

if(is.null(cost.outcomes)) {

cost.outcomes <- list(hi = 0, mi = 1, fa = 1, cr = 0)

if(quiet == FALSE) {

  message("Setting cost.outcomes = list(hi = 0, mi = 1, fa = 1, cr = 0)")

}

}

testthat::expect_true(!is.null(cost.outcomes),
            info = "cost.outcomes is NULL")

testthat::expect_is(cost.outcomes,
                   class = "list",
                   info = "cost.outcomes must be a list in the form list(hi = x, mi = x, fa = x, cr = x)")

testthat::expect_true(all(names(cost.outcomes) %in% c("hi", "mi", "fa", "cr")),
                       info = "cost.outcomes must be a list in the form list(hi = x, mi = x, fa = x, cr = x)")

## cost.cues =========================================

if(!is.null(cost.cues) & goal != "cost") {

  message("Note: You specified cost.cues but goal = '", goal, "' (not 'cost'). Trees will ignore these costs during growth")

}


# Append cost.cues
cost.cues <- cost.cues.append(formula,
                                        data,
                                        cost.cues = cost.cues)

testthat::expect_true(!is.null(cost.cues),
            info = "cost.cues is NULL")

testthat::expect_is(cost.cues,
          class = "list")

testthat::expect_true(all(names(cost.cues) %in% names(data)),
            info = "At least one of the values you specified in cost.cues is not in data")

## stopping.rule ====================================

stopping.rule_valid <- c("exemplars", "levels")

testthat::expect_true(stopping.rule %in% stopping.rule_valid)

## stopping.par ====================================

testthat::expect_gt(stopping.par, expected = 0)
testthat::expect_lt(stopping.par, expected = 1)

## decision.labels ===================================

testthat::expect_true(!is.null(decision.labels),
            info = "decision.labels is NULL")

testthat::expect_equal(length(decision.labels), 2)

## repeat.cues ============================================
testthat::expect_is(repeat.cues, "logical")

# Data quality checks ----------------------------------------------------------

## Criterion is in data ===================================

testthat::expect_true(criterion_name %in% names(data),
            info = paste("The criterion", criterion_name, "is not in your data object"))

## No missing criterion values ============================

testthat::expect_true(all(!is.na(data[[criterion_name]])),
            info = "At least one of the criterion values are missing. Please remove these cases and try again.")

## Criterion has two unique values

testthat::expect_equal(length(unique(data[[criterion_name]])),
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

testthat::expect_true(is.data.frame(data),
            info = "Object is not a dataframe")

testthat::expect_true(criterion_name %in% names(data.test),
            info = paste("The criterion", criterion_name, "is not in your data.test object"))

}

## Remove cues not specified in formula ========================

data <- model.frame(formula = formula,
                    data = data,
                    na.action = NULL)

## Convert factor columns to character

data <- data %>%
  dplyr::mutate_if(is.factor, paste)

# Do the same to data.test

if(!is.null(data.test)) {

data.test <- model.frame(formula = formula,
                    data = data.test,
                    na.action = NULL)

## Convert factor columns to character

data.test <- data.test %>%
  dplyr::mutate_if(is.factor, paste) %>%
  dplyr::as_tibble()

}

# Get cue names
cue_names <- names(data)[2:ncol(data)]

# Convert data to tibble

data <- data %>%
  tibble::as_tibble()

# Create FFTrees object ------------------------------------------------

  x <- list(

            criterion_name = criterion_name,
            cue_names = cue_names,

            # Formula
            formula = formula,                                  # Original formula

            # Tree info
            trees = list(n = NULL,
                         best = NULL,
                         definitions = NULL,
                         inwords = NULL,
                         stats = NULL,
                         level_stats = NULL,
                         decisions = list(train = list(),
                                          test = list())),

            # Raw training data
            data = list(train = data,
                        test = data.test),

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
                          quiet = quiet,
                          my.tree = my.tree,
                          do.lr = do.lr,
                          do.cart = do.cart,
                          do.svm = do.svm,
                          do.rf = do.rf,
                          do.comp = do.comp),


            # One row per algorithm competition

            competition = list(train = data.frame(algorithm = NA,
                                                  n = NA,
                                                  hi = NA,
                                                  fa = NA,
                                                  mi = NA,
                                                  cr = NA,
                                                  sens = NA,
                                                  spec = NA,
                                                  far = NA,
                                                  ppv = NA,
                                                  npv = NA,
                                                  acc = NA,
                                                  bacc = NA, cost = NA, cost_decisions = NA, cost_cues = NA),
                               test = data.frame(algorithm = NA,
                                                 n = NA,
                                                 hi = NA,
                                                 fa = NA,
                                                 mi = NA,
                                                 cr = NA,
                                                 sens = NA,
                                                 spec = NA,
                                                 far = NA,
                                                 ppv = NA,
                                                 npv = NA,
                                                 acc = NA,
                                                 bacc = NA, cost = NA, cost_decisions = NA, cost_cues = NA),
                               models = list(lr = NULL, cart = NULL, rf = NULL, svm = NULL))

            )

  class(x) <- "FFTrees"

  return(x)


}
