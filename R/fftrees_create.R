#' Create an object of class \code{FFTrees}
#'
#' @description \code{fftrees_create} creates an \code{FFTrees} object.
#'
#' \code{fftrees_create} is called internally by the main \code{\link{FFTrees}} function.
#' Its main purpose is to verify and store various parameters
#' (e.g., to denote algorithms, goals, thresholds) to be used in maximization processes
#' and for evaluation purposes (e.g., \code{sens.w} and cost values).
#'
#' @param data Training data (as data frame).
#' @param formula A formula (with a binary criterion variable).
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
#'
#' @param do.lr logical.
#' @param do.cart logical.
#' @param do.svm logical.
#' @param do.rf logical.
#' @param do.comp logical.
#'
#' @param quiet logical
#'
#'
#' @return A new \code{FFTrees} object.
#'
#' @keywords internal
#'
#' @seealso
#' \code{\link{fftrees_define}} for defining FFTs;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @import testthat
#' @importFrom magrittr "%>%"
#' @importFrom tibble as_tibble
#'
#' @export

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
                           do.lr = TRUE,
                           do.svm = TRUE,
                           do.cart = TRUE,
                           do.rf = TRUE,
                           do.comp = TRUE,
                           quiet = NULL) {

  # Provide user feedback: ----

  if (!quiet) {
    msg <- "Aiming to create a new FFTrees object:\n"
    cat(u_f_ini(msg))
  }

  # 1. Validation tests: ------

  # data: ----

  testthat::expect_true(!is.null(data),
                        info = "data is NULL"
  )

  testthat::expect_true(is.data.frame(data),
                        info = "data is not a dataframe"
  )


  # formula: ----

  testthat::expect_true(!is.null(formula),
                        info = "formula is NULL"
  )

  testthat::expect_type(formula,
                        type = "language"
  )

  criterion_name <- paste(formula)[2]


  # algorithm: ----

  algorithm_valid <- c("ifan", "dfan")

  testthat::expect_true(!is.null(algorithm),
                        info = "algorithm is NULL"
  )

  testthat::expect_true(algorithm %in% algorithm_valid)


  # sens.w: ----

  testthat::expect_true(!is.null(sens.w),
                        info = "sens.w is NULL"
  )

  testthat::expect_lte(sens.w, expected = 1)
  testthat::expect_gte(sens.w, expected = 0)


  # goal: ----

  goal_valid <- c("acc", "bacc", "wacc", "dprime", "cost")  # ToDo: Setting "dprime" does not seem to work!

  if (is.null(goal)) { # goal NOT set by user:

    if (!is.null(cost.outcomes) | !is.null(cost.cues)) { # use cost goal:

      goal <- "cost"
      if (!quiet) { cat(u_f_msg("\u2014 Setting 'goal = cost'\n")) }

    } else { # use accuracy goal:

      if (enable_wacc(sens.w)){ # use wacc:

        goal <- "wacc"
        if (!quiet) { cat(u_f_msg("\u2014 Setting 'goal = wacc'\n")) }

      } else { # use bacc (as bacc == wacc):

        goal <- "bacc"
        if (!quiet) { cat(u_f_msg("\u2014 Setting 'goal = bacc'\n")) }

      }

    }

  } else { # feedback user setting:

    if (!quiet) {
      msg <- paste0("\u2014 User set 'goal = ", goal, "'\n")
      cat(u_f_msg(msg))
      }

  } # if (is.null(goal)) else.

  # Verify goal:
  testthat::expect_true(!is.null(goal),
                        info = "goal is NULL"
  )

  testthat::expect_true(goal %in% goal_valid)

  if ((goal == "wacc") & (enable_wacc(sens.w) == FALSE)){ # correct to bacc:

    if (!quiet) {
      cat(u_f_msg("\u2014 The goal was set to 'wacc', but 'sens.w = 0.50': Setting 'goal = bacc'\n"))
    }
    goal <- "bacc"

  }


  # goal.chase: ----

  if (goal == "cost" & is.null(goal.chase)) { # use cost:

    goal.chase <- "cost"

    if (!quiet) { cat(u_f_msg("\u2014 Setting 'goal.chase = cost'\n")) }

  } else if (is.null(goal.chase)) { # use accuracy:

    if (enable_wacc(sens.w)){ # use wacc:

      goal.chase <- "wacc"
      if (!quiet) { cat(u_f_msg("\u2014 Setting 'goal.chase = wacc'\n")) }

    } else { # use bacc (as bacc == wacc):

      goal.chase <- "bacc"
      if (!quiet) { cat(u_f_msg("\u2014 Setting 'goal.chase = bacc'\n")) }

    }

  } else { # feedback user setting:

    if (!quiet) {
      msg <- paste0("\u2014 User set 'goal.chase = ", goal.chase, "'\n")
      cat(u_f_msg(msg))
      }

  }

  # Verify goal.chase:
  testthat::expect_true(!is.null(goal.chase),
                        info = "goal.chase is NULL"
  )

  testthat::expect_true(goal.chase %in% goal_valid)

  if ((goal.chase == "wacc") & (enable_wacc(sens.w) == FALSE)){ # correct to bacc:

    if (!quiet) {
      cat(u_f_msg("\u2014 The goal.chase was set to 'wacc', but 'sens.w = 0.50': Setting 'goal.chase = bacc'\n"))
    }
    goal.chase <- "bacc"

  }


  # goal.threshold: ----

  # Note: Default is set to goal.threshold = "bacc" (in FFTrees.R).

  # Use argument value from FFTrees(), but provide feedback:
  if (!quiet) {

    if (goal.threshold == "bacc"){ # report using bacc (i.e., the default):

      msg <- paste0("\u2014 Setting 'goal.threshold = ", goal.threshold, "'\n")
      cat(u_f_msg(msg))

    } else { # report user setting:

      msg <- paste0("\u2014 User set 'goal.threshold = ", goal.threshold, "'\n")
      cat(u_f_msg(msg))

    }

  } # if (!quiet).


  # Verify goal.threshold:
  testthat::expect_true(!is.null(goal.threshold),
                        info = "goal.threshold is NULL"
  )

  testthat::expect_true(goal.threshold %in% goal_valid)

  if ((goal.threshold == "wacc") & (enable_wacc(sens.w) == FALSE)){ # correct to bacc:

    if (!quiet) {
      cat(u_f_msg("\u2014 The goal.threshold was set to 'wacc', but 'sens.w = 0.50': Setting 'goal.threshold = bacc'\n"))
    }
    goal.threshold <- "bacc"

  }


  # Verify consistency of sens.w and bacc_wacc choices: ----

  # If a non-default sens.w has been set, but 'wacc' is neither used in 'goal' nor in 'goal.chase':
  if ((enable_wacc(sens.w)) & (goal != "wacc") & (goal.chase != "wacc")){ # provide feedback:

    if (!quiet) {
      msg <- paste0("You set sens.w = ", sens.w, ": Did you mean to set 'goal' or 'goal.chase' to 'wacc'?\n")
      cat(u_f_msg(msg))
    }

  }


  # numthresh.method: ----

  numthresh.method_valid <- c("optimise", "median")

  testthat::expect_true(substr(numthresh.method, 1, 1) %in% substr(numthresh.method_valid, 1, 1),
                        info = paste0(
                          "numthresh.method is not valid\nTry one of the following: ",
                          paste(numthresh.method_valid, collapse = ", ")
                        )
  )


  # numthresh.n: ----

  numthresh.n_valid <- c(3:20)

  testthat::expect_true(numthresh.n %in% numthresh.n_valid,
                        info = paste0(
                          "numthresh.n is not valid\nTry one of the following: ",
                          paste(numthresh.n_valid, collapse = ", ")
                        )
  )



  # max.levels: ----

  if (is.null(max.levels)) {

    max.levels <- 4  # default

    if (!quiet) {
      cat(u_f_msg("\u2014 Setting 'max.levels = 4'\n"))
    }

  } else { # user set max.levels:

    if (!quiet) {
      msg <- paste0("\u2014 User set 'max.levels = ", max.levels, "'\n")
      cat(u_f_msg(msg))
    }

  }

  testthat::expect_true(!is.null(max.levels),
                        info = "max.levels is NULL"
  )

  testthat::expect_true(max.levels %in% 1:6,
                        info = "max.levels must be an integer between 1 and 6"
  )


  # cost.outcomes: ----

  if (!is.null(cost.outcomes) & goal != "cost") {
    message("You specified cost.outcomes, but goal = '", goal, "' (not 'cost')\nFFT creation will ignore cost.outcomes, but report them in tree statistics.")
  }

  if (is.null(cost.outcomes)) { # use defaults:

    cost.outcomes <- list(hi = 0, mi = 1, fa = 1, cr = 0)

    if (!quiet) {
      cat(u_f_msg("\u2014 Setting 'cost.outcomes = list(hi = 0, mi = 1, fa = 1, cr = 0)'\n"))
    }
  }

  testthat::expect_true(!is.null(cost.outcomes),
                        info = "cost.outcomes is NULL"
  )

  testthat::expect_type(cost.outcomes,
                        type = "list"
                        # info = "cost.outcomes must be a list in the form list(hi = x, mi = x, fa = x, cr = x)"
  )

  testthat::expect_true(all(names(cost.outcomes) %in% c("hi", "mi", "fa", "cr")),
                        info = "cost.outcomes must be a list in the form list(hi = x, mi = x, fa = x, cr = x)"
  )


  # cost.cues: ----

  if (!is.null(cost.cues) & goal != "cost") {
    message("You specified cost.cues, but goal = '", goal, "' (not 'cost'):\nFFT creation will ignore cost.cues, but report them in tree statistics.")
  }

  if ((!quiet) & (!is.null(cost.cues))) {
    cat(u_f_msg("\u2014 Setting list of 'cost.cues'\n"))
  }

  # Append cost.cues (for all cues in data):
  cost.cues <- cost_cues_append(formula,
                                data,
                                cost.cues = cost.cues
                                )
  # str(cost.cues)  # 4debugging

  testthat::expect_true(!is.null(cost.cues), info = "cost.cues is NULL")

  testthat::expect_type(cost.cues, type = "list")

  testthat::expect_true(all(names(cost.cues) %in% names(data)),
                        info = "At least one of the values specified in cost.cues is not in data")


  # stopping.rule: ----

  valid_stopping_rules <- c("exemplars", "levels")

  testthat::expect_true(stopping.rule %in% valid_stopping_rules)


  # stopping.par: ----

  testthat::expect_gt(stopping.par, expected = 0)
  testthat::expect_lt(stopping.par, expected = 1)


  # decision.labels: ----

  testthat::expect_true(!is.null(decision.labels),
                        info = "decision.labels is NULL"
  )

  testthat::expect_equal(length(decision.labels), 2)


  # repeat.cues: ----

  testthat::expect_type(repeat.cues, type = "logical")



  # 2. Data quality checks: ------

  # Criterion is in data: ----

  testthat::expect_true(criterion_name %in% names(data),
                        info = paste("The criterion", criterion_name, "is not in data")
  )

  # No missing criterion values: ----

  testthat::expect_true(all(!is.na(data[[criterion_name]])),
                        info = "At least one of the criterion values are missing. Please remove these cases and try again."
  )

  # Criterion has two unique values: : ----

  testthat::expect_equal(length(unique(data[[criterion_name]])),
                         expected = 2,
                         info = "The criterion variable is non-binary"
  )


  # Make criterion logical: ----

  if (inherits(data[[criterion_name]], "character") |
      inherits(data[[criterion_name]], "factor")) {

    # Save original values as decision.labels:
    decision.labels <- unique(data[[criterion_name]])

    # Convert criterion to logical:
    data[[criterion_name]] <- data[[criterion_name]] == decision.labels[2]

    if (!quiet) {
      msg <- paste0("\u2014 Setting target to ", criterion_name, " == ", decision.labels[2], "\n")
      cat(u_f_msg(msg))
    }

  }


  # Check that criterion is in data.test: ----

  if (!is.null(data.test)) {
    testthat::expect_true(is.data.frame(data),
                          info = "Object is not a dataframe."
    )

    testthat::expect_true(criterion_name %in% names(data.test),
                          info = paste("The criterion", criterion_name, "is not in data.test")
    )
  }


  # Remove cues not included in formula: ----

  data <- model.frame(
    formula = formula,
    data = data,
    na.action = NULL
  )


  # Convert factors to character variables: ----

  data <- data %>%
    dplyr::mutate_if(is.factor, paste)

  # Do the same to data.test:
  if (!is.null(data.test)) {

    data.test <- model.frame(
      formula = formula,
      data = data.test,
      na.action = NULL
    )

    # Convert factor columns to character:

    data.test <- data.test %>%
      dplyr::mutate_if(is.factor, paste) %>%
      tibble::as_tibble()
  }

  # Get cue names: ----

  cue_names <- names(data)[2:ncol(data)]


  # Convert data to tibble: ----

  data <- data %>%
    tibble::as_tibble()



  # 3. Create the FFTrees object: ------

  x <- list(
    criterion_name = criterion_name,
    cue_names = cue_names,

    # Formula:
    formula = formula, # original formula

    # Tree info:
    trees = list(
      n = NULL,
      best = NULL,
      definitions = NULL,
      inwords = NULL,
      stats = NULL,
      level_stats = NULL,
      decisions = list(
        train = list(),
        test = list()
      )
    ),

    # Raw training data:
    data = list(
      train = data,
      test = data.test
    ),

    # Parameters:
    params = list(
      algorithm = algorithm,
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
      do.comp = do.comp
    ),


    # One row per algorithm competition:
    competition = list(

      train = data.frame(
        algorithm = NA,
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
        bacc = NA, cost = NA, cost_dec = NA, cost_cue = NA
      ),

      test = data.frame(
        algorithm = NA,
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
        bacc = NA, cost = NA, cost_dec = NA, cost_cue = NA
      ),

      models = list(lr = NULL, cart = NULL, rf = NULL, svm = NULL)
    )
  )

  class(x) <- "FFTrees"


  # Provide user feedback: ----

  if (!x$params$quiet) {
    cat(u_f_fin("Successfully created a new FFTrees object.\n"))
  }


  # Output: ------

  return(x)

} # fftrees_create().

# eof.
