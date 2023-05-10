#' Create an object of class \code{FFTrees}
#'
#' @description \code{fftrees_create} creates an \code{FFTrees} object.
#'
#' \code{fftrees_create} is called internally by the main \code{\link{FFTrees}} function.
#' Its main purpose is to verify and store various parameters
#' (e.g., to denote algorithms, goals, thresholds) to be used in maximization processes
#' and for evaluation purposes (e.g., \code{sens.w} and cost values).
#'
#' @param formula A formula (with a binary criterion variable).
#' @param data Training data (as data frame).
#' @param data.test Data for testing models/prediction (as data frame).
#' @param algorithm Algorithm for growing FFTs (\code{"ifan"} or \code{"dfan"}) (as character string).
#'
#' @param goal Measure used to select FFTs (as character string).
#' @param goal.chase Measure used to optimize FFT creation (as character string).
#' @param goal.threshold Measure used to optimize cue thresholds (as character string).
#'
#' @param max.levels integer.
#' @param numthresh.method string.
#' @param numthresh.n integer.
#' @param repeat.cues logical.
#' @param stopping.rule string.
#' @param stopping.par numeric.
#'
#' @param sens.w numeric.
#'
#' @param cost.outcomes list.
#' @param cost.cues list.
#'
#' @param main string.
#' @param decision.labels string.
#'
#' @param my.goal The name of an optimization measure defined by \code{my.goal.fun} (as a character string).
#' Example: \code{my.goal = "my_acc"} (see \code{my.goal.fun} for corresponding function).
#' Default: \code{my.goal = NULL}.
#' @param my.goal.fun The definition of an outcome measure to optimize, defined as a function
#' of the frequency counts of the 4 basic classification outcomes \code{hi, fa, mi, cr}
#' (i.e., an R function with 4 arguments \code{hi, fa, mi, cr}).
#' Example: \code{my.goal.fun = function(hi, fa, mi, cr){(hi + cr)/(hi + fa + mi + cr)}} (i.e., accuracy).
#' Default: \code{my.goal.fun = NULL}.
#'
#' @param my.tree A verbal description of an FFT, i.e., an "FFT in words" (as character string).
#' For example, \code{my.tree = "If age > 20, predict TRUE. If sex = {m}, predict FALSE. Otherwise, predict TRUE."}.
#'
#' @param do.comp logical.
#' @param do.lr logical.
#' @param do.cart logical.
#' @param do.svm logical.
#' @param do.rf logical.
#'
#' @param quiet A list of logical elements.
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

fftrees_create <- function(formula = NULL,
                           data = NULL,
                           data.test = NULL,
                           algorithm = NULL,
                           #
                           goal = NULL,
                           goal.chase = NULL,
                           goal.threshold = NULL,
                           #
                           max.levels = NULL,
                           numthresh.method = NULL,
                           numthresh.n = NULL,
                           repeat.cues = NULL,
                           stopping.rule = NULL,
                           stopping.par = NULL,
                           #
                           sens.w = NULL,
                           #
                           cost.outcomes = NULL,
                           cost.cues = NULL,
                           #
                           main = NULL,
                           decision.labels = NULL,
                           #
                           my.goal = NULL,      # e.g., "my_acc",  # name of my.goal (as character)
                           my.goal.fun = NULL,  # e.g., function(hi, fa, mi, cr){(hi + cr)/(hi + fa + mi + cr)},  # a function of (hi, fa, mi, cr)
                           my.tree = NULL,
                           #
                           do.comp = TRUE,
                           do.lr = TRUE,
                           do.svm = TRUE,
                           do.cart = TRUE,
                           do.rf = TRUE,
                           #
                           quiet = NULL) {

  # Provide user feedback: ----

  if (!quiet$ini) {

    # msg <- "Aiming to create a new FFTrees object:\n"
    # cat(u_f_ini(msg))

    # basic:
    cli::cli_alert("Create an FFTrees object:", class = "alert-start")

    # # more:
    # cli::cli_h2(in_blue("Create FFT"))
    # cli::cli_alert(in_darkgrey("Create a new FFTrees object:"), class = "alert-start")

  }

  # 1. Validation tests: ------

  # data: ----

  testthat::expect_true(!is.null(data), info = "data is NULL")
  testthat::expect_true(is.data.frame(data), info = "data is not a dataframe")


  # formula: ----

  # Get criterion (from formula):
  criterion_name <- get_lhs_formula(formula)

  if (!criterion_name %in% names(data)){
    stop(paste0("Criterion variable '", criterion_name, "' was not found in data"))
  }


  # algorithm: ----

  testthat::expect_true(!is.null(algorithm), info = "algorithm is NULL")
  testthat::expect_true(algorithm %in% algorithm_options)  # use global constant


  # sens.w: ----

  testthat::expect_true(!is.null(sens.w), info = "sens.w is NULL")
  testthat::expect_lte(sens.w, expected = 1)
  testthat::expect_gte(sens.w, expected = 0)


  # goal: ----

  # Current set of valid goals (for FFT selection):

  if (!is.null(my.goal)){

    valid_goal <- c(goal_options, my.goal)  # add my.goal (name) to default

  } else { # default:

    valid_goal <- goal_options  # use global constant

  }

  if (is.null(goal)) { # goal NOT set by user:

    if (!is.null(cost.outcomes) | !is.null(cost.cues)) { # use 'cost' goal per default:

      goal <- "cost"

      # Provide user feedback:
      if (!quiet$set) { cat(u_f_msg("\u2014 Setting 'goal = cost'\n")) }

    } else { # use accuracy defaults (bacc/wacc):

      if (enable_wacc(sens.w)){ # use wacc:

        goal <- "wacc"

        # Provide user feedback:
        if (!quiet$set) { cat(u_f_msg("\u2014 Setting 'goal = wacc'\n")) }

      } else { # use bacc (as bacc == wacc):

        goal <- "bacc"

        # Provide user feedback:
        if (!quiet$set) { cat(u_f_msg("\u2014 Setting 'goal = bacc'\n")) }

      }

    }

  } else { # feedback user setting:

    if (!quiet$set) { # Provide user feedback:

      msg <- paste0("\u2014 User set 'goal = ", goal, "'\n")

      cat(u_f_msg(msg))

    }

  } # if (is.null(goal)) else.

  # Verify goal:
  testthat::expect_true(!is.null(goal), info = "goal is NULL")
  testthat::expect_true(goal %in% valid_goal)

  if ((goal == "wacc") & (!enable_wacc(sens.w))){ # correct to "bacc":

    if (any(sapply(quiet, isFALSE))) { # Provide user feedback:

      wrn_msg <- "User set 'goal = wacc', but 'sens.w = 0.50': Setting 'goal = bacc'"

      # cat(u_f_hig(wrn_msg, "\n"))

      cli::cli_alert_warning(wrn_msg)

    }

    goal <- "bacc"

  }


  # goal.chase: ----

  if (goal == "cost" & is.null(goal.chase)) { # set to 'cost' as well:

    goal.chase <- "cost"

    if (!quiet$set) { # Provide user feedback:

      cat(u_f_msg("\u2014 Setting 'goal.chase = cost'\n"))

    }

  } else if (is.null(goal.chase)) { # use accuracy defaults (bacc/wacc):

    if (enable_wacc(sens.w)){ # set to 'wacc':

      goal.chase <- "wacc"

      # Provide user feedback:
      if (!quiet$set) { cat(u_f_msg("\u2014 Setting 'goal.chase = wacc'\n")) }

    } else { # set to 'bacc' (as bacc == wacc):

      goal.chase <- "bacc"

      # Provide user feedback:
      if (!quiet$set) { cat(u_f_msg("\u2014 Setting 'goal.chase = bacc'\n")) }

    }

  } else { # Note user setting:

    if (!quiet$set) { # Provide user feedback:

      msg <- paste0("\u2014 User set 'goal.chase = ", goal.chase, "'\n")

      cat(u_f_msg(msg))

    }

  }

  # Verify goal.chase:

  testthat::expect_true(!is.null(goal.chase), info = "goal.chase is NULL")
  testthat::expect_true(goal.chase %in% valid_goal)

  if ((goal.chase == "wacc") & (!enable_wacc(sens.w))){ # correct to "bacc":

    if (any(sapply(quiet, isFALSE))) { # Provide user feedback:

      wrn_msg <- "User set 'goal.chase = wacc', but 'sens.w = 0.50': Setting 'goal.chase = bacc'"

      # cat(u_f_hig(wrn_msg, "\n"))

      cli::cli_alert_warning(wrn_msg)

    }

    goal.chase <- "bacc"

  }


  # goal.threshold: ----

  if (is.null(goal.threshold)) { # use accuracy defaults (bacc/wacc):

    if (enable_wacc(sens.w)){ # set to 'wacc':

      goal.threshold <- "wacc"

      if (!quiet$set) { # Provide user feedback:

        cat(u_f_msg("\u2014 Setting 'goal.threshold = wacc'\n"))

      }

    } else { # set to 'bacc' (as bacc == wacc):

      goal.threshold <- "bacc"

      # Provide user feedback:
      if (!quiet$set) { cat(u_f_msg("\u2014 Setting 'goal.threshold = bacc'\n")) }

    }

  } else { # Note user setting:

    if (!quiet$set) { # Provide user feedback:

      msg <- paste0("\u2014 User set 'goal.threshold = ", goal.threshold, "'\n")

      cat(u_f_msg(msg))

    }

  }


  # # OLD code start: ----
  #
  # # Note: Default was set to goal.threshold = "bacc" (in FFTrees.R).
  #
  # # Use argument value from FFTrees(), but provide feedback:
  # if (!quiet$set) {
  #
  #   if (goal.threshold == "bacc"){ # report using bacc (i.e., the default):
  #
  #     msg <- paste0("\u2014 Setting 'goal.threshold = ", goal.threshold, "'\n")
  #     cat(u_f_msg(msg))
  #
  #   } else { # report user setting:
  #
  #     msg <- paste0("\u2014 User set 'goal.threshold = ", goal.threshold, "'\n")
  #     cat(u_f_msg(msg))
  #
  #   }
  #
  # } # if (any(sapply(quiet, isFALSE))).
  #
  #
  # # OLD code end. ----


  # Verify goal.threshold:

  testthat::expect_true(!is.null(goal.threshold), info = "goal.threshold is NULL")
  testthat::expect_true(goal.threshold %in% valid_goal)

  if ((goal.threshold == "wacc") & (!enable_wacc(sens.w))){ # correct to "bacc":

    if (any(sapply(quiet, isFALSE))) { # Provide user feedback:

      wrn_msg <- "User set 'goal.threshold = wacc', but 'sens.w = 0.50': Setting 'goal.threshold = bacc'"

      # cat(u_f_hig(wrn_msg, "\n"))

      cli::cli_alert_warning(wrn_msg)

    }

    goal.threshold <- "bacc"

  }

  if (goal.threshold == "cost") { # note that this only makes sense for outcome costs:

    if (any(sapply(quiet, isFALSE))) { # Provide user feedback:

      wrn_msg <- "Optimizing cue thresholds for 'cost' only uses 'cost.outcomes', as 'cost.cues' are constant per cue."

      # cat(u_f_hig(wrn_msg, "\n"))

      cli::cli_alert_warning(wrn_msg)

    }

  }


  # my.goal and my.goal.fun: ----

  if (!is.null(my.goal)){

    testthat::expect_true(is.character(my.goal), info = "Provided 'my.goal' is not of type 'character'")
    testthat::expect_true(length(my.goal) == 1,  info = "Provided 'my.goal' is not of length 1")
    testthat::expect_true(is.function(my.goal.fun),  info = "Provided 'my.goal.fun' is not of type 'function'")

    # my.goal.fun must only use 4 freq arguments:
    my_goal_arg_valid <- c("hi", "fa", "mi", "cr")
    fn_arg_names <- names(formals(my.goal.fun))
    # print(fn_arg_names)  # 4debugging

    if (any(fn_arg_names %in% my_goal_arg_valid == FALSE)){

      invalid_args <- setdiff(fn_arg_names, my_goal_arg_valid)
      invalid_avec <- paste(invalid_args, collapse = ", ")

      stop("my.goal.fun must contain 4 arguments (hi, fa, mi, cr), but not ", invalid_avec)

    }

    if (any(my_goal_arg_valid %in% fn_arg_names == FALSE)){

      missing_args <- setdiff(my_goal_arg_valid, fn_arg_names)
      missing_avec <- paste(missing_args, collapse = ", ")
      if (length(missing_args) < 2) {be <- "is"} else { be <- "are"}

      cli::cli_alert_warning("my.goal.fun usually contains 4 arguments (hi, fa, mi, cr), but (", missing_avec, ") ", be, " missing")

    }

  } # if (my.goal).


  cur_goals <- c(goal, goal.chase, goal.threshold)  # IFF all goals are set.


  # Verify consistency of sens.w and bacc_wacc choices: ----

  # If a non-default sens.w has been set, but 'wacc' is not a goal:
  if ((enable_wacc(sens.w)) & (!"wacc" %in% cur_goals)) { # provide feedback:

    if (any(sapply(quiet, isFALSE))) { # Provide user feedback:

      wrn_msg <- paste0("You set 'sens.w = ", sens.w, "': Did you mean to set a goal to 'wacc'?")

      # cat(u_f_hig(wrn_msg, "\n"))

      cli::cli_alert_warning(wrn_msg)
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

    if (!quiet$set) { # Provide user feedback:

      cat(u_f_msg("\u2014 Setting 'max.levels = 4'\n"))

    }

  } else { # user set max.levels:

    if (!quiet$set) { # Provide user feedback:

      msg <- paste0("\u2014 User set 'max.levels = ", max.levels, "'\n")

      cat(u_f_msg(msg))

    }

  }

  testthat::expect_true(!is.null(max.levels), info = "max.levels is NULL")
  testthat::expect_true(max.levels %in% 1:6, info = "max.levels must be an integer between 1 and 6")


  # cost.outcomes: ----

  if (!is.null(cost.outcomes)) { # A: user set cost.outcomes:

    if (any(sapply(quiet, isFALSE))) {

      cos <- paste(unlist(cost.outcomes), collapse = " ")

      if (!quiet$set){ # Provide user feedback:

        msg <- paste0("\u2014 User set 'cost.outcomes' = (", cos, ")\n")

        cat(u_f_msg(msg))

      }

      if (!"cost" %in% cur_goals){

        cos <- paste(unlist(cost.outcomes), collapse = " ")

        my_sp <- "  "
        wrn_msg <- paste0("Specified 'cost.outcomes' = {cos}, but no goal = 'cost':\n{my_sp}FFT creation will ignore costs, but report cost statistics.")

        # cat(u_f_hig(wrn_msg, "\n"))

        cli::cli_alert_warning(wrn_msg)

      }

    }

  } else { # B: use default cost.outcomes:

    # cost.outcomes <- list(hi = 0, fa = 1, mi = 1, cr = 0)  # default values (analogous to accuracy: r = -1)
    cost.outcomes <- cost_outcomes_default  # use global default

    if (!quiet$set) { # Provide user feedback:

      cos <- paste(unlist(cost.outcomes), collapse = " ")
      msg <- paste0("\u2014 Using default 'cost.outcomes' = (", cos, ")\n")
      cat(u_f_msg(msg))

    }

  }

  # Verify cost.outcomes:
  testthat::expect_true(!is.null(cost.outcomes), info = "cost.outcomes is NULL")
  testthat::expect_type(cost.outcomes, type = "list")
  testthat::expect_true(all(names(cost.outcomes) %in% c("hi", "fa", "mi", "cr")),
                        info = "cost.outcomes must be a list in the form list(hi = a, fa = b, mi = c, cr = d)")


  # cost.cues: ----

  if (!is.null(cost.cues)) { # A: user set cost.cues:

    if (any(sapply(quiet, isFALSE))) {

      ccs <- paste(unlist(cost.cues), collapse = " ")

      if (!quiet$set){ # Provide user feedback:

        msg <- paste0("\u2014 User set 'cost.cues' = (", ccs, ")")

        cat(u_f_msg(msg, "\n"))

      }

      if (!"cost" %in% cur_goals){

        ccs <- paste(unlist(cost.cues), collapse = " ")
        my_sp <- "  "

        wrn_msg <- paste0("Specified 'cost.cues' = {ccs}, but no goal = 'cost':\n{my_sp}FFT creation will ignore costs, but report cost statistics.")

        # cat(u_f_hig(wrn_msg, "\n"))

        cli::cli_alert_warning(wrn_msg)

      }

    }

  } else { # B: use default cost.cues:

    if (!quiet$set) { # Provide user feedback:

      msg <- paste0("\u2014 Using default 'cost.cues' = (", cost_cues_default, " per cue)\n")

      cat(u_f_msg(msg))

    }

  }

  # Append cost.cues (to all cues in data):
  cost.cues <- cost_cues_append(formula,
                                data,
                                cost.cues = cost.cues)
  # str(cost.cues)  # 4debugging

  # Verify cost.cues:
  testthat::expect_true(!is.null(cost.cues), info = "cost.cues is NULL")
  testthat::expect_type(cost.cues, type = "list")
  testthat::expect_true(all(names(cost.cues) %in% names(data)),
                        info = "At least one of the cue names specified in cost.cues is not in data")


  # stopping.rule: ----

  testthat::expect_true(stopping.rule %in% stopping_rules,  # use global constant
                        info = paste0("The stopping.rule must be in ('", paste(stopping_rules, collapse = "', '"), "')"))

  # stopping.par: ----

  if (stopping.rule == "exemplars"){ # default: 0 < stopping.par < 1:

    testthat::expect_gt(stopping.par, expected = 0)
    testthat::expect_lt(stopping.par, expected = 1)

  } else if (stopping.rule == "levels"){ # stopping.par must be a positive integer:

    stopping.par <- as.integer(stopping.par)  # aim to coerce to integer

    testthat::expect_true(is.integer(stopping.par))
    testthat::expect_gt(stopping.par, expected = 0)

  } else if (stopping.rule == "statdelta"){ # stopping.par must numeric:

    stopping.par <- as.numeric(stopping.par)  # aim to coerce to numeric

    testthat::expect_true(is.numeric(stopping.par))

  } else { # unknown stopping.rule:

    warning(paste0("Unknown stopping.par constraints for 'stopping.rule = ", stopping.rule, "'"))

  }


  if (!quiet$set) { # Provide user feedback:

    msg <- paste0("\u2014 Using 'stopping.rule = ", stopping.rule, "' (with 'stopping.par = ", stopping.par, "')\n")

    cat(u_f_msg(msg))

  }

  # # Disallow some combination:
  # if (stopping.rule == "statdelta" & goal.chase == "cost"){
  #
  #   stop("The stopping.rule 'statdelta' requires an accuracy measure as its 'goal.chase' value (e.g., 'bacc')")
  #
  # }


  # decision.labels: ----

  testthat::expect_true(!is.null(decision.labels), info = "decision.labels is NULL")
  testthat::expect_equal(length(decision.labels), 2)


  # repeat.cues: ----

  testthat::expect_type(repeat.cues, type = "logical")


  # 2. Verify and pre-process criterion and data: ------

  # Verify data and criterion: ----

  verify_data_and_criterion(data = data, criterion_name = criterion_name, mydata = "train")  # no output

  if (!is.null(data.test)) { # same for test data:
    verify_data_and_criterion(data = data.test, criterion_name = criterion_name, mydata = "test")  # no output
  }


  # # OLD code start: ----
  #
  # # Convert (a character or factor) criterion to logical: ----
  #
  # if (inherits(data[[criterion_name]], "character") |
  #     inherits(data[[criterion_name]], "factor")) {
  #
  #   # Save original values as decision.labels:
  #   decision.labels <- unique(data[[criterion_name]])
  #
  #   # Remove any NA values from decision.labels (if present):
  #   decision.labels <- decision.labels[!is.na(decision.labels)]
  #
  #   # Main: Convert criterion to logical:
  #   data[[criterion_name]] <- data[[criterion_name]] == decision.labels[2]  # Note: NA values remain NA
  #
  #   if (any(sapply(quiet, isFALSE))) {
  #
  #     msg_lgc <- paste0("Converted the criterion to logical by '", criterion_name, " == ", decision.labels[2], "'.")
  #
  #     # cat(u_f_hig("\u2014 ", msg_lgc), "\n")
  #
  #     cli::cli_alert_warning(msg_lgc)
  #
  #   }
  #
  # } # Note: Moved into clean_data() TO ALSO repeat for data.test.
  #
  # # OLD code end. ----


  # Clean/pre-process training data: ----

  # # OLD code start: ----
  #
  # # A. Remove any cues not in formula:
  # data <- model.frame(
  #   formula = formula,
  #   data = data,
  #   na.action = NULL
  # )
  #
  # # B. Handle NA cases:
  # if ( (allow_NA_pred | allow_NA_crit) & any(is.na(data)) ){
  #   data <- handle_NA_data(data = data, criterion_name = criterion_name,
  #                          mydata = "train", quiet = quiet)
  # }
  #
  # # C. Convert any factor variables to character variables:
  # data <- data %>%
  #   dplyr::mutate_if(is.factor, paste)
  #
  # # D. Convert to tibble:
  # data <- data %>%
  #   tibble::as_tibble()
  #
  # # OLD code end. ----

  data <- clean_data(data = data, criterion_name = criterion_name, formula = formula,
                     mydata = "train", quiet = quiet)


  # Clean/pre-process data.test (same steps): ----

  if (!is.null(data.test)) { # same for test data:

    # # OLD code start: ----
    #
    # # A. Remove any cues not in formula:
    # data.test <- model.frame(
    #   formula = formula,
    #   data = data.test,
    #   na.action = NULL
    # )
    #
    # # B. Handle NA cases:
    # if ( (allow_NA_pred | allow_NA_crit) & any(is.na(data.test)) ){
    #   data.test <- handle_NA_data(data = data.test, criterion_name = criterion_name,
    #                               mydata = "test", quiet = quiet)
    # }
    #
    # # C. Convert any factor variables to character variables:
    # data.test <- data.test %>%
    #   dplyr::mutate_if(is.factor, paste)
    #
    # # D. Convert to tibble:
    # data.test <- data.test %>%
    #   tibble::as_tibble()
    #
    # # OLD code end. ----

    data.test <- clean_data(data = data.test, criterion_name = criterion_name, formula = formula,
                            mydata = "test", quiet = quiet)

  }



  # 3. Create the FFTrees object: ------

  # Get cue names:
  cue_names <- names(data)[2:ncol(data)]  # (all except for criterion)

  # Create x (as list):
  x <- list(

    # Names of criterion vs. cues:
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

    # Store parameters (as list):
    params = list(
      algorithm = algorithm,
      #
      goal = goal,
      goal.chase = goal.chase,
      goal.threshold = goal.threshold,
      #
      max.levels = max.levels,
      numthresh.method = numthresh.method,
      numthresh.n = numthresh.n,
      repeat.cues = repeat.cues,
      stopping.rule = stopping.rule,
      stopping.par = stopping.par,
      #
      sens.w = sens.w,
      #
      cost.outcomes = cost.outcomes,
      cost.cues = cost.cues,
      #
      main = main,
      decision.labels = decision.labels,
      #
      my.goal = my.goal,
      my.goal.fun = my.goal.fun,
      my.tree = my.tree,
      #
      do.comp = do.comp,
      do.lr = do.lr,
      do.cart = do.cart,
      do.svm = do.svm,
      do.rf = do.rf,
      #
      quiet = quiet
    ),

    # One row per algorithm competition:
    competition = list(

      train = data.frame(
        algorithm = NA,
        n = NA,
        hi = NA, fa = NA, mi = NA, cr = NA,
        sens = NA, spec = NA, far = NA,
        ppv = NA, npv = NA,
        acc = NA, bacc = NA,
        cost = NA, cost_dec = NA, cost_cue = NA
      ),

      test = data.frame(
        algorithm = NA,
        n = NA,
        hi = NA, fa = NA, mi = NA, cr = NA,
        sens = NA, spec = NA, far = NA,
        ppv = NA, npv = NA,
        acc = NA, bacc = NA,
        cost = NA, cost_dec = NA, cost_cue = NA
      ),

      models = list(lr = NULL, cart = NULL, rf = NULL, svm = NULL)

    ) # competition.

  ) # x.

  class(x) <- "FFTrees"


  # Provide user feedback: ----

  if (!quiet$fin) {

    # cat(u_f_fin("Successfully created a new FFTrees object.\n"))

    cli::cli_alert_success("Created an FFTrees object.")

  }


  # Output: ------

  return(x)

} # fftrees_create().

# eof.
