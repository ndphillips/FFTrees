# util_abc.R:
# Miscellaneous auxiliary/utility functions.
# ------------------------------------------

# General/miscellaneous helper functions
# (grouped into loose categories).


# (A) Applying or computing stuff: ------


# apply_break: ------

# Takes a direction, threshold value, and cue vector, and returns a vector of decisions.

apply_break <- function(direction,
                        threshold.val,
                        cue.v,
                        cue.class) {

  testthat::expect_true(direction %in% c("!=", "=", "<", "<=", ">", ">="))
  testthat::expect_length(threshold.val, 1)

  # direction = cue_direction_new
  # threshold.val = cue_threshold_new
  # cue.v = data_current[[cues_name_new]]
  # cue.class = cue_class_new

  if (is.character(threshold.val)) {
    threshold.val <- unlist(strsplit(threshold.val, ","))
  }

  if (cue.class %in% c("numeric", "integer")) {
    threshold.val <- as.numeric(threshold.val)
  }

  if (direction == "!=") {
    output <- (cue.v %in% threshold.val) == FALSE
  }

  if (direction == "=") {
    output <- cue.v %in% threshold.val
  }

  if (direction == "<") {
    output <- cue.v < threshold.val
  }

  if (direction == "<=") {
    output <- cue.v <= threshold.val
  }

  if (direction == ">") {
    output <- cue.v > threshold.val
  }

  if (direction == ">=") {
    output <- cue.v >= threshold.val
  }

  return(output)

} # apply_break().



# cost_cues_append: ------

# Goal: Get cost.cues for ALL cues in data.
# ToDo: Distinguish function input from output.

cost_cues_append <- function(formula,
                             data,
                             cost.cues = NULL) {

  # Prepare: ------

  criterion_name <- paste(formula)[2]

  data_mf <- model.frame(
    formula = formula,
    data = data
  )

  cue_df <- data_mf[, 2:ncol(data_mf), drop = FALSE]
  cue_name_v <- names(cue_df)


  # Main: ------

  if (is.null(cost.cues)) { # Case 1: No cost.cues provided: ----

    cost.cues <- lapply(1:ncol(cue_df), FUN = function(x) {
      0
    })
    names(cost.cues) <- names(cue_df)


  } else { # if (is.null(cost.cues) == FALSE) { # Case 2: cost.cues provided: ----

    # Make sure all named cues in cost.cues are in data:
    {
      cue_not_in_data <- sapply(names(cost.cues), FUN = function(x) {
        x %in% cue_name_v == FALSE
      })

      if (any(cue_not_in_data)) {

        missing_cues <- paste(cost.cues[cue_not_in_data, 1], collapse = ",")

        warning(paste0("The cue(s) {", missing_cues, "} specified in cost.cues are not present in the data."))
      }
    }

    # Add any missing cue costs as 0:
    {
      cost_cues_org <- cost.cues

      cost.cues <- lapply(1:ncol(cue_df), FUN = function(x) {
        0
      })
      names(cost.cues) <- names(cue_df)

      for (i in 1:length(cost.cues)) {

        cue_name_i <- names(cost.cues)[i]

        if (names(cost.cues)[i] %in% names(cost_cues_org)) {
          cost.cues[[i]] <- cost_cues_org[[cue_name_i]]
        }
      }
    }
  } # if (is.null(cost.cues) == FALSE).


  # Output: ------

  return(cost.cues)

} # cost_cues_append().



# fact_clean: ------

#' Clean factor variables in prediction data
#'
#' @param data.train A training dataset
#' @param data.test A testing dataset
#' @param show.warning logical

fact_clean <- function(data.train,
                       data.test,
                       show.warning = T) {


  # 1. Look for new factor values in test set that are not in training set: ----

  orig.vals.ls <- lapply(1:ncol(data.train), FUN = function(x) {
    unique(data.train[, x])
  })

  # 2. can.predict.mtx: ----
  can.predict.mtx <- matrix(1, nrow = nrow(data.test), ncol = ncol(data.test))

  for (i in 1:ncol(can.predict.mtx)) {

    test.vals.i <- data.test[, i]

    if (is.numeric(test.vals.i)) {

      can.predict.mtx[, i] <- 1

    } else {

      can.predict.mtx[, i] <- paste(test.vals.i) %in% paste(orig.vals.ls[[i]])

    }
  }

  # 3. model.can.predict: ----
  model.can.predict <- isTRUE(all.equal(rowMeans(can.predict.mtx), 1))

  if (identical(mean(model.can.predict), 1) == FALSE & show.warning == TRUE) {

    warning(paste(sum(model.can.predict), " out of ",
                  nrow(data.test), " cases (", round(sum(model.can.predict == 0) / length(model.can.predict), 2) * 100,
                  "%) were removed from the test dataset.",
                  sep = ""
    ))
  }

  # Output: ----

  output <- data.test[model.can.predict, ]

  return(output)

} # fact_clean().




# (B) Enabling stuff: ------


# enable_wacc: ------

# Test whether wacc makes sense (iff sens.w differs from its default of 0.50).

# The argument sens.w_epsion provides a threshold value:
# Minimum required difference from the sens.w default value (sens.w = 0.50).

# Output: Boolean value.

enable_wacc <- function(sens.w, sens.w_epsilon = 10^-4){

  out <- FALSE

  if (abs(sens.w - .50) >= sens.w_epsilon){
    out <- TRUE
  }

  return(out)

} # enable_wacc().




# (C) Getting stuff: ------


# get_bacc_wacc: ------

# Obtain either bacc or wacc (for displays in print and plot functions).
# Output: Named vector (with name specifying the current type of measure).

get_bacc_wacc <- function(sens, spec,  sens.w){

  if (enable_wacc(sens.w)){ # wacc:

    value <- (sens * sens.w) + (spec * (1 - sens.w))
    names(value) <- "wacc"

  } else { # bacc:

    value <- (sens + spec) / 2  # = (sens * .50) + (spec * .50)
    names(value) <- "bacc"

  }

  return(value)

} # get_bacc_wacc().

# # Check:
# get_bacc_wacc(1, .80, .500)
# get_bacc_wacc(1, .80, .501)
# get_bacc_wacc(1, .80, 0)



# get_best_tree: ------

#' Select the best tree (from current set of FFTs)
#'
#' \code{get_best_tree} selects (looks up and identifies) the best tree (as an integer)
#' from the set (or \dQuote{fan}) of FFTs contained in the current \code{FFTrees} object \code{x},
#' an existing type of \code{data} ('train' or 'test'), and
#' a \code{goal} for which corresponding statistics are available
#' in the designated \code{data} type (in \code{x$trees$stats}).
#'
#' Importantly, \code{get_best_tree} only identifies and selects the `tree` identifier
#' (as an integer) from the set of \emph{existing} trees with known statistics,
#' rather than creating new trees or computing new cue thresholds.
#' More specifically, \code{goal} is used for identifying and selecting the `tree`
#' identifier (as an integer) of the best FFT from an existing set of FFTs, but not for
#' computing new cue thresholds (see \code{goal.threshold} and \code{fftrees_cuerank()}) or
#' creating new trees (see \code{goal.chase} and \code{fftrees_ranktrees()}).
#'
#' @param x An \code{FFTrees} object.
#'
#' @param data The type of data to consider (as character: either 'train' or 'test').
#'
#' @param goal character. A goal to maximize or minimize when selecting a tree from an existing \code{x}
#' (for which values exist in \code{x$trees$stats}).
#'
#' @param my.goal.max logical. Default direction for user-defined \code{my.goal}: Should \code{my.goal} be maximized?
#' Default: \code{my.goal.max = TRUE}.
#'
#' @return An integer denoting the \code{tree} that maximizes/minimizes \code{goal} in \code{data}.
#'
#' @seealso
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.

get_best_tree <- function(x,
                          data,
                          goal,
                          my.goal.max = TRUE  # Default direction for my.goal: maximize (ToDo: currently not set anywhere)
){

  # Verify inputs: ------

  # x: ----

  testthat::expect_true(inherits(x, "FFTrees"),
                        info = "Argument x is no FFTrees object")

  # data: ----

  testthat::expect_true(data %in% c("train", "test"))

  if (is.null(x$trees$stats$test) & (data == "test")){
    message("You asked for 'test' data, but x only contains training statistics. I'll use data = 'train' instead...")
    data <- "train"
  }

  # goal: ----

  # # (a) narrow goal range:
  #
  # valid_tree_select_goal_narrow <- c("acc", "bacc", "wacc", "dprime", "cost")
  # testthat::expect_true(goal %in% valid_tree_select_goal_narrow)

  # (b) wide goal range:

  # Goals to maximize (more is better):
  max_goals <- c("hi", "cr",
                 "sens", "spec",
                 "ppv", "npv",
                 "acc", "bacc", "wacc",
                 "dprime",
                 "pci")

  # Goals to minimize (less is better):
  min_goals <- c("mi", "fa",
                 "cost", "cost_dec", "cost_cue",
                 "mcu")

  # Current goal is user-defined my.goal:
  if (!is.null(x$params$my.goal) & (goal == x$params$my.goal)){

    if (my.goal.max) { # add my.goal to max_goals:

      max_goals <- c(max_goals, x$params$my.goal)

      if (!x$params$quiet) {
        msg <- paste0("\u2014 Selecting an FFT to maximize your goal = '", x$params$my.goal, "'\n")
        cat(u_f_hig(msg))
      }

    } else { # add my.goal to min_goals:

      min_goals <- c(min_goals, x$params$my.goal)

      if (!x$params$quiet) {
        msg <- paste0("\u2014 Selecting an FFT to minimize your goal = '", x$params$my.goal, "'\n")
        cat(u_f_hig(msg))
      }

    }

  }

  valid_tree_select_goal <- c(max_goals, min_goals)
  testthat::expect_true(goal %in% valid_tree_select_goal)


  # Get tree stats (from x given data): ------

  cur_stats <- x$trees$stats[[data]]
  cur_names <- names(cur_stats)

  ix_goal <- which(cur_names == goal)
  cur_goal_vals <- as.vector(cur_stats[[ix_goal]])

  if (goal %in% max_goals){ # more is better:

    cur_ranks <- rank(-cur_goal_vals, ties.method = "first")  # low ranks indicate higher/better values

  } else { # goal %in% min_goals / less is better:

    cur_ranks <- rank(+cur_goal_vals, ties.method = "first")  # low rank indicate lower/better values
  }

  tree <- cur_stats$tree[cur_ranks == min(cur_ranks)]  # get tree with minimum rank


  # Output: -----

  # print(paste0("Select best tree = ", tree))  # 4debugging

  tree <- as.integer(tree)  # aim to convert to integer
  testthat::expect_true(is.integer(tree))  # verify integer

  return(tree) # as integer

} # get_best_tree().




# get_fft_definitions: ------

# Goal: Extract (and verify) ALL definitions from an FFTrees object (as 1 df).
# Output: Verified tree definitions of x$trees$definitions (as 1 df); else NA.

get_fft_definitions <- function(x){

  # verify input:
  testthat::expect_s3_class(x, class = "FFTrees")

  # main: get definitions from object:
  x_tree_df <- x$trees$definitions  # definitions (as df/tibble)

  # verify:
  if (verify_fft_definition(x_tree_df)){

    return(x_tree_df)

  } else {

    return(NA)

  }

} # get_fft_definitions().



# get_lhs_formula: ------

# Goal: Get criterion variable from formula (and verify formula).

get_lhs_formula <- function(formula){

  # Verify formula:
  testthat::expect_true(!is.null(formula), info = "formula is NULL")
  testthat::expect_type(formula, type = "language")

  # Main:
  lhs_name <- paste(formula)[2]

  # Output:
  return(lhs_name)

} # get_lhs_formula().




# (D) Strings or quotes: ------


# add_quotes: ------

add_quotes <- function(x) {

  toString(sQuote(x))

} # add_quotes().



# exit_word: ------

exit_word <- function(data){

  if (data == "test"){ "Predict" } else { "Decide" }

} # exit_word().





# (E) FFTrees package: ------


#' \code{FFTrees} package.
#'
#' Create and evaluate fast-and-frugal trees (FFTs).
#'
#' @docType package
#' @name FFTrees
#' @importFrom dplyr %>%

NULL


# R version check: ------

## quiets concerns of R CMD check re: the .'s that appear in pipelines:
if (getRversion() >= "2.15.1") utils::globalVariables(c(".", "tree", "tree_new", "tree", "level"))




# ToDo: ------

# - etc.

# eof.
