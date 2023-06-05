# util_abc.R:
# Miscellaneous auxiliary/utility functions.
# ------------------------------------------

# General/miscellaneous helper functions
# (grouped into loose categories).


# (A) Applying or computing stuff: ------


# - apply_break: ------

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



# - fact_clean: ------

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


# - enable_wacc: ------

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


# - get_bacc_wacc: ------

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



# - get_best_tree: ------

#' Select the best tree (from current set of FFTs)
#'
#' \code{get_best_tree} selects (looks up and identifies) the best tree (as an integer)
#' from the set (or \dQuote{fan}) of FFTs contained in the current \code{FFTrees} object \code{x},
#' an existing type of \code{data} ('train' or 'test'), and
#' a \code{goal} for which corresponding statistics are available
#' in the designated \code{data} type (in \code{x$trees$stats}).
#'
#' Importantly, \code{get_best_tree} only identifies and selects the `tree` \emph{identifier}
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
#' @param goal A goal (as character) to be maximized or minimized when selecting a tree
#' from an existing \code{FFTrees} object \code{x} (with existing \code{x$trees$stats}).
#'
#' @param my.goal.max Default direction for user-defined \code{my.goal} (as logical):
#' Should \code{my.goal} be maximized?
#' Default: \code{my.goal.max = TRUE}.
#'
#' @return An integer denoting the \code{tree} that maximizes/minimizes \code{goal} in \code{data}.
#'
#' @family utility functions
#'
#' @seealso
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export

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
  if (!is.null(x$params$my.goal) && (goal == x$params$my.goal)){

    if (my.goal.max) { # add my.goal to max_goals:

      max_goals <- c(max_goals, x$params$my.goal)

      if (any(sapply(x$params$quiet, isFALSE))) {
        msg <- paste0("\u2014 Selecting an FFT to maximize goal = '", x$params$my.goal, "'\n")
        cat(u_f_hig(msg))
      }

    } else { # add my.goal to min_goals:

      min_goals <- c(min_goals, x$params$my.goal)

      if (any(sapply(x$params$quiet, isFALSE))) {
        msg <- paste0("\u2014 Selecting an FFT to minimize goal = '", x$params$my.goal, "'\n")
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



# - get_exit_type: ------

# Goal: Convert/get various exit type descriptions (from a vector x)
#       given current exit_types (given by global constant).
# Output: Verified FFT exit types (else Error from verify_exit_type()).


#' Get exit type (from a vector \code{x} of FFT exit descriptions)
#'
#' \code{get_exit_type} checks and converts a vector \code{x}
#' of FFT exit descriptions into exits of an FFT
#' that correspond to the current options of
#' \code{exit_types} (as a global constant).
#'
#' \code{get_exit_type} also verifies that the exit types conform to an FFT
#' (e.g., only the exits of the final node are bi-directional).
#'
#' @param x A vector of FFT exit descriptions.
#'
#' @param verify A flag to turn verification on/off (as logical).
#' Default: \code{verify = TRUE}.
#'
#' @examples
#' get_exit_type(c(0, 1, .5))
#' get_exit_type(c(FALSE,   " True ",  2/4))
#' get_exit_type(c("noise", "signal", "final"))
#' get_exit_type(c("left",  "right",  "both"))
#'
#' @return A vector of \code{exit_types} (or an error).
#'
#' @family utility functions
#'
#' @seealso
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export

get_exit_type <- function(x, verify = TRUE){

  # Prepare: ----

  extypes <- rep(NA, length(x))  # initialize

  x <- trimws(tolower(as.character(x)))  # 4robustness


  # Main: ----

  # Case 1:
  extyp_1 <- exit_types[1]  # from global constant

  extypes[x == extyp_1] <- extyp_1
  extypes[x == "0"]     <- extyp_1
  extypes[x == "false"] <- extyp_1
  extypes[x == "noise"] <- extyp_1
  extypes[x == "left"]  <- extyp_1

  # Case 2:
  extyp_2 <- exit_types[2]  # from global constant

  extypes[x == extyp_2]  <- extyp_2
  extypes[x == "1"]      <- extyp_2
  extypes[x == "true"]   <- extyp_2
  extypes[x == "signal"] <- extyp_2
  extypes[x == "right"]  <- extyp_2

  # Case 3:
  extyp_3 <- exit_types[3]  # from global constant

  extypes[x == extyp_3] <- extyp_3
  extypes[x == "0.5"]   <- extyp_3
  extypes[x == "both"]  <- extyp_3
  extypes[x == "final"] <- extyp_3


  if (verify){

    # Verify that extypes describe an FFT:
    verify_exit_type(extypes) # verify (without consequences)

  }


  # Output: ----

  return(extypes)

} # get_exit_type().

# # Check:
# get_exit_type(c(0, FALSE, " Left ", " NOISE ", "both"))
# get_exit_type(c(1, TRUE, " RigHT ", " SIGnal ", "final"))
# get_exit_type(c(TRUE, FALSE, " right ", "LEFT ", " signaL ", " Noise", 3/6))




# - get_exit_word: ------

# Goal: Get "Decide" for 'train' data vs. "Predict" for 'test' data.

get_exit_word <- function(data){

  if (data == "test"){ "Predict" } else { "Decide" }

} # get_exit_word().




# - get_fft_df: ------

# Goal: Extract (and verify) ALL definitions from an FFTrees object (as 1 df).
# Output: Verified tree definitions of x$trees$definitions (as 1 df); else NA.


#' Get FFT definitions (from an \code{FFTrees} object \code{x})
#'
#' \code{get_fft_df} gets the FFT definitions
#' of an \code{FFTrees} object \code{x}
#' (as a \code{data.frame}).
#'
#' The FFTs in the \code{data.frame} returned
#' are represented in the one-line per FFT definition format
#' used by an \code{FFTrees} object.
#'
#' In addition to looking up \code{x$trees$definitions},
#' \code{get_fft_df} verifies that the FFT definitions
#' are valid (given current settings).
#'
#' @param x An \code{FFTrees} object.
#'
#' @return A set of FFT definitions (as a \code{data.frame}/\code{tibble},
#' in the one-line per FFT definition format used by an \code{FFTrees} object).
#'
#' @family utility functions
#' @family tree definition and manipulation functions
#'
#' @seealso
#' \code{\link{read_fft_df}} for reading one FFT definition from tree definitions;
#' \code{\link{write_fft_df}} for writing one FFT to tree definitions;
#' \code{\link{add_fft_df}} for adding FFTs to tree definitions;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export

get_fft_df <- function(x){

  # verify input:
  testthat::expect_s3_class(x, class = "FFTrees")

  # main: get definitions from object:
  x_tree_df <- x$trees$definitions  # definitions (as df/tibble)

  # verify:
  if (verify_ffts_df(x_tree_df)){

    return(x_tree_df)

  } else {

    return(NA)

  }

} # get_fft_df().



# - get_lhs_formula: ------

# Goal: Get the (name of the) criterion variable from (LHS of) a formula (and verify formula).

get_lhs_formula <- function(formula){

  # Verify formula:
  testthat::expect_true(!is.null(formula), info = "formula is NULL")
  testthat::expect_type(formula, type = "language")

  # Main:
  lhs_name <- paste(formula)[2]

  # Output:
  return(lhs_name)

} # get_lhs_formula().





# (D) Handling strings or quotes: ------


# - add_quotes: ------

add_quotes <- function(x) {

  toString(sQuote(x))

} # add_quotes().




# (E) Combinatorics: Number of combinations and permutations: --------


# - all_permutations: List all permutations of a vector/set x / permute a set/vector x: ------

# See also: library(combinat)
# set <- c("a", "b", "c")
# pm  <- combinat::permn(x = set)

# Recursive definition:

all_permutations <- function(x) {

  # initialize: ----
  out <- NA
  n <- length(x)

  if (n == 1) { # basic case: ----

    out <- x

  } else { # Use recursion: ----

    out <- NULL  # init/stopping case

    for (i in 1:n) { # loop: ----

      out <- rbind(out, cbind(x[i], all_permutations(x[-i])))

    }
  }

  return(out)

} # all_permutations().

# # Check:
# all_permutations(246)
# all_permutations(1:3)
# all_permutations(c("A", "B", "b", "a"))


# - all_combinations: List all combinations of a length n of a set x: ------

# # (a) Using utils::combn:
# m <- utils::combn(x = 1:4, m = 2)
# m
# is.matrix(m)
# t(m)
# is.vector(m)  # if m == length(x)

all_combinations <- function(x, length){

  # Prepare: ----

  # Verify inputs:
  if (all(is.na(x)) || is.na(length)){
    return(NA)
  }

  if (length > length(x)){
    message(paste0("all_combinations: length must not exceed length(x). Using length = ", length(x)))
    length <- length(x)
  }

  out <- NA  # initialize


  # Main: ----

  # Use utils::combn to obtain a matrix:
  mx <- utils::combn(x = x, m = length)

  if (is.vector(mx)){

    out <- mx  # return as is

  } else if (is.matrix(mx)){

    out <- t(mx)  # transpose m into matrix of rows

  }


  # Output: ----

  return(out)

} # all_combinations().

# # Check:
# all_combinations(x = c("a", "b", "c"), 2)
# all_combinations(x = 1:5, length = 2)
# all_combinations(x = 1:25, 2)  # Note: 25 * 24 / 2 = 300 combinations.
# all_combinations(x = 1:3, length = 1)
# all_combinations(x = 1:3, length = 88)
# all_combinations(x = 1:3, length = NA)
# all_combinations(x = NA, length = 1)



# - all_subsets: List all combinations of all sub-lengths 0 < n < length(x) of a set x: ------

# Goal: Get all subsets of x (i.e., all possible combinations of all possible lengths 0 < n < length(x)).
#       Note: The extreme NULL (an empty set) is NOT, but the full set (all of x) can be included/returned.

all_subsets <- function(x, include_x = TRUE){

  # Prepare: ----

  if (length(x) < 2){
    return(x)
  }

  l_out <- vector("list", 0)  # initialize


  # Main: ----

  if (include_x){

    max_size <- length(x)  # a. include maximum subset with ALL length(x) elements

  } else {

    max_size <- (length(x) - 1)  # b. exclude maximum subset with ALL length(x) elements

  }

  # Loop through set sizes:
  for (i in 1:max_size){

    l_i <- utils::combn(x = x, m = i, simplify = FALSE)

    l_out <- c(l_out, l_i)

  } # for i.


  # Output: ----

  return(l_out) # as list

} # all_subsets().

# # Check:
# all_subsets(1:4)
# all_subsets(1:4, include_x = FALSE)  # excluding 1:4 set
# all_subsets(LETTERS[1:3])
# all_subsets(NA)
# all_subsets("X")



# (F) FFTrees package: ------


#' \code{FFTrees} package.
#'
#' Create and evaluate fast-and-frugal trees (FFTs).
#'
#' @docType package
#' @name FFTrees
#' @importFrom dplyr %>%

NULL


# - R version check: ------

## quiets concerns of R CMD check re: the .'s that appear in pipelines:
if (getRversion() >= "2.15.1") utils::globalVariables(c(".", "tree", "tree_new", "tree", "level"))



# ToDo: ------

# - Get all_subsets() based on all_combinations()?

# eof.
