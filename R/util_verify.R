# util_verify.R:
# Verification helpers/utility functions.
# ---------------------------------------

# Functions for validating or verifying stuff: ------


# verify_data_and_criterion: ------

# Inputs:
# - data (as df)
# - criterion_name (character, a name in data)
#
# Auxiliary arguments:
# - mydata (the data type of 'data': either "train" or "test")
#
# Output:
# - modified data (as df)
#
# Side-effect: Report failed tests.


verify_data_and_criterion <- function(data, criterion_name, mydata){

  # Verify data types:
  testthat::expect_true(is.data.frame(data),
                        info = paste0("The ", mydata, " data is not a data.frame"))
  testthat::expect_true(is.character(criterion_name),
                        info = paste0("The criterion_name is not of type character"))

  # Verify that criterion occurs in data:
  testthat::expect_true(criterion_name %in% names(data),
                        info = paste0("The criterion name '", criterion_name, "' does not occur in ", mydata, " data"))

  # Verify the number of criterion values:
  if (!allow_NA_crit){ # default:

    # Verify that criterion does NOT contain NA values:
    testthat::expect_true(all(!is.na(data[[criterion_name]])),
                          info = "At least one of the criterion values are missing. Please remove NA values and try again")

    # Verify that criterion is binary:
    testthat::expect_equal(length(unique(data[[criterion_name]])),
                           expected = 2,
                           info = "The criterion variable is non-binary")

  } else { # allow_NA_crit: the criterion must maximally contain 3 distinct values:

    testthat::expect_lt(length(unique(data[[criterion_name]])),
                        expected = 4)#,
    # info = "The criterion variable must only contain binary values, plus optional NA values")

  }

  # NO output.

} # verify_data_and_criterion().



# verify_dir_sym: ------

# Goal: Verify a vector x of direction symbols
#       (given global constant directions_df)

verify_dir_sym <- function(x){

  valid <- FALSE

  # Get first 6 direction symbols:
  dir_sym <- directions_df$direction[1:6]  # from global constant.

  if (all(x %in% dir_sym)){ # verify: valid direction symbol

    valid <- TRUE  # set value

  } else {

    missing_sym <- setdiff(x, dir_sym)
    msg <- paste0("verify_dir_sym: Some symbols (", paste0(missing_sym, collapse = ", "),
                  ") are NOT in (", paste0(dir_sym, collapse = ", "), ")")
    message(msg)

  }


  # Output: ----

  return(valid)

} # verify_dir_sym().

# # Check:
# verify_dir_sym(c("=", "!=", ">", ">=", "<", "<=", "=")) # is TRUE
# verify_dir_sym(c("==")) # is FALSE
# verify_dir_sym(c("=<", "+", "=>")) # are FALSE



# verify_exit_type: ------

# Goal: Ensure that a vector x contains valid exit types
#       given the options of current exit_types (as global constant).

verify_exit_type <- function(x){

  valid <- FALSE

  if ( all(x %in% exit_types) ) { # c1. only valid exit_types:

    if ( x[length(x)] == exit_types[3] ){ # c2. final exit:

      if ( all(x[-length(x)] %in% exit_types[1:2]) ){ # c3. non-final exits:

        valid <- TRUE

      } else {

        stop("All non-final exit types must be in: ", paste(exit_types[1:2], collapse = ", "))

      } # c3.

    } else {

      cur_fin_exit <- x[length(x)]

      stop("The final node's exit type must be ", exit_types[3], ", but is ", cur_fin_exit)

    } # c2.

  } else {

    invalid_exits <- setdiff(x, exit_types)

    stop("Some exit types are invalid: ", paste(invalid_exits, collapse = ", "))

  } # c1.

  # Output: ----

  return(valid)

} # verify_exit_type().

# # Check:
# verify_exit_type(c(1, 0, 1, 0.5))
# verify_exit_type(c(0.5))
#
# # Fails:
# verify_exit_type(c(1, 0, 2, 0))
# verify_exit_type(c(1, 0, 1, 0))
# verify_exit_type(c(1, 0, 0.5, 0.5))



# verify_train_test_data: ------

# Goal: Ensure that train and test data are sufficiently similar (e.g., contain the same variables)
#       and provide feedback on any existing differences.
#
# Currently, it is only verified that both DFs have some cases and
# contain the SAME names (but the content or order of variables is not checked or altered).
# Future versions may want to verify that 'test' data is valid, given current FFTs:
# data fits to FFTs in FFTrees object (i.e., object$trees$definitions) or tree.definitions
# (e.g., data contains all required variables to create the current FFTs).
#
# Output: Boolean.

verify_train_test_data <- function(train_data, test_data){

  # Initialize: ----

  valid <- FALSE

  train_names <- names(train_data)
  test_names  <- names(test_data)

  train_names_not_in_test <- setdiff(train_names, test_names)
  test_names_not_in_train <- setdiff(test_names,  train_names)


  # Conditions: ----

  if (nrow(train_data) < 1){

    msg <- paste("The 'train' data contains no cases (rows).")
    warning(msg)

  } else if (nrow(test_data) < 1){

    msg <- paste("The 'test' data contains no cases (rows).")
    warning(msg)

  } else if (length(train_names_not_in_test) > 0){

    msg <- paste("Some variables occur in 'train' data, but not in 'test' data:",
                 paste(train_names_not_in_test, collapse = ", "))
    warning(msg)

  } else if (length(test_names_not_in_train) > 0){

    msg <- paste("Some variables occur in 'test' data, but not in 'train' data:",
                 paste(test_names_not_in_train, collapse = ", "))
    warning(msg)

    same_names <- FALSE

  } else { # all tests passed:

    valid <- TRUE

  }


  # Output: ----

  return(valid)

} # verify_train_test_data().

# # Check:
# (df1 <- data.frame(matrix( 1:9,  nrow = 3)))
# (df2 <- data.frame(matrix(11:22, ncol = 3)))
# (df3 <- data.frame(matrix(31:45, nrow = 3)))
# (df0 <- df1[-(1:3), ])
#
# # FALSE cases:
# verify_train_test_data(df0, df1)
# verify_train_test_data(df1, df0)
# verify_train_test_data(df1, df3)
# verify_train_test_data(df3, df1)
# # TRUE cases:
# verify_train_test_data(df1, df2)
# verify_train_test_data(df1, df2[ , 3:1])




# verify_all_cues_in_data: ------

# Goal: Are all current cues (as vector) in current data (as df)?
# Output: Boolean.

verify_all_cues_in_data <- function(cues, data){

  # Verify inputs: ----

  testthat::expect_true(length(cues) > 0)
  testthat::expect_true(is.character(cues), info = "Provided 'cues' are not names (of type 'character')")
  testthat::expect_true(is.data.frame(data), info = "Provided 'data' are not a data.frame")

  # Initialize: ----

  valid <- FALSE
  data_names <- names(data)


  # Main: ----

  cues_not_in_data <- setdiff(cues, data_names)

  if (length(cues_not_in_data) > 0) {

    msg <- paste("Some cues do not appear in 'data':",
                 paste(cues_not_in_data, collapse = ", "))
    warning(msg)

  } else { # all tests passed:

    valid <- TRUE

  }

  # Output: ----

  return(valid)

} # verify_all_cues_in_data().

# Check:
# verify_all_cues_in_data(NA, titanic)
# verify_all_cues_in_data(c("age", "chol", "ca", "fbs", "slope"), heart.train)
# verify_all_cues_in_data(c("fbs", "AGE", " chol ", "XY", "fbs"), heart.train)




# verify_tree_arg: ------

# Goals: Verify AND return a "tree" ARGUMENT from an FFtrees object x, given a data type
#        [to be used in print(x) and plot(x)].
# Output: Returns either a tree number (as numeric) or "best.train"/"best.test" (as character).

verify_tree_arg <- function(x, data, tree){

  # Verify inputs: ----

  if (!inherits(x, "FFTrees")) {
    stop("You did not include a valid 'FFTrees' object or specify the tree directly with 'my.tree' or 'tree.definitions'.\nCreate a valid FFTrees object with FFTrees() or by manually specifying an FFT.")
  }

  # testthat::expect_true(data %in% c("train", "test"))
  if (!data %in% c("test", "train")){
    stop("The data must be 'test' or 'train'.")
  }

  if (inherits(tree, "character")) {
    tree <- tolower(tree)  # 4robustness
  }

  if (tree == "best.test" & is.null(x$tree$stats$test)) {

    warning("You asked for the 'best.test' tree, but there are no 'test' data. Used the best tree for 'train' data instead...")

    tree <- "best.train"
  }

  if (is.numeric(tree) & (tree %in% 1:x$trees$n) == FALSE) {

    stop(paste0("You asked for a tree that does not exist. This object has ", x$trees$n, " trees."))
  }

  if (inherits(data, "character")) {

    if (data == "test" & is.null(x$trees$stats$test)) {

      stop("You asked for 'test' data, but there are no 'test' data. Consider using data = 'train' instead...")
    }

    if (tree == "best"){ # handle abbreviation:

      if (data == "train") {

        if (any(sapply(x$params$quiet, isFALSE))) { # user feedback:
          msg <- paste0("Assuming you want the 'best.train' tree for 'train' data...\n")
          cat(u_f_hig(msg))
        }

        tree <- "best.train"

      } else if (data == "test") {

        if (any(sapply(x$params$quiet, isFALSE))) { # user feedback:
          msg <- paste0("Assuming you want the 'best.test' tree for 'test' data...\n")
          cat(u_f_hig(msg))
        }

        tree <- "best.test"

      } else {

        stop("You asked for a 'best' tree, but have not specified 'train' or 'test' data...")
      }
    }

  } # if (inherits(data, "character")).


  # Output: ----

  return(tree) # (as character OR numeric)

} # verify_tree_arg().



# verify_ffts_df: ------

# Goal: Verify a set of existing tree definitions (defs as df, from an FFTrees object).
# Inputs: ffts_df FFT definitions (1-line per FFT, as df, usually from x$trees$definitions or get_fft_df(x)).
# Output: Boolean.

verify_ffts_df <- function(ffts_df){

  # verify ffts_df:
  testthat::expect_true(is.data.frame(ffts_df), info = "Input 'ffts_df' are not a data.frame")

  # verify nrow(ffts_df) > 0:
  if (nrow(ffts_df) < 1){

    message("Input to verify_ffts_df is empty: nrow(ffts_df) = ", nrow(ffts_df))

    return(FALSE)

  }

  # Main: verify variable names (of EXISTING tree definitions):
  provided_vars <- names(ffts_df)
  req_tdef_vars <- c("tree", "nodes",  "classes", "cues", "directions", "thresholds", "exits") # [mostly plural]

  if (all(req_tdef_vars %in% provided_vars)){

    # Verify variables further (e.g., verify their contents):

    # ToDo: verify classes (requires data)
    # ToDo: verify cues (requires data)

    # Verify directions:

    # (a) as list elements:
    lapply(ffts_df$directions, FUN = function(x){

      directions <- trimws(unlist(strsplit(x, split = fft_node_sep, fixed = TRUE)))
      # print(directions)  # 4debugging
      testthat::expect_true(verify_dir_sym(directions))

    })

    # (b) as 1 long vector:
    # directions <- trimws(unlist(strsplit(ffts_df$directions, split = fft_node_sep, fixed = TRUE)))
    # print(directions)  # 4debugging
    # testthat::expect_true(verify_dir_sym(directions))

    # ToDo: verify thresholds

    # Verify exits:

    # (a) as list elements:
    lapply(ffts_df$exits, FUN = function(x){

      exits <- trimws(unlist(strsplit(x, split = fft_node_sep, fixed = TRUE)))
      # print(exits)  # 4debugging
      testthat::expect_true(verify_exit_type(exits))

    })


    # Output 1:

    return(TRUE)


  } else {

    missing_vars <- setdiff(req_tdef_vars, provided_vars)

    message("Input to verify_ffts_df is not a valid (set of) FFT definition(s).\nMissing variables: ", paste(missing_vars, collapse = ", "))


    # Output 2:

    return(FALSE)

  }

} # verify_ffts_df().



# verify_fft_as_df: ------

# Goal: Verify the components (as df) to-be-turned into a tree definition (for an FFTrees object).
# Inputs: fft_df: Definition of 1 FFT (as tidy df, 1 row per node)
#         with tree elements (class, cue, direction, threshold, exit) as separate vectors
#         (e.g., from get_fft_df(x)).
# Output: Boolean/logical value.

verify_fft_as_df <- function(fft_df){

  # verify fft_df:
  testthat::expect_true(is.data.frame(fft_df), info = "Input 'fft_df' is not a data.frame")

  # verify nrow(fft_df) > 0:
  if (nrow(fft_df) < 1){

    message("Input to verify_fft_as_df is empty: nrow(fft_df) = ", nrow(fft_df))

    return(FALSE)

  }

  # Main: verify variable names (of a FUTURE tree definition):
  provided_vars <- names(fft_df)
  req_tree_vars <- c("class", "cue", "direction", "threshold", "exit")  # [all singular]
  # Note: c("tree", "nodes") are only part of EXISTING tree definitions (i.e., not needed here).

  if (all(req_tree_vars %in% provided_vars)){

    # Verify variables further (e.g., verify their contents):
    # ToDo: verify class (requires data)
    # ToDo: verify cue (requires data)
    testthat::expect_true(verify_dir_sym(fft_df$direction))
    # ToDo: verify threshold
    testthat::expect_true(verify_exit_type(fft_df$exit))

    return(TRUE)

  } else {

    missing_vars <- setdiff(req_tree_vars, provided_vars)

    message("Input to verify_fft_as_df is not a valid FFT (as tidy df, 1 row per node). Missing variables: ", paste(missing_vars, collapse = ", "))

    return(FALSE)

  }

} # verify_fft_as_df().


# ToDo: ------

# - etc.

# eof.
