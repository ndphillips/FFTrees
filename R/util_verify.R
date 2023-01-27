# util_verify.R:
# Verification helpers/utility functions.
# ---------------------------------------

# Functions for validating or verifying stuff: ------


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

        if (!x$params$quiet) { # user feedback:
          msg <- paste0("Assuming you want the 'best.train' tree for 'train' data...\n")
          cat(u_f_hig(msg))
        }

        tree <- "best.train"

      } else if (data == "test") {

        if (!x$params$quiet) { # user feedback:
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



# verify_fft_definition: ------

# Goal: Verify a set of existing tree definitions (defs as df, from an FFTrees object).
# Inputs: ffts_df FFT definitions (1-line per FFT, as df, usually from x$trees$definitions or get_fft_definitions(x)).
# Output: Boolean.


verify_fft_definition <- function(ffts_df){

  # verify ffts_df:
  testthat::expect_true(is.data.frame(ffts_df), info = "Input 'ffts_df' are not a data.frame")

  # verify nrow(ffts_df) > 0:
  if (nrow(ffts_df) < 1){

    message("Input 'ffts_df' is empty: nrow(ffts_df) = ", nrow(ffts_df))

    return(FALSE)

  }

  # Main: verify variable names (of EXISTING tree definitions):
  provided_vars <- names(ffts_df)
  req_tdef_vars <- c("tree", "nodes",  "classes", "cues", "directions", "thresholds", "exits") # [mostly plural]

  if (all(req_tdef_vars %in% provided_vars)){

    # ToDo: Verify variables further (e.g., verify their contents).

    return(TRUE)

  } else {

    missing_vars <- setdiff(req_tdef_vars, provided_vars)

    message("Input 'ffts_df' is not a valid (set of) FFT definition(s).\nMissing variables: ", paste(missing_vars, collapse = ", "))

    return(FALSE)

  }

} # verify_fft_definition().



# verify_fft_as_df: ------

# Goal: Verify the components (as df) to-be-turned into a tree definition (for an FFTrees object).
# Inputs: fft_df: Definition of 1 FFT (as df) with tree elements as separate vectors (e.g., from get_fft_definitions(x)).
# Output: Boolean.

verify_fft_as_df <- function(fft_df){

  # verify fft_df:
  testthat::expect_true(is.data.frame(fft_df), info = "Input 'fft_df' is not a data.frame")

  # verify nrow(fft_df) > 0:
  if (nrow(fft_df) < 1){

    message("Input 'fft_df' is empty: nrow(fft_df) = ", nrow(fft_df))

    return(FALSE)

  }

  # Main: verify variable names (of a FUTURE tree definition):
  provided_vars <- names(fft_df)
  req_tree_vars <- c("class", "cue", "direction", "threshold", "exit")  # [all singular]
  # Note: c("tree", "nodes") are only part of EXISTING tree definitions (i.e., not needed here).

  if (all(req_tree_vars %in% provided_vars)){

    # ToDo: Verify variables further (e.g., verify their contents).

    return(TRUE)

  } else {

    missing_vars <- setdiff(req_tree_vars, provided_vars)

    message("Input 'fft_df' is not a valid FFT (as df, with 1 row per cue). Missing variables: ", paste(missing_vars, collapse = ", "))

    return(FALSE)

  }

} # verify_fft_as_df().


# ToDo: ------

# - etc.

# eof.
