# util_data.R:
# Data-related utility functions.
# ------------------------------------------

# Handle NA cases: ------

# Inputs:
# - data (as df)
# - criterion_name (in data)
#
# Output:
# - modified data (as df)
#
# Side-effect: Report on NA cases and corresponding conversions.


handle_NA <- function(data, criterion_name){

  # Prepare: ------

  # Verify inputs: ----

  testthat::expect_true(is.data.frame(data))
  testthat::expect_true(is.character(criterion_name))


  # Identify roles & NA data types: ----

  # NA values (in data):
  nr_NA <- colSums(is.na(data))
  ix_NA <- nr_NA > 0
  nm_NA <- names(ix_NA)

  # Roles (criterion vs. predictors):
  ix_crit <- names(data) == criterion_name
  ix_pred <- names(data) != criterion_name

  # Data types:
  all_types <- sapply(data, typeof)

  ix_chr <- sapply(data, is.character)
  ix_fct <- sapply(data, is.factor)
  ix_log <- sapply(data, is.logical)
  ix_num <- sapply(data, is.numeric)

  # Combinations (for predictors):
  ix_pred_chr <- ix_pred & ix_chr
  ix_pred_fct <- ix_pred & ix_fct
  ix_pred_log <- ix_pred & ix_log
  ix_pred_num <- ix_pred & ix_num

  # Combinations:

  # character predictors:
  ix_pred_chr_NA <- ix_pred_chr & ix_NA
  nr_pred_chr_NA <- nr_NA[ix_pred_chr_NA]
  nm_pred_chr_NA <- names(data)[ix_pred_chr_NA]

  # factor predictors:
  ix_pred_fct_NA <- ix_pred_fct & ix_NA
  nr_pred_fct_NA <- nr_NA[ix_pred_fct_NA]
  nm_pred_fct_NA <- names(data)[ix_pred_fct_NA]

  # logical predictors:
  ix_pred_log_NA <- ix_pred_log & ix_NA
  nr_pred_log_NA <- nr_NA[ix_pred_log_NA]
  nm_pred_log_NA <- names(data)[ix_pred_log_NA]

  # numeric predictors:
  ix_pred_num_NA <- ix_pred_num & ix_NA
  nr_pred_num_NA <- nr_NA[ix_pred_num_NA]
  nm_pred_num_NA <- names(data)[ix_pred_num_NA]

  # criterion:
  ix_crit_NA <- ix_crit & ix_NA
  nr_crit_NA <- nr_NA[ix_crit_NA]
  nm_crit_NA <- names(data)[ix_crit_NA]


  # Provide user feedback: Report NA values ----

  cli::cli_alert_info("Found NA values in data variables:")

  if (any(ix_pred_chr_NA)){ # character predictors:

    msg_chr <- paste0("\u2014 ", sum(ix_pred_chr_NA), " predictors (of type character): ",
                      paste0(nm_pred_chr_NA, collapse = ", "), " (",
                      paste0(nr_pred_chr_NA, collapse = ", "), ")")

    cli::cli_alert_info(msg_chr)

  }

  if (any(ix_pred_fct_NA)){ # factor predictors:

    msg_fct <- paste0("\u2014 ", sum(ix_pred_fct_NA), " predictors (of type factor):    ",
                      paste0(nm_pred_fct_NA, collapse = ", "), " (",
                      paste0(nr_pred_fct_NA, collapse = ", "), ")")

    cli::cli_alert_info(msg_fct)

  }

  if (any(ix_pred_log_NA)){ # logical predictors:

    msg_log <- paste0("\u2014 ", sum(ix_pred_log_NA), " predictors (of type logical):   ",
                      paste0(nm_pred_log_NA, collapse = ", "), " (",
                      paste0(nr_pred_log_NA, collapse = ", "), ")")

    cli::cli_alert_info(msg_log)

  }

  if (any(ix_pred_num_NA)){ # numeric predictors:

    msg_num <- paste0("\u2014 ", sum(ix_pred_num_NA), " predictors (of type numeric):   ",
                      paste0(nm_pred_num_NA, collapse = ", "), " (",
                      paste0(nr_pred_num_NA, collapse = ", "), ")")

    cli::cli_alert_info(msg_num)

  }

  if (any(ix_crit_NA)){ # criterion:

    crit_type <- all_types[ix_crit]

    msg_crit <- paste0("\u2014 ", sum(ix_crit_NA), " criterion (of type ", crit_type, "):    ",
                      paste0(nm_crit_NA, collapse = ", "), " (",
                      paste0(nr_crit_NA, collapse = ", "), ")")

    cli::cli_alert_info(msg_crit)

  }


  # Main: Handle NA values (by role and type) ------

  # # Convert factor NA values to a missing <NA> factor level:
  # data <- data %>%
  #  dplyr::mutate_if(is.factor, addNA) %>%
  #  dplyr::mutate_if(is.character, addNA)


  if (any(ix_pred_chr_NA)){ # NA values in character predictors: ----

    # Replace NA values:
    data[ix_pred_chr] <- data[ix_pred_chr] %>%
      dplyr::mutate_if(is.character, addNA)

    # Provide user feedback:
    cli::cli_alert_success("Converted {sum(nr_pred_chr_NA)} NA case{?s} in {sum(ix_pred_chr_NA)} character predictor{?s} to <NA>.")

  }

  if (any(ix_pred_fct_NA)){ # NA values in factor predictors: ----

    # Replace NA values:
    data[ix_pred_fct] <- data[ix_pred_fct] %>%
      dplyr::mutate_if(is.factor, addNA)

    # Provide user feedback:
    cli::cli_alert_success("Converted {sum(nr_pred_fct_NA)} NA case{?s} in {sum(ix_pred_fct_NA)} factor predictor{?s} to <NA>.")

  }


  # +++ here now +++


  if (any(ix_pred_log_NA)){ # NA values in logical predictors: ----

    # ToDo: What to do about NA values in logical variables?

    # Provide user feedback:
    cli::cli_alert_warning("There are NA cases in logical predictors.")

  }

  if (any(ix_pred_num_NA)){ # NA values in numeric predictors: ----

    # ToDo: What to do about NA values in numeric variables?

    # Provide user feedback:
    cli::cli_alert_warning("There are NA cases in numeric predictors.")

  }

  if (any(ix_crit_NA)){ # NA values in criterion: ----

    # ToDo: What to do about NA values in criterion?

    # Provide user feedback:
    cli::cli_alert_warning("There are NA values in the criterion variable.")

  }


  # print(data)  # 4debugging


  # Output: ------

  return(data)

} # handle_NA().



# ToDo: ------

# - Handle NAs in logical predictors.
# - Handle NAs in numeric predictors.
# - Handle NAs in criterion variable.

# eof.
