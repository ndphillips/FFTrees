# util_data.R:
# Data-related utility functions.
# ------------------------------------------


# clean_data: ------

# Goal: Pre-process data (using the same steps for both train AND test data).
#
# Inputs:
# - data (as df)
# - criterion_name (character, a name in data)
# - formula
#
# Auxiliary arguments:
# - mydata (the data type of 'data': either "train" or "test")
# - quiet (as list)
#
# Output:
# - modified data (as df)
#
# Side-effect: Report on NA cases and corresponding conversions.


clean_data <- function(data, criterion_name, formula,
                       # auxiliary args:
                       mydata, quiet){

  # Prepare: ------

  # Verify inputs: ----

  testthat::expect_true(is.data.frame(data))
  testthat::expect_true(is.character(criterion_name))
  testthat::expect_true(is.character(mydata))
  testthat::expect_true(is.list(quiet))


  # Main: ------

  # 1. Remove any cues not in formula:
  data <- model.frame(
    formula = formula,
    data = data,
    na.action = NULL
  )


  # 2. Handle NA cases in data:
  if ( (allow_NA_pred | allow_NA_crit) & any(is.na(data)) ){

    data <- handle_NA_data(data = data, criterion_name = criterion_name,
                           # auxiliary args:
                           mydata = mydata,  # indicate data type
                           quiet = quiet)

  }


  # 3. Convert any factor variable to character variable:
  data <- data %>%
    dplyr::mutate_if(is.factor, paste)


  # 4. Convert a character criterion to logical:
  if (inherits(data[[criterion_name]], "character")) {

    # Save original values (as decision.labels):
    decision.labels <- unique(data[[criterion_name]])

    # Remove any NA values (if present):
    decision.labels <- decision.labels[!is.na(decision.labels)]

    # Convert criterion to logical:
    data[[criterion_name]] <- data[[criterion_name]] == decision.labels[2]  # Note: NA values remain NA

    if (any(sapply(quiet, isFALSE))) { # Provide user feedback:

      msg_lgc <- paste0("Converted criterion to logical (by '", criterion_name, " == ", decision.labels[2], "') in '", mydata, "' data.")
      # cat(u_f_hig("\u2014 ", msg_lgc), "\n")

      cli::cli_alert_warning(msg_lgc)

    }

  }


  # 5. Convert data to tibble:
  data <- data %>%
    tibble::as_tibble()


  # print(data)  # 4debugging


  # Output: ------

  return(data)

} # clean_data().





# handle_NA_data: ------

# Goal: Identify, handle, and report NA cases in criterion and data (train AND test).
#
# Inputs:
# - data (as df)
# - criterion_name (character, a name in data)
#
# Auxiliary arguments:
# - mydata (the data type of 'data': either "train" or "test")
# - quiet (as list)
#
# Output:
# - modified data (as df)
#
# Side-effect: Report on NA cases and corresponding conversions.


handle_NA_data <- function(data, criterion_name, mydata, quiet){

  # Prepare: ------

  # Verify inputs: ----

  testthat::expect_true(is.data.frame(data))
  testthat::expect_true(is.character(criterion_name))
  testthat::expect_true(is.character(mydata))
  testthat::expect_true(is.list(quiet))


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


  # Provide user feedback: Report NA values (by role and type) ----

  if (!quiet$mis) {

    cli::cli_alert_info("Found NA values in '{mydata}' data:")

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

  } # if (!quiet$mis).


  # Main: ------

  # Handle NA values (by role and type) ------

  # # Convert factor NA values to a missing <NA> factor level:
  # data <- data %>%
  #  dplyr::mutate_if(is.factor, addNA) %>%
  #  dplyr::mutate_if(is.character, addNA)

  if (any(ix_pred_chr_NA)){ # NA values in character predictors: ----

    # Replace NA values:
    data[ix_pred_chr] <- data[ix_pred_chr] %>%
      dplyr::mutate_if(is.character, addNA)  # add NA as a new factor level

    if (!quiet$mis) { # Provide user feedback:
      cli::cli_alert_success("Converted {sum(nr_pred_chr_NA)} NA case{?s} in {sum(ix_pred_chr_NA)} character predictor{?s} to <NA>.")
    }

  }

  if (any(ix_pred_fct_NA)){ # NA values in factor predictors: ----

    # Replace NA values:
    data[ix_pred_fct] <- data[ix_pred_fct] %>%
      dplyr::mutate_if(is.factor, addNA)  # add NA as a new factor level

    if (!quiet$mis) { # Provide user feedback:
      cli::cli_alert_success("Converted {sum(nr_pred_fct_NA)} NA case{?s} in {sum(ix_pred_fct_NA)} factor predictor{?s} to <NA>.")
    }

  }

  if (any(ix_pred_log_NA)){ # NA values in logical predictors: ----

    # Replace NA values:
    data[ix_pred_log] <- data[ix_pred_log] %>%
      dplyr::mutate_if(is.logical, addNA)  # add NA as a new factor level

    if (!quiet$mis) { # Provide user feedback:
      cli::cli_alert_success("Converted {sum(nr_pred_log_NA)} NA case{?s} in {sum(ix_pred_log_NA)} logical predictor{?s} to <NA>.")
    }

  }


  # +++ here now +++


  if (any(ix_pred_num_NA)){ # NA values in numeric predictors: ----

    # Keep NA values in numeric predictors (but remove in classtable() of 'util_stats.R').

    if (!quiet$mis) { # Provide user feedback:
      cli::cli_alert_warning("Keeping the {sum(nr_pred_num_NA)} NA case{?s} in {sum(ix_pred_num_NA)} numeric predictor{?s}.")
    }

  }

  if (any(ix_crit_NA)){ # NA values in criterion: ----

    # ToDo: What to do about NA values in criterion?

    if (!quiet$mis) { # Provide user feedback:
      cli::cli_alert_warning("Keeping the {sum(nr_crit_NA)} NA case{?s} in the criterion {nm_crit_NA}.")
    }

  }


  # print(data)  # 4debugging


  # Output: ------

  return(data)

} # handle_NA_data().



# ToDo: ------

# - Handle consequences of allowing NAs in numeric predictors.
# - Handle NAs in criterion variable.

# eof.
