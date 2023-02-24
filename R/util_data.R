# util_data.R:
# Data-related utility functions.
# ------------------------------------------

# Handle NA cases: ------

handle_NA <- function(data, criterion_name){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(is.data.frame(data))
  testthat::expect_true(is.character(criterion_name))


  # Main: ----

  # # Convert factor NA values to a missing <NA> factor level:
  # data <- data %>%
  #  dplyr::mutate_if(is.factor, addNA) %>%
  #  dplyr::mutate_if(is.character, addNA)

  ix_crit <- names(data) == criterion_name
  ix_pred <- names(data) != criterion_name

  ix_chr <- sapply(data, is.character)
  ix_fct <- sapply(data, is.factor)
  ix_num <- sapply(data, is.numeric)

  ix_pred_chr <- ix_pred & ix_chr
  ix_pred_fct <- ix_pred & ix_fct
  ix_pred_num <- ix_pred & ix_num


  if (any(is.na(data[ix_pred_chr]))){

    # Replace NA values:
    data[ix_pred_chr] <- data[ix_pred_chr] %>%
      dplyr::mutate_if(is.character, addNA)

    # Provide user feedback:
    cli::cli_alert_info("Converted NA cases in character predictors to <NA>.")

  }

  if (any(is.na(data[ix_pred_fct]))){

    # Replace NA values:
    data[ix_pred_fct] <- data[ix_pred_fct] %>%
      dplyr::mutate_if(is.factor, addNA)

    # Provide user feedback:
    cli::cli_alert_info("Converted NA cases in factor predictors to an <NA> level.")

  }

  if (any(is.na(data[ix_pred_num]))){

    # ToDo: What to do about NA values in numeric variables?

    # Provide user feedback:
    cli::cli_alert_warning("There are NA cases in numeric predictors.")

  }

  if (any(is.na(data[ix_crit]))){

    # ToDo: What to do about NA values in criterion?

    # Provide user feedback:
    cli::cli_alert_warning("There are NA values in the criterion variable.")

  }


  # print(data)  # 4debugging


  # Output: ----

  return(data)

} # handle_NA().



# ToDo: ------

# - etc.

# eof.
