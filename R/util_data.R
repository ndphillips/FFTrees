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

  } else {

    # Mention NA cases (if present):
    if (any(is.na(data))){

      sum_NA_all <- sum(is.na(data))

      cli::cli_alert_danger("Found {sum_NA_all} NA value{?s} in '{mydata}' data.")

    }

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



# describe_data: ------

#  Goal: Describe key features of a dataset
#        (e.g. to show gist of data in vignettes).

#' Describe data
#'
#' Calculate key descriptive statistics for a given set of data.
#'
#' @param data A data frame with a criterion variable \code{criterion_name}.
#' @param data_name A character string specifying a name for the data.
#' @param criterion_name A character string specifying the criterion name.
#' @param baseline_value The value in \code{criterion_name} denoting the baseline
#' (e.g., \code{TRUE} or \code{FALSE}).
#'
#' @return A data frame with the descriptive statistics.
#'
#' @examples
#' data(heartdisease)
#' describe_data(heartdisease, "heartdisease",
#'               criterion_name = "diagnosis",
#'               baseline_value = TRUE)
#'
#' @export

describe_data <- function(data, data_name, criterion_name, baseline_value) {

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(is.data.frame(data))
  testthat::expect_true(is.character(criterion_name))
  testthat::expect_true(criterion_name %in% names(data))
  testthat::expect_true(baseline_value %in% data[[criterion_name]])


  # Main: ----

  # Number of cases/rows:
  n_cases <- nrow(data)

  # Baseline (proportion of values with baseline_value):
  baseline_percent <- (mean(data[[criterion_name]] == baseline_value, na.rm = TRUE) * 100)

  # Number of predictors:
  n_predictors <- ncol(data) - 1

  # Number of NAs:
  n_NA <- sum(sapply(data, function(col) sum(is.na(col))))

  # Percentage of NAs:
  percent_NA <- (n_NA / (n_cases * ncol(data))) * 100

  # As df:
  result_df <- tibble(
    "Name" = data_name,
    "Cases_n" = n_cases,
    "Criterion" = criterion_name,
    "Baseline_pct" = baseline_percent,
    "Predictors_n" = n_predictors,
    "NAs_n" = n_NA,
    "NAs_pct" = percent_NA
  )


  # Output: ----

  return(result_df)

} # describe_data().



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
#
# @importFrom stats na.omit


handle_NA_data <- function(data, criterion_name, mydata, quiet){

  # Prepare: ------

  # Verify inputs: ----

  testthat::expect_true(is.data.frame(data))
  testthat::expect_true(is.character(criterion_name))
  testthat::expect_true(is.character(mydata))
  testthat::expect_true(is.list(quiet))


  # Identify columns/variables with NA values (by roles & data types): ----

  # NA values (in data):
  nr_NA <- colSums(is.na(data))  # Note: per column in data
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

    cli::cli_alert_info("Found {sum(nr_NA)} NA value{?s} in '{mydata}' data:")

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

  if (any(ix_pred_chr_NA)){ # 1. NA values in character predictors: ----

    # Replace NA values:
    data[ix_pred_chr] <- data[ix_pred_chr] %>%
      dplyr::mutate_if(is.character, addNA)  # add NA as a new factor level

    if (!quiet$mis) { # Provide user feedback:
      cli::cli_alert_success("Converted {sum(nr_pred_chr_NA)} NA value{?s} in {sum(ix_pred_chr_NA)} character predictor{?s} to <NA>.")
    }

  } # 1. character NA.


  if (any(ix_pred_fct_NA)){ # 2. NA values in factor predictors: ----

    # Replace NA values:
    data[ix_pred_fct] <- data[ix_pred_fct] %>%
      dplyr::mutate_if(is.factor, addNA)  # add NA as a new factor level

    if (!quiet$mis) { # Provide user feedback:
      cli::cli_alert_success("Converted {sum(nr_pred_fct_NA)} NA value{?s} in {sum(ix_pred_fct_NA)} factor predictor{?s} to <NA>.")
    }

  } # 2. factor NA.


  if (any(ix_pred_log_NA)){ # 3. NA values in logical predictors: ----

    # Replace NA values:
    data[ix_pred_log] <- data[ix_pred_log] %>%
      dplyr::mutate_if(is.logical, addNA)  # add NA as a new factor level

    if (!quiet$mis) { # Provide user feedback:
      cli::cli_alert_success("Converted {sum(nr_pred_log_NA)} NA value{?s} in {sum(ix_pred_log_NA)} logical predictor{?s} to <NA>.")
    }

  } # 3. logical NA.



  if (any(ix_pred_num_NA)){ # 4. NA values in numeric predictors: ----

    # Keep NA values in numeric predictors (but remove in classtable() of 'util_stats.R').
    # OR: Allow to replace NA-values in numeric predictors by mean/median?

    if (replace_NA_num_pred){ # use global constant:

      # Replace NAs in numeric predictors:
      data[ix_pred_num_NA] <- replace_NA_num(df = data[ix_pred_num_NA])

      if (!quiet$mis) { # Provide user feedback:
        cli::cli_alert_warning("Replaced {sum(nr_pred_num_NA)} NA value{?s} in {sum(ix_pred_num_NA)} numeric predictor{?s} of '{mydata}' data.")
      }

    } else {

      # Do nothing / keep NA values.

      if (!quiet$mis) { # Provide user feedback:
        cli::cli_alert_warning("Keeping {sum(nr_pred_num_NA)} NA value{?s} in {sum(ix_pred_num_NA)} numeric predictor{?s}.")
      }

    }

  } # 4. numeric NA.



  if (any(ix_crit_NA)){ # 5. NA values in criterion variable: ----

    # ToDo: What to do about NA values in criterion?

    crit_v <- data[ , ix_crit]
    print(crit_v)  # 4debugging

    if (allow_NA_crit){ # Handle NA values in criterion:

      # Remove incomplete cases:

      # # (a) radical:
      # data <- stats::na.omit(data)  # filter cases containing NA values in ANY variable

      # (b) nuanced:
      ix_crit_v_NA <- is.na(crit_v)
      data <- data[!ix_crit_v_NA, ]  # filter cases in which crit_v contains NA values

      if (!quiet$mis) { # Provide user feedback:
        cli::cli_alert_warning("Found {nr_crit_NA} NA value{?s} in the criterion '{nm_crit_NA}' of '{mydata}' data. Removing corresponding cases left {nrow(data)} case{?s}.")
      }

    } else { # do nothing, but report status:

      if (!quiet$mis) { # Provide user feedback:
        cli::cli_alert_warning("Keeping {nr_crit_NA} NA value{?s} in the criterion '{nm_crit_NA}' of '{mydata}' data, retaining {nrow(data)} case{?s}.")
      }

    }

  } # 5. criterion NA.


  # print(data)  # 4debugging


  # Output: ------

  return(data)

} # handle_NA_data().




# replace_NA_vec: ------

# Goal: Replace NA-values in a vector by mean() of existing values.
#       df$x_1[is.na(df$x_1)] <- mean(df$x_1, na.rm = TRUE)

replace_NA_vec <- function(v){

  # by data type:
  if (is.numeric(v)){

    v[is.na(v)] <- mean(v, na.rm = TRUE)  # replace NA values by mean

    # v[is.na(v)] <- median(v, na.rm = TRUE)  # replace NA values by median

  } else {

    stop("Cannot handle data type of v")

  }

  return(v)

} # replace_NA_vec().

# # Check:
# v <- c(4, 2, NA, 9, 4)
# replace_NA_vec(v)



# replace_NA_num: ------

# Goal: Replace NA-values in all numeric variables (in df) by mean().

replace_NA_num <- function(df){

  # Apply replace_NA_vec() ONLY to numeric columns of df:

  ix_num <- sapply(X = df, FUN = is.numeric)  # ix of numeric columns

  df[ix_num] <- apply(X = df[ix_num], MARGIN = 2, FUN = replace_NA_vec)  # replace

  # Output:
  return(df)

} # replace_NA_num().

# # Check:
# df <- data.frame(a_0 = letters[1:5],
#                  x_1 = c(4, 2, NA, 9, 4),
#                  x_2 = c(-2, -1, NA, 2, 1),
#                  x_3 = c(1, NA, 3, 4, 5))
#
# replace_NA_num(df)
# class(df$x_3)

# Note: See some tidyverse solutions at
# <https://www.codingprof.com/how-to-replace-nas-with-the-mean-in-r-examples/>



# cue_class_of_matrix: ------

# # Handle special case:
# # Numeric cues have been turned into class c("matrix", "array").
# # Goal: If data type is "double" or "integer", then set class to "numeric".
#
# cue_class_of_matrix <- function(cue, cue_class){
#
#   if ("matrix" %in% cue_class){
#
#     cue_type <- typeof(cue)
#
#     if (cue_type %in% c("double", "integer")){
#       return("numeric")
#     } else {
#       return(cue_type)
#     }
#
#   } else {
#
#     return(cue_class)  # unchanged
#
#   }
#
# } # cue_class_of_matrix().

# Note: Obsolete/Fixed by adding as.vector() when determining cue class.


# ToDo: ------

# - Create describe_data() analogous to handle_NA_data():
#   dims / criterion / predictors (by type) / NA values (by type)
# - Handle the consequences of allowing NAs in numeric predictors.
# - Handle NAs in the criterion variable.


# eof.
