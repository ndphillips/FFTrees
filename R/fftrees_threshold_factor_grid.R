#' Perform a grid search over factor and return accuracy statistics for a given factor cue
#'
#' @param thresholds numeric. A vector of factor thresholds to consider.
#' @param cue_v numeric. Feature/cue values.
#' @param criterion_v logical. A logical vector of (TRUE) criterion values.
#' @param directions character. Character vector of threshold directions to consider.
#'
#' @param goal.threshold A character string indicating the criterion to maximize when \emph{optimizing cue thresholds}:
#' \code{"acc"} = overall accuracy, \code{"bacc"} = balanced accuracy, \code{"wacc"} = weighted accuracy,
#' \code{"dprime"} = discriminability, \code{"cost"} = costs (based only on \code{cost.outcomes}, as \code{cost.cues} are constant per cue).
#' Default: \code{goal.threshold = "bacc"}.
#'
#' @param sens.w numeric. Sensitivity weight parameter (from \code{0} to \code{1}, for computing \code{wacc}).
#' Default: \code{sens.w = .50}.
#'
#' @param my.goal Name of an optional, user-defined goal (as character string). Default: \code{my.goal = NULL}.
#' @param my.goal.fun User-defined goal function (with 4 arguments \code{hi fa mi cr}). Default: \code{my.goal.fun = NULL}.
#'
#' @param cost.each numeric. A constant cost value to add to each value (e.g., the cost of the cue).
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying
#' the costs of a hit, false alarm, miss, and correct rejection, respectively, in some common currency.
#' For instance, \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that
#' a false alarm and miss cost \code{10} and \code{20} units, respectively, while correct decisions have no cost.
#'
#' @return A data frame containing accuracy statistics for factor thresholds.
#'
#' @import testthat
#' @importFrom magrittr "%>%"
#'
#' @seealso \code{\link{fftrees_threshold_numeric_grid}} for numeric cues.
#'
#' @export

fftrees_threshold_factor_grid <- function(thresholds = NULL,
                                          cue_v = NULL,
                                          criterion_v = NULL,
                                          directions = "=",  # categorical default (cue class in c("c", "f", "l"))
                                          #
                                          goal.threshold = NULL, # (was "bacc", but NULL enforces consistency w calling function)
                                          #
                                          sens.w = NULL, # (was ".50", but NULL enforces consistency w calling function)
                                          #
                                          my.goal = NULL,
                                          my.goal.fun = NULL,
                                          #
                                          cost.each = NULL, # (was "0", but NULL enforces consistency w calling function)
                                          cost.outcomes = NULL # (was "list(hi = 0, fa = 1, mi = 1, cr = 0)", but NULL enforces consistency w calling function)
) {

  # Prepare: ------

  # Assertions:

  if (!allow_NA_crit){

    testthat::expect_true(!any(is.na(criterion_v)))

  }

  # No NA values (any more):
  testthat::expect_true(!any(is.na(cue_v)), info = "Cannot compute thresholds for cues with NA values")
  # Note: Any NA values in categorical variables have been turned into an <NA> level (if allow_NA_pred).


  # Feedback:

  # if (debug){ # Provide debugging feedback:
  #
  #   # Report thresholds:
  #   cur_thresholds <- paste(thresholds, collapse = ", ")
  #   cli::cli_alert_info("\u2014 fftrees_threshold_factor_grid() with thresholds = {cur_thresholds}.")
  #
  # } # if (debug).


  # Main: ------

  if (!is.null(thresholds)) { # if thresholds exist:

    thresholds_n <- length(thresholds)
    case_n <- length(cue_v)

    results <- matrix(NA, nrow = thresholds_n, ncol = 5)


    # Loop 1 over all thresholds: ------
    # C++

    for (i in 1:thresholds_n) {

      threshold_i <- thresholds[i]

      # Create vector of decisions
      decisions_i <- cue_v == threshold_i

      # Calculate frequency of decision outcomes:
      hi_i <- sum(decisions_i == TRUE  & criterion_v == TRUE)
      fa_i <- sum(decisions_i == TRUE  & criterion_v == FALSE)
      mi_i <- sum(decisions_i == FALSE & criterion_v == TRUE)
      cr_i <- sum(decisions_i == FALSE & criterion_v == FALSE)

      n_i <- hi_i + fa_i + mi_i + cr_i

      # Return values to results:
      results[i, ] <- c(n_i, hi_i, fa_i, mi_i, cr_i)

    } # for (i in 1:thresholds_n).

    # Convert to named dataframe:
    results <- as.data.frame(results)
    names(results) <- c("n", "hi", "fa", "mi", "cr")

    # Add thresholds and directions:
    results$threshold <- thresholds

    # Add statistics to results: ----
    new_stats <- add_stats(data = results,
                           #
                           sens.w = sens.w,
                           #
                           my.goal     = my.goal,         # (just passing to helper)
                           my.goal.fun = my.goal.fun,
                           #
                           cost.outcomes = cost.outcomes,
                           cost.each = cost.each          # +++ here now +++  NOTE: Was not used here, but added on 2023-02-02.
    )

    # Add new statistics (to previous results): ----
    results <- cbind(results, new_stats)

    # Order by goal.threshold and change column order:
    row_order <- order(results[ , goal.threshold], decreasing = TRUE)

    results <- results[row_order, ]


    # Loop 2 over cumulative thresholds: ------
    # C++

    results_cum <- as.data.frame(matrix(NA, nrow = thresholds_n, ncol = 6))
    names(results_cum) <- c("threshold", "n", "hi", "fa", "mi", "cr")

    for (i in 1:thresholds_n) {

      threshold_i <- results$threshold[1:i]

      # Create vector of decisions:
      decisions_i <- cue_v %in% threshold_i

      # Calculate decisions:
      hi_i <- sum(decisions_i == TRUE  & criterion_v == TRUE)
      fa_i <- sum(decisions_i == TRUE  & criterion_v == FALSE)
      mi_i <- sum(decisions_i == FALSE & criterion_v == TRUE)
      cr_i <- sum(decisions_i == FALSE & criterion_v == FALSE)

      n_i <- hi_i + fa_i + mi_i + cr_i

      # Return values to results:
      results_cum[i, 2:6] <- c(n_i, hi_i, fa_i, mi_i, cr_i)
      results_cum[i, 1]   <- paste(threshold_i, collapse = ",")

    } # for (i in 1:thresholds_n).

    results_cum$direction <- "="

    # TO DO / ToDo:
    # - Add reverse directions

    # results_cum$threshold_complexity <- 1:thresholds_n
    #
    # # Add reverse directions
    #
    # results_cum$thresholds_rev <- unlist(lapply(X = results_cum$threshold, FUN = function(x) {
    #
    #   paste(setdiff(thresholds, unlist(strsplit(x, split = ","))), collapse = ",")
    #
    # }))

    results <- results_cum

    # Add statistics to results: ----
    new_stats <- add_stats(data = results,
                           #
                           sens.w = sens.w,
                           #
                           my.goal     = my.goal,         # (just passing to helper)
                           my.goal.fun = my.goal.fun,
                           #
                           cost.outcomes = cost.outcomes,
                           cost.each = cost.each
    )

    # Add new statistics (to previous results): ----
    results <- cbind(results, new_stats)


    # Clean up results: ----

    # Arrange rows by goal.threshold and change column order:
    row_order <- order(results[, goal.threshold], decreasing = TRUE)

    # Define the set of reported stats [rep_stats_v]: ----
    if (!is.null(my.goal)){ # include my.goal (name and value):

      rep_stats_v <- c("threshold", "direction",
                       "n",  "hi", "fa", "mi", "cr",
                       "sens", "spec",  "ppv", "npv",
                       "acc", "bacc", "wacc",
                       my.goal,  # (+)
                       "dprime",
                       "cost_dec", "cost")

    } else { # default set of reported stats:

      rep_stats_v <- c("threshold", "direction",
                       "n",  "hi", "fa", "mi", "cr",
                       "sens", "spec",  "ppv", "npv",
                       "acc", "bacc", "wacc",
                       "dprime",
                       "cost_dec", "cost")

    } # if my.goal().

    # Arrange rows and select columns:
    results <- results[row_order, rep_stats_v]

    # Re-set rownames:
    # rownames(results) <- 1:nrow(results)  # NOT needed and potentially confusing (when comparing results).

    # Remove invalid directions: ----
    results[results$direction %in% directions, ]


  } else { # no thresholds exist: ----

    if (!is.null(my.goal)){ # include my.goal (name and value):

      results <- data.frame("threshold" = NA, "direction" = NA,
                            "n" = NA,  "hi" = NA, "fa" = NA, "mi" = NA, "cr" = NA,
                            "sens" = NA, "spec" = NA,  "ppv"  = NA, "npv"  = NA,
                            "acc"  = NA, "bacc" = NA, "wacc" = NA,
                            my.goal = NA,  # (+)
                            "dprime" = NA,
                            "cost_dec" = NA, "cost" = NA)

    } else { # default set of reported stats:

      results <- data.frame("threshold" = NA, "direction" = NA,
                            "n" = NA,  "hi" = NA, "fa" = NA, "mi" = NA, "cr" = NA,
                            "sens" = NA, "spec" = NA,  "ppv"  = NA, "npv"  = NA,
                            "acc"  = NA, "bacc" = NA, "wacc" = NA,
                            # my.goal = NA,  # (+)
                            "dprime" = NA,
                            "cost_dec" = NA, "cost" = NA)

    } # if my.goal().

  } # if (!is.null(thresholds)).


  # Output: ----

  return(results)

} # fftrees_threshold_factor_grid().

# eof.
