#' Perform a grid search over thresholds and return accuracy statistics for a given numeric cue
#'
#' @param thresholds numeric. A vector of thresholds to consider.
#' @param cue_v numeric. Feature values.
#' @param criterion_v logical. A logical vector of (TRUE) criterion values.
#' @param directions character. Possible directions to consider.
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
#' @return A data frame containing accuracy statistics for numeric thresholds.
#'
#' @seealso \code{\link{fftrees_threshold_factor_grid}} for factor cues.
#'
#' @export

fftrees_threshold_numeric_grid <- function(thresholds,
                                           cue_v,
                                           criterion_v,
                                           directions = c(">", "<="),  # numeric defaults (cue class in c("n", "i"))
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

  thresholds_n <- length(thresholds)

  results_gt <- matrix(NA, nrow = thresholds_n, ncol = 5)

  # Loop over all thresholds: ------
  # C++

  for (i in 1:thresholds_n) {

    threshold_i <- thresholds[i]

    # Create vector of decisions:
    decisions_i <- cue_v > threshold_i

    # Calculate decisions:
    hi_i <- sum((decisions_i == TRUE)  & (criterion_v == TRUE),  na.rm = TRUE)
    fa_i <- sum((decisions_i == TRUE)  & (criterion_v == FALSE), na.rm = TRUE)
    mi_i <- sum((decisions_i == FALSE) & (criterion_v == TRUE),  na.rm = TRUE)
    cr_i <- sum((decisions_i == FALSE) & (criterion_v == FALSE), na.rm = TRUE)

    n_i <- hi_i + fa_i + mi_i + cr_i

    # Return values to results:
    results_gt[i, ] <- c(n_i, hi_i, fa_i, mi_i, cr_i)

  } # for (i in 1:thresholds_n).


  # Convert to named dataframe: ----

  results_gt <- as.data.frame(results_gt)
  names(results_gt) <- c("n",  "hi", "fa", "mi", "cr")
  results_gt$direction <- ">"
  results_gt$threshold <- thresholds


  # Get results if using <= threshold: ----

  results_lt <- results_gt[ , c(1,  4, 5, 2, 3)]
  names(results_lt) <- c("n",  "hi", "fa", "mi", "cr")
  results_lt        <- results_lt[ , c("n",  "hi", "fa", "mi", "cr")]

  results_lt$direction <- "<="
  results_lt$threshold <- thresholds

  if (setequal("<=", directions)) {
    results <- results_lt
  }

  if (setequal(">", directions)) {
    results <- results_gt
  }

  if (setequal(c(">", "<="), directions)) {
    results <- rbind(results_gt, results_lt)
  }


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

  # Add new accuracy statistics (to previous results): ----
  results <- cbind(results, new_stats)


  # Clean up results: ----

  # Arrange rows by goal.threshold and change column order:
  row_order <- order(results[ , goal.threshold], decreasing = TRUE)

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

  # Remove invalid directions:
  results[results$direction %in% directions, ]


  # Output: ----

  return(results)

} # fftrees_threshold_numeric_grid().

# eof.
