#' Perform a grid search over thresholds and return accuracy statistics for a given numeric cue
#'
#' @param thresholds numeric. A vector of thresholds to consider.
#' @param cue_v numeric. Feature values.
#' @param criterion_v logical. A logical vector of (TRUE) criterion values.
#' @param directions character. Possible directions to consider.
#' @param sens.w numeric. Sensitivity weight parameter (from 0 to 1, for computing \code{wacc}).
#' Default: \code{sens.w = .50}.
#' @param cost.each numeric. Cost to add to each value (e.g.; cost of  the cue).
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying
#' the costs of a hit, false alarm, miss, and correct rejection, respectively.
#' For instance, \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that
#' a false alarm and miss cost 10 and 20, respectively, while correct decisions have no cost.
#' @param goal.threshold character. A string indicating the statistic to maximize when calculting cue thresholds: "acc" = overall accuracy, "wacc" = weighted accuracy, "bacc" = balanced accuracy.
#'
#' @return A data frame containing accuracy statistics for several numeric thresholds.
#'
#' @seealso \code{\link{fftrees_threshold_factor_grid}} for factor cues.
#'
#' @export

fftrees_threshold_numeric_grid <- function(thresholds,
                                           cue_v,
                                           criterion_v,
                                           directions = c(">", "<="),
                                           sens.w = .50,
                                           cost.each = 0,
                                           cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0),
                                           goal.threshold = "bacc") {

  thresholds_n <- length(thresholds)

  results_gt <- matrix(NA, nrow = thresholds_n, ncol = 5)

  # Loop over all thresholds: ------
  # C++

  for (i in 1:thresholds_n) {

    threshold_i <- thresholds[i]

    # Create vector of decisions:
    decisions_i <- cue_v > threshold_i

    # Calculate decisions:
    hi_i <- sum(decisions_i == TRUE  & criterion_v == TRUE,  na.rm = TRUE)
    fa_i <- sum(decisions_i == TRUE  & criterion_v == FALSE, na.rm = TRUE)
    mi_i <- sum(decisions_i == FALSE & criterion_v == TRUE,  na.rm = TRUE)
    cr_i <- sum(decisions_i == FALSE & criterion_v == FALSE, na.rm = TRUE)

    n_i <- hi_i + fa_i + mi_i + cr_i

    # Return values to results:
    results_gt[i, ] <- c(n_i, hi_i, fa_i, mi_i, cr_i)

  } # for (i in 1:thresholds_n).


  # Convert to named dataframe: ----
  results_gt <- as.data.frame(results_gt)
  names(results_gt) <- c("n", "hi", "fa", "mi", "cr")
  results_gt$direction <- ">"
  results_gt$threshold <- thresholds

  # Get results if using <= threshold: ----
  results_lt <- results_gt[, c(1, 4, 5, 2, 3)]
  names(results_lt) <- c("n", "hi", "fa", "mi", "cr")
  results_lt <- results_lt[, c("n", "hi", "fa", "mi", "cr")]

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

  new_stats <- add_stats(results,
                         sens.w = sens.w,
                         cost.outcomes = cost.outcomes,
                         cost.each = cost.each
  )

  # Add accuracy statistics: ----
  results <- cbind(results, new_stats)

  # Order by goal.threshold and change column order:
  ord_new <- order(results[, goal.threshold], decreasing = TRUE)

  results <- results[ord_new, c(
    "threshold", "direction", "n", "hi", "fa", "mi", "cr",
    "sens", "spec", "ppv", "npv", "bacc", "acc", "wacc",
    "cost_decisions", "cost"
  )]

  # Remove invalid directions:
  results[results$direction %in% directions, ]

  # Output: ----
  return(results)

} # fftrees_threshold_numeric_grid().

# eof.
