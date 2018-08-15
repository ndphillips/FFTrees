#' Performs a grid search over thresholds and returns accuracy statistics for a given numeric cue
#'
#' @param thresholds numeric. A vector of thresholds to consider
#' @param cue.v numeric. Feature values
#' @param criterion.v logical. Criterion values
#' @param sens.w numeric.
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying the costs of a hit, false alarm, miss, and correct rejection rspectively. E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a false alarm and miss cost 10 and 20 respectively while correct decisions have no cost.
#' @param goal character.
#'
#' @examples
#'
#' threshold_numeric_grid(thresholds = c(10, 30, 50, 70, 90),
#'                        cue.v =  c(13, 16, 24, 35, 56, 76, 87, 95),
#'                        criterion.v = c( 0,  0,  1,  0,  1,  1,  1,  0),
#'                        sens.w = .5,
#'                        cost.outcomes = c(0, 1, 1, 0))
#'
threshold_numeric_grid <- function(thresholds,
                                   cue.v,
                                   criterion.v,
                                   sens.w = .5,
                                   cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0),
                                   goal = "acc") {

  # thresholds = cue.levels
  # cue.v = cue.v
  # criterion.v = criterion.v
  # sens.w = sens.w
  # cost.outcomes = cost.outcomes
  # goal = goal
  #

  thresholds_n <- length(thresholds)
  data_n <- length(cue.v)

  results_gt <- matrix(NA, nrow = thresholds_n, ncol = 4)

  # Loop over all thresholds
  # C++

  for(i in 1:thresholds_n) {

    threshold_i <- thresholds[i]

    # Create vector of decisions
    decisions_i <- cue.v > threshold_i

    # Calculate decisions
    hi_i <- sum(decisions_i == TRUE & criterion.v == TRUE)
    fa_i <- sum(decisions_i == TRUE & criterion.v == FALSE)
    mi_i <- sum(decisions_i == FALSE & criterion.v == TRUE)
    cr_i <- sum(decisions_i == FALSE & criterion.v == FALSE)

    # Return values to results
    results_gt[i, ] <- c(hi_i, fa_i, mi_i,  cr_i)

  }

  # Get results if using <= threshold
  results_lt <- results_gt[,c(3, 4, 1, 2)]

  # Combine
  results <- rbind(results_gt, results_lt)

  # Convert to named dataframe
  results <- as.data.frame(results)
  names(results) <- c("hi", "fa", "mi", "cr")

  # Add thresholds and directions
  results$threshold <- rep(thresholds, 2)
  results$direction <- rep(c(">", "<"), each = thresholds_n)

  # Add accuracy statistics
  results <- cbind(results, Add_Stats(results,
                                      sens.w = sens.w,
                                      cost.outcomes = cost.outcomes))


  # Order by goal and change column order
  results <- results[order(-results[goal]), c("threshold", "direction", "hi", "fa", "mi", "cr", "sens", "spec", "bacc", "acc", "wacc", "cost")]

  return(results)

}
