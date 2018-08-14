#' Performs a grid search over factor and returns accuracy statistics for a given factor cue
#'
#' @param thresholds numeric. A vector of factor thresholds to consider
#' @param cue.v numeric. Feature values
#' @param criterion.v logical. Criterion values
#' @param sens.w numeric.
#' @param cost.outcomes numeric.
#' @param cost.cue numeric.
#' @param goal.chase character.
#'
#'
#' @examples
#'
#'
#' threshold_numeric_grid(thresholds = c(10, 30, 50, 70, 90),
#'                        cue.v =  c(13, 16, 24, 35, 56, 76, 87, 95),
#'                        criterion.v = c( 0,  0,  1,  0,  1,  1,  1,  0),
#'                        sens.w = .5,
#'                        cost.outcomes = c(0, 1, 1, 0))
#'
#'
threshold_factor_grid <- function(thresholds,
                                   cue.v,
                                   criterion.v,
                                   sens.w = .5,
                                   cost.outcomes = c(0, 1, 1, 0),
                                   goal.chase = "acc") {



  thresholds_n <- length(thresholds)
  data_n <- length(cue.v)

  results <- matrix(NA, nrow = thresholds_n, ncol = 4)

  # Loop 1 over all thresholds
  # C++

  for(i in 1:thresholds_n) {

    threshold_i <- thresholds[i]

    # Create vector of decisions
    decisions_i <- cue.v == threshold_i

    # Calculate decisions
    hi_i <- sum(decisions_i == TRUE & criterion.v == TRUE)
    fa_i <- sum(decisions_i == TRUE & criterion.v == FALSE)
    mi_i <- sum(decisions_i == FALSE & criterion.v == TRUE)
    cr_i <- sum(decisions_i == FALSE & criterion.v == FALSE)

    # Return values to results

    results[i, ] <- c(hi_i, fa_i, mi_i,  cr_i)

  }


  # Convert to named dataframe
  results <- as.data.frame(results)
  names(results) <- c("hi", "fa", "mi", "cr")

  # Add thresholds and directions
  results$threshold <- thresholds

  # Add accuracy statistics
  results <- cbind(results, Add_Stats(results,
                                      sens.w = sens.w,
                                      cost.outcomes = cost.outcomes))
  results <- results[order(-results[goal.chase]),]

  # Loop 2 over cumulative thresholds
  # C++

  results_cum <- as.data.frame(matrix(NA, nrow = thresholds_n, ncol = 5))
  names(results_cum) <- c("threshold", "hi", "fa", "mi", "cr")

  for(i in 1:thresholds_n) {

    threshold_i <- results$threshold[1:i]

    # Create vector of decisions
    decisions_i <- cue.v %in% threshold_i

    # Calculate decisions
    hi_i <- sum(decisions_i == TRUE & criterion.v == TRUE)
    fa_i <- sum(decisions_i == TRUE & criterion.v == FALSE)
    mi_i <- sum(decisions_i == FALSE & criterion.v == TRUE)
    cr_i <- sum(decisions_i == FALSE & criterion.v == FALSE)

    # Return values to results

    results_cum[i, 2:5] <- c(hi_i, fa_i, mi_i,  cr_i)
    results_cum[i, 1] <- paste(threshold_i, collapse = ",")
  }


  results_cum$direction <- "="


  # TO DO
  # Add reverse directions

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

  # Add accuracy statistics

  results <- cbind(results, Add_Stats(results))

  # Order by goal and change column order
  results <- results[order(-results[goal.chase]), c("threshold", "direction", "hi", "fa", "mi", "cr", "sens", "spec", "bacc", "acc", "wacc", "cost")]

  return(results)

}
