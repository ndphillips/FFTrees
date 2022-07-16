#' Performs a grid search over factor and returns accuracy statistics for a given factor cue
#'
#' @param thresholds numeric. A vector of factor thresholds to consider
#' @param cue_v numeric. Feature values
#' @param criterion_v logical. Criterion values
#' @param directions character. Character vector of threshold directions to consider.
#' @param sens.w numeric.
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying the costs of a hit, false alarm, miss, and correct rejection rspectively. E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a false alarm and miss cost 10 and 20 respectively while correct decisions have no cost.
#' @param cost.each numeric.
#' @param goal.threshold character.
#'
#' @import testthat
#' @importFrom  magrittr "%>%"
#' @export
#' @return A data frame containing accuracy statistics for several factor thresholds
#'
fftrees_threshold_factor_grid <- function(thresholds = NULL,
                                          cue_v = NULL,
                                          criterion_v = NULL,
                                          directions = "=",
                                          sens.w = .5,
                                          cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0),
                                          cost.each = 0,
                                          goal.threshold = "bacc") {

  # Assertions
  testthat::expect_true(!any(is.na(criterion_v)))
  testthat::expect_true(!any(is.na(cue_v)))


  if (!is.null(thresholds)) {
    thresholds_n <- length(thresholds)
    case_n <- length(cue_v)

    results <- matrix(NA, nrow = thresholds_n, ncol = 5)

    # Loop 1 over all thresholds
    # C++

    for (i in 1:thresholds_n) {
      threshold_i <- thresholds[i]

      # Create vector of decisions
      decisions_i <- cue_v == threshold_i

      # Calculate decisions
      hi_i <- sum(decisions_i == TRUE & criterion_v == TRUE)
      fa_i <- sum(decisions_i == TRUE & criterion_v == FALSE)
      mi_i <- sum(decisions_i == FALSE & criterion_v == TRUE)
      cr_i <- sum(decisions_i == FALSE & criterion_v == FALSE)

      n_i <- hi_i + fa_i + mi_i + cr_i

      # Return values to results

      results[i, ] <- c(n_i, hi_i, fa_i, mi_i, cr_i)
    }

    # Convert to named dataframe
    results <- as.data.frame(results)
    names(results) <- c("n", "hi", "fa", "mi", "cr")

    # Add thresholds and directions
    results$threshold <- thresholds

    # Add accuracy statistics
    results <- cbind(results, Add_Stats(results,
      sens.w = sens.w,
      cost.outcomes = cost.outcomes
    ))

    # Order by goal.threshold and change column order
    ord_new <- order(results[, goal.threshold], decreasing = TRUE)

    results <- results[ord_new, ]

    # Loop 2 over cumulative thresholds
    # C++

    results_cum <- as.data.frame(matrix(NA, nrow = thresholds_n, ncol = 6))
    names(results_cum) <- c("threshold", "n", "hi", "fa", "mi", "cr")

    for (i in 1:thresholds_n) {
      threshold_i <- results$threshold[1:i]

      # Create vector of decisions
      decisions_i <- cue_v %in% threshold_i

      # Calculate decisions
      hi_i <- sum(decisions_i == TRUE & criterion_v == TRUE)
      fa_i <- sum(decisions_i == TRUE & criterion_v == FALSE)
      mi_i <- sum(decisions_i == FALSE & criterion_v == TRUE)
      cr_i <- sum(decisions_i == FALSE & criterion_v == FALSE)

      n_i <- hi_i + fa_i + mi_i + cr_i

      # Return values to results

      results_cum[i, 2:6] <- c(n_i, hi_i, fa_i, mi_i, cr_i)
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

    new_stats <- Add_Stats(
      data = results,
      sens.w = sens.w,
      cost.outcomes = cost.outcomes,
      cost.each = cost.each
    )

    # Add accuracy statistics
    results <- cbind(results, new_stats)

    # Order by goal.threshold and change column order
    ord_new <- order(results[, goal.threshold], decreasing = TRUE)

    results <- results[ord_new, c(
      "threshold", "direction", "n", "hi", "fa", "mi", "cr",
      "sens", "spec", "ppv", "npv", "bacc", "acc", "wacc",
      "cost_decisions", "cost"
    )]

    # Remove invalid directions
    results[results$direction %in% directions, ]
  } else {
    results <- data.frame(
      "threshold" = NA,
      "direction" = NA,
      "n" = NA,
      "hi" = NA,
      "fa" = NA,
      "mi" = NA,
      "cr" = NA,
      "sens" = NA,
      "spec" = NA,
      "bacc" = NA,
      "acc" = NA,
      "wacc" = NA,
      "cost_decisions" = NA,
      "cost" = NA
    )
  }

  return(results)
}
