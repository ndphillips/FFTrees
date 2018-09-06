#' Performs a grid search over factor and returns accuracy statistics for a given factor cue
#'
#' @param thresholds numeric. A vector of factor thresholds to consider
#' @param cue.v numeric. Feature values
#' @param criterion.v logical. Criterion values
#' @param directions character. Character vector of threshold directions to consider.
#' @param sens.w numeric.
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying the costs of a hit, false alarm, miss, and correct rejection rspectively. E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a false alarm and miss cost 10 and 20 respectively while correct decisions have no cost.
#' @param cost.each numeric.
#' @param goal.threshold character.
#'
threshold_factor_grid <- function(thresholds = NULL,
                                   cue.v = NULL,
                                   criterion.v = NULL,
                                   directions = "=",
                                   sens.w = .5,
                                   cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0),
                                   cost.each = 0,
                                   goal.threshold = "bacc") {



  if(!is.null(thresholds)) {

  thresholds_n <- length(thresholds)
  case_n <- length(cue.v)

  results <- matrix(NA, nrow = thresholds_n, ncol = 5)

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

    n_i <- hi_i + fa_i + mi_i + cr_i

    # Return values to results

    results[i, ] <- c(n_i, hi_i, fa_i, mi_i,  cr_i)

  }

  # Convert to named dataframe
  results <- as.data.frame(results)
  names(results) <- c("n", "hi", "fa", "mi", "cr")

  # Add thresholds and directions
  results$threshold <- thresholds

  # Add accuracy statistics
  results <- cbind(results, Add_Stats(results,
                                      sens.w = sens.w,
                                      cost.outcomes = cost.outcomes))
  results <- results[order(-results[goal.threshold]),]

  # Loop 2 over cumulative thresholds
  # C++

  results_cum <- as.data.frame(matrix(NA, nrow = thresholds_n, ncol = 6))
  names(results_cum) <- c("threshold", "n", "hi", "fa", "mi", "cr")

  for(i in 1:thresholds_n) {

    threshold_i <- results$threshold[1:i]

    # Create vector of decisions
    decisions_i <- cue.v %in% threshold_i

    # Calculate decisions
    hi_i <- sum(decisions_i == TRUE & criterion.v == TRUE)
    fa_i <- sum(decisions_i == TRUE & criterion.v == FALSE)
    mi_i <- sum(decisions_i == FALSE & criterion.v == TRUE)
    cr_i <- sum(decisions_i == FALSE & criterion.v == FALSE)

    n_i <- hi_i + fa_i + mi_i + cr_i

    # Return values to results

    results_cum[i, 2:6] <- c(n_i, hi_i, fa_i, mi_i,  cr_i)
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

  new_stats <-  Add_Stats(data = results,
                          sens.w = sens.w,
                          cost.outcomes = cost.outcomes,
                          cost.each = cost.each)

  # Add accuracy statistics
  results <- cbind(results, new_stats)


  # Order by goal.threshold and change column order
  results <- results[order(-results[goal.threshold]), c("threshold", "direction", "n", "hi", "fa", "mi", "cr", "sens", "spec", "bacc", "acc", "wacc", "costout", "cost")]


  # Remove invalid directions
  results[results$direction %in% directions, ]

  } else {

    results <- data.frame("threshold" = NA,
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
                          "costout" = NA,
                          "cost" = NA)

  }



  return(results)

}
