  #' Adds decision statistics to a dataframe containing hr, cr, mi and fa
  #'
  #' @param data dataframe. With named (integer) columns hi, cr, mi, fa
  #' @param sens.w numeric. Sensitivity weight
  #' @param cost.outcomes numeric. Numeric vector of length 4 indicating the costs of hi, cr, mi and fa
  Add_Stats <- function(data,
                        sens.w = .5,
                        cost.outcomes = c(0, 0, 0, 0)) {

    data$acc <- with(data, (hi + cr) / (hi + cr + fa + mi))
    data$sens <- with(data, hi / (hi + mi))
    data$spec <- with(data, cr / (cr + fa))
    data$bacc <- with(data, sens * .5 + spec * .5)
    data$wacc <- with(data, sens * sens.w + spec * (1 - sens.w))
    data$cost <- with(data, -1 * (hi * cost.outcomes[1] + fa * cost.outcomes[2] + mi * cost.outcomes[3] + cr * cost.outcomes[4]))

    # reorder
    data <- data[, c("sens", "spec", "acc", "bacc", "wacc", "cost")]

    return(data)

  }
