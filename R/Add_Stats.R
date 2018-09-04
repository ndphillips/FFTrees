#' Adds decision statistics to a dataframe containing hr, cr, mi and fa
#'
#' @param data dataframe. With named (integer) columns hi, cr, mi, fa
#' @param sens.w numeric. Sensitivity weight
#' @param cost.each numeric. An optional fixed cost added to all outputs (e.g.; the cost of the cue)
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying the costs of a hit, false alarm, miss, and correct rejection rspectively. E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a false alarm and miss cost 10 and 20 respectively while correct decisions have no cost.
Add_Stats <- function(data,
                      sens.w = .5,
                      cost.each = NULL,
                      cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0)) {

  if(is.null(cost.each)) {cost.each <- 0}

    data$acc <- with(data, (hi + cr) / (hi + cr + fa + mi))
    data$sens <- with(data, hi / (hi + mi))
    data$spec <- with(data, cr / (cr + fa))
    data$far <- with(data, 1 - spec)
    data$bacc <- with(data, sens * .5 + spec * .5)
    data$wacc <- with(data, sens * sens.w + spec * (1 - sens.w))
    data$costout <- with(data, -1 * (hi * cost.outcomes$hi + fa * cost.outcomes$fa + mi * cost.outcomes$mi + cr * cost.outcomes$cr)) / data$n
    data$cost <- data$costout - cost.each

    # reorder
    data <- data[, c("sens", "spec", "far", "acc", "bacc", "wacc", "costout", "cost")]

    return(data)

  }
