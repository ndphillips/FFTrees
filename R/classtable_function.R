#' Calculates several classification statistics from binary prediction and criterion (e.g.; truth) vectors
#' @param prediction.v A binary vector of predictions
#' @param criterion.v A binary vector of criterion (true) values
#' @param cost.outcomes numeric. A vector of length 4 specifying the costs of a hit, false alarm, miss, and correct rejection rspectively. E.g.; \code{cost.outcomes = c(0, 10, 20, 0)} means that a false alarm and miss cost 10 and 20 respectively while correct decisions have no cost.
#' @param cost.v numeric. An optional vector of additional costs to be added to each case.
#' @param sens.w numeric. Weight given to sensitivity, must range from 0 to 1.
#' @importFrom stats qnorm
#' @export
#' @examples
#'
#'
#'  # classification statistics for 5 cases
#' classtable(prediction.v = c(0, 0, 0, 1, 1),
#'            criterion.v = c(0, 0, 1, 0, 1))
#'
#'
#'

classtable <- function(prediction.v,
                       criterion.v,
                       sens.w = .5,
                       cost.v = NULL,
                       cost.outcomes = c(0, 1, 1, 0)) {

#
#   prediction.v = pred.vec
#   criterion.v = criterion.v
#   cost.v = rep(cue.cost.i, cases.n)
#   sens.w = sens.w
#   cost.outcomes = cost.outcomes


if(is.null(cost.v)) {cost.v <- rep(0, length(prediction.v))}

if(any(c("FALSE", "TRUE") %in% paste(prediction.v))) {

  prediction.v <- as.logical(paste(prediction.v))

}

if(any(c("FALSE", "TRUE") %in% paste(criterion.v))) {

  criterion.v <- as.logical(paste(criterion.v))

  }

  correction <- .25

  # Remove NA values

  prediction.v <- prediction.v[is.finite(criterion.v)]
  criterion.v <- criterion.v[is.finite(criterion.v)]

  N <- min(length(criterion.v), length(prediction.v))

  if(N > 0) {

  hi <- sum(prediction.v == 1 & criterion.v == 1, na.rm = TRUE)
  mi <- sum(prediction.v == 0 & criterion.v == 1, na.rm = TRUE)
  fa <- sum(prediction.v == 1 & criterion.v == 0, na.rm = TRUE)
  cr <- sum(prediction.v == 0 & criterion.v == 0, na.rm = TRUE)


  if(hi == 0 | mi == 0 | cr == 0 | fa == 0) {

    hi.c <- hi + .5
    mi.c <- mi + .5
    fa.c <- fa + .5
    cr.c <- cr + .5

  } else {

    hi.c <- hi
    mi.c <- mi
    fa.c <- fa
    cr.c <- cr

  }

  # Sensitivity
  sens <- hi / (hi + mi)
  sens.c <- hi.c / (hi.c + mi.c)

  # False-alarm rate
  far <- fa / (cr + fa)
  far.c <- fa.c / (cr.c + fa.c)

  # Specificity
  spec <- 1 - far

  # Percent correct
  acc <- (hi + cr) / (hi + cr + mi + fa)

  # ppv (positive predictive value)
  ppv <- hi / (hi + fa)

  # npv (negative predictive value)
  npv <- cr / (mi + cr)

  # bpv (balanced predictive value)
  bpv <- (ppv + npv) / 2

  # bacc (sens - FAR)
  bacc <- (sens + spec) / 2

  # wacc (waited accuracy)
  wacc <- sens * sens.w + spec * (1 - sens.w)

  # d-prime
  dprime <- qnorm(sens.c) - qnorm(far.c)

  # cost (outcome costs)
  cost <- (as.numeric(c(hi, fa, mi, cr) %*% cost.outcomes) + sum(cost.v)) / N

  }

  if(N == 0) {

    hi <- NA
    mi <- NA
    fa <- NA
    cr <- NA
    sens <- NA
    spec <- NA
    ppv <- NA
    npv <- NA
    bpv <- NA
    far <- NA
    acc <- NA
    bacc <- NA
    wacc <- NA
    dprime <- NA
    cost <- NA

  }

  result <- data.frame(
    n = N,
    hi = hi,
    mi = mi,
    fa = fa,
    cr = cr,
    sens = sens,
    spec = 1 - far,
    ppv = ppv,
    npv = npv,
    far = far,
    acc = acc,
    bacc = bacc,
    wacc = wacc,
    bpv = bpv,
    dprime = dprime,
    cost = cost)

  return(result)

}
