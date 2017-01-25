#' Calculates several classification statistics from binary prediction and criterion (e.g.; truth) vectors
#' @param prediction.v A binary vector of predictions
#' @param criterion.v A binary vector of criterion (true) values
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
                       criterion.v) {


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

  N <- length(criterion.v)

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

  # bacc (sens - FAR)
  bacc <- (sens + spec) / 2

  # d-prime
  dprime <- qnorm(sens.c) - qnorm(far.c)

  }

  if(N == 0) {

    hi <- NA
    mi <- NA
    fa <- NA
    cr <- NA
    sens <- NA
    spec <- NA
    far <- NA
    acc <- NA
    bacc <- NA
    dprime <- NA

  }

  result <- data.frame(
    n = N,
    hi = hi,
    mi = mi,
    fa = fa,
    cr = cr,
    sens = sens,
    spec = 1 - far,
    far = far,
    acc = acc,
    bacc = bacc,
    dprime = dprime)

  return(result)

}
