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


  if((hi + mi) == 0 | (cr + fa) == 0) {

    hi.c <- hi + correction
    mi.c <- mi + correction
    fa.c <- fa + correction
    cr.c <- cr + correction

  } else {

    hi.c <- hi
    mi.c <- mi
    fa.c <- fa
    cr.c <- cr

  }


  hr <- hi.c / (hi.c + mi.c)
  far <- fa.c / (cr.c + fa.c)

  v <- hr - far

  dprime <- qnorm(hr) - qnorm(far)

  }

  if(N == 0) {

    hi <- NA
    mi <- NA
    fa <- NA
    cr <- NA
    hr <- NA
    far <- NA
    v <- NA
    dprime <- NA

  }

  result <- data.frame(
    n = N,
    hi = hi,
    mi = mi,
    fa = fa,
    cr = cr,
    hr = hr,
    far = far,
    v = v,
    dprime = dprime)

  return(result)

}
