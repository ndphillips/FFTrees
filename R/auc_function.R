#' Calculates AUC (Area under the Curve) using trapezoidal approximation
#' @param sens.v a vector of sensitivities
#' @param spec.v A vector of specificities
#' @export
#' @examples
#'
#' # Calculate the AUC for a vector of hit rates and false alarm rates
#'
#' auc(sens.v = c(.1, .3, .5, .7), spec.v = c(.05, .1, .15, .3))
#'
#'

auc <- function(sens.v, spec.v) {

  far.v <- 1 - spec.v

  hr.order <- order(sens.v)

  sens.v <- sens.v[hr.order]
  far.v <- far.v[hr.order]

  sens.v <- c(0, sens.v, 1)
  far.v <- c(0, far.v, 1)

  # Remove bad (i.e.. non-increasing values)

  sens.v.n <- sens.v[1]
  far.v.n <- far.v[1]

  if(all(is.finite(sens.v) & is.finite(far.v))) {

  for(i in 2:length(sens.v)) {

    if(sens.v[i] >= sens.v.n[length(sens.v.n)] & far.v[i] >= far.v.n[length(far.v.n)]) {

      sens.v.n <- c(sens.v.n, sens.v[i])
      far.v.n <- c(far.v.n, far.v[i])

    }
  }

  sens.v <- sens.v.n
  far.v <- far.v.n
  n <- length(sens.v)


  auc.i <- sum((far.v[2:n] - far.v[1:(n - 1)]) * sens.v[1:(n - 1)] +
    (far.v[2:n] - far.v[1:(n - 1)]) * (sens.v[2:(n)] - sens.v[1:(n-1)]) / 2)

  } else {

    auc.i <- NA
}

  return(auc.i)

}
