#' Calculates AUC (Area under the Curve)
#' @param hr.v a vector of hit rates
#' @param far.v A vector of false alarm rates
#' @export
#'

auc <- function(hr.v, far.v) {


  hr.order <- order(hr.v)

  hr.v <- hr.v[hr.order]
  far.v <- far.v[hr.order]

  hr.v <- c(0, hr.v, 1)
  far.v <- c(0, far.v, 1)


  # Remove bad (i.e.. non-increasing values)

  hr.v.n <- hr.v[1]
  far.v.n <- far.v[1]

  for(i in 2:length(hr.v)) {

    if(hr.v[i] >= hr.v.n[length(hr.v.n)] & far.v[i] >= far.v.n[length(far.v.n)]) {

      hr.v.n <- c(hr.v.n, hr.v[i])
      far.v.n <- c(far.v.n, far.v[i])

    }


  }

  hr.v <- hr.v.n
  far.v <- far.v.n
  n <- length(hr.v)


  auc.i <- sum((far.v[2:n] - far.v[1:(n - 1)]) * hr.v[1:(n - 1)] +
    (far.v[2:n] - far.v[1:(n - 1)]) * (hr.v[2:(n)] - hr.v[1:(n-1)]) / 2)



  return(auc.i)

}
