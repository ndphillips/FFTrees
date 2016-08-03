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

    if(hr.v[i] > hr.v.n[length(hr.v.n)] & far.v[i] >= far.v.n[length(far.v.n)]) {

      hr.v.n <- c(hr.v.n, hr.v[i])
      far.v.n <- c(far.v.n, far.v[i])

    }


  }

  hr.v <- hr.v.n
  far.v <- far.v.n


  far.d.v <- far.v[2:length(far.v)] - far.v[1:(length(far.v) - 1)]
  hr.d.v <- hr.v[2:length(hr.v)] - hr.v[1:(length(hr.v) - 1)]

  aoc.i <- 1- sum(far.d.v * hr.d.v)

  return(aoc.i)

}
