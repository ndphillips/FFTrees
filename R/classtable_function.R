#' Calculates classification statistics from binary prediction and truth vectors
#' @param prediction.v A binary vector of predictions
#' @param criterion.v A binary vector of criterion (true) values
#' @importFrom stats qnorm
#' @export
#'

classtable <- function(prediction.v,
                       criterion.v) {


  correction <- .25
  hr.weight <- .5

  #
  #   prediction.v <- sample(c(0, 1), size = 100, replace = T)
  #   criterion.v <- sample(c(0, 1), size = 100, replace = T)
  #   correction <- .25
  #   hr.weight <- .5


  # prediction.v = subset(decision.df, levelout <= current.level)$decision
  # criterion.v = subset(decision.df, levelout <= current.level)$criterion
  # correction = correction
  # hr.weight = hr.weight


  hi <- sum(prediction.v == 1 & criterion.v == 1)
  mi <- sum(prediction.v == 0 & criterion.v == 1)
  fa <- sum(prediction.v == 1 & criterion.v == 0)
  cr <- sum(prediction.v == 0 & criterion.v == 0)

  hr <- hi / (hi + mi)
  far <- fa / (cr + fa)
  v <- hr - far
  dprime <- qnorm(hr) - qnorm(far)

  correct.index <- hi == 0 | mi == 0 | fa == 0 | cr == 0

  hi.c <- hi
  mi.c <- mi
  fa.c <- fa
  cr.c <- cr

  hi.c[correct.index] <- hi[correct.index] + correction
  mi.c[correct.index] <- mi[correct.index] + correction
  fa.c[correct.index] <- fa[correct.index] + correction
  cr.c[correct.index] <- cr[correct.index] + correction

  hr.c <- hi.c / (hi.c + mi.c)
  far.c <- fa.c / (fa.c + cr.c)
  v.c <- hr.c - far.c
  dprime.c <- qnorm(hr.c) - qnorm(far.c)


  v.w <- hr * hr.weight - far * (1 - hr.weight)
  dprime.w <- qnorm(hr) * hr.weight - qnorm(far) * (1 - hr.weight)

  v.c.w <- (hr.c * hr.weight - far.c * (1 - hr.weight)) * (1 / hr.weight)
  dprime.c.w <- qnorm(hr.c) * hr.weight - qnorm(far.c) * (1 - hr.weight)

  result <- data.frame(
    hi = hi, mi = mi, fa = fa, cr = cr,
    hr = hr,
    far = far,
    v = v.c.w,
    dprime = dprime.c.w,
    correction = correction,
    hr.weight = hr.weight
  )

  return(result)

}
