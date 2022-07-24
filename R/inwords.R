#' Displays a verbal description of an FFT.
#'
#' @description \code{inwords} provides a verbal description of a fast-and-frugal tree (FFT)
#' from an \code{FFTrees} object.
#'
#' @param x An \code{FFTrees} object.
#' @param tree The tree to display (as numeric).
#'
#' @return A verbal description of an FFT (as a string).
#'
#' @seealso
#' \code{\link{plot.FFTrees}} for plotting FFTs;
#' \code{\link{FFTrees}} for creating FFTs from data.
#'
#' @export

inwords <- function(x, tree = 1) {

  testthat::expect_is(x, class = "FFTrees")

  x$trees$inwords[[tree]]

} # inwords().

# eof.
