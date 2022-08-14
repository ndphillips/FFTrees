#' Provide a verbal description of an FFT
#'
#' @description \code{inwords} generates and provides a verbal description
#' of a fast-and-frugal tree (FFT) from an \code{FFTrees} object.
#'
#' @param x An \code{FFTrees} object.
#' @param tree The tree to display (as numeric).
#'
#' @return A verbal description of an FFT (as a string).
#'
#' @seealso
#' \code{\link{print.FFTrees}} for printing FFTs;
#' \code{\link{plot.FFTrees}} for plotting FFTs;
#' \code{\link{summary.FFTrees}} for summarizing FFTs;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export

inwords <- function(x, tree = 1) {

  testthat::expect_is(x, class = "FFTrees")

  x$trees$inwords[[tree]]

} # inwords().

# eof.
