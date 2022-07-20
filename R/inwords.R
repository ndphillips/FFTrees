#' Display a verbal description of a tree in an FFTrees object
#'
#' @param x FFTrees.
#' @param tree numeric. The tree to display
#' @return A string describing an FFT
#' @export
#'
inwords <- function(x, tree = 1) {
  testthat::expect_is(x, class = "FFTrees")

  x$trees$inwords[[tree]]
}
