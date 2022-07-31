#' Create FFT definitions
#'
#' @description \code{fftrees_define} creates definitions of fast-and-frugal trees
#' (FFTs, as an \code{FFTrees} object).
#'
#' \code{fftrees_define} usually passes passes \code{x} either
#' to \code{\link{fftrees_grow_fan}} (to create new FFTs by applying algorithms to data) or
#' to \code{\link{fftrees_wordstofftrees}} (if \code{my.tree} is specified).
#' If \code{object} is provided, \code{fftrees_define} uses the trees from this \code{FFTrees} object.
#'
#' @param x An \code{FFTrees} object.
#' @param object An \code{FFTrees} object.
#'
#' @return An \code{FFTrees} object with tree definitions.
#'
#' @keywords internal
#'
#' @seealso
#' \code{\link{fftrees_create}} for creating \code{FFTrees} objects;
#' \code{\link{fftrees_grow_fan}} for creating FFTs by applying algorithms to data;
#' \code{\link{fftrees_wordstofftrees}} for creating FFTs from verbal descriptions;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @import testthat
#'
#' @export

fftrees_define <- function(x, object = NULL) {

  if (is.null(object) == FALSE) {

    testthat::expect_is(object, "FFTrees", info = "You specified an object, but it is not of class 'FFTrees'.")
    testthat::expect_true(!is.null(object$trees$definitions))

    # 0. Use trees in object:
    x$trees$definitions <- object$trees$definitions
    x$trees$n <- nrow(object$trees$definitions)

  } else if (!is.null(x$params$my.tree)) {

    # 1. Create FFT from verbal description:
    x <- fftrees_wordstofftrees(x, my.tree = x$params$my.tree)

  } else if (x$params$algorithm %in% c("ifan", "dfan")) {

    # 2. Create FFT by applying algorithm to data:
    x <- fftrees_grow_fan(x)

  } else {

    stop("I don't know how to define your trees...")

  }

  return(x)

} # fftrees_define().

# eof.
