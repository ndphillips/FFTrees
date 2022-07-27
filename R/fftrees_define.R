#' Create definitions of FFTs.
#'
#' @description \code{fftrees_define} creates definitions of fast-and-frugal trees
#' (FFTs, as an \code{FFTrees} object).
#'
#' @param x FFTrees.
#' @param object FFTrees.
#'
#' @return An \code{FFTrees} object with tree definitions.
#'
#' @keywords internal
#'
#' @import testthat
#'
#' @export

fftrees_define <- function(x, object = NULL) {

  if (is.null(object) == FALSE) {

    testthat::expect_is(object, "FFTrees", info = "You specified an object, but it is not of class 'FFTrees'")
    testthat::expect_true(!is.null(object$trees$definitions))

    # Use trees in object:
    x$trees$definitions <- object$trees$definitions
    x$trees$n <- nrow(object$trees$definitions)

  } else if (!is.null(x$params$my.tree)) {

    # Create FFT from verbal description:
    x <- fftrees_wordstofftrees(x, my.tree = x$params$my.tree)

  } else if (x$params$algorithm %in% c("ifan", "dfan")) {

    # Create FFT by algorithm:
    x <- fftrees_grow_fan(x)

  } else {

    stop("I don't know how to define your trees...")

  }

  return(x)

} # fftrees_define().

# eof.
