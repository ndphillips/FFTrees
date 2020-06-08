#' Create definitions of FFTrees
#'
#' @param x FFTrees.
#' @param object FFTrees.
#' @import testthat
#'
fftrees_define <- function(x, object = NULL) {

  if(is.null(object) == FALSE) {

    testthat::expect_is(object, "FFTrees", info = "You specified object but it is not of class 'FFTrees'")
    testthat::expect_true(!is.null(object$trees$definitions))

    x$trees$definitions <- object$trees$definitions
    x$trees$n <- nrow(object$trees$definitions)

  } else if(!is.null(x$params$my.tree)) {

    x <- fftrees_wordstofftrees(x, my.tree = x$params$my.tree)

  } else if(x$params$algorithm %in% c("ifan", "dfan")) {

      x <- fftrees_grow_fan(x)

    } else {

    stop("I don't know how to define your trees...")

  }

  return(x)

}
