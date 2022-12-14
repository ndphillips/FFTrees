#' Create FFT definitions
#'
#' @description \code{fftrees_define} creates fast-and-frugal trees
#' (FFTs) from provided definitions or by applying algorithms (when no definitions are provided),
#' and returns an \code{FFTrees} object.
#'
#' \code{fftrees_define} usually passes passes \code{x} either
#' to \code{\link{fftrees_grow_fan}} (to create new FFTs by applying algorithms to data) or
#' to \code{\link{fftrees_wordstofftrees}} (if \code{my.tree} is specified).
#'
#' If an existing \code{FFTrees} object \code{object} is provided,
#' \code{fftrees_define} uses the trees from this \code{FFTrees} object.
#'
#' @param x The current \code{FFTrees} object (to be changed and returned).
#'
#' @param object An existing \code{FFTrees} object (with tree definitions).
#'
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

    # 1. An existing FFTrees object is provided: ----

    # Verify x and object:
    testthat::expect_s3_class(x, class = "FFTrees")
    testthat::expect_true(!is.null(object$trees$definitions))

    # Change object x by using the tree definitions of object:
    x$trees$definitions <- object$trees$definitions
    x$trees$n <- as.integer(nrow(object$trees$definitions))

    if (!x$params$quiet) {
      message("Using trees defined in 'object'")
    }

  } else if (!is.null(x$params$my.tree)) {

    # 2. Create new FFT from verbal description: ----

    if (!x$params$quiet) {
      message("Using trees specified in 'my.tree'") # # in fftrees_grow_fan()
    }

    x <- fftrees_wordstofftrees(x, my.tree = x$params$my.tree)


  } else if (x$params$algorithm %in% c("ifan", "dfan")) {

    # 3. Create new FFT by applying algorithm to data: ----

    # if (!x$params$quiet) {
    #   message("Using algorithm to grow new trees")  # in fftrees_grow_fan()
    # }

    x <- fftrees_grow_fan(x)



  } else {

    stop("I don't know how to define your trees...")

  }

  return(x)

} # fftrees_define().

# eof.
