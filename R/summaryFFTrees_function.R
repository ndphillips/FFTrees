#' Returns a summary of an fft object
#' @param object An FFTrees object
#' @param ... Additional arguments (currently ignored)
#' @export
#'

summary.FFTrees <- function(object, ...) {

  return(object$tree.stats)

}
