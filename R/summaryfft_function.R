#' Returns a summary of an fft object
#' @param object An fft object
#' @param ... Additional arguments
#' @export
#'

summary.fft <- function(object, ...) {

  return(object$fft.stats)

}
