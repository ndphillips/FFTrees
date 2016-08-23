#' Deprecated functions
#'
#' These functions have been renamed and deprecated in \pkg{FFTrees}:
#' \code{fft()} (use \code{\link{FFTrees}()})
#' @rdname deprecated
#' @keywords internal
#' @param ... arguments passed from the old functions to the new functions
#' @export
fft <- function(...) {
  .Deprecated("FFTrees")
  FFTrees(...)
}
