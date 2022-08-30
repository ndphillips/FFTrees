#' Open the \strong{FFTrees} package guide
#'
#' @importFrom utils browseVignettes vignette
#'
#' @return No return value, called for side effects.
#'
#' @export

FFTrees.guide <- function() {

  utils::vignette(topic = "guide", package = "FFTrees")
  # utils::browseVignettes(package = "FFTrees")

} # FFTrees.guide().

# eof.
