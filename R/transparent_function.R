#' Makes transparent colors
#' @param orig.col a color
#' @param trans.val A number between 0 and 1 indicating how transparent to make the color
#' @importFrom grDevices rgb
#' @export
#'

transparent <- function(orig.col = "red",
                        trans.val = .5)
{
  n.cols <- length(orig.col)
  orig.col <- col2rgb(orig.col)
  final.col <- rep(NA, n.cols)
  for (i in 1:n.cols) {
    final.col[i] <- rgb(orig.col[1, i], orig.col[2, i], orig.col[3,
                                                                 i], alpha = (1 - trans.val) * 255, maxColorValue = 255)
  }
  return(final.col)
}
