
# Apply break function
#   Takes a direction, threshold value, and cue vector, and returns a vector of decisions
apply.break <- function(direction,
                        threshold.val,
                        cue.v,
                        cue.class
) {


  if(is.character(threshold.val)) {threshold.val <- unlist(strsplit(threshold.val, ","))}

  if(cue.class %in% c("numeric", "integer")) {threshold.val <- as.numeric(threshold.val)}


  if(direction == "!=") {output <- (cue.v %in% threshold.val) == F}
  if(direction == "=") {output <- cue.v %in% threshold.val}
  if(direction == "<") {output <- cue.v < threshold.val}
  if(direction == "<=") {output <- cue.v <= threshold.val}
  if(direction == ">") {output <- cue.v > threshold.val}
  if(direction == ">=") {output <- cue.v >= threshold.val}


  return(output)

}

# text.outline
# adds text with a white background - taken from Dirk Wulff www.dirkwulff.org
text.outline <- function(x, y,
                         labels = 'test',
                         col = 'black',
                         font = 1,
                         bg = 'white',
                         r = 0.02,
                         h = 1,
                         w = 1,
                         cex = 1,
                         adj = .5,
                         pos = NULL){

  # Draw background
  is <- seq(0, 2 * pi, length = 72)
  for(i in is){
    xn = x + cos(i) * r * w
    yn = y + sin(i) * r * h
    text(xn, yn, labels = labels, col = bg, cex = cex, adj = adj, pos = pos, font = font)
  }

  # Foreground
  text(x, y, labels = labels, col = col, cex = cex, adj = adj, pos = pos, font = font)
}


# transparent
#  make text transparent
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

