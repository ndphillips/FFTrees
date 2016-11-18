#' Creates a network plot. Code taken from Dirk Wulff (www.dirkwulff.org)
#'
#' @param edges dataframe. A dataframe showing
#' @param nodesize numeric. Nodesize adjustment
#' @param edgesize numeric. Edgesize adjustment
#' @param mincon integer. Minimum connection cutoff
#' @importFrom igraph graph_from_data_frame get.vertex.attribute layout_with_dh
#' @importFrom graphics text points segments plot lines plot.new plot.window
#' @export
#'
#'
#'
#'
network_plot = function(edges,
                        nodesize = .1,
                        edgesize = 1,
                        mincon = 0) {

par(ask = TRUE)
# Get overall frequencies
{
  frequencies <- c()

  for (i in 1:nrow(edges)) {

    frequencies <- c(frequencies, rep(edges[i, 1], times = edges[i, 3]), rep(edges[i, 2], times = edges[i, 3]))

  }

  frequencies <- sort(table(frequencies))
}

# Barplot
{
bp.vals <- barplot(rev(frequencies / sum(frequencies)),
                   plot = FALSE)

par(mar = c(5, 8, 4, 1) + .1)

barplot(height = frequencies / sum(frequencies),
        xlab = "", main = "", yaxt = "n",
        beside = FALSE, xlim = c(0, 1), horiz = TRUE,
        col = gray(1 - frequencies / sum(frequencies)))

mtext(names(frequencies), side = 2, at = bp.vals[,1], las = 1, line = 1)
mtext("Proportion of simulations where cue was used in FFTrees", side = 3, cex = .8, font = 3)

text(frequencies / sum(frequencies), bp.vals[,1], pos = 4,
     labels = paste0(round(frequencies / sum(frequencies) * 100, 1), "%"))
}

# Network plot
{
  par(mar = c(5, 4, 4, 1) + .1)
# Remove lower bound connections
edges <- edges[edges[,3] >= mincon,]

g <- igraph::graph_from_data_frame(edges, directed = FALSE)

cue.names <- igraph::get.vertex.attribute(g)$name
# l <- igraph::layout_with_fr(g)
locations <- igraph::layout_with_dh(g)

  # Setup plotting region
  plot.new()

  if(min(locations[,1]) < 0) {x.min <- min(locations[,1]) * 1.1}
  if(max(locations[,1]) < 0) {x.max <- max(locations[,1]) * .9}
  if(min(locations[,2]) < 0) {y.min <- min(locations[,2]) * 1.1}
  if(max(locations[,2]) < 0) {y.max <- max(locations[,2]) * .9}

  if(min(locations[,1]) > 0) {x.min <- min(locations[,1]) * .9}
  if(max(locations[,1]) > 0) {x.max <- max(locations[,1]) * 1.1}
  if(min(locations[,2]) > 0) {y.min <- min(locations[,2]) * .9}
  if(max(locations[,2]) > 0) {y.max <- max(locations[,2]) * 1.1}

  plot.window(xlim = c(x.min, x.max),
              ylim = c(y.min, y.max))
  # Create edges

  for (i in 1:nrow(edges)) {

    is <- c(which(cue.names == edges[i, 1]),
            which(cue.names == edges[i, 2]))

    lines(x = c(locations[is[1], 1], locations[is[2], 1]),
          y = c(locations[is[1], 2], locations[is[2], 2]),
          lwd = edges[i ,3] ^ edgesize - .3,
          lty = 1,
          col = 'grey50')

  }

  # Add points

  for(i in 1:length(cue.names)){
    #  points(l[i,1] + .04, l[i, 2] - .04, cex = cnt[v[i]]**nodesize, pch = 16, col = 'grey50')

    freq.i <- cnt[names(cnt) == cue.names[i]]

    relfreq.i <- freq.i / sum(cnt)

    points(x = locations[i, 1],
           y = locations[i, 2],
           cex = cnt[cue.names[i]] * nodesize,
           pch = 21,
           col = "black",
           bg = gray(1 - relfreq.i))
  }

  # Add text

  for(i in 1:length(cue.names)){
    text(x = locations[i, 1],
         y = locations[i, 2],
         labels = cue.names[i],
         cex = cnt[cue.names[i]] ^ .08 - .4)
  }

}

par(ask = FALSE)

}
