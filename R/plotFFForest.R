#' Creates a network plot. Code taken from Dirk Wulff (www.dirkwulff.org)
#'
#' @param x FFForest. An FFForest object created from FFForest()
#' @param node.cex.lim numeric. Nodesize adjustment
#' @param line.cex.lim numeric. Edgesize adjustment
#' @param mincon integer. Minimum connection cutoff
#' @param ... currently ignored
#' @importFrom igraph graph_from_data_frame get.vertex.attribute layout_with_dh
#' @importFrom graphics text points segments plot lines plot.new plot.window barplot
#' @export
#'
#'
#'
#'
plot.FFForest = function(x,
                         node.cex.lim = c(1, 10),
                         line.cex.lim = c(.3, 5),
                         mincon = 0,
                         ...) {


  par(mfrow = c(1, 2))

edges <- x$connections
edges$line.lwd <- with(edges, N / sum(N))
edges$line.lwd <- with(edges, (line.lwd - min(line.lwd)) / (max(line.lwd) - min(line.lwd)) * (line.cex.lim[2] - line.cex.lim[1]) + line.cex.lim[1])

# Get overall frequencies

  frequencies <- c()

  for (i in 1:nrow(edges)) {

    frequencies <- c(frequencies, rep(edges[i, 1], times = edges[i, 3]), rep(edges[i, 2], times = edges[i, 3]))

  }

  frequencies <- sort(table(frequencies))
  node.cex <- (frequencies - min(frequencies)) / (max(frequencies) - min(frequencies)) * (node.cex.lim[2] - node.cex.lim[1]) + node.cex.lim[1]


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
mtext("Importance", side = 1, cex = 1.5, line = 3)

text(frequencies / sum(frequencies), bp.vals[,1], pos = 4,
     labels = paste0(round(frequencies / sum(frequencies) * 100, 0), "%"))
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
          lwd = edges$line.lwd[i],
          lty = 1,
          col = 'grey50')

  }

  # Add points

  for(i in 1:length(cue.names)){
    #  points(l[i,1] + .04, l[i, 2] - .04, cex = frequencies[v[i]]**nodesize, pch = 16, col = 'grey50')

    freq.i <- frequencies[names(frequencies) == cue.names[i]]

    relfreq.i <- freq.i / sum(frequencies)

    # White mask

    # points(x = locations[i, 1],
    #        y = locations[i, 2],
    #        cex = mean(node.cex.lim),
    #        pch = 16,
    #        col = yarrr::transparent("white", .2))

    #bg = gray(1 - relfreq.i))

    points(x = locations[i, 1],
           y = locations[i, 2],
           cex = node.cex[names(node.cex) == cue.names[i]],
           pch = 21,
           lwd = 5,
           col = yarrr::piratepal("basel", trans = .1, length.out = nrow(locations))[i],
           bg = yarrr::transparent("white", trans = .2))
           #bg = gray(1 - relfreq.i))
  }

  # Add text

  for(i in 1:length(cue.names)){
    text.outline(x = locations[i, 1],
                 y = locations[i, 2],
                 labels = cue.names[i],
                 cex = frequencies[cue.names[i]] ^ .08 - .4,
                 r = .004)
  }

}

# Heatplot
#
# p <- ggplot(edges, aes(cue1, cue2)) + geom_tile(aes(fill = N),
#                                colour = "white") + scale_fill_gradient(low = "white",high = "steelblue")




}
