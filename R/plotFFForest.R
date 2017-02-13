#' Creates a network plot. Code taken from Dirk Wulff (www.dirkwulff.org)
#'
#' @param x FFForest. An FFForest object created from FFForest()
#' @param node.cex.lim numeric. Nodesize adjustment
#' @param line.cex.lim numeric. Edgesize adjustment
#' @param mincon integer. Minimum connection cutoff
#' @param lo string. The layout of the network plot. Either 'kk' (Kamada-Kawai, the default), 'dh' (Davidson-Harel) or 'fr' (Fruchterman-Reingold)
#' @param palette string. A string vector of colors
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
                         lo = "kk",
                         palette = NULL,
                         ...) {



  par(mfrow = c(1, 2))

edges <- x$connections
edges <- edges[edges[,3] >= mincon,]
edges$line.lwd <- with(edges, N / sum(N))
edges$line.lwd <- with(edges, (line.lwd - min(line.lwd)) / (max(line.lwd) - min(line.lwd)) * (line.cex.lim[2] - line.cex.lim[1]) + line.cex.lim[1])

# Get overall frequencies

  frequencies <- c()

  for (i in 1:nrow(edges)) {

    frequencies <- c(frequencies, rep(edges[i, 1], times = edges[i, 3]), rep(edges[i, 2], times = edges[i, 3]))

  }

  frequencies <- sort(table(frequencies))
  node.cex <- (frequencies - min(frequencies)) / (max(frequencies) - min(frequencies)) * (node.cex.lim[2] - node.cex.lim[1]) + node.cex.lim[1]

  # Colors

  if(is.null(palette)) {

    palette <- yarrr::piratepal("basel", length.out = length(frequencies), trans = .4)} else {

      palette <- rep(palette, length.out = length(frequencies))}



# Barplot
{
bp.vals <- barplot(rev(frequencies / sum(frequencies)),
                   plot = FALSE)

par(mar = c(5, 8, 4, 1) + .1)

barplot(height = frequencies / sum(frequencies),
        xlab = "", main = "", yaxt = "n",
        beside = FALSE, xlim = c(0, 1), horiz = TRUE,
        col = palette[length(frequencies):1]
      #  col = gray(1 - frequencies / sum(frequencies))
        )

mtext(names(frequencies), side = 2, at = bp.vals[,1], las = 1, line = 1)
mtext("Importance", side = 1, cex = 1.5, line = 3)

text(frequencies / sum(frequencies), bp.vals[,1], pos = 4,
     labels = paste0(round(frequencies / sum(frequencies) * 100, 0), "%"))
}

# Network plot
{
  par(xpd = TRUE)
  par(mar = c(5, 4, 4, 1) + .1)
# Remove lower bound connections


g <- igraph::graph_from_data_frame(edges, directed = FALSE)

if(lo == "kk") {locations <- igraph::layout_with_kk(g)}
if(lo == "dh") {locations <- igraph::layout_with_dh(g)}
if(lo == "fr") {locations <- igraph::layout_with_fr(g)}


# Reorder cue.names and locations by frequencies
cue.names <- names(frequencies)
locations <- locations[rev(match(igraph::get.vertex.attribute(g)$name, cue.names)),]


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
          col = gray(1 - edges$line.lwd[i] / max(edges$line.lwd), alpha = .5))

  }

  # Add points

  for(i in 1:length(cue.names)){
    #  points(l[i,1] + .04, l[i, 2] - .04, cex = frequencies[v[i]]**nodesize, pch = 16, col = 'grey50')

    freq.i <- frequencies[names(frequencies) == cue.names[i]]

    relfreq.i <- freq.i / sum(frequencies)


    points(x = locations[i, 1],
           y = locations[i, 2],
           cex = node.cex[names(node.cex) == cue.names[i]],
           pch = 21,
           lwd = 5,
           col = palette[length(frequencies) - i + 1],
           bg = yarrr::transparent("white", trans = .2))
           #bg = gray(1 - relfreq.i))

  }

  for(i in 1:length(cue.names)) {

    text.outline(x = locations[i, 1],
                 y = locations[i, 2],
                 labels = cue.names[i],
                 cex = frequencies[cue.names[i]] ^ .08 - .4,
                 r = .004)

  }


}

par(xpd = TRUE)


# Heatplot
#
# p <- ggplot(edges, aes(cue1, cue2)) + geom_tile(aes(fill = N),
#                                colour = "white") + scale_fill_gradient(low = "white",high = "steelblue")

}
