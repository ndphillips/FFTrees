#' Creates a network plot. Code taken from Dirk Wulff (www.dirkwulff.org)
#'
#' @param x FFForest. An FFForest object created from FFForest()
#' @param nodesize numeric. Nodesize adjustment
#' @param edgesize numeric. Edgesize adjustment
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
                         nodesize = .1,
                         edgesize = 1,
                         mincon = 0,
                         ...) {

par(ask = TRUE)

edges <- x$connections
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
mtext("Proportion of simulations where cue was used in FFForest", side = 3, cex = .8, font = 3)

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
    #  points(l[i,1] + .04, l[i, 2] - .04, cex = frequencies[v[i]]**nodesize, pch = 16, col = 'grey50')

    freq.i <- frequencies[names(frequencies) == cue.names[i]]

    relfreq.i <- freq.i / sum(frequencies)

    points(x = locations[i, 1],
           y = locations[i, 2],
           cex = relfreq.i * nodesize,
           pch = 21,
           col = "black",
           bg = gray(1 - relfreq.i))
  }

  # Add text

  for(i in 1:length(cue.names)){
    text(x = locations[i, 1],
         y = locations[i, 2],
         labels = cue.names[i],
         cex = frequencies[cue.names[i]] ^ .08 - .4)
  }

}

# ROC
{

plot(1, xlim = c(0, 1),
     ylim = c(0, 1),
     xlab = "FAR",
     ylab = "HR",
     type = "n",
     main = "ROC Curve", yaxt = "n", xaxt = "n")

  axis(side = 2, at = seq(0, 1, .1), las = 1)
  axis(side = 1, at = seq(0, 1, .1), las = 1)

  abline(h = seq(0, 1, .1),
         v = seq(0, 1, .1),
         lwd = c(.75, .25), col = gray(.5, .5))

points(x$tree.sim$far.test,
       x$tree.sim$hr.test,
       pch = 16,
       col = transparent("green", .8))



points(x$cart.sim$far.test, x$cart.sim$hr.test,
       pch = 16,
       col = transparent("red", .8))

points(x$rf.sim$far.test, x$rf.sim$hr.test,
       pch = 16,
       col = transparent("purple", .8))

points(median(x$tree.sim$far.test),
       median(x$tree.sim$hr.test),
       pch = "+",
       cex = 2,
       col = "white")

points(median(x$tree.sim$far.test),
       median(x$tree.sim$hr.test),
       pch = "+",
       cex = 1.5,
       col = transparent("green", 0))

points(median(x$cart.sim$far.test),
       median(x$cart.sim$hr.test),
       pch = "+",
       cex = 2,
       col = "white")

points(median(x$cart.sim$far.test),
       median(x$cart.sim$hr.test),
       pch = "+",
       cex = 1.5,
       col = "red")

points(median(x$rf.sim$far.test),
       median(x$rf.sim$hr.test),
       pch = "+",
       cex = 2,
       col = transparent("white", 0))

points(median(x$rf.sim$far.test),
       median(x$rf.sim$hr.test),
       pch = "+",
       cex = 1.5,
       col = 'purple')

}


par(ask = FALSE)

}
