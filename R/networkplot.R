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
                        nodesize = .7,
                        edgesize = 2,
                        mincon = 0) {


  # Remove lower bound connections

  edges <- edges[edges[,3] >= mincon,]


  {
    frequencies <- c()

    for (i in 1:nrow(edges)) {

      frequencies <- c(frequencies, rep(edges[i, 1], times = edges[i, 3]), rep(edges[i, 2], times = edges[i, 3]))

    }

    cnt <- table(frequencies)
  }


  g <- igraph::graph_from_data_frame(edges, directed = FALSE)

  v <- igraph::get.vertex.attribute(g)$name
  #l = layout_with_fr(g)
  l <- layout_with_dh(g)

  # Setup plotting region
  plot.new()
  plot.window(c(min(l[,1]),max(l[,1])),
              c(min(l[,2]),max(l[,2])))

  # Create edges

  for (i in 1:nrow(edges)) {

    is = c(which(v == edges[i, 1]), which(v == edges[i, 2]))
    lines(x = c(l[is[1], 1], l[is[2], 1]),
          y = c(l[is[1], 2], l[is[2], 2]),
          lwd = edges[i ,3] ^ edgesize - .3,
          lty = 1,
          col = 'grey50')

  }

  # Add points

  for(i in 1:length(v)){
    #  points(l[i,1] + .04, l[i, 2] - .04, cex = cnt[v[i]]**nodesize, pch = 16, col = 'grey50')

    freq.i <- cnt[names(cnt) == v[i]]


    relfreq.i <- freq.i / sum(cnt)

    points(x = l[i, 1],
           y = l[i, 2],
           cex = cnt[v[i]] * nodesize,
           pch = 21,
           col = "black",
           bg = gray(1 - relfreq.i))
  }

  # Add text

  for(i in 1:length(v)){
    text(x = l[i,1],
         y = l[i,2],
         labels = v[i],
         cex = cnt[v[i]] ^ .08 - .4)
  }

}
