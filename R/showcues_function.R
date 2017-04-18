#' Visualizes cue accuracies from an FFTrees object in a ROC space
#'
#' @param x An FFTrees object
#' @param data A string indicating whether or not to show training ("train") or testing ("test") cue accuracies
#' @param cue.accuracies dataframe. An optional dataframe specifying cue accuracies directly (without specifying an FFTrees object x)
#' @param main Main plot description
#' @param top An integer indicating how many of the top cues to highlight
#' @importFrom graphics text points abline legend mtext segments rect arrows axis par layout plot
#' @export
#'

showcues <- function(x = NULL,
                     data = "train",
                     cue.accuracies = NULL,
                     main = NULL,
                     top = 5) {

  palette <- "basel"

  if(palette %in% yarrr::piratepal("names")) {

    palette <- yarrr::piratepal(palette, length.out = top, trans = .1)

  } else {

    palette <- rep(palette, length.out = top)

  }


if(is.null(x) == FALSE) {

goal <- x$params$goal

if(data == "train") {

  cue.df <- x$cue.accuracies$train

}

if(data == "test") {

  if(is.null(x$cue.accuracies$test)) {stop("There are no test statistics")}
  if(is.null(x$cue.accuracies$test) == FALSE) {cue.df <- x$cue.accuracies$test}

}

}

if(is.null(x) & is.null(cue.accuracies) == FALSE) {

  cue.df <- cue.accuracies

}


if(nrow(cue.df) < top) {top <- nrow(cue.df)}

cue.df$rank <- rank(-cue.df$bacc, ties.method = "first")

cue.df <- cue.df[order(cue.df$rank),]

cue.df$col <- rep(palette, length.out = nrow(cue.df))

# GENERAL PLOTTING SPACE

if(is.null(main)) {main <- "Individual Cue Accuracies"}

plot(1, xlim = c(0, 1), ylim  = c(0, 1), type = "n",
     xlab = "1 - Specificity", ylab = "Sensitivity", main = main,
     yaxt = "n", xaxt = "n"
)

axis(2, at = seq(0, 1, .1), las = 1, lwd = 0, lwd.ticks = 1)
axis(1, at = seq(0, 1, .1), las = 1, lwd = 0, lwd.ticks = 1)


#if(data == "test") {mtext("Testing", 3, line = .5, cex = 1)}
#if(data == "train") {mtext("Training", 3, line = .5, cex = 1)}

par("xpd" = FALSE)

 rect(-100, -100, 100, 100, col = gray(.96))
 abline(h = seq(0, 1, .1), lwd = c(1.5, .75), col = gray(1))
 abline(v = seq(0, 1, .1), lwd = c(1.5, .75), col = gray(1))
 abline(a = 0, b = 1, col = gray(.7), lty = 1)

# Non-top cues

if(any(cue.df$rank > top)) {

with(cue.df[cue.df$rank > top,], points(1 - spec, sens, cex = 1))
with(cue.df[cue.df$rank > top,],  text(1 - spec, sens,
                                      labels = rank,
                                      pos = 3,
                                      cex = .8,
                                      pch = 21,
                                      bg = "white"))
}

# Top x cues

for (i in top:1) {

  with(cue.df[cue.df$rank == i,] ,
      points(x = 1 - spec, y = sens,
             col = col,
             bg = gray(1, alpha = 1),
             lwd = 2, cex = 3, pch = 21)
      )

  with(cue.df[cue.df$rank == i,], text(1 - spec, sens,
                                       labels = rank,
                                      # pos = 3,
                                       cex = 1))

}


# Bottom right label
location.df <- data.frame(element = c("points", "point.num", "cue.name", "cue.thresh", "sens", "spec", "bacc"),
                          x.loc = c(.5, .5, .67, .68, .83, .9, .97),
                          adj = c(.5, 0, 1, 0, .5, .5, .5),
                          cex = c(1, 1, 1, 1, 1, 1, 1)
                          )

cue.box.x0 <- .45
cue.box.x1 <- 1.02
cue.box.y0 <- 0
cue.box.y1 <- .43

cue.lab.h <- (cue.box.y1 - cue.box.y0) / top

cue.lab.y <- rev(seq((cue.box.y0 + cue.lab.h / 2), cue.box.y1 - cue.lab.h / 2, length.out = top))
cue.sep.y <- seq(cue.box.y0 + cue.lab.h, cue.box.y1 - cue.lab.h, length.out = top - 1)

header.y <- mean(c(cue.box.y1, .48))
label.cex <- .8

# Background
rect(cue.box.x0, cue.box.y0, cue.box.x1, .48,
     col = transparent("white", trans.val = .1),
     border = gray(.2))

# Column labels

text(x = c(location.df[location.df$element == "point.num",]$x.loc,
           mean(c(location.df$x.loc[location.df$element == "cue.name"],
           location.df$x.loc[location.df$element == "cue.thresh"])),
           location.df$x.loc[location.df$element == "sens"],
           location.df$x.loc[location.df$element == "spec"],
           location.df$x.loc[location.df$element == "bacc"]),
     y = header.y,
     labels = c("rank", "cue + thresh", "sens", "spec", "bacc"),
     font = 1, cex = label.cex)

segments(cue.box.x0, cue.box.y1, 1.02, cue.box.y1)

segments(rep(cue.box.x0, 4), cue.sep.y, rep(1.02, 4), cue.sep.y, lty = 3)


# Points
points(x = rep(location.df[location.df$element == "point.num",]$x.loc, top),
      y = cue.lab.y,
      pch = 21,
      col = cue.df$col[1:top],
      bg = "white",
      lwd = 2,
      cex = 3)

# Cue numbers
text(x = rep(location.df[location.df$element == "point.num",]$x.loc, top),
     y = cue.lab.y,
     labels = cue.df$rank[1:top],
   #  adj = subset(location.df, element == "point.num")$adj,
     cex = label.cex)


# Cue names
text(x = rep(location.df[location.df$element == "cue.name",]$x.loc, top),
     y = cue.lab.y,
     labels = cue.df$cue[cue.df$rank <= top],
     adj = location.df[location.df$element == "cue.name",]$adj,
     cex = label.cex)

# Thresholds
thresh.text <- paste(cue.df$direction[cue.df$rank <= top], cue.df$threshold[cue.df$rank <= top])
thresh.text[nchar(thresh.text) > 10] <- paste(substr(thresh.text[nchar(thresh.text) > 10], start = 1, stop = 10), "...", sep = "")


text(x = rep(location.df[location.df$element == "cue.thresh",]$x.loc, top),
     y = cue.lab.y,
     labels = thresh.text,
     adj = location.df[location.df$element == "cue.thresh",]$adj,
     cex = label.cex)

# HR
text(x = rep(location.df[location.df$element == "sens",]$x.loc, top),
     y = cue.lab.y,
     labels = round(cue.df$sens[cue.df$rank <= top], 2),
     adj = location.df[location.df$element == "sens",]$adj,
     cex = label.cex)

# FAR

text(x = rep(location.df[location.df$element == "spec",]$x.loc, top),
     y = cue.lab.y,
     labels = round(cue.df$spec[cue.df$rank <= top], 2),
     adj = location.df[location.df$element == "spec",]$adj,
     cex = label.cex)

# v

text(x = rep(location.df[location.df$element == "bacc",]$x.loc, top),
     y = cue.lab.y,
     labels = round(cue.df$bacc[cue.df$rank <= top], 2),
     adj = location.df[location.df$element == "bacc",]$adj,
     cex = label.cex)


}
