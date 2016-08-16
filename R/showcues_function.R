#' Visualizes cue accuracies in a ROC space
#'
#' @param x An FFT object
#' @param which.data A string indicating whether or not to show training ("train") or testing ("test") cue accuracies
#' @param main Main plot description
#' @param top An integer indicating how many of the top cues to highlight
#' @param palette An optional vector of colors
#' @importFrom graphics text points abline legend mtext segments rect arrows axis par layout plot
#' @export
#'

showcues <- function(x = NULL,
                    which.data = "train",
                    main = NULL,
                    top = 5,
                    palette = c("#0C5BB07F", "#EE00117F", "#15983D7F", "#EC579A7F",
                                "#FA6B097F", "#149BED7F", "#A1C7207F", "#FEC10B7F", "#16A08C7F",
                                "#9A703E7F")) {


cue.df <- x$cue.accuracies

stat.names <- c("hi", "mi", "fa", "cr", "hr", "far", "v", "dprime")

if(which.data == "train") {

  cue.df <- cue.df[,c("cue.name", "cue.class", "level.threshold", "level.sigdirection", paste(stat.names, ".train", sep = ""))]
  names(cue.df)[names(cue.df) %in% paste(stat.names, ".train", sep = "")] <- stat.names

}

if(which.data == "test") {

  cue.df <- cue.df[,c("cue.name", "cue.class", "level.threshold", "level.sigdirection", paste(stat.names, ".test", sep = ""))]
  names(cue.df)[names(cue.df) %in% paste(stat.names, ".test", sep = "")] <- stat.names
}

rownames(cue.df) <- 1:nrow(cue.df)

if(nrow(cue.df) < top) {top <- nrow(cue.df)}


# GENERAL PLOTTING SPACE

if(is.null(main)) {main <- "Marginal Cue Accuracies"}

plot(1, xlim = c(0, 1), ylim  = c(0, 1), type = "n",
     xlab = "False Alarm Rate (FAR)", ylab = "Hit Rate (HR)", main = main,
     yaxt = "n", xaxt = "n"
)

axis(2, at = seq(0, 1, .1), las = 1)
axis(1, at = seq(0, 1, .1))


if(which.data == "test") {mtext("Testing data", 3, line = .5, cex = .8)}
if(which.data == "train") {mtext("Training data", 3, line = .5, cex = .8)}


rect(-100, -100, 100, 100, col = gray(.96))
abline(h = seq(0, 1, .1), lwd = c(2, 1), col = "white")
abline(v = seq(0, 1, .1), lwd = c(2, 1), col = "white")
abline(a = 0, b = 1)

# Non-top cues

cues.nontop <- cue.df[rank(-cue.df$v) > top,]

if(nrow(cues.nontop) > 0) {

with(cues.nontop, points(far, hr, cex = 1))

with(cues.nontop,
       text(far, hr, labels = row.names(cues.nontop),
            pos = 3, cex = .8))

}

# Top x cues

cues.top <- cue.df[rank(-cue.df$v) <= top,]
cues.top <- cues.top[order(cues.top$v),]

with(cues.top,
     points(far, hr, pch = 21, bg = palette, col = "white", cex = 2, lwd = 2))

with(cues.top,
     text(far, hr, labels = row.names(cues.top), pos = 3))


# Bottom right label

rect(.5, -.2, 1.02, .47,
     col = transparent("white", trans.val = .1),
     border = NA)

# Top Points
with(cues.top,
     points(rep(.52, top), seq(0, .4, length.out = top),
            pch = 21,
            bg = palette,
            col = "white", cex = 1.5)
     )

add.text <- function(labels, x, y.min, y.max, cex = .7, adj = 1) {

  text(rep(x, top),
       seq(y.min, y.max, length.out = top),
       labels,
       cex = cex,
       adj = adj
  )

}

# Cue numbers
add.text(row.names(cues.top), .54, 0, .4, adj = 0, cex = 1)

# Cue names
text(.66, .44, "cue", adj = 0, cex = .8)
add.text(substr(cues.top$cue.name, 1, 10), .65, 0, .4, cex = .8, adj = 1)

# Thresholds
thresh.text <- paste(cues.top$level.sigdirection, cues.top$level.threshold)
thresh.text[nchar(thresh.text) > 15] <- paste(substr(thresh.text[nchar(thresh.text) > 15], start = 1, stop = 12), "...", sep = "")
add.text(thresh.text, .67, 0, .4, cex = .8, adj = 0)

# HR and FAR stats
#add.text(round(cues.top$v, 2), .85, 0, .4, cex = .8)
#text(.85, .44, "v", adj = 1, cex = .8)

add.text(round(cues.top$hr, 2), .92, 0, .4, cex = .8)
add.text(round(cues.top$far, 2), .99, 0, .4, cex = .8)
text(.92, .44, "HR", adj = 1, cex = .8)
text(.99, .44, "FAR", adj = 1, cex = .8)


# connection lines
#
# segments(x0 = cues.top$far,
#          y0 = cues.top$hr,
#          x1 = rep(.62, top),
#          y1 = seq(0, .4, length.out = top),
#          col = gray(.9)
# )

}
