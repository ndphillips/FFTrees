#' Visualizes cue accuracies from an FFTrees object in a ROC space
#'
#' @param x An FFTrees object
#' @param data A string indicating whether or not to show training ("train") or testing ("test") cue accuracies
#' @param main Main plot description
#' @param top An integer indicating how many of the top cues to highlight
#' @param palette An optional vector of colors
#' @importFrom graphics text points abline legend mtext segments rect arrows axis par layout plot
#' @export
#'

showcues <- function(x = NULL,
                    data = "train",
                    main = NULL,
                    top = 5,
                    palette = c("#0C5BB07F", "#EE00117F", "#15983D7F", "#EC579A7F",
                                "#FA6B097F", "#149BED7F", "#A1C7207F", "#FEC10B7F", "#16A08C7F",
                                "#9A703E7F")) {
#
# x <- a
#   data = "train"
#   main = NULL
#   top = 5
#   palette = c("#0C5BB07F", "#EE00117F", "#15983D7F", "#EC579A7F",
#               "#FA6B097F", "#149BED7F", "#A1C7207F", "#FEC10B7F", "#16A08C7F",
#               "#9A703E7F")

if(data == "train") {

  cue.df <- x$cue.accuracies$train

}

if(data == "test") {

  if(is.null(x$cue.accuracies$test)) {stop("There are no test statistics")}
  if(is.null(x$cue.accuracies$test) == F) {cue.df <- x$cue.accuracies$test}

}

if(nrow(cue.df) < top) {top <- nrow(cue.df)}

# GENERAL PLOTTING SPACE

if(is.null(main)) {main <- "Marginal Cue Accuracies"}

plot(1, xlim = c(0, 1), ylim  = c(0, 1), type = "n",
     xlab = "False Alarm Rate (FAR)", ylab = "Hit Rate (HR)", main = main,
     yaxt = "n", xaxt = "n"
)

axis(2, at = seq(0, 1, .1), las = 1)
axis(1, at = seq(0, 1, .1))


if(data == "test") {mtext("Testing data", 3, line = .5, cex = .8)}
if(data == "train") {mtext("Training data", 3, line = .5, cex = .8)}

par("xpd" = F)

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
location.df <- data.frame(element = c("points", "point.num", "cue.name", "cue.thresh", "hr", "far"),
                          x.loc = c(.53, .55, .69, .7, .88, .96),
                          adj = c(.5, 0, 1, 0, .5, .5),
                          cex = c(1, 1, 1, 1, 1, 1)
                          )

y.loc <- seq(.05, .4, length.out = top)
header.y <- .45
label.cex <- .8

# Background
rect(.5, .0, 1.02, .48,
     col = transparent("white", trans.val = .1),
     border = gray(.2))

# Column labels

text(x = c(mean(c(location.df$x.loc[location.df$element == "cue.name"],
           location.df$x.loc[location.df$element == "cue.thresh"])),
           location.df$x.loc[location.df$element == "hr"],
           location.df$x.loc[location.df$element == "far"]),
     y = header.y,
     labels = c("Cue", "HR", "FAR"), font = 2, cex = label.cex)

# Points
points(x = rep(subset(location.df, element == "points")$x.loc, top),
      y = y.loc,
      pch = 21,
      bg = palette,
      col = "white", cex = 1.5)

# Cue numbers
text(x = rep(subset(location.df, element == "point.num")$x.loc, top),
     y = y.loc,
     labels = row.names(cues.top),
     adj = subset(location.df, element == "point.num")$adj,
     cex = label.cex)


# Cue names
text(x = rep(subset(location.df, element == "cue.name")$x.loc, top),
     y = y.loc,
     labels = cues.top$cue,
     adj = subset(location.df, element == "cue.name")$adj,
     cex = label.cex)

# Thresholds
thresh.text <- paste(cues.top$direction, cues.top$threshold)
thresh.text[nchar(thresh.text) > 10] <- paste(substr(thresh.text[nchar(thresh.text) > 10], start = 1, stop = 10), "...", sep = "")


text(x = rep(subset(location.df, element == "cue.thresh")$x.loc, top),
     y = y.loc,
     labels = thresh.text,
     adj = subset(location.df, element == "cue.thresh")$adj,
     cex = label.cex)

# HR
text(x = rep(subset(location.df, element == "hr")$x.loc, top),
     y = y.loc,
     labels = round(cues.top$hr, 2),
     adj = subset(location.df, element == "hr")$adj,
     cex = label.cex)

# HR

text(x = rep(subset(location.df, element == "far")$x.loc, top),
     y = y.loc,
     labels = round(cues.top$far, 2),
     adj = subset(location.df, element == "far")$adj,
     cex = label.cex)

}
