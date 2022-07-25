#' Visualizes cue accuracies (as points in ROC space).
#'
#' @description \code{showcues} plots the cue accuracies of an \code{FFTrees} object
#' created by the \code{\link{FFTrees}} function (as points in ROC space).
#'
#' \code{showcues} is called when the main \code{\link{plot.FFTrees}} function is set to \code{what = "cues"}.
#'
#' @param x An \code{FFTrees} object created by the \code{\link{FFTrees}} function.
#' @param data A string indicating whether to show cue accuracies in training (\code{"train"}) or testing (\code{"test"}).
#' @param cue.accuracies An optional data frame specifying cue accuracies directly (without specifying an \code{FFTrees} object x).
#' @param main Main plot title (as character string).
#' @param top How many of the top cues should be highlighted (as an integer)?
#'
#' @return A plot showing cue accuracies (of an \code{FFTrees} object) (as points in ROC space).
#'
#' @examples
#' # Create fast-and-frugal trees (FFTs) for heart disease:
#' heart.fft <- FFTrees(formula = diagnosis ~ .,
#'                      data = heart.train,
#'                      data.test = heart.test,
#'                      main = "Heart Disease",
#'                      decision.labels = c("Healthy", "Diseased")
#'                      )
#'
#' # Show cue accuracies (in ROC space):
#' showcues(heart.fft,
#'          main = "Cue accuracies for predicting heart disease")
#'
#' @family plot functions
#'
#' @seealso
#' \code{\link{plot.FFTrees}} for plotting FFTs;
#' \code{\link{FFTrees}} for creating FFTs from data.
#'
#' @importFrom graphics text points abline legend mtext segments rect arrows axis par layout plot
#'
#' @export

showcues <- function(x = NULL,
                     data = "train",
                     cue.accuracies = NULL,
                     main = NULL,
                     top = 5) {

  # 0. Parameters: ------

  par0 <- par(no.readonly = TRUE)
  on.exit(par(par0), add = TRUE)

  palette <- rep(gray(.5, .5), length.out = top)  # colors


  # 1. Read inputs (into cue.df): ------

  if (is.null(x) == FALSE) {

    goal <- x$params$goal

    if (data == "train") {

      if (is.null(x$cues$stats$train)) {

        stop("There are no training statistics in this object.")

      }

      cue.df <- x$cues$stats$train

    }

    if (data == "test") {

      if (is.null(x$cues$stats$test)) {

        stop("There are no test statistics in this object.")

      }

      if (is.null(x$cues$stats$test) == FALSE) {

        cue.df <- x$cues$stats$test

      }
    }
  } # if (is.null(x).

  if (is.null(x) & is.null(cue.accuracies) == FALSE) {

    cue.df <- cue.accuracies

  }

  if (nrow(cue.df) < top) {

    top <- nrow(cue.df)

  }

  cue.df$rank <- rank(-cue.df$wacc, ties.method = "first")

  # Order by goal.threshold and change column order:
  ord_new <- order(cue.df$rank)

  cue.df <- cue.df[ord_new, ]

  cue.df$col <- rep(palette, length.out = nrow(cue.df)) # colors


  # 2. Setup main plot: ------

  # Main title:
  if (is.null(main)) {

    if (is.null(x$params$main)) {

      main <- "Individual cue accuracies"

    } else {

      main <- x$params$main

    }
  }

  # Plotting area:
  plot(1,
       xlim = c(0, 1), ylim = c(0, 1), type = "n",
       main = main,
       xlab = expression(1 - Specificity), # was: "1 - Specificity",
       ylab = "Sensitivity",
       yaxt = "n", xaxt = "n"
  )

  # Axes:
  axis(2, at = seq(0, 1, .1), las = 1, lwd = 0, lwd.ticks = 1)
  axis(1, at = seq(0, 1, .1), las = 1, lwd = 0, lwd.ticks = 1)

  # if(data == "test") {mtext("Testing", 3, line = .5, cex = 1)}
  # if(data == "train") {mtext("Training", 3, line = .5, cex = 1)}

  par("xpd" = FALSE)

  rect(-100, -100, 100, 100, col = gray(.96))
  abline(h = seq(0, 1, .1), lwd = c(1.5, .75), col = gray(1))
  abline(v = seq(0, 1, .1), lwd = c(1.5, .75), col = gray(1))
  abline(a = 0, b = 1, col = gray(.7), lty = 1)


  # 3. Plot cues (as points): ------

  #    a. Non-top cues: ----

  if (any(cue.df$rank > top)) {
    with(cue.df[cue.df$rank > top, ], points(1 - spec, sens, cex = 1))
    with(cue.df[cue.df$rank > top, ], text(1 - spec, sens,
                                           labels = rank,
                                           pos = 3,
                                           cex = .8,
                                           pch = 21,
                                           bg = "white"
    ))
  }

  #    b. Top-x cues: ----
  for (i in top:1) {

    with(
      cue.df[cue.df$rank == i, ],
      points(
        x = 1 - spec, y = sens,
        col = col,
        bg = gray(1, alpha = 1),
        lwd = 2, cex = 3, pch = 21
      )
    )

    with(cue.df[cue.df$rank == i, ], text(1 - spec, sens,
                                          labels = rank,
                                          # pos = 3,
                                          cex = 1
    ))

  } # for (i in top:1)..


  # 4. Bottom right label: ------

  location.df <- data.frame(
    element = c("points", "point.num", "cue.name", "cue.thresh", "sens", "spec", "wacc"),
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

  header.y  <- mean(c(cue.box.y1, .48))
  label.cex <- .8

  # Background:
  rect(cue.box.x0, cue.box.y0, cue.box.x1, .48,
       col = scales::alpha("white", .2),
       border = gray(.2)
  )

  #    a. Column labels: ----
  text(
    x = c(
      location.df[location.df$element == "point.num", ]$x.loc,
      mean(c(
        location.df$x.loc[location.df$element == "cue.name"],
        location.df$x.loc[location.df$element == "cue.thresh"]
      )),
      location.df$x.loc[location.df$element == "sens"],
      location.df$x.loc[location.df$element == "spec"],
      location.df$x.loc[location.df$element == "wacc"]
    ),
    y = header.y,
    labels = c("rank", "cue + thresh", "sens", "spec", "wacc"),
    font = 1, cex = label.cex
  )

  segments(cue.box.x0, cue.box.y1, 1.02, cue.box.y1)

  segments(rep(cue.box.x0, 4), cue.sep.y, rep(1.02, 4), cue.sep.y, lty = 3)

  #    b. Cue names, directions, and thresholds: ----

  # Points:
  points(
    x = rep(location.df[location.df$element == "point.num", ]$x.loc, top),
    y = cue.lab.y,
    pch = 21,
    col = cue.df$col[1:top],
    bg = "white",
    lwd = 2,
    cex = 3
  )

  # Cue numbers:
  text(
    x = rep(location.df[location.df$element == "point.num", ]$x.loc, top),
    y = cue.lab.y,
    labels = cue.df$rank[1:top],
    #  adj = subset(location.df, element == "point.num")$adj,
    cex = label.cex
  )

  # Cue names:
  text(
    x = rep(location.df[location.df$element == "cue.name", ]$x.loc, top),
    y = cue.lab.y,
    labels = cue.df$cue[cue.df$rank <= top],
    adj = location.df[location.df$element == "cue.name", ]$adj,
    cex = label.cex
  )

  # Cue directions & thresholds:
  thresh.text <- paste(cue.df$direction[cue.df$rank <= top], cue.df$threshold[cue.df$rank <= top])
  mnc <- 12  # max number of characters
  thresh.text[nchar(thresh.text) > mnc] <- paste(substr(thresh.text[nchar(thresh.text) > mnc], start = 1, stop = mnc), "...", sep = "") # truncate strings

  text(
    x = rep(location.df[location.df$element == "cue.thresh", ]$x.loc, top),
    y = cue.lab.y,
    labels = thresh.text,
    adj = location.df[location.df$element == "cue.thresh", ]$adj,
    cex = label.cex
  )


  #    c. Cue accuracy stats: ----

  # sens:
  text(
    x = rep(location.df[location.df$element == "sens", ]$x.loc, top),
    y = cue.lab.y,
    labels = round(cue.df$sens[cue.df$rank <= top], 2),
    adj = location.df[location.df$element == "sens", ]$adj,
    cex = label.cex
  )

  # spec:
  text(
    x = rep(location.df[location.df$element == "spec", ]$x.loc, top),
    y = cue.lab.y,
    labels = round(cue.df$spec[cue.df$rank <= top], 2),
    adj = location.df[location.df$element == "spec", ]$adj,
    cex = label.cex
  )

  # wacc:
  text(
    x = rep(location.df[location.df$element == "wacc", ]$x.loc, top),
    y = cue.lab.y,
    labels = round(cue.df$wacc[cue.df$rank <= top], 2),
    adj = location.df[location.df$element == "wacc", ]$adj,
    cex = label.cex
  )

  # Currently NO output!

} # showcues().


# ToDo: ------

# - allow using color palette again (for top cues).
#
# - fix bug: Currently no cue statistics for "test" data in FFTrees object.

# eof.
