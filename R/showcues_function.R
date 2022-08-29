#' Visualize cue accuracies (as points in ROC space)
#'
#' @description \code{showcues} plots the cue accuracies of an \code{FFTrees} object
#' created by the \code{\link{FFTrees}} function (as points in ROC space).
#'
#' If the optional arguments \code{cue.accuracies} and \code{alt.goal} are specified,
#' their values take precedence over the corresponding settings of an \code{FFTrees} object \code{x}
#' (but do not change \code{x}).
#'
#' \code{showcues} is called when the main \code{\link{plot.FFTrees}} function is set to \code{what = "cues"}.
#'
#' @param x An \code{FFTrees} object created by the \code{\link{FFTrees}} function.
#' @param cue.accuracies An optional data frame specifying cue accuracies directly (without specifying \code{FFTrees} object \code{x}).
#' @param alt.goal An optional alternative goal to sort the current cue accuracies (without using the goal of \code{FFTrees} object \code{x}).
#' @param main A main plot title (as character string).
#' @param top How many of the top cues should be highlighted (as an integer)?
#' @param quiet Should user feedback messages be printed (as logical)?
#' Default: \code{quiet = FALSE} (i.e., show messages).
#'
#' @param ... Graphical parameters (passed to \code{\link{plot}}).
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
#'          main = "Predicting heart disease")
#'
#' @family plot functions
#'
#' @seealso
#' \code{\link{print.FFTrees}} for printing FFTs;
#' \code{\link{plot.FFTrees}} for plotting FFTs;
#' \code{\link{summary.FFTrees}} for summarizing FFTs;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @importFrom graphics abline arrows axis layout legend mtext par points plot rect segments text
#' @importFrom scales comma
#'
#' @export

showcues <- function(x = NULL,
                     # data = "train", #' @param data A string indicating whether to show cue accuracies in training (\code{"train"}) or testing (\code{"test"}).
                     # Note: Removed argument (data = "train" vs. "test"), as cue accuracies are currently only computed for training data (see GitHub issue #63).
                     cue.accuracies = NULL,
                     alt.goal = NULL,  # alternative goal (takes local priority over x$params$goal)
                     main = NULL,
                     top = 5,
                     quiet = FALSE,
                     ...  # graphical parameters, passed to plot()
) {

  # 0. Parameters: ------

  par0 <- par(no.readonly = TRUE)
  on.exit(par(par0), add = TRUE)

  # Set color palette:
  palette <- rep(gray(.5, .5), length.out = top)
  # palette <- c("deepskyblue", "deeppink", "forestgreen", "darkorange", "gold",
  #              "firebrick", "grey50", "black")[1:top]

  # 1. Read user input (to set cue.df and goal): ------

  # cue.df: ----

  if (is.null(cue.accuracies) == FALSE) { # prioritize user input:

    cue.df <- cue.accuracies

    if (quiet == FALSE){
      message("Using cue.accuracies provided:")  # user feedback
    }

  } else if (is.null(x) == FALSE) { # use object x info:

    # if (data == "train") {
    #
    #   if (is.null(x$cues$stats$train)) {
    #     stop("There are no cue training statistics in this object.")
    #   }
    #
    #   cue.df <- x$cues$stats$train
    #
    # } else if (data == "test") {
    #
    #   if (is.null(x$cues$stats$test)) {
    #     stop("There are no cue test statistics in this object.")
    #   }
    #
    #   cue.df <- x$cues$stats$test
    #
    # } else { # other data values:
    #
    #   stop("The data argument must be set to either 'train' or 'test'.")
    #
    # }

    # Use cue training statistics:
    if (is.null(x$cues$stats$train)) {

      stop("There are no cue training statistics for this object.")

    } else {

      cue.df <- x$cues$stats$train

      if (quiet == FALSE){
        message("Using cue training statistics of object x:")  # user feedback
      }

    }

  } else { # cue.df not set:

    stop("No data frame of cue accuracies was found.")

  }


  # goal: ----

  if (!is.null(alt.goal)){ # prioritize user input:

    goal <- alt.goal

  } else if (is.null(x) == FALSE) { # # use object info:

    goal <- x$params$goal

  } else { # set default:

    goal <- "wacc"

  }


  # Verify values: ----

  if (goal %in% names(cue.df) == FALSE){
    stop(paste0("The goal == '", goal, "' does not correspond to a variable in cue.df"))
  }

  if (nrow(cue.df) < top) { top <- nrow(cue.df) }


  # Sort cue.df by current goal: ------

  # # Bug: Always rank cue.df by goal == "wacc":
  # cue.df$rank <- rank(-cue.df$wacc, ties.method = "first")

  # Fix: Rank cue.df by current goal:
  goal_ix <- which(names(cue.df) == goal)
  goal_vc <- cue.df[[goal_ix]]  # values corresponding to current goal (as vector)
  cue.df$rank <- rank(-goal_vc, ties.method = "first")

  # Order by rank and change row order:
  ord_new <- order(cue.df$rank)
  cue.df  <- cue.df[ord_new, ]
  goal_vc <- cue.df[[goal_ix]]  # update sorted order, to report in table (below)

  # Feedback note/subtitle:
  if (goal == "wacc"){
    sens.w <- x$params$sens.w
    subnote <- paste0("Cue accuracies ranked by ", goal, " (sens.w = ", round(sens.w, 2), ")")
  } else {
    subnote <- paste0("Cue accuracies ranked by ", goal)
  }

  # User feedback:
  if (quiet == FALSE){ message(subnote) }

  # Adjust color palette:
  cue.df$col <- rep(palette, length.out = nrow(cue.df))


  # 2. Setup main plot: ------

  # Main title:
  if (is.null(main)) {

    if (is.null(x$params$main)) { # default title:

      main <- "Individual cue accuracies"

    } else { # use main:

      main <- x$params$main

    }
  }

  # Plotting area:
  plot(1,
       xlim = c(0, 1), ylim = c(0, 1), type = "n",
       main = main,
       xlab = expression(1 - Specificity),
       ylab = "Sensitivity",
       yaxt = "n", xaxt = "n",
       ...  # other graphical parameters
  )

  # Axes:
  axis(1, at = seq(0, 1, .10), las = 1, lwd = 0, lwd.ticks = 1) # x-axis + lbls
  axis(2, at = seq(0, 1, .10), las = 1, lwd = 0, lwd.ticks = 1) # y-axis + lbls

  # Subtitle (as margin text):
  mtext(paste0(subnote, ":"), side = 3, line = 0.25, adj = 0, cex = .90)

  # if (data == "test")  {mtext("Testing",  3, line = .5, adj = 1, cex = .9)}
  # if (data == "train") {mtext("Training", 3, line = .5, adj = 1, cex = .9)}

  par("xpd" = FALSE)

  # Plot region and grid:
  rect(-100, -100, 100, 100, col = gray(.96))
  abline(h = seq(0, 1, .1), lwd = c(2, rep(1, 4)), col = gray(1)) # horizontal grid
  abline(v = seq(0, 1, .1), lwd = c(2, rep(1, 4)), col = gray(1)) # vertical grid

  # Diagonal:
  abline(a = 0, b = 1, col = gray(.60), lty = 1)


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
    x.loc = c(.50, .50, .67, .68, .83, .90, .97),
    adj = c(.5, 0, 1, 0, .5, .5, .5),
    cex = c(1, 1, 1, 1, 1, 1, 1)
  )

  cue.box.y_max <- .43
  cue.box.x0    <- .45
  cue.box.x1 <- 1.02
  cue.box.y0 <- 0

  if (top >= 5){
    cue.box.y1 <- .38
  } else {
    cue.box.y1 <- c(.18, .23, .28, .33)[top]
  }

  cue.lab.h <- (cue.box.y1 - cue.box.y0) / top

  cue.lab.y <- rev(seq((cue.box.y0 + cue.lab.h / 2), (cue.box.y1 - cue.lab.h / 2), length.out = top))

  if (top > 1){
    cue.sep.y <- seq(cue.box.y0 + cue.lab.h, cue.box.y1 - cue.lab.h, length.out = top - 1)
  } else {
    cue.sep.y <- 0
  }

  header.y  <- mean(c(cue.box.y1, cue.box.y_max))
  label.cex <- .80

  # Background:
  rect(cue.box.x0, cue.box.y0, cue.box.x1, cue.box.y_max,
       col = gray(.98),
       border = gray(.20)
  )

  #    a. Column labels: ----

  if (!goal %in% c("sens", "spec", "wacc")){ # report on current goal:

    label_fin <- goal

  } else { # default:

    label_fin <- "wacc"

  }

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
    labels = c(c("Rank", "cue + thresh", "sens", "spec"), label_fin),
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

  # 1. sens:
  text(
    x = rep(location.df[location.df$element == "sens", ]$x.loc, top),
    y = cue.lab.y,
    labels = scales::comma(cue.df$sens[cue.df$rank <= top], accuracy = .01),
    adj = location.df[location.df$element == "sens", ]$adj,
    cex = label.cex
  )

  # 2. spec:
  text(
    x = rep(location.df[location.df$element == "spec", ]$x.loc, top),
    y = cue.lab.y,
    labels = scales::comma(cue.df$spec[cue.df$rank <= top], accuracy = .01),
    adj = location.df[location.df$element == "spec", ]$adj,
    cex = label.cex
  )

  # 3. wacc OR alt.goal:
  if (!goal %in% c("sens", "spec", "wacc")){ # report alt.goal values (from above):

    values_fin <- scales::comma(goal_vc[cue.df$rank <= top], accuracy = .01)  # use goal_vc values

  } else { # default:

    values_fin <- scales::comma(cue.df$wacc[cue.df$rank <= top], accuracy = .01)  # use "wacc" values

  }

  text(
    x = rep(location.df[location.df$element == "wacc", ]$x.loc, top),
    y = cue.lab.y,
    labels = values_fin,
    adj = location.df[location.df$element == "wacc", ]$adj,
    cex = label.cex
  )



  # Currently NO output! ----

} # showcues().


# Done: ------

# Bug fixes:
# - Fixed bug: Use current goal for ranking/sorting cue.df
#   (and report as final column in summary table, iff different from current columns).
# - Fixed bug: There currently exist no cue statistics for "test" data in FFTrees object.
#   Hence, removed data argument and using x$cues$stats$train whenever x is provided.

# Extensions:
# - Added alt.goal option (to set an optional alternative goal for sorting cue accuracies);
# - Added quiet argument (to to provide option for hiding feedback messages);
# - Added margin text (to signal ranking criterion in plot).


# ToDo: ------

# - Allow setting and using color palette again (for top cues)?

# eof.
