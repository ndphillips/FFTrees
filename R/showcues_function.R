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
#' @param quiet Should user feedback messages be suppressed (as a list of 3 logical arguments)?
#' Default: \code{quiet = list(ini = TRUE, fin = FALSE, set = FALSE)}.
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
                     quiet = list(ini = TRUE, fin = FALSE, set = TRUE),  # a list of 3 Boolean args,
                     ...  # graphical parameters, passed to plot()
) {

  # 0. Parameters: ------

  par0 <- par(no.readonly = TRUE)
  on.exit(par(par0), add = TRUE)

  # Set color palette:
  palette <- rep(gray(.5, .5), length.out = top)
  # palette <- c("deepskyblue", "deeppink", "forestgreen", "darkorange", "gold",
  #              "firebrick", "grey50", "black")[1:top]

  # 1. Read user input (to set cue_df and goal): ------

  # cue_df: ----

  if (is.null(cue.accuracies) == FALSE) { # prioritize user input:

    cue_df <- cue.accuracies

    # Provide user feedback: ----

    if (any(sapply(quiet, isFALSE))) {
      cat(u_f_ini("Plotting cue.accuracies provided:\n"))
    }

  } else if (is.null(x) == FALSE) { # use object x info:

    # if (data == "train") {
    #
    #   if (is.null(x$cues$stats$train)) {
    #     stop("There are no cue training statistics in this object.")
    #   }
    #
    #   cue_df <- x$cues$stats$train
    #
    # } else if (data == "test") {
    #
    #   if (is.null(x$cues$stats$test)) {
    #     stop("There are no cue test statistics in this object.")
    #   }
    #
    #   cue_df <- x$cues$stats$test
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

      cue_df <- x$cues$stats$train

      # Provide user feedback: ----

      if (any(sapply(quiet, isFALSE))) {
        cat(u_f_ini("Plotting cue training statistics:\n"))
      }

    }

  } else { # cue_df not set:

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

  if (goal %in% names(cue_df) == FALSE){
    stop(paste0("The goal == '", goal, "' does not correspond to a variable in cue_df"))
  }

  if (nrow(cue_df) < top) { top <- nrow(cue_df) }


  # Sort cue_df by current goal: ------

  # # Bug: Always rank cue_df by goal == "wacc":
  # cue_df$rank <- rank(-cue_df$wacc, ties.method = "first")

  # Fix: Rank cue_df by current goal:
  goal_ix <- which(names(cue_df) == goal)
  goal_vc <- cue_df[[goal_ix]]  # values corresponding to current goal (as vector)
  cue_df$rank <- rank(-goal_vc, ties.method = "first")

  # Order by rank and change row order:
  ord_new <- order(cue_df$rank)
  cue_df  <- cue_df[ord_new, ]
  goal_vc <- cue_df[[goal_ix]]  # update sorted order, to report in table (below)


  # Compose subnote:
  if (goal == "wacc"){
    sens.w  <- x$params$sens.w
    subnote <- paste0("Cue accuracies ranked by ", goal, " (sens.w = ", round(sens.w, 2), ")")
  } else {
    subnote <- paste0("Cue accuracies ranked by ", goal)
  }

  # Provide user feedback:
  if (any(sapply(quiet, isFALSE))) { cat(u_f_msg(paste0("\u2014 ", subnote, "\n"))) }

  # Adjust color palette:
  cue_df$col <- rep(palette, length.out = nrow(cue_df))


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
  axis(1, at = seq(0, 1, .10), las = 1, lwd = 0, lwd.ticks = 1)  # x-axis + lbls
  axis(2, at = seq(0, 1, .10), las = 1, lwd = 0, lwd.ticks = 1)  # y-axis + lbls

  # Subtitle (as margin text):
  mtext(paste0(subnote, ":"), side = 3, line = 0.25, adj = 0, cex = .90)

  # if (data == "test")  {mtext("Testing",  3, line = .5, adj = 1, cex = .9)}
  # if (data == "train") {mtext("Training", 3, line = .5, adj = 1, cex = .9)}

  par("xpd" = FALSE)

  # Plot region and grid:
  rect(-100, -100, 100, 100, col = gray(.96))
  abline(h = seq(0, 1, .1), lwd = c(2, rep(1, 4)), col = gray(1))  # horizontal grid
  abline(v = seq(0, 1, .1), lwd = c(2, rep(1, 4)), col = gray(1))  # vertical grid

  # Diagonal:
  abline(a = 0, b = 1, col = gray(.60), lty = 1)


  # 3. Plot cues (as points): ------

  #    a. Non-top cues: ----

  if (any(cue_df$rank > top)) {

    with(cue_df[cue_df$rank > top, ], points(1 - spec, sens, cex = 1))
    with(cue_df[cue_df$rank > top, ], text(1 - spec, sens,
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
      cue_df[cue_df$rank == i, ],
      points(
        x = 1 - spec, y = sens,
        col = col,
        bg = gray(1, alpha = 1),
        lwd = 2, cex = 3, pch = 21
      )
    )

    with(cue_df[cue_df$rank == i, ], text(1 - spec, sens,
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

  cue_box_y_max <- .43
  cue_box_x0    <- .45
  cue_box_x1 <- 1.02
  cue_box_y0 <- 0

  if (top >= 5){
    cue_box_y1 <- .38
  } else {
    cue_box_y1 <- c(.18, .23, .28, .33)[top]
  }

  cue_lbl_h <- (cue_box_y1 - cue_box_y0) / top

  cue_lbl_y <- rev(seq((cue_box_y0 + cue_lbl_h / 2), (cue_box_y1 - cue_lbl_h / 2), length.out = top))

  if (top > 1){
    cue_sep_y <- seq(cue_box_y0 + cue_lbl_h, cue_box_y1 - cue_lbl_h, length.out = top - 1)
  } else {
    cue_sep_y <- 0
  }

  header_y  <- mean(c(cue_box_y1, cue_box_y_max))
  label_cex <- .80

  # Background:
  rect(cue_box_x0, cue_box_y0, cue_box_x1, cue_box_y_max,
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
    y = header_y,
    labels = c(c("Rank", "cue + thresh", "sens", "spec"), label_fin),
    font = 1, cex = label_cex
  )

  segments(cue_box_x0, cue_box_y1, 1.02, cue_box_y1)

  segments(rep(cue_box_x0, 4), cue_sep_y, rep(1.02, 4), cue_sep_y, lty = 3)

  #    b. Cue names, directions, and thresholds: ----

  # Points:
  points(
    x = rep(location.df[location.df$element == "point.num", ]$x.loc, top),
    y = cue_lbl_y,
    pch = 21,
    col = cue_df$col[1:top],
    bg = "white",
    lwd = 2,
    cex = 3
  )

  # Cue numbers:
  text(
    x = rep(location.df[location.df$element == "point.num", ]$x.loc, top),
    y = cue_lbl_y,
    labels = cue_df$rank[1:top],
    #  adj = subset(location.df, element == "point.num")$adj,
    cex = label_cex
  )

  # Cue names:
  text(
    x = rep(location.df[location.df$element == "cue.name", ]$x.loc, top),
    y = cue_lbl_y,
    labels = cue_df$cue[cue_df$rank <= top],
    adj = location.df[location.df$element == "cue.name", ]$adj,
    cex = label_cex
  )

  # Cue directions & thresholds:
  thresh.text <- paste(cue_df$direction[cue_df$rank <= top], cue_df$threshold[cue_df$rank <= top])
  mnc <- 12  # max number of characters
  thresh.text[nchar(thresh.text) > mnc] <- paste(substr(thresh.text[nchar(thresh.text) > mnc], start = 1, stop = mnc), "...", sep = "") # truncate strings

  text(
    x = rep(location.df[location.df$element == "cue.thresh", ]$x.loc, top),
    y = cue_lbl_y,
    labels = thresh.text,
    adj = location.df[location.df$element == "cue.thresh", ]$adj,
    cex = label_cex
  )


  #    c. Cue accuracy stats: ----

  # 1. sens:
  text(
    x = rep(location.df[location.df$element == "sens", ]$x.loc, top),
    y = cue_lbl_y,
    labels = scales::comma(cue_df$sens[cue_df$rank <= top], accuracy = .01),
    adj = location.df[location.df$element == "sens", ]$adj,
    cex = label_cex
  )

  # 2. spec:
  text(
    x = rep(location.df[location.df$element == "spec", ]$x.loc, top),
    y = cue_lbl_y,
    labels = scales::comma(cue_df$spec[cue_df$rank <= top], accuracy = .01),
    adj = location.df[location.df$element == "spec", ]$adj,
    cex = label_cex
  )

  # 3. wacc OR alt.goal:
  if (!goal %in% c("sens", "spec", "wacc")){ # report alt.goal values (from above):

    values_fin <- scales::comma(goal_vc[cue_df$rank <= top], accuracy = .01)  # use goal_vc values

  } else { # default:

    values_fin <- scales::comma(cue_df$wacc[cue_df$rank <= top], accuracy = .01)  # use "wacc" values

  }

  text(
    x = rep(location.df[location.df$element == "wacc", ]$x.loc, top),
    y = cue_lbl_y,
    labels = values_fin,
    adj = location.df[location.df$element == "wacc", ]$adj,
    cex = label_cex
  )


  # Currently NO output! ----


} # showcues().


# Done: ------

# Bug fixes:
# - Fixed bug: Use current goal for ranking/sorting cue_df
#   (and report as final column in summary table, iff different from current columns).
# - Fixed bug: There currently exist no cue statistics for "test" data in FFTrees object.
#   Hence, removed data argument and using x$cues$stats$train whenever x is provided.

# Extensions:
# - Added alt.goal option (to set an optional alternative goal for sorting cue accuracies);
# - Added quiet argument (to to provide option for hiding feedback messages);
# - Added margin text (to signal ranking criterion in plot).


# ToDo: ------

# - Rename showcues() to plot_cues()?
# - Allow setting and using color palette again (for top cues)?

# eof.
