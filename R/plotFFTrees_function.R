#' Plots an FFTrees object.
#'
#' @description Plots an FFTrees object created by the FFTrees() function.
#' @param x A FFTrees object created from \code{"FFTrees()"}
#' @param data One of two strings 'train' or 'test'. In this case, the corresponding dataset in the x object will be used.
#' @param what string. What should be plotted? \code{'tree'} (the default) shows one tree (specified by \code{'tree'}). \code{'cues'} shows the marginal accuracy of cues in an ROC space, \code{"roc"} shows an roc curve of the tree(s)
#' @param tree integer. An integer indicating which tree to plot (only valid when the tree argument is non-empty). To plot the best training (or test) tree with respect to the \code{goal} specified during FFT construction, use "best.train" or "best.test"
#' @param main character. The main plot label.
#' @param hlines logical. Should horizontal panel separation lines be shown?
#' @param cue.labels character. An optional string of labels for the cues / nodes.
#' @param decision.labels character. A string vector of length 2 indicating the content-specific name for noise and signal cases.
#' @param cue.cex numeric. The size of the cue labels.
#' @param threshold.cex numeric. The size of the threshold labels.
#' @param decision.cex numeric. The size of the decision labels.
#' @param comp logical. Should the performance of competitive algorithms (e.g.; logistic regression, random forests etc.) be shown in the ROC plot (if available?)
#' @param stats logical. Should statistical information be plotted? If \code{FALSE}, then only the tree (without any reference to statistics) will be plotted.
#' @param show.header,show.tree,show.confusion,show.levels,show.roc,show.icons,show.iconguide logical. Logical arguments indicating which specific elements of the plot to show.
#' @param label.tree,label.performance string. Optional arguments to define lables for the tree and performance section(s).
#' @param n.per.icon Number of cases per icon
#' @param which.tree deprecated argument, only for backwards compatibility, use \code{"tree"} instead.
#' @param level.type string. How should bottom levels be drawn? Can be \code{"bar"} or \code{"line"}
#' @param decision.names deprecated arguments.
#' @param ... Currently ignored.
#' @importFrom stats anova predict formula model.frame
#' @importFrom graphics text points abline legend mtext segments rect arrows axis par layout plot
#' @importFrom grDevices gray col2rgb rgb
#' @export
#' @return A plot vizualizing an FFT
#' @examples
#'
#' # Create FFTrees of the heart disease data
#' heart.fft <- FFTrees(
#'   formula = diagnosis ~ .,
#'   data = heartdisease
#' )
#'
#' # Visualise the tree
#' plot(heart.fft,
#'   main = "Heart Disease Diagnosis",
#'   decision.labels = c("Absent", "Present")
#' )
#'
#'
#' # See the vignette for more details
#' vignette("FFTrees_plot", package = "FFTrees")
#'
plot.FFTrees <- function(x = NULL,
                         data = "train",
                         what = "tree",
                         tree = 1,
                         main = NULL,
                         hlines = TRUE,
                         cue.labels = NULL,
                         decision.labels = NULL,
                         cue.cex = NULL,
                         threshold.cex = NULL,
                         decision.cex = 1,
                         comp = TRUE,
                         stats = TRUE,
                         show.header = NULL,
                         show.tree = NULL,
                         show.confusion = NULL,
                         show.levels = NULL,
                         show.roc = NULL,
                         show.icons = NULL,
                         show.iconguide = NULL,
                         label.tree = NULL,
                         label.performance = NULL,
                         n.per.icon = NULL,
                         which.tree = NULL,
                         level.type = "bar",
                         decision.names = NULL,
                         ...) {

  # Check for invalid or missing arguments

  # Input validation
  {
    par0 <- par(no.readonly = TRUE)
    on.exit(par(par0), add = TRUE)

    if (what %in% c("cues", "tree", "roc") == FALSE) {
      stop("what must be either 'cues', 'tree', or 'roc'")
    }

    if (is.null(decision.names) == FALSE) {
      warning("decision.names is deprecated, use decision.lables instead")

      decision.labels <- decision.names
    }
  }

  # If what == cues, then send inputs to showcues()
  if (what == "cues") {
    showcues(x = x, data = data, main = main)
  }

  if (what != "cues") {

    # Determine layout
    {
      if (what == "tree") {
        if (stats == TRUE) {
          if (is.null(show.header)) {
            show.header <- TRUE
          }
          if (is.null(show.tree)) {
            show.tree <- TRUE
          }
          if (is.null(show.confusion)) {
            show.confusion <- TRUE
          }
          if (is.null(show.levels)) {
            show.levels <- TRUE
          }
          if (is.null(show.roc)) {
            show.roc <- TRUE
          }
          if (is.null(show.icons)) {
            show.icons <- TRUE
          }
          if (is.null(show.iconguide)) {
            show.iconguide <- TRUE
          }
        }

        if (stats == FALSE) {
          if (is.null(show.header)) {
            show.header <- FALSE
          }
          if (is.null(show.tree)) {
            show.tree <- TRUE
          }
          if (is.null(show.confusion)) {
            show.confusion <- FALSE
          }
          if (is.null(show.levels)) {
            show.levels <- FALSE
          }
          if (is.null(show.roc)) {
            show.roc <- FALSE
          }
          if (is.null(show.icons)) {
            show.icons <- FALSE
          }
          if (is.null(show.iconguide)) {
            show.iconguide <- FALSE
          }
        }
      }

      if (what == "roc") {
        show.header <- FALSE
        show.tree <- FALSE
        show.confusion <- FALSE
        show.levels <- FALSE
        show.roc <- TRUE
        show.icons <- FALSE
        show.top <- FALSE
      }


      # Top, middle, bottom
      if (show.header & show.tree & (show.confusion | show.levels | show.roc)) {
        show.top <- TRUE
        show.middle <- TRUE
        show.bottom <- TRUE

        layout(matrix(1:3, nrow = 3, ncol = 1),
          widths = c(6),
          heights = c(1.2, 3, 1.8)
        )
      }

      # Top and middle
      if (show.header & show.tree & (show.confusion == FALSE & show.levels == FALSE & show.roc == FALSE)) {
        show.top <- TRUE
        show.middle <- TRUE
        show.bottom <- FALSE

        layout(matrix(1:2, nrow = 2, ncol = 1),
          widths = c(6),
          heights = c(1.2, 3)
        )
      }

      # Middle and bottom
      if (show.header == FALSE & show.tree & (show.confusion | show.levels | show.roc)) {
        show.top <- FALSE
        show.middle <- TRUE
        show.bottom <- TRUE

        layout(matrix(1:2, nrow = 2, ncol = 1),
          widths = c(6),
          heights = c(3, 1.8)
        )
      }

      # Middle
      if (show.header == FALSE & show.tree & (show.confusion == FALSE & show.levels == FALSE & show.roc == FALSE)) {
        show.top <- FALSE
        show.middle <- TRUE
        show.bottom <- FALSE

        layout(matrix(1:1, nrow = 1, ncol = 1),
          widths = c(6),
          heights = c(3)
        )
      }

      # Bottom
      if (show.header == FALSE & show.tree == FALSE) {
        show.top <- FALSE
        show.middle <- FALSE
        show.bottom <- TRUE

        nplots <- show.confusion + show.levels + show.roc

        layout(matrix(1:nplots, nrow = 1, ncol = nplots),
          widths = c(3 * nplots),
          heights = c(3)
        )
      }
    }

    # -------------------------
    # Setup data
    # --------------------------
    {
      # Extract important parameters from x
      goal <- x$params$goal

      if (is.null(decision.labels)) {
        if (("decision.labels" %in% names(x$params))) {
          decision.labels <- x$params$decision.labels
        } else {
          decision.labels <- c(0, 1)
        }
      }

      if (is.null(main)) {
        if (("main" %in% names(x$params))) {
          if (is.null(x$params$main)) {
            if (show.header) {
              main <- "Data"
            } else {
              main <- ""
            }
          } else {
            main <- x$params$main
          }
        } else {
          if (inherits(data, "character")) {
            if (data == "train") {
              main <- "Data (Training)"
            }
            if (data == "test") {
              main <- "Data (Testing)"
            }
          }

          if (inherits(data, "data.frame")) {
            main <- "Test Data"
          }
        }
      }




      # Check for problems and depreciated arguments
      {
        if (is.null(which.tree) == FALSE) {
          warning("The which.tree argument is deprecated and is now just called tree. Please use tree from now on to avoid this message.")

          tree <- which.tree
        }

        if (!inherits(x, "FFTrees")) {
          stop("You did not include a valid FFTrees class object or specify the tree directly with level.names, level.classes (etc.). Either create a valid FFTrees object with FFTrees() or specify the tree directly.")
        }

        if (tree == "best.test" & is.null(x$tree.stats$test)) {
          message("You wanted to plot the best test tree (tree = 'best.test') but there were no test data, I'll plot the best training tree instead")

          tree <- "best.train"
        }

        if (is.numeric(tree) & (tree %in% 1:x$trees$n) == FALSE) {
          stop(paste("You asked for a tree that does not exist. This object has", x$trees$n, "trees"))
        }

        if (inherits(data, "character")) {
          if (data == "test" & is.null(x$trees$stats$test)) {
            stop("You asked to plot the test data but there are no test data in the FFTrees object")
          }
        }
      }


      # DEFINE PLOTTING TREE

      if (tree == "best.train") {
        tree <- x$trees$best$train
      }

      if (tree == "best.test") {
        tree <- x$trees$best$test
      }

      # DEFINE CRITICAL OBJECTS

      decision.v <- x$trees$decisions[[data]][[tree]]$decision
      tree.stats <- x$trees$stats[[data]]
      level.stats <- x$trees$level_stats[[data]][x$trees$level_stats[[data]]$tree == tree, ]


      n.exemplars <- nrow(x$data[[data]])
      n.pos <- sum(x$data[[data]][[x$criterion_name]])
      n.neg <- sum(x$data[[data]][[x$criterion_name]] == FALSE)
      mcu <- x$trees$stats[[data]]$mcu[tree]
      crit.br <- mean(x$data[[data]][[x$criterion_name]])


      final.stats <- tree.stats[tree, ]

      # ADD LEVEL STATISTICS
      n.levels <- nrow(level.stats)

      # Add marginal classification statistics to level.stats
      level.stats$hi.m <- NA
      level.stats$mi.m <- NA
      level.stats$fa.m <- NA
      level.stats$cr.m <- NA

      for (i in 1:n.levels) {
        if (i == 1) {
          level.stats$hi.m[1] <- level.stats$hi[1]
          level.stats$mi.m[1] <- level.stats$mi[1]
          level.stats$fa.m[1] <- level.stats$fa[1]
          level.stats$cr.m[1] <- level.stats$cr[1]
        }

        if (i > 1) {
          level.stats$hi.m[i] <- level.stats$hi[i] - level.stats$hi[i - 1]
          level.stats$mi.m[i] <- level.stats$mi[i] - level.stats$mi[i - 1]
          level.stats$fa.m[i] <- level.stats$fa[i] - level.stats$fa[i - 1]
          level.stats$cr.m[i] <- level.stats$cr[i] - level.stats$cr[i - 1]
        }
      }
    }

    # -------------------------
    # Define plotting parameters
    # --------------------------
    {{
      # Panels
      panel.line.lwd <- 1
      panel.line.col <- gray(0)
      panel.line.lty <- 1


      # Some general parameters

      ball.col <- c(gray(0), gray(0))
      ball.bg <- c(gray(1), gray(1))
      ball.pch <- c(21, 24)
      ball.cex <- c(1, 1)
      error.col <- "red"
      correct.col <- "green"
      max.label.length <- 100
      def.par <- par(no.readonly = TRUE)
      ball.box.width <- 10
      label.box.height <- 2
      label.box.width <- 5


      # Define cue labels
      {
        if (is.null(cue.labels)) {
          cue.labels <- level.stats$cue
        }

        # Trim labels
        cue.labels <- strtrim(cue.labels, max.label.length)
      }


      # Node Segments
      segment.lty <- 1
      segment.lwd <- 1

      continue.segment.lwd <- 1
      continue.segment.lty <- 1

      exit.segment.lwd <- 1
      exit.segment.lty <- 1

      decision.node.cex <- 4
      exit.node.cex <- 4
      panel.title.cex <- 2


      # Classification table
      classtable.lwd <- 1

      # ROC
      roc.lwd <- 1
      roc.border.col <- gray(0)

      # Label sizes
      {
        # Set cue label size

        if (is.null(cue.cex)) {
          cue.cex <- c(1.5, 1.5, 1.25, 1, 1, 1)
        } else {
          if (length(cue.cex) < 6) {
            cue.cex <- rep(cue.cex, length.out = 6)
          }
        }

        # Set break label size

        if (is.null(threshold.cex)) {
          threshold.cex <- c(1.5, 1.5, 1.25, 1, 1, 1)
        } else {
          if (length(threshold.cex) < 6) {
            threshold.cex <- rep(threshold.cex, length.out = 6)
          }
        }
      }

      if (show.top & show.middle & show.bottom) {
        plotting.parameters.df <- data.frame(
          n.levels = 1:6,
          plot.height = c(10, 12, 15, 19, 23, 27),
          plot.width = c(14, 16, 20, 24, 28, 34),
          label.box.text.cex = cue.cex,
          break.label.cex = threshold.cex
        )
      } else if (show.top == FALSE & show.middle & show.bottom == FALSE) {
        plotting.parameters.df <- data.frame(
          n.levels = 1:6,
          plot.height = c(10, 12, 15, 19, 23, 25),
          plot.width = c(14, 16, 20, 24, 28, 32) * 1,
          label.box.text.cex = cue.cex,
          break.label.cex = threshold.cex
        )
      } else {
        plotting.parameters.df <- data.frame(
          n.levels = 1:6,
          plot.height = c(10, 12, 15, 19, 23, 25),
          plot.width = c(14, 16, 20, 24, 28, 32) * 1,
          label.box.text.cex = cue.cex,
          break.label.cex = threshold.cex
        )
      }

      if (n.levels < 6) {
        label.box.text.cex <- plotting.parameters.df$label.box.text.cex[n.levels]
        break.label.cex <- plotting.parameters.df$break.label.cex[n.levels]
        plot.height <- plotting.parameters.df$plot.height[n.levels]
        plot.width <- plotting.parameters.df$plot.width[n.levels]
      }

      if (n.levels >= 6) {
        label.box.text.cex <- plotting.parameters.df$label.box.text.cex[6]
        break.label.cex <- plotting.parameters.df$break.label.cex[6]
        plot.height <- plotting.parameters.df$plot.height[6]
        plot.width <- plotting.parameters.df$plot.width[6]
      }


      # Colors

      exit.node.bg <- "white"
      #
      # error.colfun <- circlize::colorRamp2(c(0, 50, 100),
      #                            colors = c("white", "red", "black"))
      #
      # correct.colfun <-  circlize::colorRamp2(c(0, 50, 100),
      #                            colors = c("white", "green", "black"))
      #
      # error.bg <- scales::alpha(error.colfun(35), .8)
      # error.border <-  scales::alpha(error.colfun(65), .9)
      # correct.bg <- scales::alpha(correct.colfun(35), .8)
      # correct.border <-  scales::alpha(correct.colfun(65), .9)

      error.bg <- "#FF7352CC"
      error.border <- "#AD1A0AE6"
      correct.bg <- "#89FF6FCC"
      correct.border <- "#24AB18E6"

      max.cex <- 6
      min.cex <- 1

      exit.node.pch <- 21

      decision.node.pch <- NA_integer_


      # balls
      ball.loc <- "variable"

      if (n.levels == 3) {
        ball.box.width <- 14
      }
      if (n.levels == 4) {
        ball.box.width <- 18
      }

      ball.box.height <- 2.5
      ball.box.horiz.shift <- 10
      ball.box.vert.shift <- -1
      ball.box.max.shift.p <- .9
      ball.box.min.shift.p <- .4

      ball.box.fixed.x.shift <- c(ball.box.min.shift.p * plot.width, ball.box.max.shift.p * plot.width)

      # Determine N per ball

      if (is.null(n.per.icon)) {
        max.n.side <- max(c(n.pos, n.neg))

        i <- max.n.side / c(1, 5, 10^(1:10))
        i[i > 50] <- 0

        n.per.icon <- c(1, 5, 10^(1:10))[which(i == max(i))]
      }

      noise.ball.pch <- ball.pch[1]
      signal.ball.pch <- ball.pch[2]
      noise.ball.col <- ball.col[1]
      signal.ball.col <- ball.col[2]
      noise.ball.bg <- ball.bg[1]
      signal.ball.bg <- ball.bg[2]

      # arrows

      arrow.lty <- 1
      arrow.lwd <- 1
      arrow.length <- 2.5
      arrow.head.length <- .08
      arrow.col <- gray(0)

      # Final stats

      spec.circle.x <- .4
      dprime.circle.x <- .5
      sens.circle.x <- .6

      stat.circle.y <- .3

      sens.circle.col <- "green"
      spec.circle.col <- "red"
      dprime.circle.col <- "blue"
      stat.outer.circle.col <- gray(.5)
    }

    # add.balls.fun() adds balls to the plot
    add.balls.fun <- function(x.lim = c(-10, 0),
                              y.lim = c(-2, 0),
                              n.vec = c(20, 10),
                              pch.vec = c(21, 21),
                              ball.cex = 1,
                              bg.vec = ball.bg,
                              col.vec = ball.col,
                              ball.lwd = .7,
                              freq.text = TRUE,
                              freq.text.cex = 1.2,
                              upper.text = "",
                              upper.text.cex = 1,
                              upper.text.adj = 0,
                              rev.order = F,
                              box.col = NULL,
                              box.bg = NULL,
                              n.per.icon = NULL) {


      # Add box

      if (is.null(box.col) == FALSE | is.null(box.bg) == FALSE) {
        rect(x.lim[1],
          y.lim[1],
          x.lim[2],
          y.lim[2],
          col = box.bg,
          border = box.col
        )
      }

      # add upper text

      text(mean(x.lim), y.lim[2] + upper.text.adj,
        label = upper.text, cex = upper.text.cex
      )


      a.n <- n.vec[1]
      b.n <- n.vec[2]

      a.p <- n.vec[1] / sum(n.vec)

      box.x.center <- sum(x.lim) / 2
      box.y.center <- sum(y.lim) / 2

      box.x.width <- x.lim[2] - x.lim[1]

      # Determine cases per ball

      if (is.null(n.per.icon)) {
        max.n.side <- max(c(a.n, b.n))

        i <- max.n.side / c(1, 5, 10, 50, 100, 1000, 10000)
        i[i > 50] <- 0

        n.per.icon <- c(1, 5, 10, 50, 100, 1000, 10000)[which(i == max(i))]
      }

      # Determine general ball locations

      a.balls <- ceiling(a.n / n.per.icon)
      b.balls <- ceiling(b.n / n.per.icon)
      n.balls <- a.balls + b.balls

      a.ball.x.loc <- 0
      a.ball.y.loc <- 0
      b.ball.x.loc <- 0
      b.ball.y.loc <- 0

      if (a.balls > 0) {
        a.ball.x.loc <- rep(-1:-10, each = 5, length.out = 50)[1:a.balls]
        a.ball.y.loc <- rep(1:5, times = 10, length.out = 50)[1:a.balls]
        a.ball.x.loc <- a.ball.x.loc * (x.lim[2] - box.x.center) / 10 + box.x.center
        a.ball.y.loc <- a.ball.y.loc * (y.lim[2] - y.lim[1]) / 5 + y.lim[1]
      }

      if (b.balls > 0) {
        b.ball.x.loc <- rep(1:10, each = 5, length.out = 50)[1:b.balls]
        b.ball.y.loc <- rep(1:5, times = 10, length.out = 50)[1:b.balls]
        b.ball.x.loc <- b.ball.x.loc * (x.lim[2] - box.x.center) / 10 + box.x.center
        b.ball.y.loc <- b.ball.y.loc * (y.lim[2] - y.lim[1]) / 5 + y.lim[1]
      }

      # if(rev.order) {
      #
      #   x <- b.ball.x.loc
      #   y <- b.ball.y.loc
      #
      #   b.ball.x.loc <- a.x.loc
      #   b.ball.y.loc <- a.y.loc
      #
      #   a.ball.x.loc <- x
      #   a.ball.y.loc <- y
      #
      # }


      # Add frequency text

      if (freq.text) {
        text(box.x.center, y.lim[1] - 1 * (y.lim[2] - y.lim[1]) / 5, prettyNum(b.n, big.mark = ","), pos = 4, cex = freq.text.cex)
        text(box.x.center, y.lim[1] - 1 * (y.lim[2] - y.lim[1]) / 5, prettyNum(a.n, big.mark = ","), pos = 2, cex = freq.text.cex)
      }

      # Draw balls

      # Noise

      suppressWarnings(if (a.balls > 0) {
        points(
          x = a.ball.x.loc,
          y = a.ball.y.loc,
          pch = pch.vec[1],
          bg = bg.vec[1],
          col = col.vec[1],
          cex = ball.cex,
          lwd = ball.lwd
        )
      })

      # Signal
      suppressWarnings(if (b.balls > 0) {
        points(
          x = b.ball.x.loc,
          y = b.ball.y.loc,
          pch = pch.vec[2],
          bg = bg.vec[2],
          col = col.vec[2],
          cex = ball.cex,
          lwd = ball.lwd
        )
      })
    }

    label.cex.fun <- function(i, label.box.text.cex = 2) {
      i <- nchar(i)

      label.box.text.cex * i^-.25
    }}

    # -------------------------
    # 1: Initial Frequencies
    # --------------------------
    if (show.top) {
      par(mar = c(0, 0, 1, 0))

      plot(1,
        xlim = c(0, 1), ylim = c(0, 1), bty = "n", type = "n",
        xlab = "", ylab = "", yaxt = "n", xaxt = "n"
      )

      if (hlines) {
        segments(0, .95, 1, .95, col = panel.line.col, lwd = panel.line.lwd, lty = panel.line.lty)
      }

      rect(.33, .8, .67, 1.2, col = "white", border = NA)

      text(x = .5, y = .95, main, cex = panel.title.cex)
      text(x = .5, y = .80, paste("N = ", prettyNum(n.exemplars, big.mark = ","), "", sep = ""), cex = 1.25)

      n.trueneg <- with(final.stats, cr + fa)
      n.truepos <- with(final.stats, hi + mi)

      text(.5, .65, paste(decision.labels[1], sep = ""), pos = 2, cex = 1.2, adj = 1)
      text(.5, .65, paste(decision.labels[2], sep = ""), pos = 4, cex = 1.2, adj = 0)


      # points(.9, .8, pch = 1, cex = 1.2)
      # text(.9, .8, labels = paste(" = ", n.per.icon, " cases", sep = ""), pos = 4)

      # Show ball examples

      par(xpd = T)

      add.balls.fun(
        x.lim = c(.35, .65),
        y.lim = c(.1, .5),
        n.vec = c(final.stats$fa + final.stats$cr, final.stats$hi + final.stats$mi),
        pch.vec = c(noise.ball.pch, signal.ball.pch),
        bg.vec = c(noise.ball.bg, signal.ball.bg),
        col.vec = c(noise.ball.col, signal.ball.col),
        ball.cex = ball.cex,
        upper.text.adj = 2,
        n.per.icon = n.per.icon
      )

      par(xpd = F)


      # Add p.signal and p.noise levels

      signal.p <- mean(x$data[[data]][[x$criterion_name]])
      noise.p <- 1 - signal.p

      p.rect.ylim <- c(.1, .6)

      # p.signal level

      text(
        x = .8, y = p.rect.ylim[2],
        labels = paste("p(", decision.labels[2], ")", sep = ""),
        pos = 3, cex = 1.2
      )



      # Filling
      rect(.775, p.rect.ylim[1],
        .825, p.rect.ylim[1] + signal.p * diff(p.rect.ylim),
        col = gray(.5, .25), border = NA
      )

      # Filltop
      segments(.775, p.rect.ylim[1] + signal.p * diff(p.rect.ylim),
        .825, p.rect.ylim[1] + signal.p * diff(p.rect.ylim),
        lwd = 1
      )

      # Outline
      rect(.775, p.rect.ylim[1],
        .825, p.rect.ylim[2],
        lwd = 1
      )

      if (signal.p < .0001) {
        signal.p.text <- "<1%"
      } else {
        signal.p.text <- paste(round(signal.p * 100, 0), "%", sep = "")
      }

      text(.825, p.rect.ylim[1] + signal.p * diff(p.rect.ylim),
        labels = signal.p.text,
        pos = 4, cex = 1.2
      )


      # p.noise level

      text(
        x = .2, y = p.rect.ylim[2],
        labels = paste("p(", decision.labels[1], ")", sep = ""),
        pos = 3, cex = 1.2
      )





      rect(.175, p.rect.ylim[1], .225, p.rect.ylim[1] + noise.p * diff(p.rect.ylim),
        col = gray(.5, .25), border = NA
      )

      # Filltop
      segments(.175, p.rect.ylim[1] + noise.p * diff(p.rect.ylim),
        .225, p.rect.ylim[1] + noise.p * diff(p.rect.ylim),
        lwd = 1
      )

      # outline
      rect(.175, p.rect.ylim[1], .225, p.rect.ylim[2],
        lwd = 1
      )

      if (noise.p < .0001) {
        noise.p.text <- "<0.01%"
      } else {
        noise.p.text <- paste(round(noise.p * 100, 0), "%", sep = "")
      }

      text(.175, p.rect.ylim[1] + noise.p * diff(p.rect.ylim),
        labels = noise.p.text,
        pos = 2, cex = 1.2
      )
    }

    # -------------------------
    # 2. TREE
    # --------------------------

    if (show.middle) {
      if (show.top == FALSE & show.bottom == FALSE) {
        par(mar = c(3, 3, 3, 3) + .1)
      } else {
        par(mar = c(0, 0, 0, 0))
      }

      # Setup plotting space

      plot(1,
        xlim = c(-plot.width, plot.width),
        ylim = c(-plot.height, 0),
        type = "n", bty = "n",
        xaxt = "n", yaxt = "n",
        ylab = "", xlab = ""
      )

      # Add  frame

      par(xpd = TRUE)

      if (show.top | show.bottom) {
        if (hlines) {
          segments(-plot.width, 0, -plot.width * .3, 0, col = panel.line.col, lwd = panel.line.lwd, lty = panel.line.lty)
          segments(plot.width, 0, plot.width * .3, 0, col = panel.line.col, lwd = panel.line.lwd, lty = panel.line.lty)
        }

        if (is.null(label.tree)) {
          label.tree <- paste("FFT #", tree, " (of ", x$trees$n, ")", sep = "")
        }

        text(
          x = 0, y = 0,
          label.tree,
          cex = panel.title.cex
        )
      }

      if (show.top == FALSE & show.bottom == FALSE) {
        if (is.null(main) & is.null(x$params$main)) {
          main <- ""
        }

        mtext(text = main, side = 3, cex = 2)
      }

      par(xpd = FALSE)


      # Create Noise and Signal panels

      if (show.iconguide) {

        # Noise Balls

        points(c(-plot.width * .7, -plot.width * .5),
          c(-plot.height * .125, -plot.height * .125),
          pch = c(noise.ball.pch, signal.ball.pch),
          bg = c(correct.bg, error.bg),
          col = c(correct.border, error.border),
          cex = ball.cex * 1.5
        )

        text(c(-plot.width * .7, -plot.width * .5),
          c(-plot.height * .125, -plot.height * .125),
          labels = c("Correct\nRejection", "Miss"),
          pos = c(2, 4), offset = 1
        )


        # Noise Panel

        text(-plot.width * .6, -plot.height * .05,
          paste("Decide ", decision.labels[1], sep = ""),
          cex = 1.2, font = 3
        )


        # Signal panel
        text(plot.width * .6, -plot.height * .05,
          paste("Decide ", decision.labels[2], sep = ""),
          cex = 1.2, font = 3
        )


        points(c(plot.width * .5, plot.width * .7),
          c(-plot.height * .125, -plot.height * .125),
          pch = c(noise.ball.pch, signal.ball.pch),
          bg = c(error.bg, correct.bg),
          col = c(error.border, correct.border),
          cex = ball.cex * 1.5
        )

        text(c(plot.width * .5, plot.width * .7),
          c(-plot.height * .125, -plot.height * .125),
          labels = c("False\nAlarm", "Hit"),
          pos = c(2, 4), offset = 1
        )
      }

      # Set initial subplot center

      subplot.center <- c(0, -4)

      # Loop over levels

      for (level.i in 1:min(c(n.levels, 6))) {
        current.cue <- cue.labels[level.i]

        # Get stats for current level
        {
          hi.i <- level.stats$hi.m[level.i]
          mi.i <- level.stats$mi.m[level.i]
          fa.i <- level.stats$fa.m[level.i]
          cr.i <- level.stats$cr.m[level.i]
        }

        # If level.i == 1, draw top textbox
        {
          if (level.i == 1) {
            rect(subplot.center[1] - label.box.width / 2,
              subplot.center[2] + 2 - label.box.height / 2,
              subplot.center[1] + label.box.width / 2,
              subplot.center[2] + 2 + label.box.height / 2,
              col = "white",
              border = "black"
            )

            points(
              x = subplot.center[1],
              y = subplot.center[2] + 2,
              cex = decision.node.cex,
              pch = decision.node.pch
            )



            text(
              x = subplot.center[1],
              y = subplot.center[2] + 2,
              labels = current.cue,
              cex = label.box.text.cex # label.cex.fun(current.cue, label.box.text.cex = label.box.text.cex)
            )
          }
        }

        # -----------------------
        # Left (Noise) Classification / New Level
        # -----------------------
        {
          # Exit node on left

          if (level.stats$exit[level.i] %in% c(0, .5) | paste(level.stats$exit[level.i]) %in% c("0", ".5")) {
            segments(subplot.center[1],
              subplot.center[2] + 1,
              subplot.center[1] - 2,
              subplot.center[2] - 2,
              lty = segment.lty,
              lwd = segment.lwd
            )

            arrows(
              x0 = subplot.center[1] - 2,
              y0 = subplot.center[2] - 2,
              x1 = subplot.center[1] - 2 - arrow.length,
              y1 = subplot.center[2] - 2,
              lty = arrow.lty,
              lwd = arrow.lwd,
              col = arrow.col,
              length = arrow.head.length
            )

            # Decision text

            if (decision.cex > 0) {
              text(
                x = subplot.center[1] - 2 - arrow.length * .7,
                y = subplot.center[2] - 2.2,
                labels = decision.labels[1],
                pos = 1, font = 3, cex = decision.cex
              )
            }


            if (ball.loc == "fixed") {
              ball.x.lim <- c(-max(ball.box.fixed.x.shift), -min(ball.box.fixed.x.shift))
              ball.y.lim <- c(
                subplot.center[2] + ball.box.vert.shift - ball.box.height / 2,
                subplot.center[2] + ball.box.vert.shift + ball.box.height / 2
              )
            }

            if (ball.loc == "variable") {
              ball.x.lim <- c(
                subplot.center[1] - ball.box.horiz.shift - ball.box.width / 2,
                subplot.center[1] - ball.box.horiz.shift + ball.box.width / 2
              )

              ball.y.lim <- c(
                subplot.center[2] + ball.box.vert.shift - ball.box.height / 2,
                subplot.center[2] + ball.box.vert.shift + ball.box.height / 2
              )
            }

            if (max(c(cr.i, mi.i), na.rm = TRUE) > 0 & show.icons == TRUE) {
              add.balls.fun(
                x.lim = ball.x.lim,
                y.lim = ball.y.lim,
                n.vec = c(cr.i, mi.i),
                pch.vec = c(noise.ball.pch, signal.ball.pch),
                #  bg.vec = c(noise.ball.bg, signal.ball.bg),
                bg.vec = c(correct.bg, error.bg),
                col.vec = c(correct.border, error.border),
                freq.text = TRUE,
                n.per.icon = n.per.icon,
                ball.cex = ball.cex
              )
            }

            # level break label

            pos.direction.symbol <- c("<=", "<", "=", "!=", ">", ">=")[which(level.stats$direction[level.i] == c(">", ">=", "!=", "=", "<=", "<"))]
            neg.direction.symbol <- c("<=", "<", "=", "!=", ">", ">=")[which(level.stats$direction[level.i] == c("<=", "<", "=", "!=", ">", ">="))]


            text.outline(
              x = subplot.center[1] - 1,
              y = subplot.center[2],
              labels = paste(pos.direction.symbol, " ", level.stats$threshold[level.i], sep = ""),
              pos = 2, cex = break.label.cex, r = .1
            )

            points(
              x = subplot.center[1] - 2,
              y = subplot.center[2] - 2,
              pch = exit.node.pch,
              cex = exit.node.cex,
              bg = exit.node.bg
            )

            text(
              x = subplot.center[1] - 2,
              y = subplot.center[2] - 2,
              labels = substr(decision.labels[1], 1, 1)
            )
          }

          # New level on left

          if (level.stats$exit[level.i] %in% c(1) | paste(level.stats$exit[level.i]) %in% c("1")) {
            segments(subplot.center[1],
              subplot.center[2] + 1,
              subplot.center[1] - 2,
              subplot.center[2] - 2,
              lty = segment.lty,
              lwd = segment.lwd
            )

            rect(subplot.center[1] - 2 - label.box.width / 2,
              subplot.center[2] - 2 - label.box.height / 2,
              subplot.center[1] - 2 + label.box.width / 2,
              subplot.center[2] - 2 + label.box.height / 2,
              col = "white",
              border = "black"
            )

            if (level.i < 6) {
              text(
                x = subplot.center[1] - 2,
                y = subplot.center[2] - 2,
                labels = cue.labels[level.i + 1],
                cex = label.box.text.cex
              )
            } else {
              text(
                x = subplot.center[1] - 2,
                y = subplot.center[2] - 2,
                labels = paste0("+ ", n.levels - 6, " More"),
                cex = label.box.text.cex,
                font = 3
              )
            }
          }
        }

        # -----------------------
        # Right (Signal) Classification / New Level
        # -----------------------
        {

          # Exit node on right

          if (level.stats$exit[level.i] %in% c(1, .5) | paste(level.stats$exit[level.i]) %in% c("1", ".5")) {
            segments(subplot.center[1],
              subplot.center[2] + 1,
              subplot.center[1] + 2,
              subplot.center[2] - 2,
              lty = segment.lty,
              lwd = segment.lwd
            )

            arrows(
              x0 = subplot.center[1] + 2,
              y0 = subplot.center[2] - 2,
              x1 = subplot.center[1] + 2 + arrow.length,
              y1 = subplot.center[2] - 2,
              lty = arrow.lty,
              lwd = arrow.lwd,
              col = arrow.col,
              length = arrow.head.length
            )

            # Decision text

            if (decision.cex > 0) {
              text(
                x = subplot.center[1] + 2 + arrow.length * .7,
                y = subplot.center[2] - 2.2,
                labels = decision.labels[2],
                pos = 1, font = 3, cex = decision.cex
              )
            }


            if (ball.loc == "fixed") {
              ball.x.lim <- c(min(ball.box.fixed.x.shift), max(ball.box.fixed.x.shift))
              ball.y.lim <- c(
                subplot.center[2] + ball.box.vert.shift - ball.box.height / 2,
                subplot.center[2] + ball.box.vert.shift + ball.box.height / 2
              )
            }

            if (ball.loc == "variable") {
              ball.x.lim <- c(
                subplot.center[1] + ball.box.horiz.shift - ball.box.width / 2,
                subplot.center[1] + ball.box.horiz.shift + ball.box.width / 2
              )

              ball.y.lim <- c(
                subplot.center[2] + ball.box.vert.shift - ball.box.height / 2,
                subplot.center[2] + ball.box.vert.shift + ball.box.height / 2
              )
            }

            if (max(c(fa.i, hi.i), na.rm = TRUE) > 0 & show.icons == TRUE) {
              add.balls.fun(
                x.lim = ball.x.lim,
                y.lim = ball.y.lim,
                n.vec = c(fa.i, hi.i),
                pch.vec = c(noise.ball.pch, signal.ball.pch),
                #      bg.vec = c(noise.ball.bg, signal.ball.bg),
                bg.vec = c(error.bg, correct.bg),
                col.vec = c(error.border, correct.border),
                freq.text = TRUE,
                n.per.icon = n.per.icon,
                ball.cex = ball.cex
              )
            }

            # level break label

            dir.symbols <- c("<=", "<", "=", "!=", ">", ">=")

            pos.direction.symbol <- dir.symbols[which(level.stats$direction[level.i] == c("<=", "<", "=", "!=", ">", ">="))]
            neg.direction.symbol <- dir.symbols[which(level.stats$direction[level.i] == rev(c("<=", "<", "=", "!=", ">", ">=")))]


            text.outline(subplot.center[1] + 1,
              subplot.center[2],
              labels = paste(pos.direction.symbol, " ", level.stats$threshold[level.i], sep = ""),
              pos = 4, cex = break.label.cex, r = .1
            )

            points(
              x = subplot.center[1] + 2,
              y = subplot.center[2] - 2,
              pch = exit.node.pch,
              cex = exit.node.cex,
              bg = exit.node.bg
            )

            text(
              x = subplot.center[1] + 2,
              y = subplot.center[2] - 2,
              labels = substr(decision.labels[2], 1, 1)
            )
          }


          # New level on right

          if (level.stats$exit[level.i] %in% 0 | paste(level.stats$exit[level.i]) %in% c("0")) {
            segments(subplot.center[1],
              subplot.center[2] + 1,
              subplot.center[1] + 2,
              subplot.center[2] - 2,
              lty = segment.lty,
              lwd = segment.lwd
            )


            if (level.i < 6) {
              rect(subplot.center[1] + 2 - label.box.width / 2,
                subplot.center[2] - 2 - label.box.height / 2,
                subplot.center[1] + 2 + label.box.width / 2,
                subplot.center[2] - 2 + label.box.height / 2,
                col = "white",
                border = "black"
              )

              text(
                x = subplot.center[1] + 2,
                y = subplot.center[2] - 2,
                labels = cue.labels[level.i + 1],
                cex = label.box.text.cex
              )
            } else {
              rect(subplot.center[1] + 2 - label.box.width / 2,
                subplot.center[2] - 2 - label.box.height / 2,
                subplot.center[1] + 2 + label.box.width / 2,
                subplot.center[2] - 2 + label.box.height / 2,
                col = "white",
                border = "black", lty = 2
              )


              text(
                x = subplot.center[1] + 2,
                y = subplot.center[2] - 2,
                labels = paste0("+ ", n.levels - 6, " More"),
                cex = label.box.text.cex,
                font = 3
              )
            }
          }
        }

        # -----------------------
        # Update plot center
        # -----------------------
        {

          if (identical(paste(level.stats$exit[level.i]), "0")) {
            subplot.center <- c(
              subplot.center[1] + 2,
              subplot.center[2] - 4
            )
          }

          if (identical(paste(level.stats$exit[level.i]), "1")) {
            subplot.center <- c(
              subplot.center[1] - 2,
              subplot.center[2] - 4
            )
          }
        }
      }
    }

    # -----------------------
    # 3. CUMULATIVE PERFORMANCE
    # -----------------------

    if (show.bottom == TRUE) {{ # OBTAIN FINAL STATISTICS

      fft.sens.vec <- tree.stats$sens
      fft.spec.vec <- tree.stats$spec

      # General plotting space
      {

        # PLOTTING PARAMETERS
        header.y.loc <- 1.0
        subheader.y.loc <- .925

        header.cex <- 1.1
        subheader.cex <- .9


        par(mar = c(0, 0, 2, 0))

        plot(1,
          xlim = c(0, 1), ylim = c(0, 1),
          bty = "n", type = "n",
          xlab = "", ylab = "",
          yaxt = "n", xaxt = "n"
        )

        par(xpd = T)

        if (hlines) {
          segments(0, 1.1, 1, 1.1, col = panel.line.col, lwd = panel.line.lwd, lty = panel.line.lty)
          rect(.25, 1, .75, 1.2, col = "white", border = NA)
        }


        if (is.null(label.performance)) {
          if (data == "train") {
            label.performance <- "Accuracy (Training)"
          }
          if (data == "test") {
            label.performance <- "Accuracy (Testing)"
          }
        }

        text(.5, 1.1, label.performance, cex = panel.title.cex)
        par(xpd = FALSE)


        pretty.dec <- function(x) {
          return(paste(round(x, 2) * 100, sep = ""))
        }

        level.max.height <- .65
        level.width <- .05
        level.center.y <- .45
        # level.bottom <- .1
        level.bottom <- level.center.y - level.max.height / 2
        level.top <- level.center.y + level.max.height / 2


        lloc <- data.frame(
          element = c("classtable", "mcu", "pci", "sens", "spec", "acc", "bacc", "roc"),
          long.name = c("Classification Table", "mcu", "pci", "sens", "spec", "acc", "bacc", "ROC"),
          center.x = c(.18, seq(.35, .65, length.out = 6), .85),
          center.y = rep(level.center.y, 8),
          width = c(.2, rep(level.width, 6), .2),
          height = c(.65, rep(level.max.height, 6), .65),
          value = c(
            NA,
            abs(final.stats$mcu - 5) / (abs(1 - 5)),
            final.stats$pci, final.stats$sens, final.stats$spec, with(final.stats, (cr + hi) / n), final.stats$bacc, NA
          ),
          value.name = c(
            NA, round(final.stats$mcu, 1), pretty.dec(final.stats$pci), pretty.dec(final.stats$sens), pretty.dec(final.stats$spec), pretty.dec(final.stats$acc),
            pretty.dec(final.stats$bacc), NA
          )
        )
      }


      # Classification table
      if (show.confusion) {
        final.classtable.x.loc <- c(lloc$center.x[lloc$element == "classtable"] - lloc$width[lloc$element == "classtable"] / 2, lloc$center.x[lloc$element == "classtable"] + lloc$width[lloc$element == "classtable"] / 2)
        final.classtable.y.loc <- c(lloc$center.y[lloc$element == "classtable"] - lloc$height[lloc$element == "classtable"] / 2, lloc$center.y[lloc$element == "classtable"] + lloc$height[lloc$element == "classtable"] / 2)

        rect(final.classtable.x.loc[1], final.classtable.y.loc[1],
          final.classtable.x.loc[2], final.classtable.y.loc[2],
          lwd = classtable.lwd
        )

        segments(mean(final.classtable.x.loc), final.classtable.y.loc[1], mean(final.classtable.x.loc), final.classtable.y.loc[2], col = gray(0), lwd = classtable.lwd)
        segments(final.classtable.x.loc[1], mean(final.classtable.y.loc), final.classtable.x.loc[2], mean(final.classtable.y.loc), col = gray(0), lwd = classtable.lwd)


        # Column titles

        text(
          x = mean(mean(final.classtable.x.loc)),
          y = header.y.loc,
          "Truth", pos = 1, cex = header.cex
        )

        text(
          x = final.classtable.x.loc[1] + .25 * diff(final.classtable.x.loc),
          y = subheader.y.loc, pos = 1, cex = subheader.cex,
          decision.labels[2]
        )

        text(
          x = final.classtable.x.loc[1] + .75 * diff(final.classtable.x.loc),
          y = subheader.y.loc, pos = 1, cex = subheader.cex,
          decision.labels[1]
        )

        # Row titles

        text(
          x = final.classtable.x.loc[1] - .01,
          y = final.classtable.y.loc[1] + .75 * diff(final.classtable.y.loc), cex = subheader.cex,
          decision.labels[2], adj = 1
        )

        text(
          x = final.classtable.x.loc[1] - .01,
          y = final.classtable.y.loc[1] + .25 * diff(final.classtable.y.loc), cex = subheader.cex,
          decision.labels[1], adj = 1
        )


        text(
          x = final.classtable.x.loc[1] - .065,
          y = mean(final.classtable.y.loc), cex = header.cex,
          "Decision"
        )

        # text(x = final.classtable.x.loc[1] - .05,
        #      y = mean(final.classtable.y.loc), cex = header.cex,
        #      "Decision", srt = 90, pos = 3)

        # Add final frequencies

        text(final.classtable.x.loc[1] + .75 * diff(final.classtable.x.loc),
          final.classtable.y.loc[1] + .25 * diff(final.classtable.y.loc),
          prettyNum(final.stats$cr, big.mark = ","),
          cex = 1.5
        )

        text(final.classtable.x.loc[1] + .25 * diff(final.classtable.x.loc),
          final.classtable.y.loc[1] + .25 * diff(final.classtable.y.loc),
          prettyNum(final.stats$mi, big.mark = ","),
          cex = 1.5
        )

        text(final.classtable.x.loc[1] + .75 * diff(final.classtable.x.loc),
          final.classtable.y.loc[1] + .75 * diff(final.classtable.y.loc),
          prettyNum(final.stats$fa, big.mark = ","),
          cex = 1.5
        )

        text(final.classtable.x.loc[1] + .25 * diff(final.classtable.x.loc),
          final.classtable.y.loc[1] + .75 * diff(final.classtable.y.loc),
          prettyNum(final.stats$hi, big.mark = ","),
          cex = 1.5
        )

        # Add symbols

        points(final.classtable.x.loc[1] + .55 * diff(final.classtable.x.loc),
          final.classtable.y.loc[1] + .05 * diff(final.classtable.y.loc),
          pch = noise.ball.pch, bg = correct.bg, col = correct.border, cex = ball.cex
        )

        points(final.classtable.x.loc[1] + .05 * diff(final.classtable.x.loc),
          final.classtable.y.loc[1] + .55 * diff(final.classtable.y.loc),
          pch = signal.ball.pch, bg = correct.bg, cex = ball.cex, col = correct.border
        )

        points(final.classtable.x.loc[1] + .55 * diff(final.classtable.x.loc),
          final.classtable.y.loc[1] + .55 * diff(final.classtable.y.loc),
          pch = noise.ball.pch, bg = error.bg, col = error.border, cex = ball.cex
        )

        points(final.classtable.x.loc[1] + .05 * diff(final.classtable.x.loc),
          final.classtable.y.loc[1] + .05 * diff(final.classtable.y.loc),
          pch = signal.ball.pch, bg = error.bg, col = error.border, cex = ball.cex
        )



        # Labels

        text(final.classtable.x.loc[1] + .62 * diff(final.classtable.x.loc),
          final.classtable.y.loc[1] + .07 * diff(final.classtable.y.loc),
          "cr",
          cex = 1, font = 3, adj = 0
        )

        text(final.classtable.x.loc[1] + .12 * diff(final.classtable.x.loc),
          final.classtable.y.loc[1] + .07 * diff(final.classtable.y.loc),
          "mi",
          cex = 1, font = 3, adj = 0
        )


        text(final.classtable.x.loc[1] + .62 * diff(final.classtable.x.loc),
          final.classtable.y.loc[1] + .57 * diff(final.classtable.y.loc),
          "fa",
          cex = 1, font = 3, adj = 0
        )

        text(final.classtable.x.loc[1] + .12 * diff(final.classtable.x.loc),
          final.classtable.y.loc[1] + .57 * diff(final.classtable.y.loc),
          "hi",
          cex = 1, font = 3, adj = 0
        )
      }

      # Levels
      if (show.levels) {
        if (level.type %in% c("line", "bar")) {

          # Color function (taken from colorRamp2 function in circlize package)
          # col.fun <- circlize::colorRamp2(c(0, .75, 1),
          #                                 c("red", "yellow", "green"),
          #                                 transparency = .5)


          add.level.fun <- function(name,
                                    sub = "",
                                    max.val = 1,
                                    min.val = 0,
                                    ok.val = .5,
                                    bottom.text = "",
                                    level.type = "line") {
            rect.center.x <- lloc$center.x[lloc$element == name]
            rect.center.y <- lloc$center.y[lloc$element == name]
            rect.height <- lloc$height[lloc$element == name]
            rect.width <- lloc$width[lloc$element == name]

            rect.bottom.y <- rect.center.y - rect.height / 2
            rect.top.y <- rect.center.y + rect.height / 2

            rect.left.x <- rect.center.x - rect.width / 2
            rect.right.x <- rect.center.x + rect.width / 2

            long.name <- lloc$long.name[lloc$element == name]
            value <- lloc$value[lloc$element == name]
            value.name <- lloc$value.name[lloc$element == name]

            #
            # level.col.fun <- circlize::colorRamp2(c(min.val, ok.val,  max.val),
            #                                        colors = c("firebrick2", "yellow", "green4"),
            #                                        transparency = .1)



            text(
              x = rect.center.x,
              y = header.y.loc,
              labels = long.name,
              pos = 1, cex = header.cex
            )
            #
            # text.outline(x = rect.center.x,
            #              y = header.y.loc,
            #              labels = long.name,
            #              pos = 1, cex = header.cex, r = .02
            # )

            value.height <- rect.bottom.y + min(c(1, ((value - min.val) / (max.val - min.val)))) * rect.height

            # Add filling

            value.s <- min(value / max.val, 1)

            delta <- 1
            gamma <- .5

            value.col.scale <- delta * value.s^gamma / (delta * value.s^gamma + (1 - value.s)^gamma)
            # value.col <- gray(1 - value.col.scale * .5)

            value.col <- gray(1, .25)

            # plot(seq(0, 1, .01), delta * seq(0, 1, .01) ^ gamma / (delta * seq(0, 1, .01) ^ gamma + (1 - seq(0, 1, .01)) ^ gamma))

            if (level.type == "bar") {
              rect(rect.left.x,
                rect.bottom.y,
                rect.right.x,
                value.height,
                # col = level.col.fun(value.s),
                col = value.col,
                # col = spec.level.fun(lloc$value[lloc$element == name]),
                border = "black"
              )


              text.outline(
                x = rect.center.x,
                y = value.height,
                labels = lloc$value.name[lloc$element == name],
                cex = 1.5, r = .008, pos = 3
              )



              # Add level border

              # rect(rect.left.x,
              #      rect.bottom.y,
              #      rect.right.x,
              #      rect.top.y,
              #      border = gray(.5, .5))
            }


            if (level.type == "line") {


              # Stem
              segments(rect.center.x,
                rect.bottom.y,
                rect.center.x,
                value.height,
                lty = 3
              )

              # Horizontal platform
              platform.width <- .02

              segments(
                rect.center.x - platform.width,
                value.height,
                rect.center.x + platform.width,
                value.height
              )


              # Text label
              text.outline(
                x = rect.center.x,
                y = value.height,
                labels = lloc$value.name[lloc$element == name],
                cex = 1.5, r = 0, pos = 3
              )

              # points(rect.center.x,
              #        value.height,
              #        cex = 5.5,
              #        pch = 21,
              #        bg = "white",
              #        col = "black", lwd = .5)
            }


            # Add subtext

            text(
              x = rect.center.x,
              y = rect.center.y - .05,
              labels = sub,
              cex = .8,
              font = 1,
              pos = 1
            )

            # Add bottom text

            text(
              x = rect.center.x,
              y = rect.bottom.y,
              labels = bottom.text,
              pos = 1
            )
          }

          paste(final.stats$cr, "/", 1, collapse = "")

          # Add 100% reference line

          # segments(x0 = lloc$center.x[lloc$element == "mcu"] - lloc$width[lloc$element == "mcu"] * .8,
          #          y0 = level.top,
          #          x1 = lloc$center.x[lloc$element == "bacc"] + lloc$width[lloc$element == "bacc"] * .8,
          #          y1 = level.top,
          #          lty = 3, lwd = .75)


          add.level.fun("mcu",
            ok.val = .75,
            max.val = 1,
            min.val = 0,
            level.type = level.type
          ) # , sub = paste(c(final.stats$cr, "/", final.stats$cr + final.stats$fa), collapse = ""))


          add.level.fun("pci", ok.val = .75, level.type = level.type) # , sub = paste(c(final.stats$cr, "/", final.stats$cr + final.stats$fa), collapse = ""))

          # text(lloc$center.x[lloc$element == "pci"],
          #      lloc$center.y[lloc$element == "pci"],
          #      labels = paste0("mcu\n", round(mcu, 2)))


          add.level.fun("spec", ok.val = .75, level.type = level.type) # , sub = paste(c(final.stats$cr, "/", final.stats$cr + final.stats$fa), collapse = ""))
          add.level.fun("sens", ok.val = .75, level.type = level.type) # , sub = paste(c(final.stats$hi, "/", final.stats$hi + final.stats$mi), collapse = ""))

          # Min acc

          min.acc <- max(crit.br, 1 - crit.br)

          add.level.fun("acc", min.val = 0, ok.val = .5, level.type = level.type) # , sub = paste(c(final.stats$hi + final.stats$cr, "/", final.stats$n), collapse = ""))

          # Add baseline to acc level

          segments(
            x0 = lloc$center.x[lloc$element == "acc"] - lloc$width[lloc$element == "acc"] / 2,
            y0 = (lloc$center.y[lloc$element == "acc"] - lloc$height[lloc$element == "acc"] / 2) + lloc$height[lloc$element == "acc"] * min.acc,
            x1 = lloc$center.x[lloc$element == "acc"] + lloc$width[lloc$element == "acc"] / 2,
            y1 = (lloc$center.y[lloc$element == "acc"] - lloc$height[lloc$element == "acc"] / 2) + lloc$height[lloc$element == "acc"] * min.acc,
            lty = 3
          )

          text(
            x = lloc$center.x[lloc$element == "acc"],
            y = (lloc$center.y[lloc$element == "acc"] - lloc$height[lloc$element == "acc"] / 2) + lloc$height[lloc$element == "acc"] * min.acc,
            labels = "BL", pos = 1
          )

          #   paste("BL = ", pretty.dec(min.acc), sep = ""), pos = 1)



          add.level.fun("bacc", min.val = 0, max.val = 1, ok.val = .5, level.type = level.type)

          # baseline

          # segments(x0 = mean(lloc$center.x[2]),
          #          y0 = lloc$center.y[1] - lloc$height[1] / 2,
          #          x1 = mean(lloc$center.x[7]),
          #          y1 = lloc$center.y[1] - lloc$height[1] / 2, lend = 1,
          #          lwd = .5,
          #          col = gray(0))
        }
      }

      # MiniROC
      if (show.roc) {
        text(lloc$center.x[lloc$element == "roc"], header.y.loc, "ROC", pos = 1, cex = header.cex)
        #    text(final.roc.center[1], subheader.y.loc, paste("AUC =", round(final.auc, 2)), pos = 1)


        final.roc.x.loc <- c(lloc$center.x[lloc$element == "roc"] - lloc$width[lloc$element == "roc"] / 2, lloc$center.x[lloc$element == "roc"] + lloc$width[lloc$element == "roc"] / 2)
        final.roc.y.loc <- c(lloc$center.y[lloc$element == "roc"] - lloc$height[lloc$element == "roc"] / 2, lloc$center.y[lloc$element == "roc"] + lloc$height[lloc$element == "roc"] / 2)

        # Plot bg
        #
        # rect(final.roc.x.loc[1],
        #      final.roc.y.loc[1],
        #      final.roc.x.loc[2],
        #      final.roc.y.loc[2],
        #      col = gray(1), lwd = .5)

        # Gridlines
        # # Horizontal
        #  segments(x0 = rep(final.roc.x.loc[1], 9),
        #           y0 = seq(final.roc.y.loc[1], final.roc.y.loc[2], length.out = 5)[2:10],
        #           x1 = rep(final.roc.x.loc[2], 9),
        #           y1 = seq(final.roc.y.loc[1], final.roc.y.loc[2], length.out = 5)[2:10],
        #           lty = 1, col = gray(.8), lwd = c(.5), lend = 3
        #           )
        #
        #  # Vertical
        #  segments(y0 = rep(final.roc.y.loc[1], 9),
        #           x0 = seq(final.roc.x.loc[1], final.roc.x.loc[2], length.out = 5)[2:10],
        #           y1 = rep(final.roc.y.loc[2], 9),
        #           x1 = seq(final.roc.x.loc[1], final.roc.x.loc[2], length.out = 5)[2:10],
        #           lty = 1, col = gray(.8), lwd = c(.5), lend = 3
        #  )

        # Plot border

        rect(final.roc.x.loc[1],
          final.roc.y.loc[1],
          final.roc.x.loc[2],
          final.roc.y.loc[2],
          border = roc.border.col,
          lwd = roc.lwd
        )


        # Axis labels

        text(c(final.roc.x.loc[1], final.roc.x.loc[2]),
          c(final.roc.y.loc[1], final.roc.y.loc[1]) - .05,
          labels = c(0, 1)
        )

        text(c(final.roc.x.loc[1], final.roc.x.loc[1], final.roc.x.loc[1]) - .02,
          c(final.roc.y.loc[1], mean(final.roc.y.loc[1:2]), final.roc.y.loc[2]),
          labels = c(0, .5, 1)
        )

        text(mean(final.roc.x.loc), final.roc.y.loc[1] - .08, "1 - Specificity (FAR)")
        text(final.roc.x.loc[1] - .04, mean(final.roc.y.loc), "Sensitivity (HR)", srt = 90)

        # Diagonal

        segments(final.roc.x.loc[1],
          final.roc.y.loc[1],
          final.roc.x.loc[2],
          final.roc.y.loc[2],
          lty = 3
        )

        label.loc <- c(.1, .3, .5, .7, .9)

        ## COMPETITIVE ALGORITHMS
        if (comp == TRUE) {

          # CART

          if ("cart" %in% x$competition[[data]]$algorithm) {
            cart.spec <- x$competition[[data]]$spec[x$competition[[data]]$algorithm == "cart"]
            cart.sens <- x$competition[[data]]$sens[x$competition[[data]]$algorithm == "cart"]

            points(final.roc.x.loc[1] + (1 - cart.spec) * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + cart.sens * lloc$height[lloc$element == "roc"],
              pch = 21, cex = 1.75,
              col = scales::alpha("red", .5),
              bg = scales::alpha("red", .3), lwd = 1
            )

            points(final.roc.x.loc[1] + (1 - cart.spec) * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + cart.sens * lloc$height[lloc$element == "roc"],
              pch = "C", cex = .7, col = gray(.2), lwd = 1
            )

            par("xpd" = FALSE)

            points(final.roc.x.loc[1] + 1.1 * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + label.loc[4] * lloc$height[lloc$element == "roc"],
              pch = 21, cex = 2.5,
              col = scales::alpha("red", .1),
              bg = scales::alpha("red", .3)
            )

            points(final.roc.x.loc[1] + 1.1 * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + label.loc[4] * lloc$height[lloc$element == "roc"],
              pch = "C", cex = .9, col = gray(.2)
            )

            text(final.roc.x.loc[1] + 1.13 * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + label.loc[4] * lloc$height[lloc$element == "roc"],
              labels = "  CART", adj = 0, cex = .9
            )

            par("xpd" = TRUE)
          }

          ## LR

          if ("lr" %in% x$competition[[data]]$algorithm) {
            lr.spec <- x$competition[[data]]$spec[x$competition[[data]]$algorithm == "lr"]
            lr.sens <- x$competition[[data]]$sens[x$competition[[data]]$algorithm == "lr"]


            points(final.roc.x.loc[1] + (1 - lr.spec) * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + lr.sens * lloc$height[lloc$element == "roc"],
              pch = 21, cex = 1.75,
              col = scales::alpha("blue", .1),
              bg = scales::alpha("blue", .2)
            )

            points(final.roc.x.loc[1] + (1 - lr.spec) * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + lr.sens * lloc$height[lloc$element == "roc"],
              pch = "L", cex = .7, col = gray(.2)
            )

            par("xpd" = F)

            points(final.roc.x.loc[1] + 1.1 * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + label.loc[3] * lloc$height[lloc$element == "roc"],
              pch = 21, cex = 2.5,
              col = scales::alpha("blue", .1),
              bg = scales::alpha("blue", .2)
            )

            points(final.roc.x.loc[1] + 1.1 * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + label.loc[3] * lloc$height[lloc$element == "roc"],
              pch = "L", cex = .9, col = gray(.2)
            )

            text(final.roc.x.loc[1] + 1.13 * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + label.loc[3] * lloc$height[lloc$element == "roc"],
              labels = "  LR", adj = 0, cex = .9
            )

            par("xpd" = T)
          }

          ## rf
          if ("rf" %in% x$competition[[data]]$algorithm) {
            rf.spec <- x$competition[[data]]$spec[x$competition[[data]]$algorithm == "rf"]
            rf.sens <- x$competition[[data]]$sens[x$competition[[data]]$algorithm == "rf"]

            points(final.roc.x.loc[1] + (1 - rf.spec) * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + rf.sens * lloc$height[lloc$element == "roc"],
              pch = 21, cex = 1.75,
              col = scales::alpha("purple", .1),
              bg = scales::alpha("purple", .3), lwd = 1
            )

            points(final.roc.x.loc[1] + (1 - rf.spec) * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + rf.sens * lloc$height[lloc$element == "roc"],
              pch = "C", cex = .7, col = gray(.2), lwd = 1
            )

            par("xpd" = FALSE)

            points(final.roc.x.loc[1] + 1.1 * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + label.loc[2] * lloc$height[lloc$element == "roc"],
              pch = 21, cex = 2.5,
              col = scales::alpha("purple", .1),
              bg = scales::alpha("purple", .3)
            )

            points(final.roc.x.loc[1] + 1.1 * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + label.loc[2] * lloc$height[lloc$element == "roc"],
              pch = "R", cex = .9, col = gray(.2)
            )

            text(final.roc.x.loc[1] + 1.13 * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + label.loc[2] * lloc$height[lloc$element == "roc"],
              labels = "  RF", adj = 0, cex = .9
            )

            par("xpd" = TRUE)
          }

          ## svm
          if ("svm" %in% x$competition[[data]]$algorithm) {
            svm.spec <- x$competition[[data]]$spec[x$competition[[data]]$algorithm == "svm"]
            svm.sens <- x$competition[[data]]$sens[x$competition[[data]]$algorithm == "svm"]

            points(final.roc.x.loc[1] + (1 - svm.spec) * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + svm.sens * lloc$height[lloc$element == "roc"],
              pch = 21, cex = 1.75,
              col = scales::alpha("orange", .1),
              bg = scales::alpha("orange", .3), lwd = 1
            )

            points(final.roc.x.loc[1] + (1 - svm.spec) * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + svm.sens * lloc$height[lloc$element == "roc"],
              pch = "C", cex = .7, col = gray(.2), lwd = 1
            )

            par("xpd" = FALSE)

            points(final.roc.x.loc[1] + 1.1 * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + label.loc[1] * lloc$height[lloc$element == "roc"],
              pch = 21, cex = 2.5,
              col = scales::alpha("orange", .1),
              bg = scales::alpha("orange", .3)
            )

            points(final.roc.x.loc[1] + 1.1 * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + label.loc[1] * lloc$height[lloc$element == "roc"],
              pch = "S", cex = .9, col = gray(.2)
            )

            text(final.roc.x.loc[1] + 1.13 * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + label.loc[1] * lloc$height[lloc$element == "roc"],
              labels = "  SVM", adj = 0, cex = .9
            )

            par("xpd" = TRUE)
          }
        }

        ## FFT
        {
          roc.order <- order(fft.spec.vec, decreasing = TRUE)
          # roc.order <- 1:x$trees$n

          fft.sens.vec.ord <- fft.sens.vec[roc.order]
          fft.spec.vec.ord <- fft.spec.vec[roc.order]

          # Add segments and points for all trees but tree

          if (length(roc.order) > 1) {
            segments(final.roc.x.loc[1] + c(0, 1 - fft.spec.vec.ord) * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + c(0, fft.sens.vec.ord) * lloc$height[lloc$element == "roc"],
              final.roc.x.loc[1] + c(1 - fft.spec.vec.ord, 1) * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + c(fft.sens.vec.ord, 1) * lloc$height[lloc$element == "roc"],
              lwd = 1,
              col = gray(0)
            )

            points(final.roc.x.loc[1] + (1 - fft.spec.vec.ord[-(which(roc.order == tree))]) * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + fft.sens.vec.ord[-(which(roc.order == tree))] * lloc$height[lloc$element == "roc"],
              pch = 21, cex = 2.5, col = scales::alpha("green", .3),
              bg = scales::alpha("white", .1)
            )

            text(final.roc.x.loc[1] + (1 - fft.spec.vec.ord[-(which(roc.order == tree))]) * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + fft.sens.vec.ord[-(which(roc.order == tree))] * lloc$height[lloc$element == "roc"],
              labels = roc.order[which(roc.order != tree)], cex = 1, col = gray(.2)
            )
          }


          # Add large point for plotted tree

          points(final.roc.x.loc[1] + (1 - fft.spec.vec[tree]) * lloc$width[lloc$element == "roc"],
            final.roc.y.loc[1] + fft.sens.vec[tree] * lloc$height[lloc$element == "roc"],
            pch = 21, cex = 3, col = gray(1), # col = scales::alpha("green", .3),
            bg = scales::alpha("green", .3), lwd = 1
          )

          text(final.roc.x.loc[1] + (1 - fft.spec.vec[tree]) * lloc$width[lloc$element == "roc"],
            final.roc.y.loc[1] + fft.sens.vec[tree] * lloc$height[lloc$element == "roc"],
            labels = tree, cex = 1.25, col = gray(.2), font = 2
          )


          # Labels
          if (comp == TRUE & any(
            is.null(x$competition$models$lr) == FALSE,
            is.null(x$competition$models$cart) == FALSE,
            is.null(x$competition$models$svm) == FALSE,
            is.null(x$competition$models$rf) == FALSE
          )) {
            par("xpd" = FALSE)

            points(final.roc.x.loc[1] + 1.1 * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + label.loc[5] * lloc$height[lloc$element == "roc"],
              pch = 21, cex = 2.5, col = scales::alpha("green", .3),
              bg = scales::alpha("green", .7)
            )

            points(final.roc.x.loc[1] + 1.1 * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + label.loc[5] * lloc$height[lloc$element == "roc"],
              pch = "#", cex = .9, col = gray(.2)
            )

            text(final.roc.x.loc[1] + 1.13 * lloc$width[lloc$element == "roc"],
              final.roc.y.loc[1] + label.loc[5] * lloc$height[lloc$element == "roc"],
              labels = "  FFT", adj = 0, cex = .9
            )

            par("xpd" = TRUE)
          }
        }
      } }}
    # # Reset plotting space
    # par(mfrow = c(1, 1))
    # par(mar = c(5, 4, 4, 1) + .1)
  }
}
