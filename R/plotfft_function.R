# plot.fft
#' Draws (and creates) a FFT.
#'
#' @description The primary purpose of this function is to visualize a Fast and Frugal Tree (FFT) for data that has already been classified using the fft() function. However, if the data have not yet been classified, the function can also implement a tree specified by the user. Inputs with the (M) header are manditory. If the tree has already been implimented, then only inputs with the (A) header should be entered. If the tree has not been implimented, then only inputs with the (B) header should be entered.
#' @param x A fft object created from fft()
#' @param data Either a dataframe, or one of two strings 'best.train' or 'best.test'
#' @param which.tree An integer indicating which tree to plot (only valid when the tree argument is non-empty). To plot the best training (or test) tree with respect to v (HR - FAR), use "best.train" or "best.test"
#' @param decision.names A string vector of length 2 indicating the content-specific name for noise (crit.vec == FALSE) and signal (crit.vec == TRUE) cases.
#' @param main The main plot label.
#' @param ... Additional arguments passed on to plot()
#' @importFrom stats anova predict formula model.frame
#' @importFrom graphics text points abline legend mtext segments rect arrows axis par layout plot
#' @importFrom grDevices gray col2rgb rgb
#' @export
#' @examples
#'
#'
#' # See the vignette for details
#'
#' vignette("fft_plot", package = "FFTrees")
#'
#'
#'

plot.fft <- function(
  x = NULL,
  data = "train",
  which.tree = "best.train", # Either a number, or "best.train" or "best.test"
  main = "Data",
  decision.names = c("Noise", "Signal"),
  ...
) {

  # -------------------------
  # TESTING VALUES
  # --------------------------

  #  x <- tree.64
  # #
  # data = "train"
  # which.tree = "best.train" # Either a number, or "best.train" or "best.test"
  # description = "Data"
  # decision.names = c("Noise", "Signal")



  level.names <- NULL


  # Some general parameters
  {
    do.roc <- F
    ball.col = c(gray(0), gray(0))
    ball.bg = c(gray(1), gray(1))
    ball.pch = c(21, 24)
    ball.cex = c(1, 1)
    error.col <- "red"
    correct.col <- "green"
    max.label.length <- 10
    n.per.ball <- NULL
    final.plot <- 1
    def.par <- par(no.readonly = TRUE)
  }

  # Check for problems

  if(class(x) != "fft" & is.null(level.names)) {

    stop("You did not include a valid fft class object or specify the tree directly with level.names, level.classes (etc.). Either create a valid fft object with fft() or specify the tree directly.")

  }

  if(which.tree == "best.test" & is.null(x$data.test)) {

    print("You wanted to plot the best test tree (which.tree = 'best.test') but there were no test data, I'll plot the best training tree instead")

    which.tree <- "best.train"

  }

  if(is.numeric(which.tree) & which.tree %in% 1:nrow(x$fft.stats) == F) {

    stop(paste("You asked for a tree that does not exist. This object has", nrow(x$fft.stats), "trees"))

  }

  if(class(data) == "character") {

    if(data == "test" & is.null(x$data.test)) {stop("You asked to plot the test data but there are no test data in the fft object")}

  }



  # -------------------------
  # Calculate decision.v and levelout.v (if missing)
  # --------------------------

if(is.null(x) == F) {

  n.trees <- nrow(x$fft.stats)

  if(is.null(which.tree)) {which.tree <- "best.train"}

  if(which.tree == "best.train") {

    which.tree <- which(x$fft.stats$v.train == max(x$fft.stats$v.train))[1]

  }

  if(which.tree == "best.test") {

    which.tree <- which(x$fft.stats$v.test == max(x$fft.stats$v.test))[1]
  }


  # Determine dataset

  if(class(data) == "character") {

    recalculate <- F

    if(data == "train") {

    data.mf <- model.frame(formula = x$formula, data = x$data.train)
    cue.df <- data.mf[,2:ncol(data.mf)]
    crit.vec <- data.mf[,1]
    recalculate <- F
    fitting.type <- "train"

    }

    if(data == "test") {

    data.mf <- model.frame(formula = x$formula, data = x$data.test)
    cue.df <- data.mf[,2:ncol(data.mf)]
    crit.vec <- data.mf[,1]
    recalculate <- F
    fitting.type <- "test"

    }
  }

  if(class(data) == "data.frame") {

    recalculate <- T

    data.mf <- model.frame(formula = x$formula, data = data)
    cue.df <- data.mf[,2:ncol(data.mf)]
    crit.vec <- data.mf[,1]
    recalculate <- T
    if(identical(data, x$data.train)) {fitting.type <- "train"}
    if(identical(data, x$data.train) == F) {fitting.type <- "test"}

  }


  # Convert crit to binary (if necessary)

  if(setequal(unique(crit.vec), c(0, 1)) == F) {

  stop("The criterion value is not binary (0s and 1s), please fix")

  }

  # Calculate tree statistics

  output <- predict.fft(object = x,
                        data = data.mf,
                        formula = x$formula,
                        which.tree = which.tree
  )


  decision.v <- output$decision[,1]
  levelout.v <- output$levelout[,1]
  level.name.v <- output$fft.stats$level.name[1]
  level.threshold.v <- output$fft.stats$level.threshold[1]
  level.sigdirection.v <- output$fft.stats$level.sigdirection[1]

}

  # -------------------------
  # Calculate level statistics
  # --------------------------
  {
    n.levels <- length(unlist(strsplit(level.name.v, ";")))
    n.cases <- nrow(cue.df)

    level.df <- as.data.frame(matrix(NA, nrow = n.levels, ncol = 9))

    names(level.df) <- c("level.num", "a", "b", "c", "d", "hr", "far", "v", "dprime")

    for (level.i in 1:(n.levels)) {

      level.stats <- classtable(prediction.v = decision.v[levelout.v == level.i],
                                criterion.v = crit.vec[levelout.v == level.i])

      level.df[level.i,] <- c(level.i, level.stats[1:8])

    }

    level.df$level.name <- unlist(strsplit(level.name.v, ";"))
    level.df$threshold <- unlist(strsplit(level.threshold.v, ";"))
    level.df$sigdirection <- unlist(strsplit(level.sigdirection.v, ";"))



    # Are any levels missing exits? If so, remove them

    null.levels <- rowSums(level.df[c("a", "b", "c", "d")]) == 0

    if(sum(null.levels == 1) > 0) {

      # print(paste("level(s) ", level.df$level.name[null.levels],
      #             " appear to have no exits. It (they) will be removed",
      #             sep = ""))

      level.names <- level.df$level.name[null.levels == FALSE]
      level.df <- level.df[null.levels == FALSE,]
      n.levels <- length(level.names)

    }
  }


  # -----
  # SINGLE TREE VISUALIZATION
  # -----

  if(do.roc == F) {



    # -------------------------
    # Define plotting parameters
    # --------------------------
    {
      {

        panel.title.cex <- 2

        level.df$level.name.t <-  strtrim(level.df$level.name, max.label.length)


        if(n.levels == 2) {

          label.box.text.cex <- 1.7
          break.label.cex <- 1.5
          plot.height <- 12
          plot.width <- 16
          ball.box.width <- 10

        }


        # Label boxes





        if(n.levels == 3) {

          label.box.text.cex <- 1.5
          break.label.cex <- 1.25

        }

        if(n.levels == 4) {

          label.box.text.cex <- 1.25
          break.label.cex <- 1

        }

        if(n.levels > 4) {

          label.box.text.cex <- 1
          break.label.cex <- .75

        }



        label.box.height <- 2
        label.box.width <- 5


        # Node Segments

        segment.lty <- 1
        segment.lwd <- 1


        continue.segment.lwd <- 1
        continue.segment.lty <- 1

        exit.segment.lwd <- 1
        exit.segment.lty <- 1

        decision.node.cex <- 4
        exit.node.cex <- 4

        if(n.levels == 3) {plot.height <- 15}
        if(n.levels == 4) {plot.height <- 19}
        if(n.levels == 5) {plot.height <- 23}


        #plot.height <- 5 * (n.levels) + 1

        if(n.levels == 3) {plot.width <- 20}
        if(n.levels == 4) {plot.width <- 24}
        if(n.levels == 5) {plot.width <- 28}


        #plot.width <- 8 * (n.levels)


        # Colors

        exit.node.bg <- "white"

        error.colfun <- circlize::colorRamp2(c(0, 50, 100),
                                   colors = c("white", "red", "black"))

        correct.colfun <-  circlize::colorRamp2(c(0, 50, 100),
                                   colors = c("white", "green", "black"))

        error.bg <- transparent(error.colfun(25), .3)
        error.border <-  transparent(error.colfun(75), .3)
        correct.bg <- transparent(correct.colfun(25), .3)
        correct.border <-  transparent(correct.colfun(75), .3)

        max.cex <- 6
        min.cex <- 1

        exit.node.pch <- 21

        decision.node.pch <- NA_integer_


        # balls
        ball.loc <- "variable"

        if(n.levels == 3) {ball.box.width <- 14}
        if(n.levels == 4) {ball.box.width <- 18}


        #ball.box.width <- 10 * (n.levels - 1)
        ball.box.height <- 2.5
        ball.box.horiz.shift <- 7
        ball.box.vert.shift <- -1

        ball.box.max.shift.p <- .9
        ball.box.min.shift.p <- .4

        ball.box.fixed.x.shift <- c(ball.box.min.shift.p * plot.width, ball.box.max.shift.p * plot.width)


        # Determine N per ball

        if(is.null(n.per.ball)) {

          max.n.side <- max(c(sum(crit.vec == 0), sum(crit.vec == 1)))

          i <- max.n.side / c(1, 5, 10, 50, 100, 1000, 10000)
          i[i > 50] <- 0

          n.per.ball <- c(1, 5, 10, 50, 100, 1000, 10000)[which(i == max(i))]

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
        arrow.length <- 3
        arrow.head.length <- .1
        arrow.col <- gray(.5)


        # Final stats

        far.circle.x <- .4
        dprime.circle.x <- .5
        hr.circle.x <- .6

        stat.circle.y <- .3

        hr.circle.col <- "green"
        far.circle.col <- "red"
        dprime.circle.col <- "blue"
        stat.outer.circle.col <- gray(.5)


      }

      add.balls.fun <- function(x.lim = c(-10, 0),
                                y.lim = c(-2, 0),
                                n.vec = c(20, 10),
                                pch.vec = c(21, 21),
                                ball.cex = 1,
                                bg.vec = ball.bg,
                                col.vec = ball.col,
                                ball.lwd = .5,
                                freq.text = T,
                                freq.text.cex = 1.2,
                                upper.text = "",
                                upper.text.cex = 1,
                                upper.text.adj = 0,
                                rev.order = F,
                                box.col = NULL,
                                box.bg = NULL,
                                n.per.ball = NULL

      ) {


        # Add box

        if(is.null(box.col) == F | is.null(box.bg) == F) {

          rect(x.lim[1],
               y.lim[1],
               x.lim[2],
               y.lim[2],
               col = box.bg,
               border = box.col)

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

        if(is.null(n.per.ball)) {

          max.n.side <- max(c(a.n, b.n))

          i <- max.n.side / c(1, 5, 10, 50, 100, 1000, 10000)
          i[i > 50] <- 0

          n.per.ball <- c(1, 5, 10, 50, 100, 1000, 10000)[which(i == max(i))]

        }

        # Determine general ball locations

        a.balls <- ceiling(a.n / n.per.ball)
        b.balls <- ceiling(b.n / n.per.ball)
        n.balls <- a.balls + b.balls

        a.ball.x.loc <- 0
        a.ball.y.loc <- 0
        b.ball.x.loc <- 0
        b.ball.y.loc <- 0

        if(a.balls > 0) {

          a.ball.x.loc <- rep(-1:-10, each = 5, length.out = 50)[1:a.balls]
          a.ball.y.loc <- rep(1:5, times = 10, length.out = 50)[1:a.balls]
          a.ball.x.loc <- a.ball.x.loc * (x.lim[2] - box.x.center) / 10 + box.x.center
          a.ball.y.loc <- a.ball.y.loc * (y.lim[2] - y.lim[1]) / 5 + y.lim[1]

        }

        if(b.balls > 0) {

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

        if(freq.text) {

          text(box.x.center, y.lim[1] - 1 * (y.lim[2] - y.lim[1]) / 5, b.n, pos = 4, cex = freq.text.cex)
          text(box.x.center, y.lim[1] - 1 * (y.lim[2] - y.lim[1]) / 5, a.n, pos = 2, cex = freq.text.cex)

        }

        # Draw balls

        # Noise

        suppressWarnings(if(a.balls > 0) {
          points(x = a.ball.x.loc,
                 y = a.ball.y.loc,
                 pch = pch.vec[1],
                 bg = bg.vec[1],
                 col = col.vec[1],
                 cex = ball.cex,
                 lwd = ball.lwd)
        }
        )

        # Signal
        suppressWarnings(if(b.balls > 0) {
          points(x = b.ball.x.loc,
                 y = b.ball.y.loc,
                 pch = pch.vec[2],
                 bg = bg.vec[2],
                 col = col.vec[2],
                 cex = ball.cex,
                 lwd = ball.lwd
          )
        }
        )


      }
    }

    layout(matrix(1:3, nrow = 3, ncol = 1),
           widths = c(6),
           heights = c(1.2, 3, 1.8))

    # -------------------------
    # 1: Initial Frequencies
    # --------------------------
    {

      par(mar = c(0, 0, 1, 0))

      plot(1, xlim = c(0, 1), ylim = c(0, 1), bty = "n", type = "n",
           xlab = "", ylab = "", yaxt = "n", xaxt = "n")


      segments(0, .95, 1, .95, col = gray(.2, .5), lwd = .5, lty = 1)
      rect(.33, .8, .67, 1.2, col = "white", border = NA)


      text(x = .5, y = .95, main, cex = panel.title.cex)


       text(x = .5, y = .80, paste("N = ", n.cases, "", sep = ""), cex = 1.25)



      final.stats <- classtable(prediction.v = decision.v,
                                criterion.v = crit.vec
      )




      # Add Top label

      # top.text <- paste("N = ", n.cases, sep = "")
      # text(.5, .80, top.text, cex = 1.2)

      n.trueneg <- with(final.stats, cr + fa)
      n.truepos <- with(final.stats, hi + mi)

      text(.5, .65, paste("True ", decision.names[1], sep = ""), pos = 2, cex = 1.2, adj = 1)

      text(.5, .65, paste("True ", decision.names[2], sep = ""), pos = 4, cex = 1.2, adj = 0)


      #points(.9, .8, pch = 1, cex = 1.2)
      #text(.9, .8, labels = paste(" = ", n.per.ball, " cases", sep = ""), pos = 4)

      # Show ball examples

      par(xpd = T)

      add.balls.fun(x.lim = c(.35, .65),
                    y.lim = c(.1, .5),
                    n.vec = c(final.stats$fa + final.stats$cr, final.stats$hi + final.stats$mi),
                    pch.vec = c(noise.ball.pch, signal.ball.pch),
                    bg.vec = c(noise.ball.bg, signal.ball.bg),
                    col.vec = c(noise.ball.col, signal.ball.col),
                    ball.cex = ball.cex,
                    upper.text.adj = 2,
                    n.per.ball = n.per.ball
      )

      par(xpd = F)






    }

    # -------------------------
    # TREE
    # --------------------------
    {


      par(mar = c(0, 0, 0, 0))

      # Setup plotting space

      plot(1,
           xlim = c(-plot.width, plot.width),
           ylim = c(-plot.height, 0),
           type = "n", bty = "n",
           xaxt = "n", yaxt = "n",
           ylab = "", xlab = ""
      )


      # Add  frame

      par(xpd = T)
      segments(-plot.width, 0, - plot.width * .2, 0, col = gray(.2, .5), lwd = .5, lty = 1)
      segments(plot.width, 0, plot.width * .2, 0, col = gray(.2, .5), lwd = .5, lty = 1)
      text(x = 0, y = 0, paste("Tree (#", which.tree, ")", sep = ""), cex = panel.title.cex)

      par(xpd = F)



      # Create Noise and Signal panels
      {

        # Noise Panel

        text(- plot.width * .6, -plot.height * .05,
             paste("Decide ", decision.names[1], sep = ""),
             cex = 1.2, font = 3
        )

        # Quotation marks
     #   text(- plot.width * .7,  -plot.height * .07, "'  '", cex = 2)

        points(c(- plot.width * .7, - plot.width * .5),
               c(-plot.height * .125, -plot.height * .125),
               pch = c(noise.ball.pch, signal.ball.pch),
               bg = c(correct.bg, error.bg),
               col = c(correct.border, error.border),
               cex = ball.cex * 1.5
        )

        text(c(- plot.width * .7, - plot.width * .5),
             c(-plot.height * .125, -plot.height * .125),
             labels = c("Correct\nRejection", "Miss"),
             pos = c(1, 1), offset = 1)




        # Signal panel


        text(plot.width * .6, -plot.height * .05,
             paste("Decide ", decision.names[2], sep = ""),
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
             pos = c(1, 1), offset = 1)





        }

      # Set initial subplot center

      subplot.center <- c(0, -4)

      # Loop over levels

      for(level.i in 1:n.levels) {

        # Get stats for current level

        a.i <- level.df$a[level.i]
        b.i <- level.df$b[level.i]
        c.i <- level.df$c[level.i]
        d.i <- level.df$d[level.i]


        # If level.i == 1, draw top textbox

        if(level.i == 1) {

          rect(subplot.center[1] - label.box.width / 2,
               subplot.center[2] + 2 - label.box.height / 2,
               subplot.center[1] + label.box.width / 2,
               subplot.center[2] + 2 + label.box.height / 2,
               col = "white",
               border = "black"
          )

          points(x = subplot.center[1],
                 y = subplot.center[2] + 2,
                 cex = decision.node.cex,
                 pch = decision.node.pch
          )

          text(x = subplot.center[1],
               y = subplot.center[2] + 2,
               labels = level.df$level.name.t[level.i],
               cex = label.box.text.cex
          )


        }


        # -----------------------
        # Left (Noise) Classification
        # -----------------------

        {

          # Exit node on left

          if((b.i + d.i) > 0 | level.i == n.levels) {

            segments(subplot.center[1],
                     subplot.center[2] + 1,
                     subplot.center[1] - 2,
                     subplot.center[2] - 2,
                     lty = segment.lty,
                     lwd = segment.lwd
            )

            arrows(x0 = subplot.center[1] - 2,
                   y0 = subplot.center[2] - 2,
                   x1 = subplot.center[1] - 2 - arrow.length,
                   y1 = subplot.center[2] - 2,
                   lty = arrow.lty,
                   lwd = arrow.lwd,
                   col = arrow.col,
                   length = arrow.head.length
            )

            # level break label

            pos.direction.symbol <- c("<=", "<", "=", "!=", ">", ">=")[which(level.df$sigdirection[level.i] == c(">", ">=", "!=", "=", "<=", "<"))]
            neg.direction.symbol <- c("<=", "<", "=", "!=", ">", ">=")[which(level.df$sigdirection[level.i] == c("<=", "<", "=", "!=", ">", ">="))]



            text(subplot.center[1] - 1,
                 subplot.center[2],
                 labels = paste(pos.direction.symbol, " ", level.df$threshold[level.i], sep = ""),
                 pos = 2, cex = break.label.cex
            )

            points(x = subplot.center[1] - 2,
                   y = subplot.center[2] - 2,
                   pch = exit.node.pch,
                   cex = exit.node.cex,
                   bg = exit.node.bg
            )

            text(x = subplot.center[1] - 2,
                 y = subplot.center[2] - 2,
                 labels =  substr(decision.names[1], 1, 1)
            )

            if(ball.loc == "fixed") {

              ball.x.lim <- c(-max(ball.box.fixed.x.shift), -min(ball.box.fixed.x.shift))
              ball.y.lim <- c(subplot.center[2] + ball.box.vert.shift - ball.box.height / 2,
                              subplot.center[2] + ball.box.vert.shift + ball.box.height / 2)

            }

            if(ball.loc == "variable") {

              ball.x.lim <- c(subplot.center[1] - ball.box.horiz.shift - ball.box.width / 2,
                              subplot.center[1] - ball.box.horiz.shift + ball.box.width / 2)

              ball.y.lim <- c(subplot.center[2] + ball.box.vert.shift - ball.box.height / 2,
                              subplot.center[2] + ball.box.vert.shift + ball.box.height / 2)

            }

            if(max(c(d.i, b.i)) > 0) {

              add.balls.fun(x.lim = ball.x.lim,
                            y.lim = ball.y.lim,
                            n.vec = c(d.i, b.i),
                            pch.vec = c(noise.ball.pch, signal.ball.pch),
                            #  bg.vec = c(noise.ball.bg, signal.ball.bg),
                            bg.vec = c(correct.bg, error.bg),
                            col.vec = c(correct.border, error.border),
                            freq.text = T,
                            n.per.ball = n.per.ball,
                            ball.cex = ball.cex
              )

            }

          }

          # New level on left

          if((b.i + d.i) == 0 & level.i != n.levels) {

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

            text(x = subplot.center[1] - 2,
                 y = subplot.center[2] - 2,
                 labels = level.df$level.name.t[level.i + 1],
                 cex = label.box.text.cex
            )

          }

        }

        # -----------------------
        # Right Node
        # -----------------------

        {

          # Exit node on right

          if((a.i + c.i) > 0 | level.i == n.levels) {


            segments(subplot.center[1],
                     subplot.center[2] + 1,
                     subplot.center[1] + 2,
                     subplot.center[2] - 2,
                     lty = segment.lty,
                     lwd = segment.lwd
            )

            arrows(x0 = subplot.center[1] + 2,
                   y0 = subplot.center[2] - 2,
                   x1 = subplot.center[1] + 2 + arrow.length,
                   y1 = subplot.center[2] - 2,
                   lty = arrow.lty,
                   lwd = arrow.lwd,
                   col = arrow.col,
                   length = arrow.head.length
            )

            # level break label

            dir.symbols <- c("<=", "<", "=", "!=", ">", ">=")

            pos.direction.symbol <- dir.symbols[which(level.df$sigdirection[level.i] == c("<=", "<", "=", "!=", ">", ">="))]
            neg.direction.symbol <- dir.symbols[which(level.df$sigdirection[level.i] == rev(c("<=", "<", "=", "!=", ">", ">=")))]


            text(subplot.center[1] + 1,
                 subplot.center[2],
                 labels = paste(pos.direction.symbol, " ", level.df$threshold[level.i], sep = ""),
                 pos = 4, cex = break.label.cex
            )

            points(x = subplot.center[1] + 2,
                   y = subplot.center[2] - 2,
                   pch = exit.node.pch,
                   cex = exit.node.cex,
                   bg = exit.node.bg
            )

            text(x = subplot.center[1] + 2,
                 y = subplot.center[2] - 2,
                 labels = substr(decision.names[2], 1, 1)
            )


            if(ball.loc == "fixed") {

              ball.x.lim <- c(min(ball.box.fixed.x.shift), max(ball.box.fixed.x.shift))
              ball.y.lim <- c(subplot.center[2] + ball.box.vert.shift - ball.box.height / 2,
                              subplot.center[2] + ball.box.vert.shift + ball.box.height / 2)

            }

            if(ball.loc == "variable") {

              ball.x.lim <- c(subplot.center[1] + ball.box.horiz.shift - ball.box.width / 2,
                              subplot.center[1] + ball.box.horiz.shift + ball.box.width / 2)

              ball.y.lim <- c(subplot.center[2] + ball.box.vert.shift - ball.box.height / 2,
                              subplot.center[2] + ball.box.vert.shift + ball.box.height / 2)

            }

            if(max(c(c.i, a.i)) > 0) {

              add.balls.fun(x.lim = ball.x.lim,
                            y.lim = ball.y.lim,
                            n.vec = c(c.i, a.i),
                            pch.vec = c(noise.ball.pch, signal.ball.pch),
                            #      bg.vec = c(noise.ball.bg, signal.ball.bg),
                            bg.vec = c(error.bg, correct.bg),
                            col.vec = c(error.border, correct.border),
                            freq.text = T,
                            n.per.ball = n.per.ball,
                            ball.cex = ball.cex
              )

            }


          }


          # New level on right

          if((a.i + c.i) == 0 & level.i != n.levels) {

            segments(subplot.center[1],
                     subplot.center[2] + 1,
                     subplot.center[1] + 2,
                     subplot.center[2] - 2,
                     lty = segment.lty,
                     lwd = segment.lwd
            )


            rect(subplot.center[1] + 2 - label.box.width / 2,
                 subplot.center[2] - 2 - label.box.height / 2,
                 subplot.center[1] + 2 + label.box.width / 2,
                 subplot.center[2] - 2 + label.box.height / 2,
                 col = "white",
                 border = "black"
            )

            text(x = subplot.center[1] + 2,
                 y = subplot.center[2] - 2,
                 labels = level.df$level.name.t[level.i + 1],
                 cex = label.box.text.cex
            )


          }

        }


        # -----------------------
        # Update plot center
        # -----------------------


        if((b.i + d.i) > 0 & (a.i + c.i) == 0) {

          subplot.center <- c(subplot.center[1] + 2,
                              subplot.center[2] - 4)
        }

        if((b.i + d.i) == 0 & (a.i + c.i) > 0) {

          subplot.center <- c(subplot.center[1] - 2,
                              subplot.center[2] - 4)
        }


      }

    }

    # -----------------------
    # CUMULATIVE PERFORMANCE
    # -----------------------
    {

    final.roc.x.loc <- NULL
    final.roc.y.loc <- NULL
    final.noise.balls.center <- NULL
    final.noise.balls.dim <- NULL
    final.sig.balls.center <- NULL
    final.sig.balls.dim <- NULL
    final.miniroc.dim <- NULL


    final.classtable.center <- NULL
    final.spec.center <- NULL
    final.hr.center <- NULL

    final.dp.loc <- NULL

    header.y.loc <- 1.0
    subheader.y.loc <- .9

    header.cex <- 1.2
    subheader.cex <- .9



    if(final.plot == 1) {

      final.classtable.center <- c(.18, .45)
      final.classtable.dim <- c(.2, .7)

      final.spec.center <- c(.35, .45)
      final.spec.dim <- c(.14, .65)

      final.hr.center <- c(.45, .45)
      final.hr.dim <- c(.14, .65)

      final.dp.center <- c(.55, .45)
      final.dp.dim <- c(.14, .65)

      final.auc.center <- c(.65, .45)
      final.auc.dim <- c(.14, .65)

      final.roc.center <- c(.85, .45)
      final.roc.dim <- c(.2, .65)

    }

    if(final.plot == 2) {

      final.noise.balls.center <- c(.125, .55)
      final.noise.balls.dim <- c(.25, .34)

      final.sig.balls.center <- c(.875, .55)
      final.sig.balls.dim <- c(.25, .34)

      final.classtable.center <- c(.5, .4)
      final.classtable.dim <- c(.2, .65)

      final.spec.center <- c(.29, .4)
      final.spec.dim <- c(.14, .65)

      final.hr.center <- c(.68, .4)
      final.hr.dim <- c(.14, .65)


    }

    if(final.plot == 3) {

      final.balls <- T

    }

    if(final.plot == 4) {

      final.balls <- F

    }


    # Check if new final stats are needed

    if(recalculate) {

      new.stats <- predict.fft(object = x,
                               formula = x$formula,
                               data = data,
                               which.tree = 1:n.trees)

      final.auc <- new.stats$trees.auc
      fft.hr.vec <- new.stats$trees$hr
      fft.far.vec <- new.stats$trees$far

      lr.stats <- lr.pred(formula = x$formula,
                          data.train = x$data.train,
                          data.test = data)

      lr.hr <- lr.stats[[1]]$hr.test[lr.stats[[1]]$threshold == .5]
      lr.far <- lr.stats[[1]]$far.test[lr.stats[[1]]$threshold == .5]

      cart.stats <- cart.pred(formula = x$formula, data.train = x$data.train, data.test = data)
      cart.hr <- cart.stats$cart.acc$hr.test[cart.stats$cart.acc$miss.cost == cart.stats$cart.acc$fa.cost]
      cart.far <- cart.stats$cart.acc$far.test[cart.stats$cart.acc$miss.cost == cart.stats$cart.acc$fa.cost]

    }

    if(recalculate == F) {

      if(data == "train") {

        final.auc <- x$auc[1,1]
        fft.hr.vec <- x$fft.stats$hr.train
        fft.far.vec <- x$fft.stats$far.train
        lr.hr <- x$lr.stats$hr.train[x$lr.stats$threshold == .5]
        lr.far <- x$lr.stats$far.train[x$lr.stats$threshold == .5]
        cart.hr <- x$cart.stats$hr.train[x$cart.stats$miss.cost == x$cart.stats$fa.cost]
        cart.far <- x$cart.stats$far.train[x$cart.stats$miss.cost == x$cart.stats$fa.cost]


      }


      if(data == "test") {

        final.auc <- x$auc[2,1]
        fft.hr.vec <- x$fft.stats$hr.test
        fft.far.vec <- x$fft.stats$far.test
        lr.hr <- x$lr.stats$hr.test[x$lr.stats$threshold == .5]
        lr.far <- x$lr.stats$far.test[x$lr.stats$threshold == .5]
        cart.hr <- x$cart.stats$hr.test[x$cart.stats$miss.cost == x$cart.stats$fa.cost]
        cart.far <- x$cart.stats$far.test[x$cart.stats$miss.cost == x$cart.stats$fa.cost]
      }
    }



    # General plotting space
    {
    par(mar = c(0, 0, 2, 0))

    plot(1, xlim = c(0, 1), ylim = c(0, 1),
         bty = "n", type = "n",
         xlab = "", ylab = "",
         yaxt = "n", xaxt = "n")

    par(xpd = T)
    segments(0, 1.1, 1, 1.1, col = gray(.2, .5), lwd = .5, lty = 1)
    rect(.25, 1, .75, 1.2, col = "white", border = NA)

    if(fitting.type == "train") {title.text <- "Performance (Fitting)"}
    if(fitting.type == "test") {title.text <- "Performance (Prediction)"}

    text(.5, 1.1, title.text, cex = panel.title.cex)
    par(xpd = F)

    }

    # Color function (taken from colorRamp2 function in circlize package)
    col.fun <- circlize::colorRamp2(c(0, .75, 1), c("red", "yellow", "green"), transparency = .2)


    #  Final Noise Balls
   if(is.null(final.noise.balls.center) == F) {

      text(final.noise.balls.center[1], header.y.loc,
           paste(decision.names[1], " Classification\n(N = ", final.stats$cr + final.stats$mi, ")", sep = ""),
           cex = header.cex, font = 3, pos = 1)


      add.balls.fun(
                    x.lim = c(final.noise.balls.center[1] - final.noise.balls.dim[1] / 2, final.noise.balls.center[1] + final.noise.balls.dim[1] / 2),
                    y.lim = c(final.noise.balls.center[2] - final.noise.balls.dim[2] / 2, final.noise.balls.center[2] + final.noise.balls.dim[2] / 2),
                    n.vec = c(final.stats$cr, final.stats$mi),
                    pch.vec = c(noise.ball.pch, signal.ball.pch),
                    bg.vec = c(correct.bg, error.bg),
                    col.vec = c(correct.border, error.border),
                    ball.cex = ball.cex,
                    n.per.ball = n.per.ball,
                    upper.text.cex = .7,
                    upper.text.adj = 1
                  )


      # text(.07, .22, "Correct\nRejections", pos = 1, adj = 1, cex = 1)
      # text(.17, .22, "Misses", pos = 1, adj = 0, cex = 1)

      }

      # Final Signal Balls

    if(is.null(final.sig.balls.center) == F) {

      text(final.sig.balls.center[1], header.y.loc,
           paste(decision.names[2], " Classification\n(N = ", final.stats$hi + final.stats$fa, ")", sep = "")
           , cex = header.cex, font = 3, pos = 1)


      add.balls.fun( x.lim = c(final.sig.balls.center[1] - final.sig.balls.dim[1] / 2, final.sig.balls.center[1] + final.sig.balls.dim[1] / 2),
                     y.lim = c(final.sig.balls.center[2] - final.sig.balls.dim[2] / 2, final.sig.balls.center[2] + final.sig.balls.dim[2] / 2),
                    n.vec = c(final.stats$fa, final.stats$hi),
                    pch.vec = c(noise.ball.pch, signal.ball.pch),
                    #   bg.vec = c(noise.ball.bg, signal.ball.bg),
                    bg.vec = c(error.bg, correct.bg),
                    col.vec = c(error.border, correct.border),
                    n.per.ball = n.per.ball,
                    ball.cex = ball.cex,
                    upper.text.cex = .7,
                    upper.text.adj = 1
      )



      # text(.84, .22, "False\nAlarms", pos = 1, adj = 1, cex = 1)
      # text(.9, .22, "Hits", pos = 1, adj = 0, cex = 1)

    }

    # Specificity level

    if(is.null(final.spec.center) == F) {

      rect.top <- final.spec.center[2] + final.spec.dim[2] / 2
      rect.bottom <-  final.spec.center[2] - final.spec.dim[2] / 2
      rect.center <-  final.spec.center[1]
      rect.width <- final.spec.dim[1] / 2

      text(rect.center, header.y.loc, "Spec", cex = header.cex, pos = 1)

    spec.level.fun <- circlize::colorRamp2(c(0, .5,  1),
                                           colors = c("red", "yellow", "green"),
                                           transparency = .3)

      rect(rect.center - rect.width / 2,
           rect.bottom,
           rect.center + rect.width / 2,
           (1 - final.stats$far) * (rect.top - rect.bottom) + rect.bottom,
           col = spec.level.fun(1 - final.stats$far),
           border = NA
      )

      rect(rect.center - rect.width / 2, rect.bottom, rect.center + rect.width / 2, rect.top, border = gray(.5, .5))


      text(x = rect.center,
           y = (rect.top + rect.bottom) / 2,
           labels = paste(round((1 - final.stats$far), 2) * 100, "%", sep = ""),
           cex = 1.2,
           font = 1)

      text(x = rect.center,
           y = (rect.top + rect.bottom) / 2.2,
           labels = paste(final.stats$cr, " / ", (final.stats$cr + final.stats$fa), sep = ""),
           cex = .8,
           font = 1,
           pos = 1)

    }

    # HR level

    if(is.null(final.hr.center) == F) {


      rect.top <- final.hr.center[2] + final.hr.dim[2] / 2
      rect.bottom <-  final.hr.center[2] - final.hr.dim[2] / 2
      rect.center <-  final.hr.center[1]
      rect.width <- final.hr.dim[1] / 2

        text(rect.center, header.y.loc, "Hit Rate", cex = header.cex, pos= 1)

        hr.level.fun <- circlize::colorRamp2(c(0, .5, 1),
                                               colors = c("red", "yellow", "green"),
                                               transparency = .3)

        # Colored Fill
        rect(rect.center - rect.width / 2,
             rect.bottom,
             rect.center + rect.width / 2,
             (final.stats$hr) * (rect.top - rect.bottom) + rect.bottom,
             col = hr.level.fun(final.stats$hr),
             border = NA
        )

        # Border
        rect(rect.center - rect.width / 2,
             rect.bottom,
             rect.center + rect.width / 2,
             rect.top,
             border = gray(.5, .5))


        text(x = rect.center,
             y = (rect.top + rect.bottom) / 2,
             labels = paste(round((final.stats$hr), 2) * 100, "%", sep = ""),
             cex = 1.2,
             font = 1)

        text(x = rect.center,
             y = (rect.top + rect.bottom) / 2.2,
             labels = paste(final.stats$hi, " / ", (final.stats$hi + final.stats$mi), sep = ""),
             cex = .8,
             font = 1,
             pos = 1)


      }

    # DP level

    if(is.null(final.dp.center) == F) {

      rect.top <- final.dp.center[2] + final.dp.dim[2] / 2
      rect.bottom <-  final.dp.center[2] - final.dp.dim[2] / 2
      rect.center <-  final.dp.center[1]
      rect.width <- final.dp.dim[1] / 2

      text(rect.center, header.y.loc, "D'", cex = header.cex, pos= 1)


      max.dprime <- 1.5
      if(final.stats$dprime > max.dprime) {dprime.r <- max.dprime}
      if(final.stats$dprime < max.dprime) {dprime.r <- final.stats$dprime}


      # Colored Fill
      rect(rect.center - rect.width / 2,
           rect.bottom,
           rect.center + rect.width / 2,
           (dprime.r / max.dprime) * (rect.top - rect.bottom) + rect.bottom,
           col = col.fun(dprime.r / max.dprime),
           border = NA
      )

      # Border
      rect(rect.center - rect.width / 2,
           rect.bottom,
           rect.center + rect.width / 2,
           rect.top,
           border = gray(.5, .5))


      text(x = rect.center,
           y = (rect.top + rect.bottom) / 2,
           labels = round((final.stats$dprime), 2),
           cex = 1.2,
           font = 1)

    }

    # AUC level

    if(is.null(final.auc.center) == F) {

      rect.top <- final.auc.center[2] + final.auc.dim[2] / 2
      rect.bottom <-  final.auc.center[2] - final.auc.dim[2] / 2
      rect.center <-  final.auc.center[1]
      rect.width <- final.auc.dim[1] / 2

      text(rect.center, header.y.loc, "AUC", cex = header.cex, pos= 1)

      # Colored Fill
      rect(rect.center - rect.width / 2,
           rect.bottom,
           rect.center + rect.width / 2,
           ((final.auc - .5) / .5) * (rect.top - rect.bottom) + rect.bottom,
           col = col.fun((final.auc - .5) / .5),
           border = NA
      )

      # Border
      rect(rect.center - rect.width / 2,
           rect.bottom,
           rect.center + rect.width / 2,
           rect.top,
           border = gray(.5, .5))


      text(x = rect.center,
           y = (rect.top + rect.bottom) / 2,
           labels = round(final.auc, 2),
           cex = 1.2,
           font = 1)

    }

    # Classification table

    if(is.null(final.classtable.center) == F) {

      final.classtable.x.loc <- c(final.classtable.center[1] - final.classtable.dim[1] / 2, final.classtable.center[1] + final.classtable.dim[1] / 2)
      final.classtable.y.loc <- c(final.classtable.center[2] - final.classtable.dim[2] / 2, final.classtable.center[2] + final.classtable.dim[2] / 2)

      rect(final.classtable.x.loc[1], final.classtable.y.loc[1],
           final.classtable.x.loc[2], final.classtable.y.loc[2]
           )

      segments(mean(final.classtable.x.loc), final.classtable.y.loc[1], mean(final.classtable.x.loc), final.classtable.y.loc[2], col = gray(.5))
      segments(final.classtable.x.loc[1], mean(final.classtable.y.loc), final.classtable.x.loc[2], mean(final.classtable.y.loc), col = gray(.5))


      text(x = mean(mean(final.classtable.x.loc)),
           y = header.y.loc,
           "Truth", pos = 1, cex = header.cex)

      # text(x = mean(mean(final.classtable.x.loc)),
      #      y = subheader.y.loc,
      #      "Truth", pos = 1, cex = subheader.cex)
   #   text(mean(mean(final.classtable.x.loc)), subheader.y.loc, "Decision", pos = 1)


      text(x = final.classtable.x.loc[1] + .25 * diff(final.classtable.x.loc),
           y = subheader.y.loc, pos = 1, cex = subheader.cex,
           decision.names[1])

      text(x = final.classtable.x.loc[1] + .75 * diff(final.classtable.x.loc),
           y = subheader.y.loc, pos = 1, cex = subheader.cex,
          decision.names[2])


      text(x = final.classtable.x.loc[1] - .02,
           y = final.classtable.y.loc[1] + .75 * diff(final.classtable.y.loc), cex = .8,
          decision.names[1], srt = 90)

      text(x = final.classtable.x.loc[1] - .02,
           y = final.classtable.y.loc[1] + .25 * diff(final.classtable.y.loc), cex = .8,
           decision.names[2], srt = 90)

      text(x = final.classtable.x.loc[1] - .05,
           y = mean(final.classtable.y.loc), cex = header.cex,
           "Decision", srt = 90, pos = 3)

      # Add final frequencies

      text(final.classtable.x.loc[1] + .25 * diff(final.classtable.x.loc),
           final.classtable.y.loc[1] + .75 * diff(final.classtable.y.loc),
           final.stats$cr, cex = 1.5)

      text(final.classtable.x.loc[1] + .75 * diff(final.classtable.x.loc),
           final.classtable.y.loc[1] + .75 * diff(final.classtable.y.loc),
           final.stats$mi, cex = 1.5)

      text(final.classtable.x.loc[1] + .25 * diff(final.classtable.x.loc),
           final.classtable.y.loc[1] + .25 * diff(final.classtable.y.loc),
           final.stats$fa, cex = 1.5)


      text(final.classtable.x.loc[1] + .75 * diff(final.classtable.x.loc),
           final.classtable.y.loc[1] + .25 * diff(final.classtable.y.loc),
           final.stats$hi, cex = 1.5)



      # Add symbols

      points(final.classtable.x.loc[1] + .05 * diff(final.classtable.x.loc),
             final.classtable.y.loc[1] + .55 * diff(final.classtable.y.loc),
             pch = noise.ball.pch, bg = correct.bg, col = correct.border, cex = ball.cex)

      points(final.classtable.x.loc[1] + .55 * diff(final.classtable.x.loc),
             final.classtable.y.loc[1] + .05 * diff(final.classtable.y.loc),
             pch = signal.ball.pch, bg = correct.bg, cex = ball.cex, col = correct.border)

      points(final.classtable.x.loc[1] + .05 * diff(final.classtable.x.loc),
             final.classtable.y.loc[1] + .05 * diff(final.classtable.y.loc),
             pch = noise.ball.pch, bg = error.bg, col = error.border, cex = ball.cex)

      points(final.classtable.x.loc[1] + .55 * diff(final.classtable.x.loc),
             final.classtable.y.loc[1] + .55 * diff(final.classtable.y.loc),
             pch = signal.ball.pch, bg = error.bg, col = error.border, cex = ball.cex)



      # Labels

      text(final.classtable.x.loc[1] + .12 * diff(final.classtable.x.loc),
           final.classtable.y.loc[1] + .55 * diff(final.classtable.y.loc),
           "Cor Rej", cex = 1, font = 3, adj = 0)

      text(final.classtable.x.loc[1] + .62 * diff(final.classtable.x.loc),
           final.classtable.y.loc[1] + .55 * diff(final.classtable.y.loc),
           "Miss", cex = 1, font = 3, adj = 0)


      text(final.classtable.x.loc[1] + .12 * diff(final.classtable.x.loc),
           final.classtable.y.loc[1] + .07 * diff(final.classtable.y.loc),
           "False Al", cex = 1, font = 3, adj = 0)

      text(final.classtable.x.loc[1] + .62 * diff(final.classtable.x.loc),
           final.classtable.y.loc[1] + .07 * diff(final.classtable.y.loc),
           "Hit", cex = 1, font = 3, adj = 0)


    }

    # MiniROC

    if(is.null(final.roc.center) == F) {





      text(final.roc.center[1], header.y.loc, "ROC", pos = 1, cex = header.cex)
  #    text(final.roc.center[1], subheader.y.loc, paste("AUC =", round(final.auc, 2)), pos = 1)


      final.roc.x.loc <- c(final.roc.center[1] - final.roc.dim[1] / 2, final.roc.center[1] + final.roc.dim[1] / 2)
      final.roc.y.loc <- c(final.roc.center[2] - final.roc.dim[2] / 2, final.roc.center[2] + final.roc.dim[2] / 2)

      # Plot border

      rect(final.roc.x.loc[1],
           final.roc.y.loc[1],
           final.roc.x.loc[2],
           final.roc.y.loc[2],
           border = gray(.5, .5))


      # Axis labels

      text(c(final.roc.x.loc[1], final.roc.x.loc[2]),
           c(final.roc.y.loc[1], final.roc.y.loc[1]) - .05,
           labels = c(0, 1))

      text(c(final.roc.x.loc[1], final.roc.x.loc[1]) - .02,
           c(final.roc.y.loc[1],  final.roc.y.loc[2]),
           labels = c(0,  1))

      text(mean(final.roc.x.loc), final.roc.y.loc[1] - .08, "FAR (1 - Spec)")
      text(final.roc.x.loc[1] - .02, mean(final.roc.y.loc), "HR")


      # Diagonal

      segments(final.roc.x.loc[1], final.roc.y.loc[1], final.roc.x.loc[2], final.roc.y.loc[2], lty = 2)


      ## CART and LR
{

      points(final.roc.x.loc[1] + cart.far * final.roc.dim[1],
             final.roc.y.loc[1] + cart.hr * final.roc.dim[2],
             pch = 21, cex = 2, col = transparent("red", .3),
             bg = transparent("red", .7))

      points(final.roc.x.loc[1] + cart.far * final.roc.dim[1],
             final.roc.y.loc[1] + cart.hr * final.roc.dim[2],
             pch = "C", cex = .9, col = gray(.2))

par("xpd" = F)

points(final.roc.x.loc[1] + 1.1 * final.roc.dim[1],
       final.roc.y.loc[1] + .6 * final.roc.dim[2],
       pch = 21, cex = 2, col = transparent("red", .3),
       bg = transparent("red", .7))

points(final.roc.x.loc[1] + 1.1 * final.roc.dim[1],
       final.roc.y.loc[1] + .6 * final.roc.dim[2],
       pch = "C", cex = .9, col = gray(.2))

text(final.roc.x.loc[1] + 1.13 * final.roc.dim[1],
     final.roc.y.loc[1] + .6 * final.roc.dim[2],
    labels = " CART", adj = 0, cex = .9)

par("xpd" = T)
#points(1.1, .6, )


      ## LR

      points(final.roc.x.loc[1] + lr.far * final.roc.dim[1],
             final.roc.y.loc[1] + lr.hr * final.roc.dim[2],
             pch = 21, cex = 2, col = transparent("blue", .3),
             bg = transparent("blue", .7))

      points(final.roc.x.loc[1] + lr.far * final.roc.dim[1],
             final.roc.y.loc[1] + lr.hr * final.roc.dim[2],
             pch = "L", cex = .9, col = gray(.2))

      par("xpd" = F)

      points(final.roc.x.loc[1] + 1.1 * final.roc.dim[1],
             final.roc.y.loc[1] + .4 * final.roc.dim[2],
             pch = 21, cex = 2, col = transparent("blue", .3),
             bg = transparent("blue", .7))

      points(final.roc.x.loc[1] + 1.1 * final.roc.dim[1],
             final.roc.y.loc[1] + .4 * final.roc.dim[2],
             pch = "L", cex = .9, col = gray(.2))

      text(final.roc.x.loc[1] + 1.13 * final.roc.dim[1],
           final.roc.y.loc[1] + .4 * final.roc.dim[2],
           labels = " LR", adj = 0, cex = .9)

      par("xpd" = T)

      }
      ## FFT
{
      roc.order <- order(fft.far.vec)

      fft.hr.vec.ord <- fft.hr.vec[roc.order]
      fft.far.vec.ord <- fft.far.vec[roc.order]

      # Add segments and points for all trees but which.tree

      segments(final.roc.x.loc[1] + c(0, fft.far.vec.ord) * final.roc.dim[1],
               final.roc.y.loc[1] + c(0, fft.hr.vec.ord) * final.roc.dim[2],
               final.roc.x.loc[1] + c(fft.far.vec.ord, 1) * final.roc.dim[1],
               final.roc.y.loc[1] + c(fft.hr.vec.ord, 1) * final.roc.dim[2], lwd = 1, col = gray(.5, .5))

      points(final.roc.x.loc[1] + fft.far.vec.ord[-(which(roc.order == which.tree))] * final.roc.dim[1],
             final.roc.y.loc[1] + fft.hr.vec.ord[-(which(roc.order == which.tree))] * final.roc.dim[2],
             pch = 21, cex = 2.5, col = transparent("green", .3),
             bg = transparent("white", .5))

      text(final.roc.x.loc[1] + fft.far.vec.ord[-(which(roc.order == which.tree))] * final.roc.dim[1],
           final.roc.y.loc[1] + fft.hr.vec.ord[-(which(roc.order == which.tree))] * final.roc.dim[2],
           labels = roc.order[-(which(roc.order == which.tree))], cex = 1, col = gray(.2))


      # Add large point for plotted tree

      points(final.roc.x.loc[1] + fft.far.vec[which.tree] * final.roc.dim[1],
             final.roc.y.loc[1] + fft.hr.vec[which.tree] * final.roc.dim[2],
             pch = 21, cex = 2.5, col = transparent("green", .3),
             bg = transparent("green", .7), lwd = 3)

      text(final.roc.x.loc[1] + fft.far.vec[which.tree] * final.roc.dim[1],
             final.roc.y.loc[1] + fft.hr.vec[which.tree] * final.roc.dim[2],
            labels = which.tree, cex = 1, col = gray(.2))




      par("xpd" = F)

      points(final.roc.x.loc[1] + 1.1 * final.roc.dim[1],
             final.roc.y.loc[1] + .8 * final.roc.dim[2],
             pch = 21, cex = 2, col = transparent("green", .3),
             bg = transparent("green", .7))

      points(final.roc.x.loc[1] + 1.1 * final.roc.dim[1],
             final.roc.y.loc[1] + .8 * final.roc.dim[2],
             pch = "#", cex = .9, col = gray(.2))

      text(final.roc.x.loc[1] + 1.13 * final.roc.dim[1],
           final.roc.y.loc[1] + .8 * final.roc.dim[2],
           labels = " FFT", adj = 0, cex = .9)

      par("xpd" = T)


    }


    }

    }

  }

  # -----
  # ROC
  # -----
#
#   if(do.roc == T) {
#
#
#     par(mfrow = c(1, 1))
#     par(mar = c(5, 4, 4, 1) + .1)
#
#     my.colors <- c("#A1C720FF",
#                    "#0C5BB0FF",
#                    "#EE0011FF")
#
#     even.point.cex <- 2
#     roc.line.lwd <- .5
#
#     # Set up plotting space
#
#     plot(1, xlim = c(0, 1),
#          ylim = c(0, 1),
#          xlab = "FAR",
#          ylab = "HR",
#          type = "n",
#          main = "ROC Curve",
#          yaxt = "n",
#          xaxt = "n"
#     )
#
#
#     axis(2, at = seq(0, 1, .1), las = 1, lwd = 0, lwd.ticks = 1)
#     axis(1, at = seq(0, 1, .1), las = 1, lwd = 0, lwd.ticks = 1)
#
#
#     rect(-100, -100, 100, 100, col = gray(.97))
#
#     abline(h = seq(0, 1, .1), lwd = c(2, 1), col = "white")
#     abline(v = seq(0, 1, .1), lwd = c(2, 1), col = "white")
#
#     abline(a = 0,
#            b = 1,
#            lty = 2)
#
#     if(do.lr == T | do.cart == T) {
#
#       legend(x = .45, y = .4,
#              legend = c("FFTs", "LR", "CART"),
#              col = my.colors,
#              lty = 1,
#              lwd = 2,
#              box.col = "white"
#       )
#
#     }
#
#
#     # Are test data included?
#     plot.test <- F
#
#     if("test.crit" %in% names(x) & length(x$test.crit > 1)) {plot.test <- T}
#
#
#     if(plot.test) {
#
#       legend(x = .75, y = .4,
#              legend = c("Fitting", "Prediction"),
#              col = "black",
#              lty = c(2, 1),
#              pch = c(21, 24),
#              lwd = 2,
#              box.col = "white"
#       )
#
#     }
#
#
#     if(plot.test == T) {type.i.vec <- c("train", "test")}
#     if(plot.test == F) {type.i.vec <- c("train")}
#
#     for(type.i in type.i.vec) {
#
#       if(type.i == "train") {
#
#         line.lty <- 2
#         line.lwd <- 1
#         point.lty <- 2
#         point.pch <- 21
#
#       }
#
#
#       if(type.i == "test") {
#
#         line.lty <- 1
#         line.lwd <- 1
#         point.lty <- 1
#         point.pch <- 24
#
#       }
#
#
#       # FFTs
#
#       fft.stats <- x$fft.stats[c("tree.num", paste(c("hr.", "far."), type.i, sep = ""))]
#
#       names(fft.stats) <- c("tree.num", "hr", "far")
#
#       fft.stats <- fft.stats[order(fft.stats$far),]
#
#
#       # Add individual tree points and labels
#
#       points(x = fft.stats$far,
#              y = fft.stats$hr
#       )
#
#       # text(x = fft.stats$far,
#       #      y = fft.stats$hr,
#       #      labels = fft.stats$num,
#       #      pos = 3
#       # )
#
#       segments(x0 = fft.stats$far[1:(n.trees - 1)],
#                y0 = fft.stats$hr[1:(n.trees - 1)],
#                x1 = fft.stats$far[2:(n.trees)],
#                y1 = fft.stats$hr[2:(n.trees)],
#                col = my.colors[1],
#                lwd = line.lwd,
#                lty = line.lty
#       )
#
#       # Add center point
#
#       tree.i <- x$best.train.tree
#
#       points(x$fft.stats[paste("far.", type.i, sep = "")][tree.i, 1],
#              x$fft.stats[paste("hr.", type.i, sep = "")][tree.i, 1],
#              cex = even.point.cex,
#              pch = point.pch,
#              lty = point.lty,
#              bg = "green")
#
#
#       # LR
#
#       plot.lr <- F
#
#       if("lr" %in% names(x) & nrow(x$lr) > 1 & do.lr == T) {plot.lr <- T}
#
#       if(plot.lr) {
#
#         lr.df <- x$lr
#         lr.df <- lr.df[order(lr.df[paste("far.", type.i, sep = "")]),]
#
#         lr.far.vec <- unlist(lr.df[paste("far.", type.i, sep = "")])
#         lr.hr.vec <- unlist(lr.df[paste("hr.", type.i, sep = "")])
#
#         n.points <- length(lr.far.vec)
#
#         segments(lr.far.vec[1:(n.points - 1)],
#                  lr.hr.vec[1:(n.points - 1)],
#                  lr.far.vec[2:n.points],
#                  lr.hr.vec[2:n.points],
#                  col = my.colors[2],
#                  lwd = line.lwd,
#                  lty = line.lty
#         )
#
#         # Add LR points threshold == .5
#
#         lr.hr.even <- lr.df[lr.df$threshold == .5, paste("hr.", type.i, sep = "")]
#         lr.far.even <- lr.df[lr.df$threshold == .5, paste("far.", type.i, sep = "")]
#
#
#         points(lr.far.even,
#                lr.hr.even,
#                cex = even.point.cex,
#                pch = point.pch,
#                lty = point.lty,
#                bg = "lightblue")
#
#       }
#
#       # CART
#
#       plot.cart <- F
#
#       if("cart" %in% names(x) & nrow(x$cart) > 1 & do.cart == T) {plot.cart <- T}
#
#       if(plot.cart) {
#
#         cart.df <- x$cart
#
#         cart.df <- cart.df[order(cart.df[paste("far.", type.i, sep = "")]),]
#
#         segments(cart.df[1:(nrow(cart.df) - 1), paste("far.", type.i, sep = "")],
#                  cart.df[1:(nrow(cart.df) - 1), paste("hr.", type.i, sep = "")],
#                  cart.df[2:(nrow(cart.df)), paste("far.", type.i, sep = "")],
#                  cart.df[2:(nrow(cart.df)), paste("hr.", type.i, sep = "")],
#                  col = my.colors[3],
#                  lwd = line.lwd,
#                  lty = line.lty
#         )
#
#
#         cart.hr.even <- cart.df[cart.df$miss.cost == cart.df$fa.cost, paste("hr.", type.i, sep = "")]
#         cart.far.even <- cart.df[cart.df$miss.cost == cart.df$fa.cost, paste("far.", type.i, sep = "")]
#
#         points(cart.far.even,
#                cart.hr.even,
#                pch = point.pch,
#                cex = even.point.cex,
#                bg = "red")
#
#       }
#
#
#     }
#
#
#
#
#   }

  # CLEANUP

  # par(def.par)  #- reset to default

}

