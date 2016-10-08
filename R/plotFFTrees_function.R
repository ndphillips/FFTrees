#' Draws a FFTrees object.
#'
#' @description The primary purpose of this function is to visualize a Fast and Frugal Tree (FFT) for data that has already been classified using the FFTrees() function. However, if the data have not yet been classified, the function can also implement a tree specified by the user. Inputs with the (M) header are manditory. If the tree has already been implimented, then only inputs with the (A) header should be entered. If the tree has not been implimented, then only inputs with the (B) header should be entered.
#' @param x A FFTrees object created from \code{"FFTrees()"}
#' @param data Either a dataframe of new data, or one of two strings 'train' or 'test'. In this case, the corresponding dataset in the x object will be used.
#' @param tree An integer indicating which tree to plot (only valid when the tree argument is non-empty). To plot the best training (or test) tree with respect to v (HR - FAR), use "best.train" or "best.test"
#' @param decision.names A string vector of length 2 indicating the content-specific name for noise and signal cases.
#' @param main The main plot label.
#' @param n.per.icon Number of exemplars per icon
#' @param which.tree depreciated argument, only for backwards compatibility, use \code{"tree"} instead.
#' @param ... Currently ignored.
#' @importFrom stats anova predict formula model.frame
#' @importFrom graphics text points abline legend mtext segments rect arrows axis par layout plot
#' @importFrom grDevices gray col2rgb rgb
#' @export
#' @examples
#'
#' # Create FFTrees of the heart disease data
#' heart.fft <- FFTrees(formula = diagnosis ~.,
#' data = heartdisease)
#'
#' # Visualise the tree
#' plot(heart.fft,
#'      main = "Heart Disease Diagnosis",
#'      decision.names = c("Absent", "Present"))
#'
#'
#' # See the vignette for more details
#' vignette("FFTrees_plot", package = "FFTrees")
#'
#'
#'

plot.FFTrees <- function(
  x = NULL,
  data = "train",
  which.tree = NULL,
  tree = "best.train",
  main = "Data",
  n.per.icon = NULL,
  decision.names = c("Noise", "Signal"),
  ...
) {
# -------------------------
# TESTING VALUES
# --------------------------


n.trees <- nrow(x$tree.stats$train)

# Check for problems and depreciated arguments
{
  if(is.null(which.tree) == F) {

    message("The which.tree argument is depreciated and is now just called tree. Please use tree from now on to avoid this message.")

    tree <- which.tree

  }

  if(class(x) != "FFTrees") {

    stop("You did not include a valid FFTrees class object or specify the tree directly with level.names, level.classes (etc.). Either create a valid FFTrees object with FFTrees() or specify the tree directly.")

  }

  if(tree == "best.test" & is.null(x$data$test)) {

    print("You wanted to plot the best test tree (tree = 'best.test') but there were no test data, I'll plot the best training tree instead")

    tree <- "best.train"

  }

  if(is.numeric(tree) & (tree %in% 1:n.trees) == F) {

    stop(paste("You asked for a tree that does not exist. This object has", n.trees, "trees"))

  }

  if(class(data) == "character") {

    if(data == "test" & is.null(x$data$test)) {stop("You asked to plot the test data but there are no test data in the FFTrees object")}

  }
}

# DEFINE PLOTTING TREE

if(tree == "best.train") {

 tree <- x$tree.stats$train$tree[which.max(x$tree.stats$train$v)]

}
if(tree == "best.test") {

 tree <- x$tree.stats$test$tree[which.max(x$tree.stats$test$v)]


}

# DEFINE CRITICAL OBJECTS

if(data == "train") {

  data.mf <- model.frame(formula = x$formula,
                         data = x$data$train)
  criterion.v <- data.mf[,1]
  decision.v <- x$decision$train[,tree]
  tree.stats <- x$tree.stats$train
  level.stats <- x$level.stats$train[x$level.stats$train$tree == tree,]
  lr.stats <- data.frame("hr" = x$lr$stats$hr.train, "far" = x$lr$stats$far.train)
  cart.stats <- data.frame("hr" = x$cart$stats$hr.train, "far" = x$cart$stats$far.train)
  n.exemplars <- nrow(data.mf)
}
if(data == "test") {

  data.mf <- model.frame(formula = x$formula,
                         data = x$data$test)
  criterion.v <- data.mf[,1]
  decision.v <- x$decision$test[,tree]
  tree.stats <- x$tree.stats$test
  level.stats <- x$level.stats$test[x$level.stats$test$tree == tree,]
  lr.stats <- data.frame("hr" = x$lr$stats$hr.test, "far" = x$lr$stats$far.test)
  cart.stats <- data.frame("hr" = x$cart$stats$hr.test, "far" = x$cart$stats$far.test)
  n.exemplars <- nrow(data.mf)
}


final.stats <- classtable(prediction.v = decision.v,
                          criterion.v = criterion.v
)

# ADD LEVEL STATISTICS
n.levels <- nrow(level.stats)
# Add marginal classification statistics to level.stats
level.stats[c("hi.m", "mi.m", "fa.m", "cr.m")] <- NA
for(i in 1:n.levels) {

  if(i == 1) {

    level.stats[1, c("hi.m", "mi.m", "fa.m", "cr.m")] <- level.stats[1, c("hi", "mi", "fa", "cr")]
  }

  if(i > 1) {

    level.stats[i, c("hi.m", "mi.m", "fa.m", "cr.m")] <- level.stats[i, c("hi", "mi", "fa", "cr")] - level.stats[i - 1, c("hi", "mi", "fa", "cr")]
  }

}

# -------------------------
# Define plotting parameters
# --------------------------
{
{

# Some general parameters

  do.roc <- F
  ball.col = c(gray(0), gray(0))
  ball.bg = c(gray(1), gray(1))
  ball.pch = c(21, 24)
  ball.cex = c(1, 1)
  error.col <- "red"
  correct.col <- "green"
  max.label.length <- 10
  def.par <- par(no.readonly = TRUE)


level.stats$level.name.t <-  strtrim(level.stats$cue, max.label.length)


ball.box.width <- 10
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
panel.title.cex <- 2


plotting.parameters.df <- data.frame(
  n.levels = 1:6,
  plot.height = c(10, 12, 15, 19, 23, 25),
  plot.width = c(14, 16, 20, 24, 28, 32),
  label.box.text.cex = c(1.7, 1.7, 1.5, 1.25, 1, 1),
  break.label.cex = c(1.5, 1.5, 1.25, 1, .75, .5)
)

if(n.levels < 6) {

label.box.text.cex <- plotting.parameters.df$label.box.text.cex[n.levels]
break.label.cex <- plotting.parameters.df$label.box.text.cex[n.levels]
plot.height <- plotting.parameters.df$plot.height[n.levels]
plot.width <- plotting.parameters.df$plot.width[n.levels]

}


if(n.levels >= 6) {

  label.box.text.cex <- plotting.parameters.df$label.box.text.cex[6]
  break.label.cex <- plotting.parameters.df$label.box.text.cex[6]
  plot.height <- plotting.parameters.df$plot.height[6]
  plot.width <- plotting.parameters.df$plot.width[6]

}


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

if(is.null(n.per.icon)) {

  max.n.side <- max(c(sum(criterion.v == 0), sum(criterion.v == 1)))

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
                          n.per.icon = NULL

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

  if(is.null(n.per.icon)) {

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
text(x = .5, y = .80, paste("N = ", n.exemplars, "", sep = ""), cex = 1.25)

n.trueneg <- with(final.stats, cr + fa)
n.truepos <- with(final.stats, hi + mi)

text(.5, .65, paste("True ", decision.names[1], sep = ""), pos = 2, cex = 1.2, adj = 1)

text(.5, .65, paste("True ", decision.names[2], sep = ""), pos = 4, cex = 1.2, adj = 0)


#points(.9, .8, pch = 1, cex = 1.2)
#text(.9, .8, labels = paste(" = ", n.per.icon, " cases", sep = ""), pos = 4)

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
              n.per.icon = n.per.icon
)

par(xpd = F)






}

# -------------------------
# 2. TREE
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
  text(x = 0, y = 0, paste("Tree (#", tree, ")", sep = ""), cex = panel.title.cex)

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

    hi.i <- level.stats$hi.m[level.i]
    mi.i <- level.stats$mi.m[level.i]
    fa.i <- level.stats$fa.m[level.i]
    cr.i <- level.stats$cr.m[level.i]

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
           labels = level.stats$level.name.t[level.i],
           cex = label.box.text.cex
      )


    }


    # -----------------------
    # Left (Noise) Classification
    # -----------------------

    {

      # Exit node on left

      if((mi.i + cr.i) > 0 | level.i == n.levels) {

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

        if(max(c(cr.i, mi.i)) > 0) {

          add.balls.fun(x.lim = ball.x.lim,
                        y.lim = ball.y.lim,
                        n.vec = c(cr.i, mi.i),
                        pch.vec = c(noise.ball.pch, signal.ball.pch),
                        #  bg.vec = c(noise.ball.bg, signal.ball.bg),
                        bg.vec = c(correct.bg, error.bg),
                        col.vec = c(correct.border, error.border),
                        freq.text = T,
                        n.per.icon = n.per.icon,
                        ball.cex = ball.cex
          )

        }

        # level break label

        pos.direction.symbol <- c("<=", "<", "=", "!=", ">", ">=")[which(level.stats$direction[level.i] == c(">", ">=", "!=", "=", "<=", "<"))]
        neg.direction.symbol <- c("<=", "<", "=", "!=", ">", ">=")[which(level.stats$direction[level.i] == c("<=", "<", "=", "!=", ">", ">="))]


        text.outline(x = subplot.center[1] - 1,
                     y = subplot.center[2],
                     labels = paste(pos.direction.symbol, " ", level.stats$threshold[level.i], sep = ""),
                     pos = 2, cex = break.label.cex, r = .1
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

      }

      # New level on left

      if((mi.i + cr.i) == 0 & level.i != n.levels) {

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
             labels = level.stats$level.name.t[level.i + 1],
             cex = label.box.text.cex
        )

      }

    }

    # -----------------------
    # Right Node
    # -----------------------

    {

      # Exit node on right

      if((hi.i + fa.i) > 0 | level.i == n.levels) {


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

        if(max(c(fa.i, hi.i)) > 0) {

          add.balls.fun(x.lim = ball.x.lim,
                        y.lim = ball.y.lim,
                        n.vec = c(fa.i, hi.i),
                        pch.vec = c(noise.ball.pch, signal.ball.pch),
                        #      bg.vec = c(noise.ball.bg, signal.ball.bg),
                        bg.vec = c(error.bg, correct.bg),
                        col.vec = c(error.border, correct.border),
                        freq.text = T,
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


      }


      # New level on right

      if((hi.i + fa.i) == 0 & level.i != n.levels) {

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
             labels = level.stats$level.name.t[level.i + 1],
             cex = label.box.text.cex
        )


      }

    }


    # -----------------------
    # Update plot center
    # -----------------------


    if((mi.i + cr.i) > 0 & (hi.i + fa.i) == 0) {

      subplot.center <- c(subplot.center[1] + 2,
                          subplot.center[2] - 4)
    }

    if((mi.i + cr.i) == 0 & (hi.i + fa.i) > 0) {

      subplot.center <- c(subplot.center[1] - 2,
                          subplot.center[2] - 4)
    }


  }

}

# -----------------------
# 3. CUMULATIVE PERFORMANCE
# -----------------------
{


# OBTAIN FINAL STATISTICS

fft.auc <- auc(tree.stats$hr[order(tree.stats$far)], tree.stats$far[order(tree.stats$far)])
fft.hr.vec <- tree.stats$hr[order(tree.stats$far)]
fft.far.vec <- tree.stats$far[order(tree.stats$far)]
lr.hr <- lr.stats$hr
lr.far <- lr.stats$far
cart.hr <- cart.stats$hr
cart.far <- cart.stats$far


# General plotting space
{

# PLOTTING PARAMETERS
header.y.loc <- 1.0
subheader.y.loc <- .9

header.cex <- 1.2
subheader.cex <- .9

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

par(mar = c(0, 0, 2, 0))

plot(1, xlim = c(0, 1), ylim = c(0, 1),
     bty = "n", type = "n",
     xlab = "", ylab = "",
     yaxt = "n", xaxt = "n")

par(xpd = T)
segments(0, 1.1, 1, 1.1, col = gray(.2, .5), lwd = .5, lty = 1)
rect(.25, 1, .75, 1.2, col = "white", border = NA)

if(data == "train") {title.text <- "Performance (Fitting)"}
if(data == "test") {title.text <- "Performance (Prediction)"}

text(.5, 1.1, title.text, cex = panel.title.cex)
par(xpd = F)

}

# Color function (taken from colorRamp2 function in circlize package)
col.fun <- circlize::colorRamp2(c(0, .75, 1), c("red", "yellow", "green"), transparency = .2)

# Specificity level
{

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
       cex = 1.2)

  text(x = rect.center,
       y = (rect.top + rect.bottom) / 2.2,
       labels = paste(final.stats$cr, " / ", (final.stats$cr + final.stats$fa), sep = ""),
       cex = .8,
       font = 1,
       pos = 1)

}

# HR level
{

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
{

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
{

  rect.top <- final.auc.center[2] + final.auc.dim[2] / 2
  rect.bottom <-  final.auc.center[2] - final.auc.dim[2] / 2
  rect.center <-  final.auc.center[1]
  rect.width <- final.auc.dim[1] / 2

  text(rect.center, header.y.loc, "AUC", cex = header.cex, pos= 1)

  # Colored Fill
  rect(rect.center - rect.width / 2,
       rect.bottom,
       rect.center + rect.width / 2,
       ((fft.auc - .5) / .5) * (rect.top - rect.bottom) + rect.bottom,
       col = col.fun((fft.auc - .5) / .5),
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
       labels = round(fft.auc, 2),
       cex = 1.2,
       font = 1)

}

# Classification table
{

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
{

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

  # Add segments and points for all trees but tree

  if(length(roc.order) > 1) {

  segments(final.roc.x.loc[1] + c(0, fft.far.vec.ord) * final.roc.dim[1],
           final.roc.y.loc[1] + c(0, fft.hr.vec.ord) * final.roc.dim[2],
           final.roc.x.loc[1] + c(fft.far.vec.ord, 1) * final.roc.dim[1],
           final.roc.y.loc[1] + c(fft.hr.vec.ord, 1) * final.roc.dim[2], lwd = 1, col = gray(.5, .5))

  points(final.roc.x.loc[1] + fft.far.vec.ord[-(which(roc.order == tree))] * final.roc.dim[1],
         final.roc.y.loc[1] + fft.hr.vec.ord[-(which(roc.order == tree))] * final.roc.dim[2],
         pch = 21, cex = 2.5, col = transparent("green", .3),
         bg = transparent("white", .5))

  text(final.roc.x.loc[1] + fft.far.vec.ord[-(which(roc.order == tree))] * final.roc.dim[1],
       final.roc.y.loc[1] + fft.hr.vec.ord[-(which(roc.order == tree))] * final.roc.dim[2],
       labels = roc.order[-(which(roc.order == tree))], cex = 1, col = gray(.2))

  }


  # Add large point for plotted tree

  points(final.roc.x.loc[1] + fft.far.vec[tree] * final.roc.dim[1],
         final.roc.y.loc[1] + fft.hr.vec[tree] * final.roc.dim[2],
         pch = 21, cex = 2.5, col = transparent("green", .3),
         bg = transparent("green", .7), lwd = 3)

  text(final.roc.x.loc[1] + fft.far.vec[tree] * final.roc.dim[1],
         final.roc.y.loc[1] + fft.hr.vec[tree] * final.roc.dim[2],
        labels = tree, cex = 1, col = gray(.2))




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

# Reset plotting space
par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 1) + .1)

}
