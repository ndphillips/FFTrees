# helper_plot.R:
# Utility functions for plots.
# ---------------------------

# (+) Plotting helper functions: ------

# 1. General plotting functions: ------



# 2. plot.FFTrees() helper functions: ------


# add_balls: Add balls to the plot ----

add_balls <- function(x.lim = c(-10, 0),
                      y.lim = c( -2, 0),
                      n.vec = c(20, 10),
                      pch.vec = c(21, 21),
                      ball.cex = 1,
                      bg.vec = ball.bg,
                      col.vec = ball.col,
                      ball.lwd = .70,
                      freq.text = TRUE,
                      freq.text.cex = 1.2,
                      upper.text = "",
                      upper.text.cex = 1,
                      upper.text.adj = 0,
                      rev.order = F,
                      box.col = NULL,
                      box.bg = NULL,
                      n.per.icon = NULL) {


  # Add box:
  if (is.null(box.col) == FALSE | is.null(box.bg) == FALSE) {

    rect(x.lim[1],
         y.lim[1],
         x.lim[2],
         y.lim[2],
         col = box.bg,
         border = box.col
    )
  }

  # Add upper text:
  text(mean(x.lim), y.lim[2] + upper.text.adj,
       label = upper.text, cex = upper.text.cex
  )

  a.n <- n.vec[1]
  b.n <- n.vec[2]

  a.p <- n.vec[1] / sum(n.vec)

  box.x.center <- sum(x.lim) / 2
  box.y.center <- sum(y.lim) / 2

  box.x.width <- x.lim[2] - x.lim[1]

  # Determine cases per ball:
  if (is.null(n.per.icon)) {
    max.n.side <- max(c(a.n, b.n))

    i <- max.n.side / c(1, 5, 10, 50, 100, 1000, 10000)
    i[i > 50] <- 0

    n.per.icon <- c(1, 5, 10, 50, 100, 1000, 10000)[which(i == max(i))]
  }

  # Determine general ball locations:

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


  # Add frequency text: ----

  if (freq.text) {
    text(box.x.center, y.lim[1] - 1 * (y.lim[2] - y.lim[1]) / 5, prettyNum(b.n, big.mark = ","), pos = 4, cex = freq.text.cex)
    text(box.x.center, y.lim[1] - 1 * (y.lim[2] - y.lim[1]) / 5, prettyNum(a.n, big.mark = ","), pos = 2, cex = freq.text.cex)
  }

  # Draw balls: ----

  # Noise:
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

  # Signal:
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

} # add_balls().


# get_x_dev: Adjust rectangle width of main title(s) ----

get_x_dev <- function(string, csf = .80){

  # Goal: Adjust rectangle width (to left and right from mid-point).
  # csf: constant scaling factor
  # Returns a difference value that depends on string width (from min = .15 upwards).

  n_char <- nchar(string)

  if (n_char > 15){ # widen rectangle:

    (n_char/100 * csf)

  } else { # default/minimum value:

    .15
  }

} # get_x_dev().



# get_label_cex: Adjust label cex to width of string i ----

get_label_cex <- function(i, label.box.text.cex = 2) {

  i <- nchar(i)

  label.box.text.cex * i^-.25

} # get_label_cex().



# add_level: Add level display to a plot ----

# df_lloc: Data frame with labels, values, and locations.

add_level <- function(name,
                      sub = "",
                      ok.val = .5,
                      min.val = 0,
                      max.val = 1,
                      bottom.text = "",
                      level.type = "line",
                      # needed from plot:
                      df_lloc    = NULL,
                      header_y   = NULL,
                      header_cex = NULL) {

  # Parameters:
  rect.center.x <- df_lloc$center.x[df_lloc$element == name]
  rect.center.y <- df_lloc$center.y[df_lloc$element == name]
  rect.height <- df_lloc$height[df_lloc$element == name]
  rect.width <- df_lloc$width[df_lloc$element == name]

  rect.bottom.y <- rect.center.y - rect.height / 2
  rect.top.y    <- rect.center.y + rect.height / 2

  rect.left.x  <- rect.center.x - rect.width / 2
  rect.right.x <- rect.center.x + rect.width / 2

  long.name <- df_lloc$long.name[df_lloc$element == name]
  value <- df_lloc$value[df_lloc$element == name]
  value.name <- df_lloc$value.name[df_lloc$element == name]

  #
  # level.col.fun <- circlize::colorRamp2(c(min.val, ok.val,  max.val),
  #                                        colors = c("firebrick2", "yellow", "green4"),
  #                                        transparency = .1)


  text(x = rect.center.x, y = header_y,
    labels = long.name, pos = 1, cex = header_cex
  )

  # text_outline(x = rect.center.x,
  #              y = header.y.loc,
  #              labels = long.name,
  #              pos = 1, cex = header_cex, r = .02
  # )

  value.height <- rect.bottom.y + min(c(1, ((value - min.val) / (max.val - min.val)))) * rect.height


  # Add filling: ----

  value.s <- min(value / max.val, 1)

  delta <- 1
  gamma <- .50

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
         # col = spec.level.fun(df_lloc$value[df_lloc$element == name]),
         border = "black"
    )

    text_outline(
      x = rect.center.x,
      y = value.height,
      labels = df_lloc$value.name[df_lloc$element == name],
      cex = 1.5, r = .008, pos = 3
    )

    # Add level border:

    # rect(rect.left.x,
    #      rect.bottom.y,
    #      rect.right.x,
    #      rect.top.y,
    #      border = gray(.5, .5))
  }


  if (level.type == "line") {

    # Stem:
    segments(rect.center.x,
             rect.bottom.y,
             rect.center.x,
             value.height,
             lty = 3
    )

    # Horizontal platform:
    platform.width <- .02

    segments(
      rect.center.x - platform.width,
      value.height,
      rect.center.x + platform.width,
      value.height
    )

    # Text label:
    text_outline(
      x = rect.center.x,
      y = value.height,
      labels = df_lloc$value.name[df_lloc$element == name],
      cex = 1.5, r = 0, pos = 3
    )

    # points(rect.center.x,
    #        value.height,
    #        cex = 5.5,
    #        pch = 21,
    #        bg = "white",
    #        col = "black", lwd = .5)
  }

  # Add subtext: ----

  text(
    x = rect.center.x,
    y = rect.center.y - .05,
    labels = sub,
    cex = .8,
    font = 1,
    pos = 1
  )

  # Add bottom text: ----

  text(
    x = rect.center.x,
    y = rect.bottom.y,
    labels = bottom.text,
    pos = 1
  )

} # add_level().


# pretty_dec: Print pretty decimal values ----

pretty_dec <- function(x) {

  return(paste(round(x, 2) * 100, sep = ""))

} # pretty_dec().





# ToDo: ------

# - etc.

# eof.
