# helper_plot.R:
# Auxiliary/utility functions for plots.
# --------------------------------------


# (1) General plotting helpers: ------


# num_space: ------

# \code{num_space} computes the width of a representation of \code{x}
# (as number of digits, in base 10)
# when using scales::comma(x) (as in \code{console_confusionmatrix} below).

num_space <- function(x) {

  if (isTRUE(all.equal(x, 0))) { return(1) }

  # else:
  nchar(scales::comma(x))

} # num_space().



# console_confusionmatrix: ------

console_confusionmatrix <- function(hi, mi, fa, cr,  sens.w,  cost) {

  # hi <- 6534  # 4debugging
  # mi <-    5
  # fa <-  765
  # cr <-   54

  # sens.w <- .70

  # cost <- 0


  # Parameters: ----

  # Labels:
  sum_lbl <- "Totals:"  # or "Sums:"

  # Number of digits in N:
  N <- (hi + mi + fa + cr)
  ndg_N <- num_space(N)

  # Column width (of columns 2 & 3):
  col_width <- max(c(8, ndg_N + 5))


  # Header row: ----

  cat("|",
      rep(" ", times = 10),
      "| True +",
      rep(" ", col_width - 7),
      "| True -",
      rep(" ", col_width - 7),
      "|",
      rep(" ", nchar(" N = ") + ndg_N - nchar(sum_lbl)),
      sum_lbl,
      "\n",
      sep = ""
  )

  # Top line:
  cat("|", rep("-", times = 10), "|", rep("-", times = col_width), "|", rep("-", times = col_width), "|\n", sep = "")


  # Decide + row: ----

  cat("| Decide +",
      " | ",
      in_grey("hi"),
      rep(" ", max(1, col_width - 4 - num_space(hi))),
      in_green(scales::comma(hi)),
      # rep(" ", max(0, col_width - num_space(hi) - 4)),
      " | ",
      in_grey("fa"),
      rep(" ", max(1, col_width - 4 - num_space(fa))),
      in_red(scales::comma(fa)),
      # rep(" ", max(0, col_width - num_space(fa) - 4)),
      " | ",
      sep = ""
  )

  cat(rep(" ", max(1, 4 + ndg_N - num_space(hi + fa))), sep = "")
  cat(scales::comma(hi + fa))

  cat("\n")


  # Decide - row: ----

  cat("| Decide -",
      " | ",
      in_grey("mi"),
      rep(" ", max(1, col_width - 4 - num_space(mi))),
      in_red(scales::comma(mi)),
      # rep(" ", max(0, col_width - num_space(mi) - 4)),
      " | ",
      in_grey("cr"),
      rep(" ", max(1, col_width - 4 - num_space(cr))),
      in_green(scales::comma(cr)),
      # rep(" ", max(0, col_width - num_space(cr) - 4)),
      " | ",
      sep = ""
  )

  cat(rep(" ", max(1, 4 + ndg_N - num_space(mi + cr))), sep = "")
  cat(scales::comma(mi + cr))

  cat("\n")

  # Bottom line:
  cat("|-", rep("-", times = 9), "|", rep("-", times = col_width), "|", rep("-", times = col_width), "|\n", sep = "")


  # Bottom totals: ----

  cat(rep(" ", times = (9 - nchar(sum_lbl))), sep = "")
  cat(sum_lbl)
  cat("    ")  #  w/o |
  # cat("  | ")  # with |

  cat(rep(" ", max(1, col_width - 2 - num_space(hi + mi))), sep = "")
  cat(scales::comma(hi + mi))
  cat("   ")  #  w/o |
  # cat(" | ")  # with |

  cat(rep(" ", max(1, col_width - 2 - num_space(cr + fa))), sep = "")
  cat(scales::comma(cr + fa))
  cat("   ")  #  w/o |
  # cat(" | ")  # with |

  cat("N = ")
  cat(cli::style_underline(scales::comma(N), sep = ""), sep = "")

  cat("\n\n")


  # Accuracy info: ----

  # Compute acc statistics (or use add_stats() above):
  acc <- (hi + cr) / N

  ppv <- hi / (hi + fa)
  npv <- cr / (cr + mi)

  sens <- hi / (hi + mi)
  spec <- cr / (cr + fa)

  # bacc <- (sens + spec) / 2  # = (sens * .50) + (spec * .50)
  # wacc <- (sens * sens.w) + (spec * (1 - sens.w))

  # Get either bacc OR wacc (based on sens.w):
  bacc_wacc <- get_bacc_wacc(sens, spec, sens.w)


  # Print labels and values:

  cat("acc  =", scales::percent(acc, accuracy = .1), sep = " ")

  cat("   ppv  =", scales::percent(ppv, accuracy = .1), sep = " ")
  cat("   npv  =", scales::percent(npv, accuracy = .1), sep = " ")

  cat("\n")

  # if (enable_wacc(sens.w)){ # print wacc:
  #   cat("wacc =", scales::percent(wacc, accuracy = .1), sep = " ")
  # } else { # print bacc:
  #   cat("bacc =", scales::percent(bacc, accuracy = .1), sep = " ")
  # }
  cat(names(bacc_wacc), "=", scales::percent(bacc_wacc, accuracy = .1), sep = " ")

  cat("   sens =", scales::percent(sens, accuracy = .1), sep = " ")
  cat("   spec =", scales::percent(spec, accuracy = .1), sep = " ")

  cat("\n")

  if (enable_wacc(sens.w)){ # print sens.w (as round percentage value):
    cat("sens.w = ", scales::percent(sens.w, accuracy = 1), sep = "")
    cat("\n")
  }

  # Baseline info: Rate of positive criterion values / "True +" cases: ----
  # cat("br   =", scales::percent((hi + mi) / N, accuracy = .1), sep = " ")

  # Bias info: Rate of positive decisions / "Decide +" cases: ----
  # cat("   bias =", scales::percent((hi + fa) / N, accuracy = .1), sep = " ")


  # cat("\n")


  # Cost info: ----

  # Moved cost info to speed & frugality feedback (in printFFTrees_function.R),
  # as cost is NOT an accuracy measure:
  # cat("E(cost) =", scales::comma(cost, accuracy = .001), sep = " ")


  # Output: none. ------

} # console_confusionmatrix().


# text_outline: ------

# Adds text with a white background (taken from Dirk Wulff www.dirkwulff.org).

text_outline <- function(x, y,
                         labels = "test",
                         col = "black",
                         font = 1,
                         bg = "white",
                         r = 0.02,
                         h = 1,
                         w = 1,
                         cex = 1,
                         adj = .5,
                         pos = NULL) {

  # Draw background:
  is <- seq(0, 2 * pi, length = 72)

  for (i in is) {
    xn <- x + cos(i) * r * w
    yn <- y + sin(i) * r * h
    text(xn, yn, labels = labels, col = bg, cex = cex, adj = adj, pos = pos, font = font)
  }

  # Foreground:
  text(x, y, labels = labels, col = col, cex = cex, adj = adj, pos = pos, font = font)

} # text_outline().



# transparent: ------

# Make text color transparent.

transparent <- function(col_orig = "red",
                        trans.val = .5) {

  n_cols <- length(col_orig)
  col_orig <- col2rgb(col_orig)
  col_final <- rep(NA, n_cols)

  for (i in 1:n_cols) {
    col_final[i] <- rgb(col_orig[1, i], col_orig[2, i], col_orig[
      3,
      i
    ], alpha = (1 - trans.val) * 255, maxColorValue = 255)
  }

  return(col_final)

} # transparent().





# (2) Specific functions for plot.FFTrees(): ------



# add_balls: Add balls to the plot ----

add_balls <- function(x.lim = c(-10, 0),
                      y.lim = c( -2, 0),
                      n.vec = c(20, 10),
                      pch.vec = c(21, 21),
                      ball.cex = 1,
                      bg.vec  = "white",
                      col.vec = "black",
                      ball.lwd = .70,
                      freq.text = TRUE,
                      freq.text.cex = 1.2,
                      upper.text = "",
                      upper.text.cex = 1,
                      upper.text.adj = 0,
                      rev.order = FALSE,
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

# lloc_row: Data frame with labels, values, and locations.

add_level <- function(name,
                      sub = "",
                      ok.val = .5,
                      min.val = 0,
                      max.val = 1,
                      bottom.text = "",
                      level.type = "line",
                      # needed from plot:
                      lloc_row   = NULL,  # element == name row (of df)
                      header_y   = NULL,
                      header_cex = NULL) {

  # Parameters:
  rect.center.x <- lloc_row$center.x
  rect.center.y <- lloc_row$center.y
  rect.height <- lloc_row$height
  rect.width <- lloc_row$width

  rect.bottom.y <- rect.center.y - rect.height / 2
  rect.top.y    <- rect.center.y + rect.height / 2

  rect.left.x  <- rect.center.x - rect.width / 2
  rect.right.x <- rect.center.x + rect.width / 2

  long.name <- lloc_row$long.name
  value <- lloc_row$value
  value.name <- lloc_row$value.name

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
         # col = spec.level.fun(lloc_row$value),
         border = "black"
    )

    text_outline(
      x = rect.center.x,
      y = value.height,
      labels = lloc_row$value.name,
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
      labels = lloc_row$value.name,
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
