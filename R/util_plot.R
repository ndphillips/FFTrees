# util_plot.R:
# Auxiliary/utility functions for plots.
# --------------------------------------


# (1) General plotting/visualization helpers: ------


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



# add_balls: Add balls/icon arrays to a plot ----

add_balls <- function(x_lim = c(-10, 0),
                      y_lim = c( -2, 0),
                      n_vec   = c(20, 10),
                      pch_vec = c(21, 21),
                      ball_cex = 1,
                      bg_vec  = "white",
                      col_vec = "black",
                      ball_lwd = .70,
                      freq_text = TRUE,
                      freq_text_cex = 1.2,
                      upper_text = "",
                      upper_text_cex = 1,
                      upper_text_adj = 0,
                      # rev_order = FALSE,  # is NOT used anywhere?
                      box_col = NULL,
                      box_bg = NULL,
                      n_per_icon = NULL) {


  par(xpd = TRUE)

  # Add box:
  if (is.null(box_col) == FALSE | is.null(box_bg) == FALSE) {

    rect(x_lim[1],
         y_lim[1],
         x_lim[2],
         y_lim[2],
         col = box_bg,
         border = box_col
    )

  }

  # Add upper text:
  text(mean(x_lim), y_lim[2] + upper_text_adj,
       label = upper_text, cex = upper_text_cex
  )

  a_n <- n_vec[1]
  b_n <- n_vec[2]

  # a_p <- n_vec[1] / sum(n_vec)  # is NOT used anywhere?

  box_x_center <- sum(x_lim) / 2
  # box_y_center <- sum(y_lim) / 2      # is NOT used anywhere?
  # box_x_width <- x_lim[2] - x_lim[1]  # is NOT used anywhere?


  if (is.null(n_per_icon)) { # determine cases per ball/icon:

    max_n_side <- max(c(a_n, b_n))

    i <- max_n_side / c(1, 5, 10, 50, 100, 1000, 10000)
    i[i > 50] <- 0

    n_per_icon <- c(1, 5, 10, 50, 100, 1000, 10000)[which(i == max(i))]

  }

  # Determine general ball/icon locations:

  a_balls <- ceiling(a_n / n_per_icon)
  b_balls <- ceiling(b_n / n_per_icon)
  # n_balls <- a_balls + b_balls  # is NOT used anywhere?

  a_ball_x <- 0
  a_ball_y <- 0
  b_ball_x <- 0
  b_ball_y <- 0

  if (a_balls > 0) {

    a_ball_x <- rep(-1:-10, each = 5, length.out = 50)[1:a_balls]
    a_ball_y <- rep(1:5, times = 10, length.out = 50)[1:a_balls]
    a_ball_x <- a_ball_x * (x_lim[2] - box_x_center) / 10 + box_x_center
    a_ball_y <- a_ball_y * (y_lim[2] - y_lim[1]) / 5 + y_lim[1]

  }

  if (b_balls > 0) {

    b_ball_x <- rep(1:10, each = 5, length.out = 50)[1:b_balls]
    b_ball_y <- rep(1:5, times = 10, length.out = 50)[1:b_balls]
    b_ball_x <- b_ball_x * (x_lim[2] - box_x_center) / 10 + box_x_center
    b_ball_y <- b_ball_y * (y_lim[2] - y_lim[1]) / 5 + y_lim[1]

  }

  # if(rev_order) {
  #
  #   x <- b_ball_x
  #   y <- b_ball_y
  #
  #   b_ball_x <- a.x.loc
  #   b_ball_y <- a.y.loc
  #
  #   a_ball_x <- x
  #   a_ball_y <- y
  #
  # }


  # Add frequency text: ----

  if (freq_text) {
    text(box_x_center, y_lim[1] - 1 * (y_lim[2] - y_lim[1]) / 5, prettyNum(b_n, big.mark = ","), pos = 4, cex = freq_text_cex)
    text(box_x_center, y_lim[1] - 1 * (y_lim[2] - y_lim[1]) / 5, prettyNum(a_n, big.mark = ","), pos = 2, cex = freq_text_cex)
  }

  # Draw balls: ----

  # Noise:
  suppressWarnings(if (a_balls > 0) {
    points(
      x = a_ball_x,
      y = a_ball_y,
      pch = pch_vec[1],
      bg  = bg_vec[1],
      col = col_vec[1],
      cex = ball_cex,
      lwd = ball_lwd
    )
  })

  # Signal:
  suppressWarnings(if (b_balls > 0) {
    points(
      x = b_ball_x,
      y = b_ball_y,
      pch = pch_vec[2],
      bg  = bg_vec[2],
      col = col_vec[2],
      cex = ball_cex,
      lwd = ball_lwd
    )
  })

  par(xpd = FALSE)

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


# get_label_cex: Adjust label cex to the width of a string i ----

# is NOT used anywhere?

# get_label_cex <- function(i, label_box_text_cex = 2) {
#
#   i <- nchar(i)
#
#   label_box_text_cex * i^-.25
#
# } # get_label_cex().



# add_level: Add level display to a plot ----

# lloc_row: Data frame with labels, size values, and locations.

add_level <- function(name,
                      sub = "",
                      ok_val = .5,
                      min_val = 0,
                      max_val = 1,
                      bottom_text = "",
                      level_type = "line",  # user argument "level.type" set in plot() to default "bar".
                      # needed from plot:
                      lloc_row   = NULL,    # element == name row (of df)
                      header_y   = NULL,
                      header_cex = NULL) {

  # Parameters:
  rect_center_x <- lloc_row$center_x
  rect_center_y <- lloc_row$center_y

  rect_height <- lloc_row$height
  rect_width  <- lloc_row$width

  rect_bottom_y <- rect_center_y - rect_height / 2
  rect_top_y    <- rect_center_y + rect_height / 2

  rect_left_x  <- rect_center_x - rect_width / 2
  rect_right_x <- rect_center_x + rect_width / 2

  long_name <- lloc_row$long_name
  value <- lloc_row$value
  value_name <- lloc_row$value_name

  # # Color gradient:
  # level_col_fun <- circlize::colorRamp2(c(min_val, ok_val,  max_val),
  #                                        colors = c("firebrick2", "yellow", "green4"),
  #                                        transparency = .1)


  text(x = rect_center_x, y = header_y,
       labels = long_name, pos = 1, cex = header_cex
  )

  # text_outline(x = rect_center_x,
  #              y = header.y.loc,
  #              labels = long_name,
  #              pos = 1, cex = header_cex, r = .02
  # )

  value_height <- rect_bottom_y + min(c(1, ((value - min_val) / (max_val - min_val)))) * rect_height


  # Add filling: ----

  value_s <- min(value / max_val, 1)

  delta <- 1
  gamma <- .50

  value_col_scale <- delta * value_s^gamma / (delta * value_s^gamma + (1 - value_s)^gamma)
  # value_col <- gray(1 - value_col_scale * .5)

  value_col <- gray(1, .25)

  # plot(seq(0, 1, .01), delta * seq(0, 1, .01) ^ gamma / (delta * seq(0, 1, .01) ^ gamma + (1 - seq(0, 1, .01)) ^ gamma))

  if (level_type == "bar") {

    rect(rect_left_x,
         rect_bottom_y,
         rect_right_x,
         value_height,
         # col = level_col_fun(value_s),
         col = value_col,
         # col = spec.level.fun(lloc_row$value),
         border = "black"
    )

    text_outline(
      x = rect_center_x,
      y = value_height,
      labels = lloc_row$value_name,
      cex = 1.5, r = .008, pos = 3
    )

    # Add level border:

    # rect(rect_left_x,
    #      rect_bottom_y,
    #      rect_right_x,
    #      rect_top_y,
    #      border = gray(.5, .5))
  }


  if (level_type == "line") {

    # stem:
    segments(rect_center_x,
             rect_bottom_y,
             rect_center_x,
             value_height,
             lty = 3
    )

    # horizontal platform:
    platform.width <- .02

    segments(
      rect_center_x - platform.width,
      value_height,
      rect_center_x + platform.width,
      value_height
    )

    # text label:
    text_outline(
      x = rect_center_x,
      y = value_height,
      labels = lloc_row$value_name,
      cex = 1.5, r = 0, pos = 3
    )

    # points(rect_center_x,
    #        value_height,
    #        cex = 5.5,
    #        pch = 21,
    #        bg = "white",
    #        col = "black", lwd = .5)
  }

  # Add subtext: ----

  text(
    x = rect_center_x,
    y = rect_center_y - .05,
    labels = sub,
    cex = .8,
    font = 1,
    pos  = 1
  )

  # Add bottom text: ----

  text(
    x = rect_center_x,
    y = rect_bottom_y,
    labels = bottom_text,
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
