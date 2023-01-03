#' Plot an \code{FFTrees} object
#'
#' @description \code{plot.FFTrees} visualizes an \code{FFTrees} object created by the \code{\link{FFTrees}} function.
#'
#' \code{plot.FFTrees} is the main plotting function of the \strong{FFTrees} package and
#' called when evaluating the generic \code{\link{plot}} on an \code{FFTrees} object.
#'
#' \code{plot.FFTrees} visualizes a selected FFT, key data characteristics, and various aspects of classification performance.
#'
#' As \code{x} may not contain test data, \code{plot.FFTrees} by default plots the performance characteristics
#' for training data (i.e., fitting), rather than for test data (i.e., for prediction).
#' When test data is available, specifying \code{data = "test"} plots prediction performance.
#'
#' Whenever the sensitivity weight (\code{sens.w}) is set to its default of \code{sens.w = 0.50},
#' a level shows \emph{balanced} accuracy (\code{bacc}). If, however, \code{sens.w} deviates from its default,
#' the level shows the tree's \emph{weighted} accuracy value (\code{wacc}) and the current \code{sens.w} value (below the level).
#'
#' Many aspects of the plot (e.g., its panels) and the FFT's appearance (e.g., labels of its nodes and exits)
#' can be customized by setting corresponding arguments.
#'
#' @param x An \code{FFTrees} object created by the \code{\link{FFTrees}} function.
#'
#' @param data The type of data in \code{x} to be plotted (as a string) or a test dataset (as a data frame).
#' \itemize{
#'   \item{A valid data string must be either \code{'train'} (for fitting performance) or \code{'test'} (for prediction performance).}
#'   \item{For a valid data frame, the specified tree is evaluated and plotted for this data (as 'test' data),
#'   but the global \code{FFTrees} object \code{x} remains unchanged unless it is re-assigned.}
#'  }
#' By default, \code{data = 'train'} (as \code{x} may not contain test data).
#'
#' @param what What should be plotted (as a string)? Valid options are:
#' \describe{
#'   \item{'all'}{Plot the tree diagram with all corresponding guides and performance statistics, but excluding cue accuracies.}
#'   \item{'cues'}{Plot only the marginal accuracy of cues in ROC space.
#'   Note that cue accuracies are \emph{not} shown when calling \code{what = 'all'} and use the \code{\link{showcues}} function.}
#'   \item{'icontree'}{Plot tree diagram with icon arrays on exit nodes.
#'   Consider also setting \code{n.per.icon} and \code{show.iconguide}.}
#'   \item{'tree'}{Plot only the tree diagram.}
#'   \item{'roc'}{Plot only the performance of tree(s) (and comparison algorithms) in ROC space.}
#' }
#' Default: \code{what = 'all'}.
#'
#' @param tree The tree to be plotted (as an integer, only valid when the corresponding tree argument is non-empty).
#' Default: \code{tree = 1}.
#' To plot the best training or best test tree with respect to the \code{goal} specified during FFT construction,
#' use \code{'best.train'} or \code{'best.test'}, respectively.
#'
#' @param main The main plot label (as a character string).
#'
#' @param cue.labels An optional string of labels for the cues / nodes (as character vector).
#' @param decision.labels A character vector of length 2 indicating the content-specific names for noise and signal predictions/exits.
#' @param cue.cex The size of the cue labels (as numeric).
#' @param threshold.cex The size of the threshold labels (as numeric).
#' @param decision.cex The size of the decision labels (as numeric).
#'
#' @param comp Should the performance of competitive algorithms (e.g.; logistic regression, random forests, etc.)
#' be shown in the ROC plot (if available, as logical)?
#'
#' @param show.header Show header with basic data properties (in top panel, as logical)?
#'
#' @param show.iconguide Show icon guide (in middle panel, as logical)?
#' @param show.tree Show nodes and exits of FFT (in middle panel, as logical)?
#' @param show.icons Show exit cases as icon arrays (in middle panel, as logical)?
#'
#' @param show.confusion Show 2x2 confusion matrix (in bottom panel, as logical)?
#' @param show.levels Show performance levels (in bottom panel, as logical)?
#' @param show.roc Show ROC curve (in bottom panel, as logical)?
#'
#' @param hlines Show horizontal panel separation lines (as logical)?
#' Default: \code{hlines = TRUE}.
#'
#' @param label.tree Label for the FFT (optional, as character string).
#' @param label.performance Labels for the performance section (optional, as character string).
#'
#' @param n.per.icon Number of cases per icon (as numeric).
#' @param level.type How should bottom levels be drawn (as a string)? Can be \code{"bar"} (the default) or \code{"line"}.
#'
#' @param which.tree Deprecated argument. Use \code{tree} instead.
#'
#' @param decision.names Deprecated argument. Use \code{decision.labels} instead.
#'
#' @param stats Deprecated argument. Should statistical information be plotted (as logical)?
#' Use \code{what = "all"} to include performance statistics
#' and \code{what = "tree"} to plot only a tree diagram.
#'
#' @param ... Graphical parameters (passed to text of panel titles,
#' to \code{\link{showcues}} when \code{what = 'cues'}, or
#' to \code{\link{title}} when \code{what = 'roc'}).
#'
#' @return An invisible \code{FFTrees} object \code{x}
#' and a plot visualizing and describing an FFT (as side effect).
#'
#'
#' @examples
#' # Create FFTs (for heartdisease data):
#' heart_fft <- FFTrees(formula = diagnosis ~ .,
#'                      data = heart.train)
#'
#' # Visualize the default FFT (Tree #1, what = 'all'):
#' plot(heart_fft, main = "Heart disease",
#'      decision.labels = c("Absent", "Present"))
#'
#' # Visualize cue accuracies (in ROC space):
#' plot(heart_fft, what = "cues",  main = "Cue accuracies for heart disease data")
#'
#' # Visualize tree diagram with icon arrays on exit nodes:
#' plot(heart_fft, what = "icontree", n.per.icon = 2,
#'      main = "Diagnosing heart disease")
#'
#' # Visualize performance comparison in ROC space:
#' plot(heart_fft, what = "roc", main = "Performance comparison for heart disease data")
#'
#' # Visualize predictions of FFT #2 (for new test data) with custom options:
#' plot(heart_fft, tree = 2, data = heart.test,
#'      main = "Predicting heart disease",
#'      cue.labels = c("1. thal?", "2. cp?", "3. ca?", "4. exang"),
#'      decision.labels = c("ok", "sick"), n.per.icon = 2,
#'      show.header = TRUE, show.confusion = FALSE, show.levels = FALSE, show.roc = FALSE,
#'      hlines = FALSE, font = 3, col = "steelblue")
#'
#' # # For details, see
#' # vignette("FFTrees_plot", package = "FFTrees")
#'
#'
#' @family plot functions
#'
#' @seealso
#' \code{\link{showcues}} for plotting cue accuracies;
#' \code{\link{print.FFTrees}} for printing FFTs;
#' \code{\link{summary.FFTrees}} for summarizing FFTs;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @importFrom stats formula model.frame predict
#' @importFrom graphics arrows axis abline layout legend mtext par plot points segments text title rect
#' @importFrom grDevices col2rgb gray rgb
#'
#' @export

plot.FFTrees <- function(x = NULL,
                         data = "train",
                         what = "all",  # valid_what <- c("all", "default",  "cues",  "tree", "icontree",  "roc")
                         tree = 1,
                         main = NULL,
                         cue.labels = NULL,
                         decision.labels = NULL,
                         cue.cex = NULL,
                         threshold.cex = NULL,
                         decision.cex = 1,
                         comp = TRUE,
                         show.header = NULL,
                         show.tree = NULL,
                         show.confusion = NULL,
                         show.levels = NULL,
                         show.roc = NULL,
                         show.icons = NULL,
                         show.iconguide = NULL,
                         hlines = TRUE,
                         label.tree = NULL,
                         label.performance = NULL,
                         n.per.icon = NULL,
                         level.type = "bar",
                         # deprecated arguments:
                         which.tree = NULL,      # deprecated: Use tree instead.
                         decision.names = NULL,  # deprecated: Use decision.labels instead.
                         stats = NULL,           # deprecated: Use what = "all" or what = "tree" instead.
                         # graphical parameters:
                         ...) {

  # Prepare: ------

  par0 <- par(no.readonly = TRUE)
  on.exit(par(par0), add = TRUE)


  # Deprecated arguments: ----

  if (is.null(which.tree) == FALSE) {

    warning("plot.FFTrees: 'which.tree' is deprecated. Use 'tree' instead.")

    tree <- which.tree
  }

  if (is.null(decision.names) == FALSE) {

    warning("plot.FFTrees: 'decision.names' is deprecated, use 'decision.labels' instead.")

    decision.labels <- decision.names
  }

  if (is.null(stats) == FALSE){

    warning("plot.FFTrees: 'stats' is deprecated, use either what = 'all' or what = 'tree' instead.")

    if (stats) { what <- "all" } else { what <- "tree" }
  }


  # Verify what: ----

  valid_what <- c("all", "default",
                  "cues", "tree", "icontree", "roc")

  what <- tolower(substr(what, 1, 3))  # 4robustness

  if (what %in% substr(valid_what, 1, 3) == FALSE) {

    valid_string <- paste(valid_what, collapse = ", ")
    valid_string_q <- sapply(strsplit(valid_string, ', '), FUN = add_quotes)

    stop(paste0("what must be a string in c(", valid_string_q, ").", sep = ""))
  }


  # Handle what: ----

  if (what == "cue") { # handle special case:

    showcues(x = x, main = main, ...)  # pass key inputs + graphical parameters

    # Note: The argument data = data was removed from showcues(),
    #       as currently no cue accuracy statistics exist in x.

  }


  if (what != "cue") { # ALL else in function: what in c("all", "tree", "roc")

    # Set show.parts parameters: ----

    if (what == "all" | what == "def") { # default:

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

    } # if (what == "all" | "def").


    if (what == "tre") { # tree diagram only:

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

    } # if (what == "tre").


    if (what == "ico") { # icontree / tree with icons:

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
        show.icons <- TRUE
      }
      if (is.null(show.iconguide)) {
        show.iconguide <- FALSE
      }

    } # if (what == "ico").


    if (what == "roc") { # roc only:

      show.header <- FALSE
      show.tree <- FALSE
      show.confusion <- FALSE
      show.levels <- FALSE
      show.roc <- TRUE
      show.icons <- FALSE
      show.top <- FALSE

      hlines <- FALSE

    } # if (what == "roc").


    # Determine layout: ----

    # Constants (currently fixed parameters):
    show_icon_guide_legend <- FALSE

    # Top, middle, and bottom:
    if (show.header & show.tree & (show.confusion | show.levels | show.roc)) {

      show.top    <- TRUE
      show.middle <- TRUE
      show.bottom <- TRUE

      layout(matrix(1:3, nrow = 3, ncol = 1),
             widths = c(6),
             heights = c(1.2, 3, 1.8)
      )
    }

    # Top and middle only:
    if (show.header & show.tree & (show.confusion == FALSE & show.levels == FALSE & show.roc == FALSE)) {

      show.top <- TRUE
      show.middle <- TRUE
      show.bottom <- FALSE

      layout(matrix(1:2, nrow = 2, ncol = 1),
             widths = c(6),
             heights = c(1.2, 3))
    }

    # Middle and bottom only:
    if (show.header == FALSE & show.tree & (show.confusion | show.levels | show.roc)) {

      show.top <- FALSE
      show.middle <- TRUE
      show.bottom <- TRUE

      layout(matrix(1:2, nrow = 2, ncol = 1),
             widths = c(6),
             heights = c(3, 1.8)
      )
    }

    # Middle only:
    if (show.header == FALSE & show.tree & (show.confusion == FALSE & show.levels == FALSE & show.roc == FALSE)) {

      show.top <- FALSE
      show.middle <- TRUE
      show.bottom <- FALSE

      layout(matrix(1:1, nrow = 1, ncol = 1),
             widths = c(6),
             heights = c(3)
      )
    }

    # Bottom only:
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


    # data: ----

    # Note: data can be either a string "train"/"test"
    #       OR an entire data frame (of new test data):

    if (inherits(data, "character")) {

      data <- tolower(data)  # 4robustness

      # testthat::expect_true(data %in% c("train", "test"))
      if (!data %in% c("test", "train")){
        stop("The data to plot must be 'test' or 'train'.")
      }
    }


    if (inherits(data, "data.frame")) {

      message("Applying FFTrees object x to new test data...")

      x <- fftrees_apply(x, mydata = "test", newdata = data)

      message("Success, but re-assign output to x or use fftrees_apply() to globally change x")

      data <- "test" # in rest of this function

    }


    # Extract important parameters from x: ------

    # goal: ----

    goal <- x$params$goal

    # decision.labels:
    if (is.null(decision.labels)) {
      if (("decision.labels" %in% names(x$params))) {
        decision.labels <- x$params$decision.labels
      } else {
        decision.labels <- c(0, 1)
      }
    }

    # main: ----

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


    # tree: ----

    # Verify tree input: ----

    tree <- verify_tree(x = x, data = data, tree = tree)  # use helper (for plotting AND printing)


    # Get "best" tree: ----

    if (tree == "best.train") {

      if (data == "test"){
        warning("You asked for the 'best.train' tree, but data was set to 'test'. Used the best tree for 'train' data instead...")
        data <- "train"
        if (is.null(main)) { main <- "Data (Training)" }
      }

      # tree <- x$trees$best$train  # using current x
      tree <- select_best_tree(x, data = "train", goal = x$params$goal)  # using helper
    }

    if (tree == "best.test") {

      if (data == "train"){
        warning("You asked for the 'best.test' tree, but data was set to 'train'. Used the best tree for 'test' data instead...")
        data <- "test"
        if (is.null(main)) { main <- "Data (Testing)" }
      }

      # tree <- x$trees$best$test  # using current x
      tree <- select_best_tree(x, data = "test", goal = x$params$goal)  # using helper
    }


    # Define critical objects: ------

    # decision_v  <- x$trees$decisions[[data]][[tree]]$decision
    tree_stats  <- x$trees$stats[[data]]
    level_stats <- x$trees$level_stats[[data]][x$trees$level_stats[[data]]$tree == tree, ]

    n_exemplars <- nrow(x$data[[data]])
    n_pos_cases <- sum(x$data[[data]][[x$criterion_name]])
    n_neg_cases <- sum(x$data[[data]][[x$criterion_name]] == FALSE)
    mcu <- x$trees$stats[[data]]$mcu[tree]
    crit_br <- mean(x$data[[data]][[x$criterion_name]])

    final_stats <- tree_stats[tree, ]


    # Add level statistics: ----

    n.levels <- nrow(level_stats)

    # Add marginal classification statistics to level_stats / Frequencies per level:

    level_stats$hi.m <- NA
    level_stats$mi.m <- NA
    level_stats$fa.m <- NA
    level_stats$cr.m <- NA

    for (i in 1:n.levels) {

      if (i == 1) {
        level_stats$hi.m[1] <- level_stats$hi[1]
        level_stats$mi.m[1] <- level_stats$mi[1]
        level_stats$fa.m[1] <- level_stats$fa[1]
        level_stats$cr.m[1] <- level_stats$cr[1]
      }

      if (i > 1) {
        level_stats$hi.m[i] <- level_stats$hi[i] - level_stats$hi[i - 1]
        level_stats$mi.m[i] <- level_stats$mi[i] - level_stats$mi[i - 1]
        level_stats$fa.m[i] <- level_stats$fa[i] - level_stats$fa[i - 1]
        level_stats$cr.m[i] <- level_stats$cr[i] - level_stats$cr[i - 1]
      }

    } # for n.levels.

    # print(level_stats)  # +++ here now +++: Tree with marginal frequency values (for each level)


    # Set plotting parameters: ----

    # Label sizes:

    # print(paste0("par('cex') = ", par("cex")))  # Note: Value varies from .66 to 1

    # Sizes not set by user:
    f_cex <- 1  # cex scaling factor

    decision_node_cex <- 4 * f_cex
    exit_node_cex     <- 4 * f_cex
    panel_title_cex   <- 2 * f_cex

    # Set by user:

    # Cue label size:
    if (is.null(cue.cex)) {
      cue.cex <- c(1.5, 1.5, 1.25, 1, 1, 1)
    } else {
      if (length(cue.cex) < 6) {
        cue.cex <- rep(cue.cex, length.out = 6)
      }
    }

    # Break label size:
    if (is.null(threshold.cex)) {
      threshold.cex <- c(1.5, 1.5, 1.25, 1, 1, 1)
    } else {
      if (length(threshold.cex) < 6) {
        threshold.cex <- rep(threshold.cex, length.out = 6)
      }
    }

    # Panel parameters:
    panel.line.lwd <- 1
    panel.line.col <- gray(0) # = black
    panel.line.lty <- 1

    # Ball parameters:
    ball.col <- c(gray(0), gray(0))  # = black
    ball.bg  <- c(gray(1), gray(1))  # = white
    ball.pch <- c(21, 24)
    ball.cex <- c(1, 1)

    error.col <- "red"
    correct.col <- "green"

    max.label.length <- 100
    def.par <- par(no.readonly = TRUE)

    ball.box.width   <- 10
    label.box.height <-  2
    label.box.width  <-  5

    # Cue labels:
    if (is.null(cue.labels)) {
      cue.labels <- level_stats$cue
    }

    # Trim labels:
    cue.labels <- strtrim(cue.labels, max.label.length)

    # Lines/node segments:
    segment.lty <- 1
    segment.lwd <- 1

    continue.segment.lwd <- 1
    continue.segment.lty <- 1

    exit.segment.lwd <- 1
    exit.segment.lty <- 1


    # Define plotting.parameters.df:
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

    } else { # n.levels >= 6:

      label.box.text.cex <- plotting.parameters.df$label.box.text.cex[6]
      break.label.cex <- plotting.parameters.df$break.label.cex[6]
      plot.height <- plotting.parameters.df$plot.height[6]
      plot.width <- plotting.parameters.df$plot.width[6]

    }


    # Colors: ----

    exit.node.bg <- "white"

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


    # Balls: ----

    ball.loc <- "variable"

    if (n.levels == 3) {
      ball.box.width <- 14
    }

    if (n.levels == 4) {
      ball.box.width <- 18
    }

    ball.box.height      <-  2.5
    ball.box.horiz.shift <- 10
    ball.box.vert.shift  <- -1
    ball.box.max.shift.p <-  .9
    ball.box.min.shift.p <-  .4

    ball.box.fixed.x.shift <- c(ball.box.min.shift.p * plot.width, ball.box.max.shift.p * plot.width)

    # Determine N per ball:
    if (is.null(n.per.icon)) {
      max_n_side <- max(c(n_pos_cases, n_neg_cases))

      i <- max_n_side / c(1, 5, 10^(1:10))
      i[i > 50] <- 0

      n.per.icon <- c(1, 5, 10^(1:10))[which(i == max(i))]
    }

    noise.ball.pch  <- ball.pch[1]
    signal.ball.pch <- ball.pch[2]
    noise.ball.col  <- ball.col[1]
    signal.ball.col <- ball.col[2]
    noise.ball.bg   <- ball.bg[1]
    signal.ball.bg  <- ball.bg[2]


    # Arrows: ----

    arrow.lty <- 1
    arrow.lwd <- 1
    arrow.length      <- 2.50
    arrow.head.length <-  .08
    arrow.col <- gray(0) # = black


    # Final stats: ----

    spec.circle.x   <- .40
    dprime.circle.x <- .50
    sens.circle.x   <- .60

    stat.circle.y   <- .30

    sens.circle.col   <- "green"
    spec.circle.col   <- "red"
    dprime.circle.col <- "blue"
    stat.outer.circle.col <- gray(.50)


    # 1: Initial Frequencies: ------

    # Parameters:

    if (show.top) {

      par(mar = c(0, 0, 1, 0))

      # Prepare plot:
      plot(1,
           xlim = c(0, 1), ylim = c(0, 1), bty = "n", type = "n",
           xlab = "", ylab = "", yaxt = "n", xaxt = "n"
      )

      # 1. Title: ----

      par(xpd = TRUE)

      # (a) lines:
      if (hlines) {

        segments(0, .95, 1, .95, col = panel.line.col, lwd = panel.line.lwd, lty = panel.line.lty) # top hline

        x_dev <- get_x_dev(main)
        y_dev <- .20
        rect((.50 - x_dev), (1 - y_dev), (.50 + x_dev), (1 + y_dev), col = "white", border = NA) # title background

      }

      # (b) label:
      text(x = .50, y = .96, main, cex = panel_title_cex, ...)  # title 1 (top): main


      # 2. Data info: ----

      # (a) N and labels:
      text(x = .50, y = .78, paste("N = ", prettyNum(n_exemplars, big.mark = ","), "", sep = ""), cex = 1.25) # N
      text(.50, .63, paste(decision.labels[1], sep = ""), pos = 2, cex = 1.2, adj = 1) # 1: False
      text(.50, .63, paste(decision.labels[2], sep = ""), pos = 4, cex = 1.2, adj = 0) # 2: True

      # (b) Show balls:
      n_true_pos <- with(final_stats, hi + mi)
      n_true_neg <- with(final_stats, fa + cr)

      add_balls(
        x.lim = c(.33, .67),
        y.lim = c(.12, .52),
        n.vec = c(n_true_neg, n_true_pos),
        pch.vec = c(noise.ball.pch, signal.ball.pch),
        bg.vec = c(noise.ball.bg, signal.ball.bg),
        col.vec = c(noise.ball.col, signal.ball.col),
        ball.cex = ball.cex,
        upper.text.adj = 2,
        n.per.icon = n.per.icon
      )

      # (c) n.per.icon legend 1 (top):
      if (show_icon_guide_legend){

        text(.98, 0, labels = paste("Showing ", n.per.icon, " cases per icon:", sep = ""), pos = 2)
        points(.98, 0, pch = noise.ball.pch,  cex = ball.cex)
        points(.99, 0, pch = signal.ball.pch, cex = ball.cex)

      } # if (show_icon_guide_legend).


      par(xpd = FALSE)


      # 3. Add p.signal and p.noise levels: -----

      signal.p <- mean(x$data[[data]][[x$criterion_name]])
      noise.p  <- (1 - signal.p)

      p.rect.ylim <- c(.10, .60)


      # (a) p.signal level (right): ----

      text(
        x = .80, y = p.rect.ylim[2],
        labels = paste("p(", decision.labels[2], ")", sep = ""),
        pos = 3, cex = 1.2
      )

      # Filling:
      rect(.775, p.rect.ylim[1],
           .825, p.rect.ylim[1] + signal.p * diff(p.rect.ylim),
           col = gray(.50, .25), border = NA
      )

      # Filltop:
      segments(.775, p.rect.ylim[1] + signal.p * diff(p.rect.ylim),
               .825, p.rect.ylim[1] + signal.p * diff(p.rect.ylim),
               lwd = 1
      )

      # Outline:
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


      # (b) p.noise level (left): ----

      text(
        x = .20, y = p.rect.ylim[2],
        labels = paste("p(", decision.labels[1], ")", sep = ""),
        pos = 3, cex = 1.2
      )


      rect(.175, p.rect.ylim[1], .225, p.rect.ylim[1] + noise.p * diff(p.rect.ylim),
           col = gray(.50, .25), border = NA
      )

      # Filltop:
      segments(.175, p.rect.ylim[1] + noise.p * diff(p.rect.ylim),
               .225, p.rect.ylim[1] + noise.p * diff(p.rect.ylim),
               lwd = 1
      )

      # Outline:
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

    } # if (show.top).


    # 2. Main TREE: ------

    if (show.middle) {

      if (show.top == FALSE & show.bottom == FALSE) {
        par(mar = c(3, 3, 3, 3) + .1)
      } else {
        par(mar = c(0, 0, 0, 0))
      }

      par(xpd = TRUE)

      # Prepare plot:
      plot(1,
           xlim = c(-plot.width, plot.width),
           ylim = c(-plot.height, 0),
           type = "n", bty = "n",
           xaxt = "n", yaxt = "n",
           ylab = "", xlab = ""
      )


      # Middle title: ----

      if (show.top | show.bottom) {

        if (hlines) {
          x_dev <- .28  # scaling factor, rather than difference
          segments(-plot.width, 0, -plot.width * x_dev, 0, col = panel.line.col, lwd = panel.line.lwd, lty = panel.line.lty)
          segments( plot.width, 0,  plot.width * x_dev, 0, col = panel.line.col, lwd = panel.line.lwd, lty = panel.line.lty)
        }

        if (is.null(label.tree)) {
          label.tree <- paste("FFT #", tree, " (of ", x$trees$n, ")", sep = "")
        }

        text(x = 0, y = 0, label.tree, cex = panel_title_cex, ...)  # title 2 (middle): (a) tree label

      } # if (show.top | show.bottom).

      if (show.top == FALSE & show.bottom == FALSE) {

        if (is.null(main) & is.null(x$params$main)) {
          main <- ""
        }

        mtext(text = main, side = 3, cex = panel_title_cex, ...)  # title 2 (middle): (b) main label

      } # if (show.top == FALSE & show.bottom == FALSE).


      # Icon guide: ------

      if (show.iconguide) {

        # Parameters:
        if (what == "ico") {

          f_x <- 1.2  # scaling factor (to stretch in x-dim)
          f_y <- 0.8  # scaling factor (to shift up)

        } else { # default scaling factors:

          f_x <- 1
          f_y <- 1

        }

        exit_word <- exit_word(data)  # either 'train':'decide' or 'test':'predict'


        # (a) Noise panel (on left): ----

        # Heading:
        text(-plot.width  * .60 * f_x,
             -plot.height * .05 * f_y,
             paste(exit_word, decision.labels[1], sep = " "),
             cex = 1.2, font = 3
        )

        # Noise balls:
        points(c(-plot.width * .70,  -plot.width  * .50) * f_x,
               c(-plot.height * .13, -plot.height * .13) * f_y,
               pch = c(noise.ball.pch, signal.ball.pch),
               bg = c(correct.bg, error.bg),
               col = c(correct.border, error.border),
               cex = ball.cex * 1.5
        )

        # Labels:
        text(c(-plot.width * .70,  -plot.width  * .50) * f_x,
             c(-plot.height * .13, -plot.height * .13) * f_y,
             labels = c("Correct\nRejection", "Miss"),
             pos = c(2, 4), offset = 1
        )



        # (b) Signal panel (on right): ----

        # Heading:
        text(plot.width   * .60 * f_x,
             -plot.height * .05 * f_y,
             paste(exit_word, decision.labels[2], sep = " "),
             cex = 1.2, font = 3
        )

        # Signal balls:
        points(c(plot.width   * .50,  plot.width  * .70 ) * f_x,
               c(-plot.height * .13, -plot.height * .13) * f_y,
               pch = c(noise.ball.pch, signal.ball.pch),
               bg = c(error.bg, correct.bg),
               col = c(error.border, correct.border),
               cex = ball.cex * 1.5
        )

        # Labels:
        text(c(plot.width * .50,    plot.width  * .70) * f_x,
             c(-plot.height * .13, -plot.height * .13) * f_y,
             labels = c("False\nAlarm", "Hit"),
             pos = c(2, 4), offset = 1
        )


        # (c) Additional lines (below icon guide): ----
        if (what == "ico" & hlines) {

          x_hline <-  plot.width  * .95 * f_x
          y_hline <- -plot.height * .19 * f_y

          segments(-x_hline, y_hline, x_hline, y_hline, col = panel.line.col, lwd = panel.line.lwd, lty = panel.line.lty)
          rect(-x_hline * .33, (y_hline - .5), x_hline * .33, (y_hline + .5), col = "white", border = NA)
        }


        # (d) n.per.icon legend 2 (middle): ----
        if (what == "ico") { show_icon_guide_legend <- TRUE } # special case

        if (show_icon_guide_legend){

          if (what == "ico") { # special case:

            x_s2 <- plot.width
            x_s1 <- plot.width - .75
            y_s1 <- plot.height * -1.10

          } else { # defaults:

            x_s2 <- plot.width
            x_s1 <- plot.width - .40
            y_s1 <- plot.height * -1

          }

          text(x_s1, y_s1, labels = paste("Showing ", n.per.icon, " cases per icon:", sep = ""), pos = 2, cex = ball.cex)
          points(x_s1, y_s1, pch = noise.ball.pch,  cex = ball.cex)
          points(x_s2, y_s1, pch = signal.ball.pch, cex = ball.cex)

        } # if (show_icon_guide_legend).

      } # if (show.iconguide).

      par(xpd = FALSE)


      # Plot main TREE: ------

      # Set initial subplot center:
      subplot.center <- c(0, -4)

      # Loop over levels: ------
      for (level.i in 1:min(c(n.levels, 6))) {

        # Cue label:
        current.cue <- cue.labels[level.i]

        # Get stats for current level:
        hi.i <- level_stats$hi.m[level.i]
        mi.i <- level_stats$mi.m[level.i]
        fa.i <- level_stats$fa.m[level.i]
        cr.i <- level_stats$cr.m[level.i]


        # Top: If level.i == 1, draw top textbox: ----

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
            cex = decision_node_cex,
            pch = decision.node.pch
          )

          text(
            x = subplot.center[1],
            y = subplot.center[2] + 2,
            labels = current.cue,
            cex = label.box.text.cex  # get_label_cex(current.cue, label.box.text.cex = label.box.text.cex)
          )

        } # if (level.i == 1).


        # Left (Noise) classification / New level: ----

        # Exit node on left: ----

        if (level_stats$exit[level.i] %in% c(0, .5) | paste(level_stats$exit[level.i]) %in% c("0", ".5")) {

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

          # Decision text:

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

            add_balls(
              x.lim = ball.x.lim,
              y.lim = ball.y.lim,
              n.vec = c(cr.i, mi.i),
              pch.vec = c(noise.ball.pch, signal.ball.pch),
              # bg.vec = c(noise.ball.bg, signal.ball.bg),
              bg.vec = c(correct.bg, error.bg),
              col.vec = c(correct.border, error.border),
              freq.text = TRUE,
              n.per.icon = n.per.icon,
              ball.cex = ball.cex
            )
          }

          # level break label:
          pos.direction.symbol <- c("<=", "<", "=", "!=", ">", ">=")[which(level_stats$direction[level.i] == c(">", ">=", "!=", "=", "<=", "<"))]
          neg.direction.symbol <- c("<=", "<", "=", "!=", ">", ">=")[which(level_stats$direction[level.i] == c("<=", "<", "=", "!=", ">", ">="))]

          text_outline(
            x = subplot.center[1] - 1,
            y = subplot.center[2],
            labels = paste(pos.direction.symbol, " ", level_stats$threshold[level.i], sep = ""),
            pos = 2, cex = break.label.cex, r = .1
          )

          points(
            x = subplot.center[1] - 2,
            y = subplot.center[2] - 2,
            pch = exit.node.pch,
            cex = exit_node_cex,
            bg = exit.node.bg
          )

          text(
            x = subplot.center[1] - 2,
            y = subplot.center[2] - 2,
            labels = substr(decision.labels[1], 1, 1)
          )
        } # if (exit node on left).


        # New level on left: ----

        if (level_stats$exit[level.i] %in% c(1) | paste(level_stats$exit[level.i]) %in% c("1")) {

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
        } # if (new level on left).


        # Right (Signal) classification / New level: ----

        # Exit node on right: ----

        if (level_stats$exit[level.i] %in% c(1, .5) | paste(level_stats$exit[level.i]) %in% c("1", ".5")) {

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

          # Decision text:

          if (decision.cex > 0) {
            text(
              x = subplot.center[1] + 2 + arrow.length * .7,
              y = subplot.center[2] - 2.2,
              labels = decision.labels[2],
              pos = 1,
              font = 3,
              cex = decision.cex
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

            add_balls(
              x.lim = ball.x.lim,
              y.lim = ball.y.lim,
              n.vec = c(fa.i, hi.i),
              pch.vec = c(noise.ball.pch, signal.ball.pch),
              # bg.vec = c(noise.ball.bg, signal.ball.bg),
              bg.vec = c(error.bg, correct.bg),
              col.vec = c(error.border, correct.border),
              freq.text = TRUE,
              n.per.icon = n.per.icon,
              ball.cex = ball.cex
            )
          }

          # level break label:
          dir.symbols <- c("<=", "<", "=", "!=", ">", ">=")

          pos.direction.symbol <- dir.symbols[which(level_stats$direction[level.i] == c("<=", "<", "=", "!=", ">", ">="))]
          neg.direction.symbol <- dir.symbols[which(level_stats$direction[level.i] == rev(c("<=", "<", "=", "!=", ">", ">=")))]


          text_outline(subplot.center[1] + 1,
                       subplot.center[2],
                       labels = paste(pos.direction.symbol, " ", level_stats$threshold[level.i], sep = ""),
                       pos = 4, cex = break.label.cex, r = .1
          )

          points(
            x = subplot.center[1] + 2,
            y = subplot.center[2] - 2,
            pch = exit.node.pch,
            cex = exit_node_cex,
            bg = exit.node.bg
          )

          text(
            x = subplot.center[1] + 2,
            y = subplot.center[2] - 2,
            labels = substr(decision.labels[2], 1, 1)
          )

        } # if (exit node on right).


        # New level on right: ----

        if (level_stats$exit[level.i] %in% 0 | paste(level_stats$exit[level.i]) %in% c("0")) {

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

        } # if (new level on right).


        # Update plot center: ----

        if (identical(paste(level_stats$exit[level.i]), "0")) {

          subplot.center <- c(
            subplot.center[1] + 2,
            subplot.center[2] - 4
          )
        } # if (identical exit 0 etc.

        if (identical(paste(level_stats$exit[level.i]), "1")) {

          subplot.center <- c(
            subplot.center[1] - 2,
            subplot.center[2] - 4
          )

        } # if (identical exit 1 etc.

      } # for (level.i etc. loop.

    } # if (show.middle).


    # 3. Cumulative performance: ----

    if (show.bottom == TRUE) { # obtain tree statistics:

      fft_sens_vec <- tree_stats$sens
      fft_spec_vec <- tree_stats$spec

      # General plotting space: ----

      # Parameters:
      header.y.loc <- 1.0
      subheader.y.loc <- .925

      header.cex <- 1.10
      subheader.cex <- .90

      par(mar = c(0, 0, 2, 0))

      plot(1,
           xlim = c(0, 1), ylim = c(0, 1),
           bty = "n", type = "n",
           xlab = "", ylab = "",
           yaxt = "n", xaxt = "n"
      )


      if (what != "roc"){

        # Set par:
        par(xpd = TRUE)

        # Bottom title: ----

        if (hlines) {

          segments(0, 1.1, 1, 1.1, col = panel.line.col, lwd = panel.line.lwd, lty = panel.line.lty)

          x_dev <- .20
          rect((.50 - x_dev), 1, (.50 + x_dev), 1.2, col = "white", border = NA) # label background
        }

        # Bottom label:
        if (is.null(label.performance)) {
          if (data == "train") {
            label.performance <- "Accuracy (Training)"
          }
          if (data == "test") {
            label.performance <- "Accuracy (Testing)"
          }
        }

        text(.50, 1.1, label.performance, cex = panel_title_cex, ...)  # title 3 (bottom): Performance

        par(xpd = FALSE)

      } # if (what != "roc").


      # Level parameters:
      level.max.height <- .65
      level.width <- .05
      level.center.y <- .45
      # level.bottom <- .1
      level.bottom <- level.center.y - level.max.height / 2
      level.top <- level.center.y + level.max.height / 2

      # Get either bacc OR wacc (based on sens.w):
      sens.w <- x$params$sens.w
      bacc_wacc <- get_bacc_wacc(sens = final_stats$sens, spec = final_stats$spec, sens.w = sens.w)
      bacc_wacc_name <- names(bacc_wacc)

      # Set labels, values, and locations (as df):
      lloc <- data.frame(
        element = c("classtable", "mcu", "pci", "sens", "spec", "acc", bacc_wacc_name, "roc"),
        long.name = c("Classification Table", "mcu", "pci", "sens", "spec", "acc", bacc_wacc_name, "ROC"),
        center.x = c(.18, seq(.35, .65, length.out = 6), .85),
        center.y = rep(level.center.y, 8),
        width = c(.2, rep(level.width, 6), .20),
        height = c(.65, rep(level.max.height, 6), .65),
        value = c(NA,
                  abs(final_stats$mcu - 5) / (abs(1 - 5)), final_stats$pci,
                  final_stats$sens, final_stats$spec,
                  with(final_stats, (cr + hi) / n), bacc_wacc, NA),
        value.name = c(NA,
                       round(final_stats$mcu, 1), pretty_dec(final_stats$pci),
                       pretty_dec(final_stats$sens), pretty_dec(final_stats$spec),
                       pretty_dec(final_stats$acc), pretty_dec(bacc_wacc), NA)
      )


      # Classification table: ----

      if (show.confusion) {

        # Parameters:
        classtable.lwd <- 1

        # x/y coordinates:
        final.classtable.x.loc <- c(lloc$center.x[lloc$element == "classtable"] - lloc$width[lloc$element == "classtable"] / 2, lloc$center.x[lloc$element == "classtable"] + lloc$width[lloc$element == "classtable"] / 2)
        final.classtable.y.loc <- c(lloc$center.y[lloc$element == "classtable"] - lloc$height[lloc$element == "classtable"] / 2, lloc$center.y[lloc$element == "classtable"] + lloc$height[lloc$element == "classtable"] / 2)

        rect(final.classtable.x.loc[1], final.classtable.y.loc[1],
             final.classtable.x.loc[2], final.classtable.y.loc[2],
             lwd = classtable.lwd
        )

        segments(mean(final.classtable.x.loc), final.classtable.y.loc[1], mean(final.classtable.x.loc), final.classtable.y.loc[2], col = gray(0), lwd = classtable.lwd)
        segments(final.classtable.x.loc[1], mean(final.classtable.y.loc), final.classtable.x.loc[2], mean(final.classtable.y.loc), col = gray(0), lwd = classtable.lwd)


        # Column titles: ----

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


        # Row titles: ----

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


        # Add final frequencies: ----

        text(final.classtable.x.loc[1] + .75 * diff(final.classtable.x.loc),
             final.classtable.y.loc[1] + .25 * diff(final.classtable.y.loc),
             prettyNum(final_stats$cr, big.mark = ","),
             cex = 1.5
        )

        text(final.classtable.x.loc[1] + .25 * diff(final.classtable.x.loc),
             final.classtable.y.loc[1] + .25 * diff(final.classtable.y.loc),
             prettyNum(final_stats$mi, big.mark = ","),
             cex = 1.5
        )

        text(final.classtable.x.loc[1] + .75 * diff(final.classtable.x.loc),
             final.classtable.y.loc[1] + .75 * diff(final.classtable.y.loc),
             prettyNum(final_stats$fa, big.mark = ","),
             cex = 1.5
        )

        text(final.classtable.x.loc[1] + .25 * diff(final.classtable.x.loc),
             final.classtable.y.loc[1] + .75 * diff(final.classtable.y.loc),
             prettyNum(final_stats$hi, big.mark = ","),
             cex = 1.5
        )

        # Add symbols: ----

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


        # Add labels: ----

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

      } # if (show.confusion).


      # Levels: ----

      if (show.levels) {

        if (level.type %in% c("line", "bar")) {

          # Color function (taken from colorRamp2 function in circlize package)
          # col.fun <- circlize::colorRamp2(c(0, .75, 1),
          #                                 c("red", "yellow", "green"),
          #                                 transparency = .5)

          paste(final_stats$cr, "/", 1, collapse = "")

          # Add 100% reference line: ----

          # segments(x0 = lloc$center.x[lloc$element == "mcu"] - lloc$width[lloc$element == "mcu"] * .8,
          #          y0 = level.top,
          #          x1 = lloc$center.x[lloc$element == "bacc"] + lloc$width[lloc$element == "bacc"] * .8,
          #          y1 = level.top,
          #          lty = 3, lwd = .75)


          # mcu level: ----

          add_level("mcu", ok.val = .75, min.val = 0, max.val = 1,
                    level.type = level.type, lloc_row = lloc[lloc$element == "mcu", ],
                    header_y = header.y.loc, header_cex = header.cex) # , sub = paste(c(final_stats$cr, "/", final_stats$cr + final_stats$fa), collapse = ""))


          # pci level: ----

          add_level("pci", ok.val = .75, min.val = 0, max.val = 1,
                    level.type = level.type, lloc_row = lloc[lloc$element == "pci", ],
                    header_y = header.y.loc, header_cex = header.cex) # , sub = paste(c(final_stats$cr, "/", final_stats$cr + final_stats$fa), collapse = ""))

          # text(lloc$center.x[lloc$element == "pci"],
          #      lloc$center.y[lloc$element == "pci"],
          #      labels = paste0("mcu\n", round(mcu, 2)))


          # spec level: ----

          add_level("spec", ok.val = .75, min.val = 0, max.val = 1,
                    level.type = level.type, lloc_row = lloc[lloc$element == "spec", ],
                    header_y = header.y.loc, header_cex = header.cex) # , sub = paste(c(final_stats$cr, "/", final_stats$cr + final_stats$fa), collapse = ""))


          # sens level: ----

          add_level("sens", ok.val = .75, min.val = 0, max.val = 1,
                    level.type = level.type, lloc_row = lloc[lloc$element == "sens", ],
                    header_y = header.y.loc, header_cex = header.cex) # , sub = paste(c(final_stats$hi, "/", final_stats$hi + final_stats$mi), collapse = ""))


          # acc level: ----

          min_acc <- max(crit_br, 1 - crit_br)

          add_level("acc", ok.val = .50, min.val = 0, max.val = 1,
                    level.type = level.type, lloc_row = lloc[lloc$element == "acc", ],
                    header_y = header.y.loc, header_cex = header.cex) # , sub = paste(c(final_stats$hi + final_stats$cr, "/", final_stats$n), collapse = ""))

          # Add baseline to acc level:
          segments(
            x0 = lloc$center.x[lloc$element  == "acc"] - lloc$width[lloc$element  == "acc"] / 2,
            y0 = (lloc$center.y[lloc$element == "acc"] - lloc$height[lloc$element == "acc"] / 2) + lloc$height[lloc$element == "acc"] * min_acc,
            x1 = lloc$center.x[lloc$element  == "acc"] + lloc$width[lloc$element  == "acc"] / 2,
            y1 = (lloc$center.y[lloc$element == "acc"] - lloc$height[lloc$element == "acc"] / 2) + lloc$height[lloc$element == "acc"] * min_acc,
            lty = 3
          )

          text(
            x = lloc$center.x[lloc$element == "acc"],
            y = (lloc$center.y[lloc$element == "acc"] - lloc$height[lloc$element == "acc"] / 2) + lloc$height[lloc$element == "acc"] * min_acc,
            labels = "BL", pos = 1
          )

          # paste("BL = ", pretty_dec(min_acc), sep = ""), pos = 1)


          # bacc OR wacc level: ----

          if (names(bacc_wacc) == "bacc"){ # show bacc level:

            add_level("bacc", ok.val = .50, min.val = 0, max.val = 1,
                      level.type = level.type, lloc_row = lloc[lloc$element == "bacc", ],
                      header_y = header.y.loc, header_cex = header.cex)

          } else { # show wacc level (and sens.w value):

            sens.w_lbl <- paste0("sens.w = .", pretty_dec(sens.w))

            add_level("wacc", ok.val = .50, min.val = 0, max.val = 1,
                      level.type = level.type, lloc_row = lloc[lloc$element == "wacc", ],
                      header_y = header.y.loc,
                      bottom.text = sens.w_lbl, header_cex = header.cex)

          } # if (bacc_wacc).


          # Add baseline (at bottom?):
          #
          # segments(x0 = mean(lloc$center.x[2]),
          #          y0 = lloc$center.y[1] - lloc$height[1] / 2,
          #          x1 = mean(lloc$center.x[7]),
          #          y1 = lloc$center.y[1] - lloc$height[1] / 2, lend = 1,
          #          lwd = .5,
          #          col = gray(0))


        } # if (level.type %in% c("line", "bar")).

      } # if (show.levels).


      # ROC curve: -----

      if (show.roc) {

        # Parameters:
        roc_border_lwd <- 1
        roc_border_col <- gray(0)

        roc_title <- "ROC"
        roc_title_font <- 1

        roc_curve_col <- gray(.01) # ~black
        roc_curve_lwd <- 1.1

        diag_col <- gray(.01) # ~black
        diag_lty <- 3

        x_lbl <- expression(1 - Specificity~(FAR)) # to plot minus, rather than dash
        y_lbl <- expression(Sensitivity~(HR))

        x_d <- .015  # distance of x-axis labels (on left) to x-axis

        # y-locations of legend labels (default: using full height):
        roc_lbl_y <- seq(.10, .90, length.out = 5)  # SVM, RF, LR, CART, FFT


        if (what == "roc"){ # ROC as main plot:

          # Rescale key coordinates:
          lloc$center.x[lloc$element == "roc"] <- .50
          lloc$center.y[lloc$element == "roc"] <- .55

          lloc$width[lloc$element == "roc"]  <- .70
          lloc$height[lloc$element == "roc"] <- .80

          # Reset some parameters:
          if (is.null(main) == FALSE) { roc_title <- main }

          roc_border_lwd <- .80
          roc_border_col <- gray(.25)

          roc_curve_col <- gray(.10) # "green2"
          roc_curve_lwd <- 1.5

          diag_col <- gray(.60)  # as in showcues()
          diag_lty <- 1          # as in showcues()

          x_d <- .035

          # y-locations of legend labels (cluster labels on top right):
          roc_lbl_y <- seq(.55, .95, length.out = 5)  # SVM, RF, LR, CART, FFT


        } # if (what == "roc").

        # ROC plot coordinates:
        final.roc.x.loc <- c(lloc$center.x[lloc$element == "roc"] - lloc$width[lloc$element == "roc"] / 2, lloc$center.x[lloc$element == "roc"] + lloc$width[lloc$element == "roc"] / 2)
        final.roc.y.loc <- c(lloc$center.y[lloc$element == "roc"] - lloc$height[lloc$element == "roc"] / 2, lloc$center.y[lloc$element == "roc"] + lloc$height[lloc$element == "roc"] / 2)


        if (what == "roc"){ # ROC as main plot:

          # Title:
          title(main = roc_title, ...)  # + graphical parameters

          # Background:
          rect(final.roc.x.loc[1], final.roc.y.loc[1], final.roc.x.loc[2], final.roc.y.loc[2],
               col = gray(.96))  # as in showcues()

          # Grid:
          x_ax_seq <- seq(final.roc.x.loc[1], final.roc.x.loc[2], length.out = 11)
          y_ax_seq <- seq(final.roc.y.loc[1], final.roc.y.loc[2], length.out = 11)
          abline(v = x_ax_seq, lwd = c(2, rep(1, 4)), col = gray(1)) # x-grid
          abline(h = y_ax_seq, lwd = c(2, rep(1, 4)), col = gray(1)) # y-grid

          # Axis ticks:
          segments(x_ax_seq, final.roc.y.loc[1], x_ax_seq, (final.roc.y.loc[1] - .025), lty = 1, lwd = 1, col = gray(.10)) # x-axis
          segments(final.roc.x.loc[1], y_ax_seq, (final.roc.x.loc[1] - .015), y_ax_seq, lty = 1, lwd = 1, col = gray(.10)) # y-axis

          # Tick labels:
          text(x_ax_seq, (final.roc.y.loc[1] - .025), labels = scales::comma(seq(0, 1, by = .1), accuracy = .1), pos = 1, cex = .9) # x-lbl
          text((final.roc.x.loc[1] - .015), y_ax_seq, labels = scales::comma(seq(0, 1, by = .1), accuracy = .1), pos = 2, cex = .9) # y-llb

          # Axis labels:
          text(mean(final.roc.x.loc), final.roc.y.loc[1] - .125, labels = x_lbl, cex = 1) # x-lab
          text(final.roc.x.loc[1] - (3.5 * x_d), mean(final.roc.y.loc), labels = y_lbl, cex = 1, srt = 90) # y-lab

          # Subtitle: Note data used
          subnote <- paste0("ROC for '", data, "' data:")
          text(x = (final.roc.x.loc[1] - .015), y = (final.roc.y.loc[2] + .03),
               labels = subnote, pos = 4, cex = subheader.cex)


        } else { # ROC as miniature plot:

          # Title:
          text(lloc$center.x[lloc$element == "roc"], header.y.loc, labels = roc_title,
               font = roc_title_font, pos = 1, cex = header.cex)

          # x-axis:
          text(c(final.roc.x.loc[1], final.roc.x.loc[2]),
               c(final.roc.y.loc[1], final.roc.y.loc[1]) - .04,
               labels = c(0, 1)
          )

          text(mean(final.roc.x.loc), final.roc.y.loc[1] - .08, labels = x_lbl) # x-lab

          # y-axis:
          text(c(final.roc.x.loc[1], final.roc.x.loc[1], final.roc.x.loc[1]) - x_d,
               c(final.roc.y.loc[1], mean(final.roc.y.loc[1:2]), final.roc.y.loc[2]),
               labels = c(0, .5, 1)
          )

          text(final.roc.x.loc[1] - (2.5 * x_d), mean(final.roc.y.loc), labels = y_lbl, srt = 90) # y-lab

          # AUC label:
          # text(final.roc.center[1], subheader.y.loc, paste("AUC =", round(final.auc, 2)), pos = 1)

          # Plot bg:
          #
          # rect(final.roc.x.loc[1],
          #      final.roc.y.loc[1],
          #      final.roc.x.loc[2],
          #      final.roc.y.loc[2],
          #      col = gray(1), lwd = .5)

          # Gridlines:
          # # Horizontal:
          #  segments(x0 = rep(final.roc.x.loc[1], 9),
          #           y0 = seq(final.roc.y.loc[1], final.roc.y.loc[2], length.out = 5)[2:10],
          #           x1 = rep(final.roc.x.loc[2], 9),
          #           y1 = seq(final.roc.y.loc[1], final.roc.y.loc[2], length.out = 5)[2:10],
          #           lty = 1, col = gray(.8), lwd = c(.5), lend = 3
          #           )
          #
          #  # Vertical:
          #  segments(y0 = rep(final.roc.y.loc[1], 9),
          #           x0 = seq(final.roc.x.loc[1], final.roc.x.loc[2], length.out = 5)[2:10],
          #           y1 = rep(final.roc.y.loc[2], 9),
          #           x1 = seq(final.roc.x.loc[1], final.roc.x.loc[2], length.out = 5)[2:10],
          #           lty = 1, col = gray(.8), lwd = c(.5), lend = 3
          #  )

        }

        # Plot border:
        rect(final.roc.x.loc[1],
             final.roc.y.loc[1],
             final.roc.x.loc[2],
             final.roc.y.loc[2],
             border = roc_border_col,
             lwd = roc_border_lwd
        )

        # Diagonal:
        segments(final.roc.x.loc[1],
                 final.roc.y.loc[1],
                 final.roc.x.loc[2],
                 final.roc.y.loc[2],
                 col = diag_col,
                 lwd = 1,
                 lty = diag_lty
        )


        # COMPETITIVE ALGORITHMS: ------

        if (comp == TRUE) {

          # CART: ----

          if ("cart" %in% x$competition[[data]]$algorithm) {

            cart.spec <- x$competition[[data]]$spec[x$competition[[data]]$algorithm == "cart"]
            cart.sens <- x$competition[[data]]$sens[x$competition[[data]]$algorithm == "cart"]

            # Plot point:
            points(final.roc.x.loc[1] + ((1 - cart.spec) * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (cart.sens * lloc$height[lloc$element == "roc"]),
                   pch = 21, cex = 1.75,
                   col = scales::alpha("red", .5),
                   bg = scales::alpha("red", .3), lwd = 1
            )

            points(final.roc.x.loc[1] + ((1 - cart.spec) * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (cart.sens * lloc$height[lloc$element == "roc"]),
                   pch = "C", cex = .7, col = gray(.2), lwd = 1
            )


            # Legend label:
            par("xpd" = TRUE)

            points(final.roc.x.loc[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (roc_lbl_y[4] * lloc$height[lloc$element == "roc"]),
                   pch = 21, cex = 2.5,
                   col = scales::alpha("red", .1),
                   bg = scales::alpha("red", .3)
            )

            points(final.roc.x.loc[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (roc_lbl_y[4] * lloc$height[lloc$element == "roc"]),
                   pch = "C", cex = .9, col = gray(.2)
            )

            text(final.roc.x.loc[1] + (1.13 * lloc$width[lloc$element == "roc"]),
                 final.roc.y.loc[1] + (roc_lbl_y[4] * lloc$height[lloc$element == "roc"]),
                 labels = "  CART", adj = 0, cex = .9
            )

            par("xpd" = FALSE)

          } # if ("cart" etc.


          # LR: ----

          if ("lr" %in% x$competition[[data]]$algorithm) {

            lr.spec <- x$competition[[data]]$spec[x$competition[[data]]$algorithm == "lr"]
            lr.sens <- x$competition[[data]]$sens[x$competition[[data]]$algorithm == "lr"]

            # Plot point:
            points(final.roc.x.loc[1] + ((1 - lr.spec) * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (lr.sens * lloc$height[lloc$element == "roc"]),
                   pch = 21, cex = 1.75,
                   col = scales::alpha("blue", .1),
                   bg = scales::alpha("blue", .2)
            )

            points(final.roc.x.loc[1] + ((1 - lr.spec) * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (lr.sens * lloc$height[lloc$element == "roc"]),
                   pch = "L", cex = .7, col = gray(.2)
            )

            # Legend label:
            par("xpd" = TRUE)

            points(final.roc.x.loc[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (roc_lbl_y[3] * lloc$height[lloc$element == "roc"]),
                   pch = 21, cex = 2.5,
                   col = scales::alpha("blue", .1),
                   bg = scales::alpha("blue", .2)
            )

            points(final.roc.x.loc[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (roc_lbl_y[3] * lloc$height[lloc$element == "roc"]),
                   pch = "L", cex = .9, col = gray(.2)
            )

            text(final.roc.x.loc[1] + (1.13 * lloc$width[lloc$element == "roc"]),
                 final.roc.y.loc[1] + (roc_lbl_y[3] * lloc$height[lloc$element == "roc"]),
                 labels = "  LR", adj = 0, cex = .9
            )

            par("xpd" = FALSE)

          } # if ("lr" etc.


          # RF: ----

          if ("rf" %in% x$competition[[data]]$algorithm) {

            rf.spec <- x$competition[[data]]$spec[x$competition[[data]]$algorithm == "rf"]
            rf.sens <- x$competition[[data]]$sens[x$competition[[data]]$algorithm == "rf"]

            # # 4debugging:
            # print(paste0("RF: 1 - rf.spec = ", round(1 - rf.spec, 2),
            #              ", rf.sens = ", round(rf.sens, 2),
            #              ", data = ", data))  # RF coordinates
            #
            # print(final.roc.y.loc[1] + (rf.sens * lloc$height[lloc$element == "roc"])) # y-coordinate

            # Plot point:
            points(final.roc.x.loc[1] + ((1 - rf.spec) * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (rf.sens * lloc$height[lloc$element == "roc"]),
                   pch = 21, cex = 1.75,
                   col = scales::alpha("purple", .1),
                   bg = scales::alpha("purple", .3), lwd = 1
            )

            points(final.roc.x.loc[1] + ((1 - rf.spec) * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (rf.sens * lloc$height[lloc$element == "roc"]),
                   pch = "R", cex = .7, col = gray(.2), lwd = 1
            )

            # Legend label:
            par("xpd" = TRUE)

            points(final.roc.x.loc[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (roc_lbl_y[2] * lloc$height[lloc$element == "roc"]),
                   pch = 21, cex = 2.5,
                   col = scales::alpha("purple", .1),
                   bg = scales::alpha("purple", .3)
            )

            points(final.roc.x.loc[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (roc_lbl_y[2] * lloc$height[lloc$element == "roc"]),
                   pch = "R", cex = .9, col = gray(.2)
            )

            text(final.roc.x.loc[1] + (1.13 * lloc$width[lloc$element == "roc"]),
                 final.roc.y.loc[1] + (roc_lbl_y[2] * lloc$height[lloc$element == "roc"]),
                 labels = "  RF", adj = 0, cex = .9
            )

            par("xpd" = FALSE)

          } # if ("rf" etc.


          # SVM: ----

          if ("svm" %in% x$competition[[data]]$algorithm) {

            svm.spec <- x$competition[[data]]$spec[x$competition[[data]]$algorithm == "svm"]
            svm.sens <- x$competition[[data]]$sens[x$competition[[data]]$algorithm == "svm"]

            # Plot point:
            points(final.roc.x.loc[1] + (1 - svm.spec) * lloc$width[lloc$element == "roc"],
                   final.roc.y.loc[1] + svm.sens * lloc$height[lloc$element == "roc"],
                   pch = 21, cex = 1.75,
                   col = scales::alpha("orange", .1),
                   bg  = scales::alpha("orange", .3), lwd = 1
            )

            points(final.roc.x.loc[1] + (1 - svm.spec) * lloc$width[lloc$element == "roc"],
                   final.roc.y.loc[1] + svm.sens * lloc$height[lloc$element == "roc"],
                   pch = "S", cex = .7, col = gray(.2), lwd = 1
            )


            # Legend label:
            par("xpd" = TRUE)

            points(final.roc.x.loc[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (roc_lbl_y[1] * lloc$height[lloc$element == "roc"]),
                   pch = 21, cex = 2.5,
                   col = scales::alpha("orange", .1),
                   bg  = scales::alpha("orange", .3)
            )

            points(final.roc.x.loc[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (roc_lbl_y[1] * lloc$height[lloc$element == "roc"]),
                   pch = "S", cex = .9, col = gray(.2)
            )

            text(final.roc.x.loc[1] + (1.13 * lloc$width[lloc$element == "roc"]),
                 final.roc.y.loc[1] + (roc_lbl_y[1] * lloc$height[lloc$element == "roc"]),
                 labels = "  SVM", adj = 0, cex = .9
            )

            par("xpd" = FALSE)

          } # if ("svm" etc.
        } # if (comp == TRUE).


        # FFTs: ----

        {
          roc_order <- order(fft_spec_vec, decreasing = TRUE)  # from highest to lowest spec
          # roc_order <- 1:x$trees$n

          fft_sens_vec_ord <- fft_sens_vec[roc_order]
          fft_spec_vec_ord <- fft_spec_vec[roc_order]

          # Add segments and points for all trees but tree:

          if (length(roc_order) > 1) {

            segments(final.roc.x.loc[1] + c(0, 1 - fft_spec_vec_ord) * lloc$width[lloc$element == "roc"],
                     final.roc.y.loc[1] + c(0, fft_sens_vec_ord) * lloc$height[lloc$element == "roc"],
                     final.roc.x.loc[1] + c(1 - fft_spec_vec_ord, 1) * lloc$width[lloc$element == "roc"],
                     final.roc.y.loc[1] + c(fft_sens_vec_ord, 1) * lloc$height[lloc$element == "roc"],
                     lwd = roc_curve_lwd,
                     col = roc_curve_col
            )

            points(final.roc.x.loc[1] + ((1 - fft_spec_vec_ord[-(which(roc_order == tree))]) * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (fft_sens_vec_ord[-(which(roc_order == tree))] * lloc$height[lloc$element == "roc"]),
                   pch = 21, cex = 2.5, col = scales::alpha("green", .60),
                   bg = scales::alpha("white", .90)
            )

            text(final.roc.x.loc[1] + ((1 - fft_spec_vec_ord[-(which(roc_order == tree))]) * lloc$width[lloc$element == "roc"]),
                 final.roc.y.loc[1] + (fft_sens_vec_ord[-(which(roc_order == tree))] * lloc$height[lloc$element == "roc"]),
                 labels = roc_order[which(roc_order != tree)], cex = 1, col = gray(.20)
            )
          }

          # Add larger point for plotted tree:

          # white point (to hide point from above):
          points(final.roc.x.loc[1] + ((1 - fft_spec_vec[tree]) * lloc$width[lloc$element == "roc"]),
                 final.roc.y.loc[1] + (fft_sens_vec[tree] * lloc$height[lloc$element == "roc"]),
                 pch = 21, cex = 3, col = gray(1), # col = scales::alpha("green", .30),
                 bg = scales::alpha("white", 1), lwd = 1
          )

          # green point:
          points(final.roc.x.loc[1] + ((1 - fft_spec_vec[tree]) * lloc$width[lloc$element == "roc"]),
                 final.roc.y.loc[1] + (fft_sens_vec[tree] * lloc$height[lloc$element == "roc"]),
                 pch = 21, cex = 3, col = gray(1), # col = scales::alpha("green", .30),
                 bg = scales::alpha("green", .30), lwd = 1
          )

          text(final.roc.x.loc[1] + ((1 - fft_spec_vec[tree]) * lloc$width[lloc$element == "roc"]),
               final.roc.y.loc[1] + (fft_sens_vec[tree] * lloc$height[lloc$element == "roc"]),
               labels = tree, cex = 1.25, col = gray(.20), font = 2
          )

          # Labels:

          if (comp == TRUE & any(
            is.null(x$competition$models$lr)   == FALSE,
            is.null(x$competition$models$cart) == FALSE,
            is.null(x$competition$models$svm)  == FALSE,
            is.null(x$competition$models$rf)   == FALSE
          )) {

            # Legend label:
            par("xpd" = TRUE)

            points(final.roc.x.loc[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (roc_lbl_y[5] * lloc$height[lloc$element == "roc"]),
                   pch = 21, cex = 2.5, col = scales::alpha("green", .3),
                   bg = scales::alpha("green", .67)
            )

            points(final.roc.x.loc[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                   final.roc.y.loc[1] + (roc_lbl_y[5] * lloc$height[lloc$element == "roc"]),
                   pch = "#", cex = .9, col = gray(.20)
            )

            text(final.roc.x.loc[1] + (1.13 * lloc$width[lloc$element == "roc"]),
                 final.roc.y.loc[1] + (roc_lbl_y[5] * lloc$height[lloc$element == "roc"]),
                 labels = "  FFT", adj = 0, cex = .9
            )

            par("xpd" = FALSE)

          }
        } # FFTs.

      } # if (show.roc).

    } # if (show.bottom).

    # # Reset plotting space:
    # par(mfrow = c(1, 1))
    # par(mar = c(5, 4, 4, 1) + .1)


  } # if (what != "cues").


  # Output: ------

  # Output x may differ from input x when applying new 'test' data (as df):
  return(invisible(x))

} # plot.FFTrees().


# ToDo: ------

# - Further cleanup & clutter reduction:
#   - Remove ROC curve parts to a separate function, and
#     handle what == "roc" as a special case (like what = "cues").

# - Issue #91: Allow data to accept new test data (as df).
#   Suggestion: Use a 'newdata' argument for this purpose, as in predict().)

# - Offer options for adding/changing color information.

# eof.
