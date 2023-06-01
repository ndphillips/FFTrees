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
#' @param what What should be plotted (as a character string)? Valid options are:
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
#' @param show.confusion Show a 2x2 confusion matrix (in bottom panel, as logical)?
#' @param show.levels Show performance levels (in bottom panel, as logical)?
#' @param show.roc Show ROC curve (in bottom panel, as logical)?
#'
#' @param hlines Show horizontal panel separation lines (as logical)?
#' Default: \code{hlines = TRUE}.
#'
#' @param label.tree A label for the FFT (optional, as character string).
#' @param label.performance A label for the performance section (optional, as character string).
#'
#' @param n.per.icon The number of cases represented by each icon (as numeric).
#' @param level.type The type of performance levels to be drawn at the bottom (as character string, either \code{"bar"} or \code{"line"}.
#' Default: \code{level.type = "bar"}.
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
                         #
                         data = "train",
                         what = "all",  # valid_what <- c("all", "default",  "cues",  "tree", "icontree",  "roc")
                         tree = 1,
                         #
                         main = NULL,
                         cue.labels = NULL,
                         decision.labels = NULL,
                         #
                         cue.cex = NULL,
                         threshold.cex = NULL,
                         decision.cex = 1,
                         #
                         comp = TRUE,
                         #
                         show.header = NULL,
                         show.tree = NULL,
                         show.confusion = NULL,
                         show.levels = NULL,
                         show.roc = NULL,
                         show.icons = NULL,
                         show.iconguide = NULL,
                         #
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
                  "cues", "tree", "icontree", "roc")  # as (local) constant

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


    # Extract key parameters from x: ------

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

      } # if (("main" %in% names(x$params))).

    } # if (is.null(main)).


    # tree: ----

    # Verify tree input: ----

    tree <- verify_tree_arg(x = x, data = data, tree = tree)  # use helper (for plotting AND printing)


    # Get "best" tree: ----

    if (tree == "best.train") {

      if (data == "test"){
        warning("You asked for the 'best.train' tree, but data was set to 'test'. Used the best tree for 'train' data instead...")
        data <- "train"
        if (is.null(main)) { main <- "Data (Training)" }
      }

      # tree <- x$trees$best$train  # using current x
      tree <- get_best_tree(x, data = "train", goal = x$params$goal)  # using helper
    }

    if (tree == "best.test") {

      if (data == "train"){
        warning("You asked for the 'best.test' tree, but data was set to 'train'. Used the best tree for 'test' data instead...")
        data <- "test"
        if (is.null(main)) { main <- "Data (Testing)" }
      }

      # tree <- x$trees$best$test  # using current x
      tree <- get_best_tree(x, data = "test", goal = x$params$goal)  # using helper
    }


    # Define critical objects: ------

    # decision_v  <- x$trees$decisions[[data]][[tree]]$decision
    tree_stats  <- x$trees$stats[[data]]
    level_stats <- x$trees$level_stats[[data]][x$trees$level_stats[[data]]$tree == tree, ]

    # Get criterion (from object x):
    criterion_name <- x$criterion_name  # (only ONCE)

    # Compute criterion baseline/base rate:
    if (allow_NA_crit){
      crit_br <- mean(x$data[[data]][[criterion_name]], na.rm = TRUE)
    } else { # default:
      crit_br <- mean(x$data[[data]][[criterion_name]])  # (from logical, i.e., proportion of TRUE values)
    }

    n_exemplars <- nrow(x$data[[data]])
    n_pos_cases <- sum(x$data[[data]][[criterion_name]] == TRUE)
    n_neg_cases <- sum(x$data[[data]][[criterion_name]] == FALSE)
    mcu <- x$trees$stats[[data]]$mcu[tree]

    final_stats <- tree_stats[tree, ]


    # Add level statistics: ----

    n_levels <- nrow(level_stats)

    # Add marginal classification statistics to level_stats / Frequencies per level:

    level_stats$hi_m <- NA  # initialize marginal freqs
    level_stats$fa_m <- NA
    level_stats$mi_m <- NA
    level_stats$cr_m <- NA

    for (i in 1:n_levels) {

      if (i == 1) {
        level_stats$hi_m[1] <- level_stats$hi[1]
        level_stats$fa_m[1] <- level_stats$fa[1]
        level_stats$mi_m[1] <- level_stats$mi[1]
        level_stats$cr_m[1] <- level_stats$cr[1]
      }

      if (i > 1) {
        level_stats$hi_m[i] <- level_stats$hi[i] - level_stats$hi[i - 1]
        level_stats$fa_m[i] <- level_stats$fa[i] - level_stats$fa[i - 1]
        level_stats$mi_m[i] <- level_stats$mi[i] - level_stats$mi[i - 1]
        level_stats$cr_m[i] <- level_stats$cr[i] - level_stats$cr[i - 1]
      }

    } # for n_levels.

    # print(level_stats)  # tree with marginal frequency values (for each level)


    # Set plotting parameters: ----

    # Label sizes:

    # print(paste0("par('cex') = ", par("cex")))  # Note: Value varies from .66 to 1

    # Sizes not set by user:
    f_cex <- 1  # cex scaling factor

    decision_node_cex <- 4 * f_cex
    exit_node_cex     <- 4 * f_cex
    panel_title_cex   <- 2 * f_cex

    # Set by user arguments:

    # Cue label size:
    if (is.null(cue.cex)) {
      cue.cex <- c(1.50, 1.50, 1.25, 1, 1, 1)
    } else {
      if (length(cue.cex) < 6) {
        cue.cex <- rep(cue.cex, length.out = 6)
      }
    }
    # print(cue.cex)  # 4debugging

    # Break label size:
    if (is.null(threshold.cex)) {
      threshold.cex <- c(1.50, 1.50, 1.25, 1, 1, 1)
    } else {
      if (length(threshold.cex) < 6) {
        threshold.cex <- rep(threshold.cex, length.out = 6)
      }
    }
    # print(threshold.cex)  # 4debugging

    # Panel parameters:
    panel_line_lwd <- 1
    col_panel_line <- gray(0) # = black
    panel_line_lty <- 1

    # Ball parameters:
    ball_col <- c(gray(0), gray(0))  # = black
    ball_bg  <- c(gray(1), gray(1))  # = white
    ball_pch <- c(21, 24)
    ball_cex <- c(1, 1)

    # error.col <- "red"      # is NOT used anywhere?
    # correct.col <- "green"  # is NOT used anywhere?

    max_label_length <- 100
    # def_par <- par(no.readonly = TRUE)  # is NOT used anywhere?

    ball_box_width   <- 10
    label_box_height <-  2
    label_box_width  <-  5

    # Cue labels:
    if (is.null(cue.labels)) {
      cue.labels <- level_stats$cue
    }

    # Trim labels:
    cue.labels <- strtrim(cue.labels, max_label_length)

    # Lines/node segments:
    segment_lty <- 1
    segment_lwd <- 1

    # continue_segment_lwd <- 1  # is NOT used anywhere?
    # continue_segment_lty <- 1  # is NOT used anywhere?

    # exit_segment_lwd <- 1  # is NOT used anywhere?
    # exit_segment_lty <- 1  # is NOT used anywhere?


    # Define plotting_parameters_df:
    if (show.top & show.middle & show.bottom) { # plot "all":

      plotting_parameters_df <- data.frame(
        n_levels = 1:6,
        plot_height = c(10, 12, 15, 19, 23, 27),        # Note: use default, +2 for n_levels == 6
        plot_width  = c(14, 16, 20, 24, 28, 34) * 1.0,  # Note: use default, +2 for n_levels == 6
        label_box_text_cex = cue.cex,
        break_label_cex = threshold.cex
      )

    } else if ((show.top == FALSE) & show.middle & (show.bottom == FALSE)) {  # only "ico" or "tree":

      plotting_parameters_df <- data.frame(
        n_levels = 1:6,
        plot_height = c(10, 12, 15, 19, 23, 25),
        plot_width  = c(14, 16, 20, 24, 28, 32) * 0.80,  # stretch wider (but not too wide for n.per.icon = 1)
        label_box_text_cex = cue.cex,
        break_label_cex = threshold.cex
      )

    } else { # default:

      plotting_parameters_df <- data.frame(
        n_levels = 1:6,
        plot_height = c(10, 12, 15, 19, 23, 25),
        plot_width  = c(14, 16, 20, 24, 28, 32) * 1,  # stretch to default width
        label_box_text_cex = cue.cex,
        break_label_cex = threshold.cex
      )
    }

    # local variables:
    if (n_levels < 6) {

      label_box_text_cex <- plotting_parameters_df$label_box_text_cex[n_levels]
      break_label_cex <- plotting_parameters_df$break_label_cex[n_levels]
      plot_height <- plotting_parameters_df$plot_height[n_levels]
      plot_width <- plotting_parameters_df$plot_width[n_levels]

    } else { # n_levels >= 6:

      label_box_text_cex <- plotting_parameters_df$label_box_text_cex[6]
      break_label_cex <- plotting_parameters_df$break_label_cex[6]
      plot_height <- plotting_parameters_df$plot_height[6]
      plot_width <- plotting_parameters_df$plot_width[6]

    }


    # Colors: ----

    col_exit_node_bg <- "white"

    # error.colfun <- circlize::colorRamp2(c(0, 50, 100),
    #                            colors = c("white", "red", "black"))
    #
    # correct.colfun <-  circlize::colorRamp2(c(0, 50, 100),
    #                            colors = c("white", "green", "black"))
    #
    # col_error_bg <- scales::alpha(error.colfun(35), .8)
    # col_error_border <-  scales::alpha(error.colfun(65), .9)
    # col_correct_bg <- scales::alpha(correct.colfun(35), .8)
    # col_correct_border <-  scales::alpha(correct.colfun(65), .9)

    col_error_bg <- "#FF7352CC"
      col_error_border <- "#AD1A0AE6"
        col_correct_bg <- "#89FF6FCC"
          col_correct_border <- "#24AB18E6"

          # max_cex <- 6  # is NOT used anywhere?
          # min_cex <- 1  # is NOT used anywhere?

          exit_node_pch <- 21

          decision_node_pch <- NA_integer_


          # Balls: ----

          ball_loc <- "variable"

          if (n_levels == 3) {
            ball_box_width <- 14
          }

          if (n_levels == 4) {
            ball_box_width <- 18
          }

          ball_box_height      <- 2.5
          ball_box_horiz_shift <- 10
          ball_box_vert_shift  <- -1
          ball_box_max_shift_p <- .9
          ball_box_min_shift_p <- .4

          ball_box_fixed_x_shift <- c(ball_box_min_shift_p * plot_width, ball_box_max_shift_p * plot_width)

          # Determine N per ball:
          if (is.null(n.per.icon)) {

            max_n_side <- max(c(n_pos_cases, n_neg_cases))

            i <- max_n_side / c(1, 5, 10^(1:10))
            i[i > 50] <- 0

            n.per.icon <- c(1, 5, 10^(1:10))[which(i == max(i))]

          }

          noise_ball_pch  <- ball_pch[1]
          signal_ball_pch <- ball_pch[2]
          noise_ball_col  <- ball_col[1]
          signal_ball_col <- ball_col[2]
          noise_ball_bg   <- ball_bg[1]
          signal_ball_bg  <- ball_bg[2]


          # Arrows: ----

          arrow_lty <- 1
          arrow_lwd <- 1
          arrow_length      <- 2.50
          arrow_head_length <-  .08
          arrow_col <- gray(0) # = black


          # Final stats: ----

          # spec_circle_x   <- .40  # is NOT used anywhere?
          # dprime_circle_x <- .50  # is NOT used anywhere?
          # sens_circle_x   <- .60  # is NOT used anywhere?

          # stat_circle_y   <- .30  # is NOT used anywhere?

          # sens_circle_col   <- "green"  # is NOT used anywhere?
          # spec_circle_col   <- "red"    # is NOT used anywhere?
          # dprime_circle_col <- "blue"   # is NOT used anywhere?
          # stat_outer_circle_col <- gray(.50)  # is NOT used anywhere?


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

              segments(0, .95, 1, .95, col = col_panel_line, lwd = panel_line_lwd, lty = panel_line_lty)  # top hline

              x_dev <- get_x_dev(main)
              y_dev <- .20
              rect((.50 - x_dev), (1 - y_dev), (.50 + x_dev), (1 + y_dev), col = "white", border = NA)  # title background

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
              x_lim = c(.33, .67),
              y_lim = c(.12, .52),
              n_vec = c(n_true_neg, n_true_pos),
              pch_vec = c(noise_ball_pch, signal_ball_pch),
              bg_vec = c(noise_ball_bg, signal_ball_bg),
              col_vec = c(noise_ball_col, signal_ball_col),
              ball_cex = ball_cex,
              upper_text_adj = 2,
              n_per_icon = n.per.icon
            )

            # (c) n.per.icon legend 1 (top):

            # show_icon_guide_legend <- TRUE  # 4debugging

            if (show_icon_guide_legend){

              text(.98, 0, labels = paste("Showing ", n.per.icon, " cases per icon:", sep = ""), pos = 2)
              points(.98, 0, pch = noise_ball_pch,  cex = ball_cex)
              points(.99, 0, pch = signal_ball_pch, cex = ball_cex)

            } # if (show_icon_guide_legend).


            par(xpd = FALSE)


            # 3. Add p_signal and p_noise levels: -----

            signal_p <- crit_br  # criterion baseline/base rate (from above)
            noise_p  <- (1 - signal_p)

            p_rect_ylim <- c(.10, .60)


            # (a) p_signal level (on right): ----

            text(
              x = .80, y = p_rect_ylim[2],
              labels = paste("p(", decision.labels[2], ")", sep = ""),
              pos = 3, cex = 1.2
            )

            # Filling:
            rect(.775, p_rect_ylim[1],
                 .825, p_rect_ylim[1] + signal_p * diff(p_rect_ylim),
                 col = gray(.50, .25), border = NA
            )

            # Filltop:
            segments(.775, p_rect_ylim[1] + signal_p * diff(p_rect_ylim),
                     .825, p_rect_ylim[1] + signal_p * diff(p_rect_ylim),
                     lwd = 1
            )

            # Outline:
            rect(.775, p_rect_ylim[1],
                 .825, p_rect_ylim[2],
                 lwd = 1
            )

            if (signal_p < .0001) {
              signal_p_text <- "<1%"
            } else {
              signal_p_text <- paste(round(signal_p * 100, 0), "%", sep = "")
            }

            text(.825, p_rect_ylim[1] + signal_p * diff(p_rect_ylim),
                 labels = signal_p_text,
                 pos = 4, cex = 1.2
            )


            # (b) p_noise level (on left): ----

            text(
              x = .20, y = p_rect_ylim[2],
              labels = paste("p(", decision.labels[1], ")", sep = ""),
              pos = 3, cex = 1.2
            )


            rect(.175, p_rect_ylim[1], .225, p_rect_ylim[1] + noise_p * diff(p_rect_ylim),
                 col = gray(.50, .25), border = NA
            )

            # Filltop:
            segments(.175, p_rect_ylim[1] + noise_p * diff(p_rect_ylim),
                     .225, p_rect_ylim[1] + noise_p * diff(p_rect_ylim),
                     lwd = 1
            )

            # Outline:
            rect(.175, p_rect_ylim[1], .225, p_rect_ylim[2],
                 lwd = 1
            )

            if (noise_p < .0001) {
              noise_p_text <- "<0.01%"
            } else {
              noise_p_text <- paste(round(noise_p * 100, 0), "%", sep = "")
            }

            text(.175, p_rect_ylim[1] + noise_p * diff(p_rect_ylim),
                 labels = noise_p_text,
                 pos = 2, cex = 1.2
            )

          } # if (show.top).


          # 2. Main TREE: ------

          if (show.middle) {

            if ((show.top == FALSE) & (show.bottom == FALSE)) {
              par(mar = c(3, 3, 3, 3) + .1)
            } else {
              par(mar = c(0, 0, 0, 0))
            }

            par(xpd = TRUE)

            # Prepare plot:
            plot(1,
                 xlim = c(-plot_width, plot_width),
                 ylim = c(-plot_height, 0),
                 type = "n", bty = "n",
                 xaxt = "n", yaxt = "n",
                 ylab = "", xlab = ""
            )


            # Middle title: ----

            if (show.top | show.bottom) {

              if (hlines) {
                x_dev <- .28  # scaling factor, rather than difference
                segments(-plot_width, 0, -plot_width * x_dev, 0, col = col_panel_line, lwd = panel_line_lwd, lty = panel_line_lty)
                segments( plot_width, 0,  plot_width * x_dev, 0, col = col_panel_line, lwd = panel_line_lwd, lty = panel_line_lty)
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

              get_exit_word <- get_exit_word(data)  # either 'train':'decide' or 'test':'predict'


              # (a) Noise panel (on left): ----

              # Parameters:

              if (what == "ico"){
                leg_head_y <- .02
                leg_ball_y <- .14
              } else {
                leg_head_y <- .05
                leg_ball_y <- .15
              }


              # Heading:
              text(-plot_width  * .60 * f_x,
                   -plot_height * leg_head_y * f_y,
                   paste(get_exit_word, decision.labels[1], sep = " "),
                   cex = 1.2, font = 3
              )

              # Noise balls:
              points(c(-plot_width  * .70, -plot_width  * .50) * f_x,
                     c(-plot_height * leg_ball_y, -plot_height * leg_ball_y) * f_y,
                     pch = c(noise_ball_pch, signal_ball_pch),
                     bg = c(col_correct_bg, col_error_bg),
                     col = c(col_correct_border, col_error_border),
                     cex = ball_cex * 1.5
              )

              # Labels:
              text(c(-plot_width  * .70, -plot_width  * .50) * f_x,
                   c(-plot_height * leg_ball_y, -plot_height * leg_ball_y) * f_y,
                   labels = c("Correct\nRejection", "Miss"),
                   pos = c(2, 4), offset = .80, cex = 1
              )



              # (b) Signal panel (on right): ----

              # Heading:
              text( plot_width  * .60 * f_x,
                    -plot_height * leg_head_y * f_y,
                    paste(get_exit_word, decision.labels[2], sep = " "),
                    cex = 1.2, font = 3
              )

              # Signal balls:
              points(c(plot_width   * .50,  plot_width  * .70 ) * f_x,
                     c(-plot_height * leg_ball_y, -plot_height * leg_ball_y) * f_y,
                     pch = c(noise_ball_pch, signal_ball_pch),
                     bg = c(col_error_bg, col_correct_bg),
                     col = c(col_error_border, col_correct_border),
                     cex = ball_cex * 1.5
              )

              # Labels:
              text(c( plot_width  * .50,  plot_width  * .70) * f_x,
                   c(-plot_height * leg_ball_y, -plot_height * leg_ball_y) * f_y,
                   labels = c("False\nAlarm", "Hit"),
                   pos = c(2, 4), offset = .80, cex = 1
              )


              # (c) Additional lines (below icon guide): ----
              if (what == "ico" & hlines) {

                x_hline <-  plot_width  * 1.0 * f_x
                y_hline <- -plot_height * .22 * f_y

                segments(-x_hline, y_hline, x_hline, y_hline, col = col_panel_line, lwd = panel_line_lwd, lty = panel_line_lty)
                rect(-x_hline * .33, (y_hline - .5), x_hline * .33, (y_hline + .5), col = "white", border = NA)
              }


              # (d) n.per.icon legend 2 (middle): ----

              if (what == "ico") { show_icon_guide_legend <- TRUE } # special case

              if (show_icon_guide_legend){

                if (what == "ico") { # special case:

                  x_s2 <- plot_width
                  x_s1 <- plot_width - .80     # left of default
                  y_s1 <- plot_height * -1.10  # lower than default

                } else { # defaults:

                  x_s2 <- plot_width
                  x_s1 <- plot_width - .40
                  y_s1 <- plot_height * -1

                }

                text(x_s1, y_s1, labels = paste("Showing ", n.per.icon, " cases per icon:", sep = ""), pos = 2, cex = ball_cex)
                points(x_s1, y_s1, pch = noise_ball_pch,  cex = ball_cex)
                points(x_s2, y_s1, pch = signal_ball_pch, cex = ball_cex)

              } # if (show_icon_guide_legend).

            } # if (show.iconguide).

            par(xpd = FALSE)


            # Plot main TREE: ------

            # Set initial subplot center:
            subplot_center <- c(0, -4)

            # Loop over levels: ------
            for (level_i in 1:min(c(n_levels, 6))) {

              # Cue label:
              cur_cue <- cue.labels[level_i]

              # Get stats for current level:
              hi_i <- level_stats$hi_m[level_i]
              fa_i <- level_stats$fa_m[level_i]
              mi_i <- level_stats$mi_m[level_i]
              cr_i <- level_stats$cr_m[level_i]


              # Top: If level_i == 1, draw top textbox: ----

              if (level_i == 1) {

                rect(subplot_center[1] - label_box_width / 2,
                     subplot_center[2] + 2 - label_box_height / 2,
                     subplot_center[1] + label_box_width / 2,
                     subplot_center[2] + 2 + label_box_height / 2,
                     col = "white",
                     border = "black"
                )

                points(
                  x = subplot_center[1],
                  y = subplot_center[2] + 2,
                  cex = decision_node_cex,
                  pch = decision_node_pch
                )

                text(
                  x = subplot_center[1],
                  y = subplot_center[2] + 2,
                  labels = cur_cue,
                  cex = label_box_text_cex  # WAS: get_label_cex(cur_cue, label_box_text_cex = label_box_text_cex)
                )

              } # if (level_i == 1).


              # Left (Noise) classification / New level: ----

              # Exit node on 0 / FALSE / noise / left: ----

              # if (level_stats$exit[level_i] %in% c(0, .5) | paste(level_stats$exit[level_i]) %in% c("0", ".5")) {
              if ( (level_stats$exit[level_i] %in% exit_types[c(1, 3)]) | (paste(level_stats$exit[level_i]) %in% paste(exit_types[c(1, 3)], collapse = ", ")) ) {

                segments(subplot_center[1],
                         subplot_center[2] + 1,
                         subplot_center[1] - 2,
                         subplot_center[2] - 2,
                         lty = segment_lty,
                         lwd = segment_lwd
                )

                arrows(
                  x0 = subplot_center[1] - 2,
                  y0 = subplot_center[2] - 2,
                  x1 = subplot_center[1] - 2 - arrow_length,
                  y1 = subplot_center[2] - 2,
                  lty = arrow_lty,
                  lwd = arrow_lwd,
                  col = arrow_col,
                  length = arrow_head_length
                )

                # Decision text:

                if (decision.cex > 0) {

                  text(
                    x = subplot_center[1] - 2 - arrow_length * .7,
                    y = subplot_center[2] - 2.2,
                    labels = decision.labels[1],
                    pos = 1, font = 3, cex = decision.cex
                  )

                }

                if (ball_loc == "fixed") {

                  ball_x_lim <- c(-max(ball_box_fixed_x_shift), -min(ball_box_fixed_x_shift))

                  ball_y_lim <- c(
                    subplot_center[2] + ball_box_vert_shift - ball_box_height / 2,
                    subplot_center[2] + ball_box_vert_shift + ball_box_height / 2
                  )

                }

                if (ball_loc == "variable") {

                  ball_x_lim <- c(
                    subplot_center[1] - ball_box_horiz_shift - ball_box_width / 2,
                    subplot_center[1] - ball_box_horiz_shift + ball_box_width / 2
                  )

                  ball_y_lim <- c(
                    subplot_center[2] + ball_box_vert_shift - ball_box_height / 2,
                    subplot_center[2] + ball_box_vert_shift + ball_box_height / 2
                  )

                }

                if ((max(c(cr_i, mi_i), na.rm = TRUE) > 0) & (show.icons == TRUE)) {

                  add_balls(
                    x_lim = ball_x_lim,
                    y_lim = ball_y_lim,
                    n_vec = c(cr_i, mi_i),
                    pch_vec = c(noise_ball_pch, signal_ball_pch),
                    ball_cex = ball_cex,
                    # bg_vec = c(noise_ball_bg, signal_ball_bg),
                    bg_vec = c(col_correct_bg, col_error_bg),
                    col_vec = c(col_correct_border, col_error_border),
                    freq_text = TRUE,
                    n_per_icon = n.per.icon
                  )

                }

                # level break label:
                pos_dir_symbol <- c("<=", "<", "=", "!=", ">", ">=")[which(level_stats$direction[level_i] == c(">", ">=", "!=", "=", "<=", "<"))]
                neg_dir_symbol <- c("<=", "<", "=", "!=", ">", ">=")[which(level_stats$direction[level_i] == c("<=", "<", "=", "!=", ">", ">="))]

                text_outline(
                  x = subplot_center[1] - 1,
                  y = subplot_center[2],
                  labels = paste(pos_dir_symbol, " ", level_stats$threshold[level_i], sep = ""),
                  pos = 2, cex = break_label_cex, r = .1
                )

                points(
                  x = subplot_center[1] - 2,
                  y = subplot_center[2] - 2,
                  pch = exit_node_pch,
                  cex = exit_node_cex,
                  bg = col_exit_node_bg
                )

                text(
                  x = subplot_center[1] - 2,
                  y = subplot_center[2] - 2,
                  labels = substr(decision.labels[1], 1, 1)
                )

              } # if (exit node on left).


              # New level on 1 / TRUE / signal / right: ----

              # if ((level_stats$exit[level_i] %in% c(1)) | (paste(level_stats$exit[level_i]) %in% c("1"))) {
              if ( (level_stats$exit[level_i] %in% exit_types[c(2)]) | (paste(level_stats$exit[level_i]) %in% paste(exit_types[c(2)], collapse = ", ")) ) {

                segments(subplot_center[1],
                         subplot_center[2] + 1,
                         subplot_center[1] - 2,
                         subplot_center[2] - 2,
                         lty = segment_lty,
                         lwd = segment_lwd
                )

                rect(subplot_center[1] - 2 - label_box_width / 2,
                     subplot_center[2] - 2 - label_box_height / 2,
                     subplot_center[1] - 2 + label_box_width / 2,
                     subplot_center[2] - 2 + label_box_height / 2,
                     col = "white",
                     border = "black"
                )

                if (level_i < 6) {

                  text(
                    x = subplot_center[1] - 2,
                    y = subplot_center[2] - 2,
                    labels = cue.labels[level_i + 1],
                    cex = label_box_text_cex
                  )

                } else {

                  text(
                    x = subplot_center[1] - 2,
                    y = subplot_center[2] - 2,
                    labels = paste0("+ ", n_levels - 6, " More"),
                    cex = label_box_text_cex,
                    font = 3
                  )

                }

              } # if (new level on right).


              # Right (Signal) classification / New level: ----

              # Exit node on 1 / TRUE / signal / right: ----

              # if ((level_stats$exit[level_i] %in% c(1, .5)) | (paste(level_stats$exit[level_i]) %in% c("1", ".5"))) {
              if ( (level_stats$exit[level_i] %in% exit_types[c(2, 3)]) | (paste(level_stats$exit[level_i]) %in% paste(exit_types[c(2, 3)], collapse = ", ")) ) {

                segments(subplot_center[1],
                         subplot_center[2] + 1,
                         subplot_center[1] + 2,
                         subplot_center[2] - 2,
                         lty = segment_lty,
                         lwd = segment_lwd
                )

                arrows(
                  x0 = subplot_center[1] + 2,
                  y0 = subplot_center[2] - 2,
                  x1 = subplot_center[1] + 2 + arrow_length,
                  y1 = subplot_center[2] - 2,
                  lty = arrow_lty,
                  lwd = arrow_lwd,
                  col = arrow_col,
                  length = arrow_head_length
                )

                # Decision text:

                if (decision.cex > 0) {
                  text(
                    x = subplot_center[1] + 2 + arrow_length * .7,
                    y = subplot_center[2] - 2.2,
                    labels = decision.labels[2],
                    pos = 1,
                    font = 3,
                    cex = decision.cex
                  )

                }

                if (ball_loc == "fixed") {

                  ball_x_lim <- c(min(ball_box_fixed_x_shift), max(ball_box_fixed_x_shift))
                  ball_y_lim <- c(
                    subplot_center[2] + ball_box_vert_shift - ball_box_height / 2,
                    subplot_center[2] + ball_box_vert_shift + ball_box_height / 2
                  )

                }

                if (ball_loc == "variable") {

                  ball_x_lim <- c(
                    subplot_center[1] + ball_box_horiz_shift - ball_box_width / 2,
                    subplot_center[1] + ball_box_horiz_shift + ball_box_width / 2
                  )

                  ball_y_lim <- c(
                    subplot_center[2] + ball_box_vert_shift - ball_box_height / 2,
                    subplot_center[2] + ball_box_vert_shift + ball_box_height / 2
                  )

                }

                if ((max(c(fa_i, hi_i), na.rm = TRUE) > 0) & (show.icons == TRUE)) {

                  add_balls(
                    x_lim = ball_x_lim,
                    y_lim = ball_y_lim,
                    n_vec = c(fa_i, hi_i),
                    pch_vec = c(noise_ball_pch, signal_ball_pch),
                    ball_cex = ball_cex,
                    # bg_vec = c(noise_ball_bg, signal_ball_bg),
                    bg_vec = c(col_error_bg, col_correct_bg),
                    col_vec = c(col_error_border, col_correct_border),
                    freq_text = TRUE,
                    n_per_icon = n.per.icon
                  )

                }

                # level break label:
                dir_symbol <- c("<=", "<", "=", "!=", ">", ">=")  # as (local) constant

                pos_dir_symbol <- dir_symbol[which(level_stats$direction[level_i] == c("<=", "<", "=", "!=", ">", ">="))]
                neg_dir_symbol <- dir_symbol[which(level_stats$direction[level_i] == rev(c("<=", "<", "=", "!=", ">", ">=")))]


                text_outline(subplot_center[1] + 1,
                             subplot_center[2],
                             labels = paste(pos_dir_symbol, " ", level_stats$threshold[level_i], sep = ""),
                             pos = 4, cex = break_label_cex, r = .1
                )

                points(
                  x = subplot_center[1] + 2,
                  y = subplot_center[2] - 2,
                  pch = exit_node_pch,
                  cex = exit_node_cex,
                  bg = col_exit_node_bg
                )

                text(
                  x = subplot_center[1] + 2,
                  y = subplot_center[2] - 2,
                  labels = substr(decision.labels[2], 1, 1)
                )

              } # if (exit node on right).


              # New level on 0 / FALSE / noise / left: ----

              # if (level_stats$exit[level_i] %in% 0 | paste(level_stats$exit[level_i]) %in% c("0")) {
              if ( (level_stats$exit[level_i] %in% exit_types[c(1)]) | (paste(level_stats$exit[level_i]) %in% paste(exit_types[c(1)], collapse = ", ")) ) {

                segments(subplot_center[1],
                         subplot_center[2] + 1,
                         subplot_center[1] + 2,
                         subplot_center[2] - 2,
                         lty = segment_lty,
                         lwd = segment_lwd
                )

                if (level_i < 6) {

                  rect(subplot_center[1] + 2 - label_box_width / 2,
                       subplot_center[2] - 2 - label_box_height / 2,
                       subplot_center[1] + 2 + label_box_width / 2,
                       subplot_center[2] - 2 + label_box_height / 2,
                       col = "white",
                       border = "black"
                  )

                  text(
                    x = subplot_center[1] + 2,
                    y = subplot_center[2] - 2,
                    labels = cue.labels[level_i + 1],
                    cex = label_box_text_cex
                  )

                } else {

                  rect(subplot_center[1] + 2 - label_box_width / 2,
                       subplot_center[2] - 2 - label_box_height / 2,
                       subplot_center[1] + 2 + label_box_width / 2,
                       subplot_center[2] - 2 + label_box_height / 2,
                       col = "white",
                       border = "black", lty = 2
                  )

                  text(
                    x = subplot_center[1] + 2,
                    y = subplot_center[2] - 2,
                    labels = paste0("+ ", n_levels - 6, " More"),
                    cex = label_box_text_cex,
                    font = 3
                  )
                }

              } # if (new level on right).


              # Update plot center: ----

              # if (identical(paste(level_stats$exit[level_i]), "0")) { # 0 / FALSE / noise / left:
              if (identical(paste(level_stats$exit[level_i]), paste0(exit_types[1]))) {

                subplot_center <- c(
                  subplot_center[1] + 2,
                  subplot_center[2] - 4
                )
              } # if (identical exit 0 / left etc.

              # if (identical(paste(level_stats$exit[level_i]), "1")) { # 1 / TRUE / signal / right:
              if (identical(paste(level_stats$exit[level_i]), paste0(exit_types[2]))) {

                subplot_center <- c(
                  subplot_center[1] - 2,
                  subplot_center[2] - 4
                )

              } # if (identical exit 1 / right etc.

            } # for (level_i etc. loop.

          } # if (show.middle).


          # 3. Cumulative performance: ----

          if (show.bottom == TRUE) { # obtain tree statistics:

            fft_sens_vec <- tree_stats$sens
            fft_spec_vec <- tree_stats$spec

            # General plotting space: ----

            # Parameters:
            header_y <- 1.0
            subheader_y <- .925

            header_cex <- 1.10
            subheader_cex <- .90

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

                segments(0, 1.1, 1, 1.1, col = col_panel_line, lwd = panel_line_lwd, lty = panel_line_lty)

                x_dev <- .20
                rect((.50 - x_dev), 1, (.50 + x_dev), 1.2, col = "white", border = NA) # label background
              }

              # Bottom label:
              if (is.null(label.performance)) { # user argument not set:

                if (data == "train") {
                  label.performance <- "Accuracy (Training)"
                }
                if (data == "test") {
                  label.performance <- "Accuracy (Testing)"
                }

              }

              text(x = .50, y = 1.1, labels = label.performance, cex = panel_title_cex, ...)  # title 3 (bottom): Performance

              par(xpd = FALSE)

            } # if (what != "roc").


            # Level parameters:
            level_height_max <- .65
            level_width    <- .05
            level_center_y <- .45
            # level_bottom <- .1
            level_bottom   <- level_center_y - (level_height_max / 2)
            level_top      <- level_center_y + (level_height_max / 2)

            # Get either bacc OR wacc (based on sens.w):
            sens.w <- x$params$sens.w
            bacc_wacc <- get_bacc_wacc(sens = final_stats$sens, spec = final_stats$spec, sens.w = sens.w)
            bacc_wacc_name <- names(bacc_wacc)

            # Set labels, values, and locations (as df):
            lloc <- data.frame(
              element = c("classtable", "mcu", "pci", "sens", "spec", "acc", bacc_wacc_name, "roc"),
              long_name = c("Classification Table", "mcu", "pci", "sens", "spec", "acc", bacc_wacc_name, "ROC"),  # used by add_level() helper function
              center_x = c(.18, seq(.35, .65, length.out = 6), .85),
              center_y = rep(level_center_y, 8),
              width  = c(.20, rep(level_width, 6), .20),
              height = c(.65, rep(level_height_max, 6), .65),
              value = c(NA,
                        abs(final_stats$mcu - 5) / (abs(1 - 5)), final_stats$pci,
                        final_stats$sens, final_stats$spec,
                        with(final_stats, (cr + hi) / n), bacc_wacc, NA),
              value_name = c(NA,
                             round(final_stats$mcu, 1), pretty_dec(final_stats$pci),     # used by add_level() helper function
                             pretty_dec(final_stats$sens), pretty_dec(final_stats$spec),
                             pretty_dec(final_stats$acc), pretty_dec(bacc_wacc), NA)
            )
            # print(lloc)  # 4debugging


            # Classification table: ----

            if (show.confusion) {

              # Parameters:
              classtable_lwd <- 1

              # x/y coordinates:
              final_classtable_x <- c(lloc$center_x[lloc$element == "classtable"] - lloc$width[lloc$element  == "classtable"] / 2, lloc$center_x[lloc$element == "classtable"] + lloc$width[lloc$element  == "classtable"] / 2)
              final_classtable_y <- c(lloc$center_y[lloc$element == "classtable"] - lloc$height[lloc$element == "classtable"] / 2, lloc$center_y[lloc$element == "classtable"] + lloc$height[lloc$element == "classtable"] / 2)

              rect(final_classtable_x[1], final_classtable_y[1],
                   final_classtable_x[2], final_classtable_y[2],
                   lwd = classtable_lwd
              )

              segments(mean(final_classtable_x), final_classtable_y[1], mean(final_classtable_x), final_classtable_y[2], col = gray(0), lwd = classtable_lwd)
              segments(final_classtable_x[1], mean(final_classtable_y), final_classtable_x[2], mean(final_classtable_y), col = gray(0), lwd = classtable_lwd)


              # Column titles: ----

              text(
                x = mean(mean(final_classtable_x)),
                y = header_y,
                "Truth", pos = 1, cex = header_cex
              )

              text(
                x = final_classtable_x[1] + .25 * diff(final_classtable_x),
                y = subheader_y, pos = 1, cex = subheader_cex,
                decision.labels[2]
              )

              text(
                x = final_classtable_x[1] + .75 * diff(final_classtable_x),
                y = subheader_y, pos = 1, cex = subheader_cex,
                decision.labels[1]
              )


              # Row titles: ----

              text(
                x = final_classtable_x[1] - .01,
                y = final_classtable_y[1] + .75 * diff(final_classtable_y), cex = subheader_cex,
                decision.labels[2], adj = 1
              )

              text(
                x = final_classtable_x[1] - .01,
                y = final_classtable_y[1] + .25 * diff(final_classtable_y), cex = subheader_cex,
                decision.labels[1], adj = 1
              )

              text(
                x = final_classtable_x[1] - .065,
                y = mean(final_classtable_y), cex = header_cex,
                "Decision"
              )

              # text(x = final_classtable_x[1] - .05,
              #      y = mean(final_classtable_y), cex = header_cex,
              #      "Decision", srt = 90, pos = 3)


              # Add final frequencies: ----

              text(final_classtable_x[1] + .75 * diff(final_classtable_x),
                   final_classtable_y[1] + .25 * diff(final_classtable_y),
                   prettyNum(final_stats$cr, big.mark = ","),
                   cex = 1.5
              )

              text(final_classtable_x[1] + .25 * diff(final_classtable_x),
                   final_classtable_y[1] + .25 * diff(final_classtable_y),
                   prettyNum(final_stats$mi, big.mark = ","),
                   cex = 1.5
              )

              text(final_classtable_x[1] + .75 * diff(final_classtable_x),
                   final_classtable_y[1] + .75 * diff(final_classtable_y),
                   prettyNum(final_stats$fa, big.mark = ","),
                   cex = 1.5
              )

              text(final_classtable_x[1] + .25 * diff(final_classtable_x),
                   final_classtable_y[1] + .75 * diff(final_classtable_y),
                   prettyNum(final_stats$hi, big.mark = ","),
                   cex = 1.5
              )


              # Add symbols: ----

              points(final_classtable_x[1] + .55 * diff(final_classtable_x),
                     final_classtable_y[1] + .05 * diff(final_classtable_y),
                     pch = noise_ball_pch, bg = col_correct_bg, col = col_correct_border, cex = ball_cex
              )

              points(final_classtable_x[1] + .05 * diff(final_classtable_x),
                     final_classtable_y[1] + .55 * diff(final_classtable_y),
                     pch = signal_ball_pch, bg = col_correct_bg, cex = ball_cex, col = col_correct_border
              )

              points(final_classtable_x[1] + .55 * diff(final_classtable_x),
                     final_classtable_y[1] + .55 * diff(final_classtable_y),
                     pch = noise_ball_pch, bg = col_error_bg, col = col_error_border, cex = ball_cex
              )

              points(final_classtable_x[1] + .05 * diff(final_classtable_x),
                     final_classtable_y[1] + .05 * diff(final_classtable_y),
                     pch = signal_ball_pch, bg = col_error_bg, col = col_error_border, cex = ball_cex
              )


              # Add labels: ----

              text(final_classtable_x[1] + .62 * diff(final_classtable_x),
                   final_classtable_y[1] + .07 * diff(final_classtable_y),
                   "cr",
                   cex = 1, font = 3, adj = 0
              )

              text(final_classtable_x[1] + .12 * diff(final_classtable_x),
                   final_classtable_y[1] + .07 * diff(final_classtable_y),
                   "mi",
                   cex = 1, font = 3, adj = 0
              )

              text(final_classtable_x[1] + .62 * diff(final_classtable_x),
                   final_classtable_y[1] + .57 * diff(final_classtable_y),
                   "fa",
                   cex = 1, font = 3, adj = 0
              )

              text(final_classtable_x[1] + .12 * diff(final_classtable_x),
                   final_classtable_y[1] + .57 * diff(final_classtable_y),
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

                # segments(x0 = lloc$center_x[lloc$element == "mcu"] - lloc$width[lloc$element == "mcu"] * .8,
                #          y0 = level_top,
                #          x1 = lloc$center_x[lloc$element == "bacc"] + lloc$width[lloc$element == "bacc"] * .8,
                #          y1 = level_top,
                #          lty = 3, lwd = .75)


                # mcu level: ----

                add_level("mcu", ok_val = .75, min_val = 0, max_val = 1,
                          level_type = level.type, lloc_row = lloc[lloc$element == "mcu", ],
                          header_y = header_y, header_cex = header_cex) # , sub = paste(c(final_stats$cr, "/", final_stats$cr + final_stats$fa), collapse = ""))


                # pci level: ----

                add_level("pci", ok_val = .75, min_val = 0, max_val = 1,
                          level_type = level.type, lloc_row = lloc[lloc$element == "pci", ],
                          header_y = header_y, header_cex = header_cex) # , sub = paste(c(final_stats$cr, "/", final_stats$cr + final_stats$fa), collapse = ""))

                # text(lloc$center_x[lloc$element == "pci"],
                #      lloc$center_y[lloc$element == "pci"],
                #      labels = paste0("mcu\n", round(mcu, 2)))


                # spec level: ----

                add_level("spec", ok_val = .75, min_val = 0, max_val = 1,
                          level_type = level.type, lloc_row = lloc[lloc$element == "spec", ],
                          header_y = header_y, header_cex = header_cex) # , sub = paste(c(final_stats$cr, "/", final_stats$cr + final_stats$fa), collapse = ""))


                # sens level: ----

                add_level("sens", ok_val = .75, min_val = 0, max_val = 1,
                          level_type = level.type, lloc_row = lloc[lloc$element == "sens", ],
                          header_y = header_y, header_cex = header_cex) # , sub = paste(c(final_stats$hi, "/", final_stats$hi + final_stats$mi), collapse = ""))


                # acc level: ----

                min_acc <- max(crit_br, 1 - crit_br)  # accuracy baseline

                add_level("acc", ok_val = .50, min_val = 0, max_val = 1,
                          level_type = level.type, lloc_row = lloc[lloc$element == "acc", ],
                          header_y = header_y, header_cex = header_cex) # , sub = paste(c(final_stats$hi + final_stats$cr, "/", final_stats$n), collapse = ""))

                # Add baseline to acc level:
                segments(
                  x0 = (lloc$center_x[lloc$element == "acc"] - lloc$width[lloc$element  == "acc"] / 2),
                  y0 = (lloc$center_y[lloc$element == "acc"] - lloc$height[lloc$element == "acc"] / 2) + (lloc$height[lloc$element == "acc"] * min_acc),
                  x1 = (lloc$center_x[lloc$element == "acc"] + lloc$width[lloc$element  == "acc"] / 2),
                  y1 = (lloc$center_y[lloc$element == "acc"] - lloc$height[lloc$element == "acc"] / 2) + (lloc$height[lloc$element == "acc"] * min_acc),
                  lty = 3
                )

                text(
                  x = lloc$center_x[lloc$element == "acc"],
                  y = (lloc$center_y[lloc$element == "acc"] - lloc$height[lloc$element == "acc"] / 2) + lloc$height[lloc$element == "acc"] * min_acc,
                  labels = "BL", pos = 1
                )

                # paste("BL = ", pretty_dec(min_acc), sep = ""), pos = 1)


                # bacc OR wacc level: ----

                if (names(bacc_wacc) == "bacc"){ # show bacc level:

                  add_level("bacc", ok_val = .50, min_val = 0, max_val = 1,
                            level_type = level.type, lloc_row = lloc[lloc$element == "bacc", ],
                            header_y = header_y, header_cex = header_cex)

                } else { # show wacc level (and sens.w value):

                  sens.w_lbl <- paste0("sens.w = .", pretty_dec(sens.w))

                  add_level("wacc", ok_val = .50, min_val = 0, max_val = 1,
                            level_type = level.type, lloc_row = lloc[lloc$element == "wacc", ],
                            header_y = header_y,
                            bottom_text = sens.w_lbl,  # (only here)
                            header_cex = header_cex)

                } # if (bacc_wacc).


                # Add baseline (at bottom?):
                #
                # segments(x0 = mean(lloc$center_x[2]),
                #          y0 = lloc$center_y[1] - lloc$height[1] / 2,
                #          x1 = mean(lloc$center_x[7]),
                #          y1 = lloc$center_y[1] - lloc$height[1] / 2, lend = 1,
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
                lloc$center_x[lloc$element == "roc"] <- .50
                lloc$center_y[lloc$element == "roc"] <- .55

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
              final_roc_x <- c(lloc$center_x[lloc$element == "roc"] - lloc$width[lloc$element  == "roc"] / 2, lloc$center_x[lloc$element == "roc"] + lloc$width[lloc$element  == "roc"] / 2)
              final_roc_y <- c(lloc$center_y[lloc$element == "roc"] - lloc$height[lloc$element == "roc"] / 2, lloc$center_y[lloc$element == "roc"] + lloc$height[lloc$element == "roc"] / 2)


              if (what == "roc"){ # ROC as main plot:

                # Title:
                title(main = roc_title, ...)  # + graphical parameters

                # Background:
                rect(final_roc_x[1], final_roc_y[1], final_roc_x[2], final_roc_y[2],
                     col = gray(.96))  # as in showcues()

                # Grid:
                x_ax_seq <- seq(final_roc_x[1], final_roc_x[2], length.out = 11)
                y_ax_seq <- seq(final_roc_y[1], final_roc_y[2], length.out = 11)
                abline(v = x_ax_seq, lwd = c(2, rep(1, 4)), col = gray(1)) # x-grid
                abline(h = y_ax_seq, lwd = c(2, rep(1, 4)), col = gray(1)) # y-grid

                # Axis ticks:
                segments(x_ax_seq, final_roc_y[1], x_ax_seq, (final_roc_y[1] - .025), lty = 1, lwd = 1, col = gray(.10)) # x-axis
                segments(final_roc_x[1], y_ax_seq, (final_roc_x[1] - .015), y_ax_seq, lty = 1, lwd = 1, col = gray(.10)) # y-axis

                # Tick labels:
                text(x_ax_seq, (final_roc_y[1] - .025), labels = scales::comma(seq(0, 1, by = .1), accuracy = .1), pos = 1, cex = .9) # x-lbl
                text((final_roc_x[1] - .015), y_ax_seq, labels = scales::comma(seq(0, 1, by = .1), accuracy = .1), pos = 2, cex = .9) # y-llb

                # Axis labels:
                text(mean(final_roc_x), final_roc_y[1] - .125, labels = x_lbl, cex = 1) # x-lab
                text(final_roc_x[1] - (3.5 * x_d), mean(final_roc_y), labels = y_lbl, cex = 1, srt = 90) # y-lab

                # Subtitle: Note data used
                subnote <- paste0("ROC for '", data, "' data:")
                text(x = (final_roc_x[1] - .015), y = (final_roc_y[2] + .03),
                     labels = subnote, pos = 4, cex = subheader_cex)


              } else { # ROC as miniature plot:

                # Title:
                text(lloc$center_x[lloc$element == "roc"], header_y, labels = roc_title,
                     font = roc_title_font, pos = 1, cex = header_cex)

                # x-axis:
                text(c(final_roc_x[1], final_roc_x[2]),
                     c(final_roc_y[1], final_roc_y[1]) - .04,
                     labels = c(0, 1)
                )

                text(mean(final_roc_x), final_roc_y[1] - .08, labels = x_lbl) # x-lab

                # y-axis:
                text(c(final_roc_x[1], final_roc_x[1], final_roc_x[1]) - x_d,
                     c(final_roc_y[1], mean(final_roc_y[1:2]), final_roc_y[2]),
                     labels = c(0, .5, 1)
                )

                text(final_roc_x[1] - (2.5 * x_d), mean(final_roc_y), labels = y_lbl, srt = 90) # y-lab

                # AUC label:
                # text(final.roc.center[1], subheader_y, paste("AUC =", round(final.auc, 2)), pos = 1)

                # Plot bg:
                #
                # rect(final_roc_x[1],
                #      final_roc_y[1],
                #      final_roc_x[2],
                #      final_roc_y[2],
                #      col = gray(1), lwd = .5)

                # Gridlines:
                # # Horizontal:
                #  segments(x0 = rep(final_roc_x[1], 9),
                #           y0 = seq(final_roc_y[1], final_roc_y[2], length.out = 5)[2:10],
                #           x1 = rep(final_roc_x[2], 9),
                #           y1 = seq(final_roc_y[1], final_roc_y[2], length.out = 5)[2:10],
                #           lty = 1, col = gray(.8), lwd = c(.5), lend = 3
                #           )
                #
                #  # Vertical:
                #  segments(y0 = rep(final_roc_y[1], 9),
                #           x0 = seq(final_roc_x[1], final_roc_x[2], length.out = 5)[2:10],
                #           y1 = rep(final_roc_y[2], 9),
                #           x1 = seq(final_roc_x[1], final_roc_x[2], length.out = 5)[2:10],
                #           lty = 1, col = gray(.8), lwd = c(.5), lend = 3
                #  )

              }

              # Plot border:
              rect(final_roc_x[1],
                   final_roc_y[1],
                   final_roc_x[2],
                   final_roc_y[2],
                   border = roc_border_col,
                   lwd = roc_border_lwd
              )

              # Diagonal:
              segments(final_roc_x[1],
                       final_roc_y[1],
                       final_roc_x[2],
                       final_roc_y[2],
                       col = diag_col,
                       lwd = 1,
                       lty = diag_lty
              )


              # COMPETITIVE ALGORITHMS: ------

              if (comp == TRUE) {

                # CART: ----

                if ("cart" %in% x$competition[[data]]$algorithm) {

                  cart_spec <- x$competition[[data]]$spec[x$competition[[data]]$algorithm == "cart"]
                  cart_sens <- x$competition[[data]]$sens[x$competition[[data]]$algorithm == "cart"]

                  # Plot point:
                  points(final_roc_x[1] + ((1 - cart_spec) * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (cart_sens * lloc$height[lloc$element == "roc"]),
                         pch = 21, cex = 1.75,
                         col = scales::alpha("red", .5),
                         bg = scales::alpha("red", .3), lwd = 1
                  )

                  points(final_roc_x[1] + ((1 - cart_spec) * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (cart_sens * lloc$height[lloc$element == "roc"]),
                         pch = "C", cex = .7, col = gray(.2), lwd = 1
                  )


                  # Legend label:
                  par("xpd" = TRUE)

                  points(final_roc_x[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (roc_lbl_y[4] * lloc$height[lloc$element == "roc"]),
                         pch = 21, cex = 2.5,
                         col = scales::alpha("red", .1),
                         bg = scales::alpha("red", .3)
                  )

                  points(final_roc_x[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (roc_lbl_y[4] * lloc$height[lloc$element == "roc"]),
                         pch = "C", cex = .9, col = gray(.2)
                  )

                  text(final_roc_x[1] + (1.13 * lloc$width[lloc$element == "roc"]),
                       final_roc_y[1] + (roc_lbl_y[4] * lloc$height[lloc$element == "roc"]),
                       labels = "  CART", adj = 0, cex = .9
                  )

                  par("xpd" = FALSE)

                } # if ("cart" etc.


                # LR: ----

                if ("lr" %in% x$competition[[data]]$algorithm) {

                  lr_spec <- x$competition[[data]]$spec[x$competition[[data]]$algorithm == "lr"]
                  lr_sens <- x$competition[[data]]$sens[x$competition[[data]]$algorithm == "lr"]

                  # Plot point:
                  points(final_roc_x[1] + ((1 - lr_spec) * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (lr_sens * lloc$height[lloc$element == "roc"]),
                         pch = 21, cex = 1.75,
                         col = scales::alpha("blue", .1),
                         bg = scales::alpha("blue", .2)
                  )

                  points(final_roc_x[1] + ((1 - lr_spec) * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (lr_sens * lloc$height[lloc$element == "roc"]),
                         pch = "L", cex = .7, col = gray(.2)
                  )

                  # Legend label:
                  par("xpd" = TRUE)

                  points(final_roc_x[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (roc_lbl_y[3] * lloc$height[lloc$element == "roc"]),
                         pch = 21, cex = 2.5,
                         col = scales::alpha("blue", .1),
                         bg = scales::alpha("blue", .2)
                  )

                  points(final_roc_x[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (roc_lbl_y[3] * lloc$height[lloc$element == "roc"]),
                         pch = "L", cex = .9, col = gray(.2)
                  )

                  text(final_roc_x[1] + (1.13 * lloc$width[lloc$element == "roc"]),
                       final_roc_y[1] + (roc_lbl_y[3] * lloc$height[lloc$element == "roc"]),
                       labels = "  LR", adj = 0, cex = .9
                  )

                  par("xpd" = FALSE)

                } # if ("lr" etc.


                # RF: ----

                if ("rf" %in% x$competition[[data]]$algorithm) {

                  rf_spec <- x$competition[[data]]$spec[x$competition[[data]]$algorithm == "rf"]
                  rf_sens <- x$competition[[data]]$sens[x$competition[[data]]$algorithm == "rf"]

                  # # 4debugging:
                  # print(paste0("RF: 1 - rf_spec = ", round(1 - rf_spec, 2),
                  #              ", rf_sens = ", round(rf_sens, 2),
                  #              ", data = ", data))  # RF coordinates
                  #
                  # print(final_roc_y[1] + (rf_sens * lloc$height[lloc$element == "roc"])) # y-coordinate

                  # Plot point:
                  points(final_roc_x[1] + ((1 - rf_spec) * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (rf_sens * lloc$height[lloc$element == "roc"]),
                         pch = 21, cex = 1.75,
                         col = scales::alpha("purple", .1),
                         bg = scales::alpha("purple", .3), lwd = 1
                  )

                  points(final_roc_x[1] + ((1 - rf_spec) * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (rf_sens * lloc$height[lloc$element == "roc"]),
                         pch = "R", cex = .7, col = gray(.2), lwd = 1
                  )

                  # Legend label:
                  par("xpd" = TRUE)

                  points(final_roc_x[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (roc_lbl_y[2] * lloc$height[lloc$element == "roc"]),
                         pch = 21, cex = 2.5,
                         col = scales::alpha("purple", .1),
                         bg = scales::alpha("purple", .3)
                  )

                  points(final_roc_x[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (roc_lbl_y[2] * lloc$height[lloc$element == "roc"]),
                         pch = "R", cex = .9, col = gray(.2)
                  )

                  text(final_roc_x[1] + (1.13 * lloc$width[lloc$element == "roc"]),
                       final_roc_y[1] + (roc_lbl_y[2] * lloc$height[lloc$element == "roc"]),
                       labels = "  RF", adj = 0, cex = .9
                  )

                  par("xpd" = FALSE)

                } # if ("rf" etc.


                # SVM: ----

                if ("svm" %in% x$competition[[data]]$algorithm) {

                  svm_spec <- x$competition[[data]]$spec[x$competition[[data]]$algorithm == "svm"]
                  svm_sens <- x$competition[[data]]$sens[x$competition[[data]]$algorithm == "svm"]

                  # Plot point:
                  points(final_roc_x[1] + (1 - svm_spec) * lloc$width[lloc$element == "roc"],
                         final_roc_y[1] + svm_sens * lloc$height[lloc$element == "roc"],
                         pch = 21, cex = 1.75,
                         col = scales::alpha("orange", .1),
                         bg  = scales::alpha("orange", .3), lwd = 1
                  )

                  points(final_roc_x[1] + (1 - svm_spec) * lloc$width[lloc$element == "roc"],
                         final_roc_y[1] + svm_sens * lloc$height[lloc$element == "roc"],
                         pch = "S", cex = .7, col = gray(.2), lwd = 1
                  )


                  # Legend label:
                  par("xpd" = TRUE)

                  points(final_roc_x[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (roc_lbl_y[1] * lloc$height[lloc$element == "roc"]),
                         pch = 21, cex = 2.5,
                         col = scales::alpha("orange", .1),
                         bg  = scales::alpha("orange", .3)
                  )

                  points(final_roc_x[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (roc_lbl_y[1] * lloc$height[lloc$element == "roc"]),
                         pch = "S", cex = .9, col = gray(.2)
                  )

                  text(final_roc_x[1] + (1.13 * lloc$width[lloc$element == "roc"]),
                       final_roc_y[1] + (roc_lbl_y[1] * lloc$height[lloc$element == "roc"]),
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

                  segments(final_roc_x[1] + c(0, 1 - fft_spec_vec_ord) * lloc$width[lloc$element == "roc"],
                           final_roc_y[1] + c(0, fft_sens_vec_ord) * lloc$height[lloc$element == "roc"],
                           final_roc_x[1] + c(1 - fft_spec_vec_ord, 1) * lloc$width[lloc$element == "roc"],
                           final_roc_y[1] + c(fft_sens_vec_ord, 1) * lloc$height[lloc$element == "roc"],
                           lwd = roc_curve_lwd,
                           col = roc_curve_col
                  )

                  points(final_roc_x[1] + ((1 - fft_spec_vec_ord[-(which(roc_order == tree))]) * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (fft_sens_vec_ord[-(which(roc_order == tree))] * lloc$height[lloc$element == "roc"]),
                         pch = 21, cex = 2.5, col = scales::alpha("green", .60),
                         bg = scales::alpha("white", .90)
                  )

                  text(final_roc_x[1] + ((1 - fft_spec_vec_ord[-(which(roc_order == tree))]) * lloc$width[lloc$element == "roc"]),
                       final_roc_y[1] + (fft_sens_vec_ord[-(which(roc_order == tree))] * lloc$height[lloc$element == "roc"]),
                       labels = roc_order[which(roc_order != tree)], cex = 1, col = gray(.20)
                  )

                }

                # Add larger point for plotted tree:

                # white point (to hide point from above):
                points(final_roc_x[1] + ((1 - fft_spec_vec[tree]) * lloc$width[lloc$element == "roc"]),
                       final_roc_y[1] + (fft_sens_vec[tree] * lloc$height[lloc$element == "roc"]),
                       pch = 21, cex = 3, col = gray(1), # col = scales::alpha("green", .30),
                       bg = scales::alpha("white", 1), lwd = 1
                )

                # green point:
                points(final_roc_x[1] + ((1 - fft_spec_vec[tree]) * lloc$width[lloc$element == "roc"]),
                       final_roc_y[1] + (fft_sens_vec[tree] * lloc$height[lloc$element == "roc"]),
                       pch = 21, cex = 3, col = gray(1), # col = scales::alpha("green", .30),
                       bg = scales::alpha("green", .30), lwd = 1
                )

                text(final_roc_x[1] + ((1 - fft_spec_vec[tree]) * lloc$width[lloc$element == "roc"]),
                     final_roc_y[1] + (fft_sens_vec[tree] * lloc$height[lloc$element == "roc"]),
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

                  points(final_roc_x[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (roc_lbl_y[5] * lloc$height[lloc$element == "roc"]),
                         pch = 21, cex = 2.5, col = scales::alpha("green", .3),
                         bg = scales::alpha("green", .67)
                  )

                  points(final_roc_x[1] + (1.10 * lloc$width[lloc$element == "roc"]),
                         final_roc_y[1] + (roc_lbl_y[5] * lloc$height[lloc$element == "roc"]),
                         pch = "#", cex = .9, col = gray(.20)
                  )

                  text(final_roc_x[1] + (1.13 * lloc$width[lloc$element == "roc"]),
                       final_roc_y[1] + (roc_lbl_y[5] * lloc$height[lloc$element == "roc"]),
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
