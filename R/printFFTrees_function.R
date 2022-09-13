#' Print basic information of fast-and-frugal trees (FFTs)
#'
#' @description \code{print.FFTrees} prints basic information on FFTs for an \code{FFTrees} object \code{x}.
#'
#' As \code{x} may not contain test data, \code{print.FFTrees} by default prints the performance characteristics for training data (i.e., fitting), rather than for test data (i.e., for prediction).
#' When test data is available, specify \code{data = "test"} to print prediction performance.
#'
#' @param x An \code{FFTrees} object created by \code{\link{FFTrees}}.
#'
#' @param tree The tree to be printed (as an integer, only valid when the corresponding tree argument is non-empty).
#' Default: \code{tree = 1}.
#' To print the best training or best test tree with respect to the \code{goal} specified during FFT construction,
#' use \code{"best.train"} or \code{"best.test"}, respectively.
#'
#' @param data The type of data in \code{x} to be printed (as a string) or a test dataset (as a data frame).
#' \itemize{
#'   \item{A valid data string must be either \code{'train'} (for fitting performance) or \code{'test'} (for prediction performance).}
#'   \item{For a valid data frame, the specified tree is evaluated and printed for this data (as 'test' data),
#'   but the global \code{FFTrees} object \code{x} remains unchanged unless it is re-assigned.}
#'  }
#' By default, \code{data = 'train'} (as \code{x} may not contain test data).
#'
#' @param ... additional arguments passed to \code{print}.
#'
#' @return An invisible \code{FFTrees} object \code{x}
#' and summary information on an FFT printed to the console (as side effect).
#'
#' @seealso
#' \code{\link{plot.FFTrees}} for plotting FFTs;
#' \code{\link{summary.FFTrees}} for summarizing FFTs;
#' \code{\link{inwords}} for obtaining a verbal description of FFTs;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export

print.FFTrees <- function(x = NULL,
                          tree = 1,
                          data = "train",
                          ...) {

  # Prepare: ------

  # - Get cue info: ----

  train.cues   <- paste(unique(unlist(strsplit(x$trees$definitions$cues[tree], ";"))), collapse = ",")
  train.cues.n <- length(unique(unlist(strsplit(train.cues, ","))))

  all.cues   <- paste(unique(unlist(strsplit(x$trees$definitions$cues, ";"))), collapse = ",")
  all.cues.n <- length(unique(unlist(strsplit(x$trees$definitions$cues, ";"))))

  n.cues <- x$trees$definitions$nodes[tree]


  # - Validate arguments: ----

  # data: ----

  # Note: data can be either a string "train"/"test"
  #       OR an entire data frame (of new test data):

  if (inherits(data, "character")) {

    data <- tolower(data)  # increase robustness

    # testthat::expect_true(data %in% c("train", "test"))
    if (!data %in% c("test", "train")){
      stop("The data to print must be 'test' or 'train'.")
    }
  }


  if (inherits(data, "data.frame")) {

    message("Applying FFTrees object x to new test data...")

    x <- fftrees_apply(x, mydata = "test", newdata = data)

    message("Success, but re-assign output to x or use fftrees_apply() to globally change x")

    data <- "test" # in rest of this function

  }


  if (data == "test" & is.null(x$trees$stats$test)){ # use "train" data:

    warning("You asked to print 'test' data, but there were no test data. Printed 'train' data instead...")

    data <- "train"
  }


  # tree: ----

  if (is.numeric(tree) & (tree %in% 1:x$trees$n) == FALSE) {
    stop(paste("You asked for a tree that does not exist. This object has", x$trees$n, "trees."))
  }

  if (tree == "best.test" & is.null(x$tree$stats$test)) {
    warning("You asked to print the 'best.test' tree, but there were no test data. Printed the best tree for 'train' data instead...")

    tree <- "best.train"
  }


  # Determine "best" tree: ------

  if (tree == "best.train") {

    if (data == "test"){
      warning("You asked to print the 'best.train' tree, but data was set to 'test'. Printed the best tree for 'train' data instead...")
      data <- "train"
      main <- "Data (Training)"
    }

    # tree <- x$trees$best$train  # using current x
    tree <- select_best_tree(x, data = "train", goal = x$params$goal)  # using helper
  }

  if (tree == "best.test") {

    if (data == "train"){
      warning("You asked to print the best test tree, but data was set to 'train'. I'll use 'test' data instead...")
      data <- "test"
      main <- "Data (Testing)"
    }

    # tree <- x$trees$best$test  # using current x
    tree <- select_best_tree(x, data = "test", goal = x$params$goal)  # using helper
  }


  # Introductory text: ------

  if ((abs(x$trees$n) - 1) < .001) { # n = (approx.) 1:
    # summary_text_1 <- paste(x$trees$n, " FFT predicting ", x$criterion_name, " with up to ", n.cues, " nodes", sep = "")
    tree_s <- "tree"
  }

  if (x$trees$n > 1) {
    # summary_text_2 <- paste(x$trees$n, " FFTs predicting ", x$criterion_name, " (", x$params$decision.labels[1], " v ", x$params$decision.labels[2], ")", sep = "")
    tree_s <- "trees"
  }

  # Algorithm, goals, etc. (used only in summary.FFTrees()):
  # params_text <- paste0("pars: algorithm = '", x$params$algorithm, "', goal = '", x$params$goal, "', goal.chase = '", x$params$goal.chase, "', x$params$sens.w = ", x$params$x$params$sens.w, ", max.levels = ", x$params$max.levels)


  # General info on FFTrees object x: ------

  if (is.null(x$params$main) == FALSE) {

    cat(x$params$main)  # object title
    cat("\n")
  }


  # FFTrees: ----

  cat(crayon::blue("FFTrees ")) # , rep("-", times = 50 - nchar("FFTrees")), "\n", sep = "")
  cat("\n")

  # Trees: ----

  cat("- Trees: ", x$trees$n, " fast-and-frugal ", tree_s, " predicting ",
      crayon::underline(x$criterion_name), "\n",
      sep = ""
  )

  # Outcome costs: ----

  cat("- Outcome costs: [hi = ", x$params$cost.outcomes$hi, ", mi = ", x$params$cost.outcomes$mi,
      ", fa = ", x$params$cost.outcomes$fa, ", cr = ", x$params$cost.outcomes$cr, "]\n",
      sep = ""
  )


  # Cue costs: ----
  #
  # out <- x$params$cost.cues[unlist(strsplit(train.cues, ","))]
  # cat("- Cue costs: [", paste(names(out), "=", out, collapse = ", "), ", ...]\n", sep = "")


  # Parameters of best.train tree: ----
  #
  # if(tree == x$trees$best$train) {
  #
  #   cat(paste("- FFT ", crayon::underline("#", x$trees$best$train, sep = ""), " optimises ", crayon::underline(x$params$goal), " using ", train.cues.n, " cues: {",
  #             crayon::underline(paste(unlist(strsplit(train.cues, ",")), collapse = ", ")), "}", sep = ""))
  #
  #   cat("\n")
  #
  # }


  cat("\n")


  # FFT description: ------

  cat(crayon::blue("FFT #", tree, ": Definition", sep = ""), sep = "")
  cat("\n")

  # FFT in words:

  tree_in_words <- inwords(x, data = data, tree = tree)

  for (i in 1:length(tree_in_words)) { # for each sentence:

    cat(paste0("[", i, "] ", tree_in_words[i], "\n"))

  }

  cat("\n")


  # Get parameter values: ------

  sens.w <- x$params$sens.w  # for computing wacc

  if (data == "train") { # (a) use stats of training data:

    task   <- "Training"
    mydata <- "train"

    hi <- x$trees$stats$train$hi[tree]
    mi <- x$trees$stats$train$mi[tree]
    fa <- x$trees$stats$train$fa[tree]
    cr <- x$trees$stats$train$cr[tree]

    N <- nrow(x$data$train)
    cost <- x$trees$stats$train$cost[tree]

  } else { # else (data == "test"): use stats of test/prediction data (by default):

    task   <- "Prediction"
    mydata <- "test"

    hi <- x$trees$stats$test$hi[tree]
    mi <- x$trees$stats$test$mi[tree]
    fa <- x$trees$stats$test$fa[tree]
    cr <- x$trees$stats$test$cr[tree]

    N <- nrow(x$data$test)
    cost <- x$trees$stats$test$cost[tree]

  }


  # Accuracy information: ------

  cat(crayon::blue("FFT #", tree, ": ", crayon::underline(task), " Accuracy\n", sep = ""), sep = "")

  # - Data info: ----

  cat(task, " data: N = ", scales::comma(N), ", ",

      # Prevalence (positive criterion values / True +):
      "Pos (+) = ", scales::comma(hi + mi), " (", scales::percent((hi + mi) / N), ") ",
      # "- ", scales::comma(cr + fa), " (", scales::percent((cr + fa) / N,")",

      # ", ",

      # Bias (positive decisions / Decisions +:
      # "Dec (+) = ", scales::comma(hi + fa), " (", scales::percent((hi + fa) / N), ") ",

      sep = ""
  )

  cat("\n\n")


  # - Confusion table: ----

  console_confusionmatrix( # See utility function in helper.R:

    hi = hi,
    mi = mi,
    fa = fa,
    cr = cr,

    sens.w = sens.w,

    cost = cost

  )

  cat("\n")


  # Speed, frugality, and cost: ------

  cat(crayon::blue("FFT #", tree, ": ", crayon::underline(task), " Speed, Frugality, and Cost\n", sep = ""), sep = "")

  cat("mcu = ", round(x$trees$stats[[mydata]]$mcu[tree], 2), sep = "")
  cat(",  pci = ", round(x$trees$stats[[mydata]]$pci[tree], 2), sep = "")
  cat(",  E(cost) = ", scales::comma(cost, accuracy = .001), sep = "")

  cat("\n\n")


  # Output: ------

  # Output x may differ from input x when applying new 'test' data (as df):
  return(invisible(x))

} # print.FFTrees().


# ToDo: ------

# - etc.

# eof.
