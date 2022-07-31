#' Print summary information of fast-and-frugal trees (FFTs)
#'
#' @description \code{print.FFTrees} provides summary information on FFTs from an \code{FFTrees} object.
#'
#' By default, \code{print.FFTrees} prints the performance characteristics for prediction (i.e., for predicting test data, rather than for fitting training data).
#' When no statistics for prediction performance (on test data) are available or when explicitly asking for fitting performance, use \code{data = "train"}.
#'
#' @param x An \code{FFTrees} object created by \code{\link{FFTrees}}.
#' @param tree The tree to explore (as integer).
#' @param data character. Must be either \code{'test'} (i.e., report prediction performance by default) or \code{'train'} (fitting performance).
#' When no statistics for prediction performance (on test data) is available in \code{x}, performance for fitting training data is reported.
#'
#' @param ... additional arguments passed to \code{print}.
#'
#' @return Prints summary information about an FFT to the console.
#'
#' @seealso
#' \code{\link{plot.FFTrees}} for plotting FFTs;
#' \code{\link{inwords}} for obtaining a verbal description of FFTs;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export

print.FFTrees <- function(x = NULL,
                          tree = 1,
                          data = "test",  # "test" or "train"
                          ...) {

  # Prepare: ------

  # - Get cue info: ----

  train.cues   <- paste(unique(unlist(strsplit(x$trees$definitions$cues[tree], ";"))), collapse = ",")
  train.cues.n <- length(unique(unlist(strsplit(train.cues, ","))))

  all.cues   <- paste(unique(unlist(strsplit(x$trees$definitions$cues, ";"))), collapse = ",")
  all.cues.n <- length(unique(unlist(strsplit(x$trees$definitions$cues, ";"))))

  n.cues <- x$trees$definitions$nodes[tree]

  # - Validate data argument: ----

  data <- tolower(data)  # for robustness

  if (!data %in% c("test", "train")){ # unknown value:

    stop("The data to print must be 'test' or 'train'.")
  }

  if (data == "test" & is.null(x$trees$stats$test)){ # use "train" data:

    message("No 'test' data available. I'll print the best training tree instead...")
    data <- "train"
  }


  # Introductory text: ------

  if (x$trees$n == 1) {
    summary.text <- paste(x$params$algorithm, " FFT predicting ", x$criterion_name, " with up to ", n.cues, " nodes", sep = "")
  }

  if (x$trees$n > 1) {
    summary.text <- paste(x$trees$n, " FFTs predicting ", x$criterion_name, " (", x$params$decision.labels[1], " v ", x$params$decision.labels[2], ")", sep = "")
  }

  params.text <- paste0("pars: algorithm = '", x$params$algorithm, "', goal = '", x$params$goal, "', goal.chase = '", x$params$goal.chase, "', x$params$sens.w = ", x$params$x$params$sens.w, ", max.levels = ", x$params$max.levels)


  # General info on FFTrees object x: ------

  if (is.null(x$params$main) == FALSE) {

    cat(x$params$main)  # object title

    cat("\n")
  }

  cat(crayon::blue("FFTrees ")) # , rep("-", times = 50 - nchar("FFTrees")), "\n", sep = "")

  cat("\n")

  cat("- Trees: ", x$trees$n, " fast-and-frugal trees predicting ",
      crayon::underline(x$criterion_name), "\n",
      sep = ""
  )

  cat("- Outcome costs: [hi = ", x$params$cost.outcomes$hi, ", mi = ", x$params$cost.outcomes$mi,
      ", fa = ", x$params$cost.outcomes$fa, ", cr = ", x$params$cost.outcomes$cr, "]\n",
      sep = ""
  )

  out <- x$params$cost.cues[unlist(strsplit(train.cues, ","))]

  # cat("- Cue costs: [", paste(names(out), "=", out, collapse = ", "), ", ...]\n", sep = "")
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

  for (i in 1:length(x$trees$inwords[[tree]])) { # for each sentence:

    cat(paste0("[", i, "] ", x$trees$inwords[[tree]][i], "\n"))

  }

  cat("\n")


  # Get parameter values: ------

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

    cost = cost

  )

  cat("\n")


  # Speed, frugality, and cost: ------

  cat(crayon::blue("FFT #", tree, ": ", crayon::underline(task), " Speed, Frugality, and Cost\n", sep = ""), sep = "")

  cat("mcu = ", round(x$trees$stats[[mydata]]$mcu[tree], 2), sep = "")
  cat(",  pci = ", round(x$trees$stats[[mydata]]$pci[tree], 2), sep = "")
  cat(",  E(cost) = ", scales::comma(cost, accuracy = .001), sep = "")

  cat("\n\n")


  # Output: none. ------

} # print.FFTrees().


# ToDo: ------

# - etc.

# eof.
