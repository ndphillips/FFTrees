#' Prints summary information from an FFTrees object
#'
#' @description Printing function for an FFTrees object
#' @param x FFTrees. A FFTrees x created from FFTrees()
#' @param tree integer. The tree to explore.
#' @param ... additional arguments passed to print.
#' @export

print.FFTrees <- function(x = NULL,
                          tree = NULL,
                          ...) {
  if (is.null(tree)) {
    tree <- x$trees$best$train
  } else {
    if (class(tree) != "numeric" || length(tree) != 1 || round(tree, 0) != tree) {
      stop("tree must be an integer")
    }
  }


  train.cues <- paste(unique(unlist(strsplit(x$trees$definitions$cues[tree], ";"))), collapse = ",")
  train.cues.n <- length(unique(unlist(strsplit(train.cues, ","))))

  all.cues <- paste(unique(unlist(strsplit(x$trees$definitions$cues, ";"))), collapse = ",")
  all.cues.n <- length(unique(unlist(strsplit(x$trees$definitions$cues, ";"))))

  n.cues <- x$trees$definitions$nodes[tree]

  if (x$trees$n == 1) {
    summary.text <- paste(x$params$algorithm, " FFT predicting ", x$metadata$criterion_name, " with up to ", n.cues, " nodes", sep = "")
  }
  if (x$trees$n > 1) {
    summary.text <- paste(x$trees$n, " FFTs predicting ", x$metadata$criterion_name, " (", x$metadata$decision.labels[1], " v ", x$metadata$decision.labels[2], ")", sep = "")
  }

  params.text <- paste0("pars: algorithm = '", x$params$algorithm, "', goal = '", x$params$goal, "', goal.chase = '", x$params$goal.chase, "', x$params$sens.w = ", x$params$x$params$sens.w, ", max.levels = ", x$params$max.levels)


  # Confusion table

  if (is.null(x$params$main) == FALSE) {
    cat(x$params$main)
    cat("\n")
  }

  cat(crayon::blue("FFTrees ")) # , rep("-", times = 50 - nchar("FFTrees")), "\n", sep = "")
  cat("\n")
  cat("- Trees: ", x$trees$n, " fast-and-frugal trees predicting ",
    crayon::underline(x$metadata$criterion_name), "\n",
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

  cat(crayon::blue("FFT #", tree, ": Definition", sep = ""), sep = "")



  cat("\n")

  for (i in 1:length(x$trees$inwords[[tree]])) {
    cat(paste0("[", i, "] ", x$trees$inwords[[tree]][i], ".\n"))
  }
  cat("\n")




  if (is.null(x$trees$results$test$stats)) {
    mydata <- "train"

    hi <- x$trees$results$train$stats$hi[tree]
    mi <- x$trees$results$train$stats$mi[tree]
    fa <- x$trees$results$train$stats$fa[tree]
    cr <- x$trees$results$train$stats$cr[tree]
    title <- "Training"
    N <- nrow(x$data$train)
    cost <- x$trees$results$train$stats$cost[tree]
  } else {
    mydata <- "test"

    hi <- x$trees$results$test$stats$hi[tree]
    mi <- x$trees$results$test$stats$mi[tree]
    fa <- x$trees$results$test$stats$fa[tree]
    cr <- x$trees$results$test$stats$cr[tree]
    title <- "Testing"
    N <- nrow(x$data$test)
    cost <- x$trees$results$test$stats$cost[tree]
  }


  cat(crayon::blue("FFT #", tree, ": ", crayon::underline(title), " Accuracy\n", sep = ""), sep = "")


  cat(title, " Data: N = ", scales::comma(N), ", ",
    "Pos (+) = ", scales::comma(hi + mi), " (", scales::percent((hi + mi) / (hi + mi + fa + cr)), ") ",
    # "- ", scales::comma(cr + fa), " (", scales::percent((cr + fa) / (hi + mi + fa + cr)),")",

    "\n\n",
    sep = ""
  )

  FFTrees:::console_confusionmatrix(
    hi = hi,
    mi = mi,
    fa = fa,
    cr = cr,
    cost = cost
  )
}
