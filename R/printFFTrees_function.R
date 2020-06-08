#' Prints summary information from an FFTrees object
#'
#' @description Printing function for an FFTrees object
#' @param x FFTrees. A FFTrees x created from FFTrees()
#' @param tree integer. The tree to explore.
#' @param ... additional arguments passed to print.
#' @export

print.FFTrees <- function(
  x = NULL,
  tree = 1,
  ...
) {


train.cues <- paste(unique(unlist(strsplit(x$trees$definitions$cues[tree], ";"))), collapse = ",")
train.cues.n <- length(unique(unlist(strsplit(train.cues, ","))))

all.cues <- paste(unique(unlist(strsplit(x$trees$definitions$cues, ";"))), collapse = ",")
all.cues.n <- length(unique(unlist(strsplit(x$trees$definitions$cues, ";"))))

n.cues <- x$trees$definitions$nodes[tree]

if(x$trees$n == 1) {summary.text <- paste(x$params$algorithm, " FFT predicting ", x$criterion_name, " with up to ", n.cues, " nodes", sep = "")}
if(x$trees$n > 1) {summary.text <- paste(x$trees$n, " FFTs predicting ", x$criterion_name, " (", x$params$decision.labels[1], " v ", x$params$decision.labels[2], ")", sep = "")}

params.text <- paste0("pars: algorithm = '", x$params$algorithm, "', goal = '", x$params$goal, "', goal.chase = '", x$params$goal.chase, "', x$params$sens.w = ", x$params$x$params$sens.w, ", max.levels = ", x$params$max.levels)


# Confusion table

if(is.null(x$params$main) == FALSE) {

  cat(x$params$main)
  cat("\n")

}

cat(crayon::blue("FFTrees ")) #, rep("-", times = 50 - nchar("FFTrees")), "\n", sep = "")
cat('\n')
cat("- Trees: ", x$trees$n, " fast-and-frugal trees predicting ",
    crayon::underline(x$criterion_name), "\n", sep = "")
cat("- Outcome costs: [hi = ", x$params$cost.outcomes$hi, ", mi = ", x$params$cost.outcomes$mi,
    ", fa = ", x$params$cost.outcomes$fa, ", cr = ", x$params$cost.outcomes$cr, "]\n", sep = "")

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

for(i in 1:length(x$trees$inwords[[tree]])) {

  cat(paste0("[", i, "] ", x$trees$inwords[[tree]][i], ".\n"))

}
cat("\n")




if(is.null(x$trees$stats$test)) {

  mydata <- "train"

  hi = x$trees$stats$train$hi[tree]
  mi = x$trees$stats$train$mi[tree]
  fa = x$trees$stats$train$fa[tree]
  cr = x$trees$stats$train$cr[tree]
  title <- "Training"
  N <- nrow(x$data$train)
  cost <- x$trees$stats$train$cost[tree]

  } else {

    mydata <- "test"

  hi = x$trees$stats$test$hi[tree]
  mi = x$trees$stats$test$mi[tree]
  fa = x$trees$stats$test$fa[tree]
  cr = x$trees$stats$test$cr[tree]
  title <- "Prediction"
  N <- nrow(x$data$test)
  cost <- x$trees$stats$test$cost[tree]

  }


cat(crayon::blue("FFT #", tree, ": ", crayon::underline(title), " Accuracy\n", sep = ""), sep = "")

cat(title, " Data: N = ", scales::comma(N), ", ",
    "Pos (+) = ", scales::comma(hi + mi), " (", scales::percent((hi + mi) / (hi + mi + fa + cr)),") ",
    # "- ", scales::comma(cr + fa), " (", scales::percent((cr + fa) / (hi + mi + fa + cr)),")",

    "\n\n", sep = "")

console_confusionmatrix(hi = hi,
                        mi = mi,
                        fa = fa,
                        cr = cr,
                        cost = cost)

cat("\n\n")
cat(crayon::blue("FFT #", tree, ": ", crayon::underline(title), " Speed and Frugality\n", sep = ""), sep = "")

cat("mcu = ", round(x$trees$stats[[mydata]]$mcu[tree], 2), sep = "")
cat(", pci = ", round(x$trees$stats[[mydata]]$pci[tree], 2), sep = "")

}

