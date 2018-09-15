#' Prints summary information from an FFTrees object
#'
#' @description Printing function for an FFTrees object
#' @param x FFTrees. A FFTrees x created from FFTrees()
#' @param tree integer. The tree to explore.
#' @param ... additional arguments passed to print.
#' @export

print.FFTrees <- function(
  x = NULL,
  tree = NULL,
  ...
) {

goal <- x$params$goal
sens.w <- x$params$sens.w
criterion.name <- paste(x$formula)[2]

n.trees <- nrow(x$tree.stats$train)
n.cues.total <- x$data.desc$train$features

if(is.null(tree)) {

tree <- 1

} else {

  if(class(tree) != "numeric" || length(tree) != 1 || round(tree, 0) != tree) {

    stop("tree must be an integer")
  }
}


train.cues <- paste(unique(unlist(strsplit(x$tree.definitions$cues[tree], ";"))), collapse = ",")
train.cues.n <- length(unique(unlist(strsplit(train.cues, ","))))

all.cues <- paste(unique(unlist(strsplit(x$tree.definitions$cues, ";"))), collapse = ",")
all.cues.n <- length(unique(unlist(strsplit(x$tree.definitions$cues, ";"))))

n.cues <- x$tree.definitions$nodes[tree]

if(n.trees == 1) {summary.text <- paste(x$params$algorithm, " FFT predicting ", criterion.name, " with up to ", n.cues, " nodes", sep = "")}
if(n.trees > 1) {summary.text <- paste(n.trees, " FFTs predicting ", criterion.name, " (", x$params$decision.labels[1], " v ", x$params$decision.labels[2], ")", sep = "")}

params.text <- paste0("pars: algorithm = '", x$params$algorithm, "', goal = '", x$params$goal, "', goal.chase = '", x$params$goal.chase, "', sens.w = ", x$params$sens.w, ", max.levels = ", x$params$max.levels)

accuracy.text <- paste("FFT ", tree, " (of ", n.trees, ") predicts ", criterion.name," using ", train.cues.n, " cues: {", paste(unlist(strsplit(train.cues, ",")), collapse = ", "), "}", sep = "")

# Confusion table


if(is.null(x$params$main) == FALSE) {

cat(x$params$main)
cat("\n")
}

cat(accuracy.text)
cat("\n")

cat("\n")
sapply(1:length(inwords(x = x, tree = tree)), FUN = function(i) {cat(paste0("[", i, "] ", inwords(x, tree, version = 1)[i], ".\n"))})
cat("\n")


x_summary <- summary(x, tree)
rownames(x_summary) <- paste0(c("cases       .",
                               "hits        .",
                               "misses      .",
                               "false al    .",
                               "corr rej    .",
                               "speed       .",
                               "frugality   .",
                               "cost        .",
                               "accuracy    .",
                               "balanced    .",
                               "sensitivity .",
                               "specificity ."), rownames(x_summary))

if(all(is.na(x_summary$test))) {
  x_summary$test <- rep("--", nrow(x_summary))
  x_summary$train <- round(x_summary$train, 3)

} else {

  if(class(x_summary$train) == "numeric") {x_summary$train <- round(x_summary$train, 2)}
  if(class(x_summary$test) == "numeric") {x_summary$test <- round(x_summary$test, 2)}

}


print(x_summary)


cat("\n")

cat(params.text)


}
