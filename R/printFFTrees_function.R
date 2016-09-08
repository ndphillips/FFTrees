#' Prints summary information from an FFTrees object
#'
#' @description Printing function for an FFTrees object
#' @param x A FFTrees object created from FFTrees()
#' @param ... additional arguments passed to print.
#' @export

print.FFTrees <- function(
  x = NULL,
  ...
) {

cues.used <- paste(unique(unlist(strsplit(x$tree.stats$train$cues, ";"))), collapse = ",")
n.cues.used <- length(unique(unlist(strsplit(x$tree.stats$train$cues, ";"))))
n.cues.total <- ncol(x$data$train) - 1
n.train.ex <- nrow(x$data$train)
n.trees <- nrow(x$tree.stats$train)

best.train.tree <- min(x$tree.stats$train$tree[x$tree.stats$train$v == max(x$tree.stats$train$v)])
best.train.hr <- round(x$tree.stats$train$hr[best.train.tree], 2)
best.train.far <- round(x$tree.stats$train$far[best.train.tree], 2)
best.train.dp <- round(x$tree.stats$train$dprime[best.train.tree], 2)
train.auc <- round(x$auc$FFTrees[1], 2)
train.pcorrect <- round((x$tree.stats$train$hi[best.train.tree] + x$tree.stats$train$cr[best.train.tree]) / x$tree.stats$train$n[best.train.tree], 2)

if(is.null(x$tree.stats$test) == F) {

n.test.ex <- nrow(x$data$test)
best.test.hr <- round(x$tree.stats$test$hr[best.train.tree], 2)
best.test.far <- round(x$tree.stats$test$far[best.train.tree], 2)
best.test.dp <- round(x$tree.stats$test$dprime[best.train.tree], 2)
test.auc <- round(x$auc$FFTrees[2], 2)
test.pcorrect <- round((x$tree.stats$test$hi[best.train.tree] + x$tree.stats$test$cr[best.train.tree]) / x$tree.stats$test$n[best.train.tree], 2)

summary.df <- data.frame("train" = c(n.train.ex,
                                     train.pcorrect,
                                     best.train.hr,
                                     best.train.far,
                                     best.train.dp),
                         "test" = c(n.test.ex,
                                    test.pcorrect,
                                    best.test.hr,
                                    best.test.far,
                                    best.test.dp)
)

rownames(summary.df) <- c("n", "p(Correct)", "Hit Rate (HR)", "False Alarm Rate (FAR)", "d-prime")

}

if(is.null(x$tree.stats$test)) {

  n.test.ex <- 0
  best.test.hr <- "--"
  best.test.far <- "--"
  best.test.dp <- "--"
  test.auc <- "--"
  test.pcorrect <- "--"

  summary.df <- data.frame("train" = c(n.train.ex,
                                       train.pcorrect,
                                       best.train.hr,
                                       best.train.far,
                                       best.train.dp)
  )

  rownames(summary.df) <- c("n", "p(Correct)", "Hit Rate (HR)", "False Alarm Rate (FAR)", "d-prime")

}



summary.text <- paste("An FFTrees object containing ", n.trees, " trees using ", n.cues.used,
                      " predictors {", cues.used, "}", sep = "")

if(is.null(test.auc)) {

auc.text <- paste("FFTrees AUC: (Train = ", train.auc, ")", sep = "")

}

if(is.null(test.auc) == F) {

  auc.text <- paste("FFTrees AUC: (Train = ", train.auc, ", Test = ", test.auc, ")", sep = "")

}

accuracy.text <- paste("My favorite training tree is #", best.train.tree, ", here is how it performed:", sep = "")

print(summary.text)
print(auc.text)
print(accuracy.text)
print(summary.df)

}
