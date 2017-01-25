#' Prints summary information from an FFTrees object
#'
#' @description Printing function for an FFTrees object
#' @param x FFTrees. A FFTrees object created from FFTrees()
#' @param ... additional arguments passed to print.
#' @export

print.FFTrees <- function(
  x = NULL,
  ...
) {

n.trees <- nrow(x$tree.stats$train)
n.cues.total <- x$data.desc$train$features
n.train.ex <- x$data.desc$train$cases

best.train.tree <- min(x$tree.stats$train$tree[x$tree.stats$train$v == max(x$tree.stats$train$v)])

best.train.cues <- paste(unique(unlist(strsplit(x$tree.stats$train$cues[best.train.tree], ";"))), collapse = ",")
best.train.cues.n <- length(unique(unlist(strsplit(best.train.cues, ","))))

#
all.cues <- paste(unique(unlist(strsplit(x$tree.stats$train$cues, ";"))), collapse = ",")
all.cues.n <- length(unique(unlist(strsplit(x$tree.stats$train$cues, ";"))))


best.train.hr <- round(x$tree.stats$train$hr[best.train.tree], 2)
best.train.far <- round(x$tree.stats$train$far[best.train.tree], 2)
best.train.spec <- 1 - round(x$tree.stats$train$far[best.train.tree], 2)
best.train.dp <- round(x$tree.stats$train$dprime[best.train.tree], 2)
best.train.v <- round(x$tree.stats$train$hr[best.train.tree] - x$tree.stats$train$far[best.train.tree], 2)
best.train.frugality <- round(x$tree.stats$train$frugality[best.train.tree], 2)

train.auc <- round(x$auc$FFTrees[1], 2)
train.pcorrect <- round((x$tree.stats$train$hi[best.train.tree] + x$tree.stats$train$cr[best.train.tree]) / x$tree.stats$train$n[best.train.tree], 2)

if(is.null(x$tree.stats$test) == FALSE) {

n.test.ex <- x$data.desc$test$cases
best.test.hr <- round(x$tree.stats$test$hr[best.train.tree], 2)
best.test.far <- round(x$tree.stats$test$far[best.train.tree], 2)
best.test.spec <- 1 - round(x$tree.stats$test$far[best.train.tree], 2)
best.test.dp <- round(x$tree.stats$test$dprime[best.train.tree], 2)
best.test.frugality <- round(x$tree.stats$test$frugality[best.train.tree], 2)


test.auc <- round(x$auc$FFTrees[2], 2)
test.pcorrect <- round((x$tree.stats$test$hi[best.train.tree] + x$tree.stats$test$cr[best.train.tree]) / x$tree.stats$test$n[best.train.tree], 2)

summary.df <- data.frame("train" = c(n.train.ex,
                                     best.train.frugality,
                                     train.pcorrect,
                                     best.train.dp,
                                     best.train.hr,
                                     best.train.spec),
                         "test" = c(n.test.ex,
                                    best.test.frugality,
                                    test.pcorrect,
                                    best.test.dp,
                                    best.test.hr,
                                    best.test.spec)
)

}

if(is.null(x$tree.stats$test)) {

  n.test.ex <- 0
  best.test.frugality <- "--"

  best.test.hr <- "--"
  best.test.far <- "--"
  best.test.dp <- "--"
  best.test.spec <- "--"
  test.auc <- "--"
  test.pcorrect <- "--"

  summary.df <- data.frame("train" = c(n.train.ex,
                                       best.train.frugality,
                                       train.pcorrect,
                                       best.train.dp,
                                       best.train.hr,
                                       best.train.spec)
  )


}

rownames(summary.df) <- c("n", "frugality", "correct", "d-prime", "sens", "spec")


summary.text <- paste("FFTrees object containing ", n.trees, " trees using up to ", all.cues.n,
                      " predictors of an original ", n.cues.total, sep = "")

if(is.null(test.auc)) {

auc.text <- paste("FFTrees AUC: (Train = ", train.auc, ")", sep = "")

}

if(is.null(test.auc) == F) {

  auc.text <- paste("FFTrees AUC: (Train = ", train.auc, ", Test = ", test.auc, ")", sep = "")

}

accuracy.text <- paste("Best training tree: #", best.train.tree, ", using ", best.train.cues.n, " cues {", best.train.cues, "}:", sep = "")

print(summary.text)
#print(auc.text)
print(accuracy.text)
print(summary.df)

}
