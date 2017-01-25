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

goal <- x$params$goal

n.trees <- nrow(x$tree.stats$train)
n.cues.total <- x$data.desc$train$features
n.train.ex <- x$data.desc$train$cases

train.tree <- min(x$tree.stats$train$tree[x$tree.stats$train[[goal]] == max(x$tree.stats$train[[goal]])])

train.cues <- paste(unique(unlist(strsplit(x$tree.stats$train$cues[train.tree], ";"))), collapse = ",")
train.cues.n <- length(unique(unlist(strsplit(train.cues, ","))))

#
all.cues <- paste(unique(unlist(strsplit(x$tree.stats$train$cues, ";"))), collapse = ",")
all.cues.n <- length(unique(unlist(strsplit(x$tree.stats$train$cues, ";"))))


train.sens <- round(x$tree.stats$train$sens[train.tree], 2)
train.far <- round(x$tree.stats$train$far[train.tree], 2)
train.spec <- 1 - round(x$tree.stats$train$far[train.tree], 2)
train.dp <- round(x$tree.stats$train$dprime[train.tree], 2)
train.bacc <- round(x$tree.stats$train$bacc[train.tree], 2)
train.frugality <- round(x$tree.stats$train$frugality[train.tree], 2)
train.mcpc <- round(mean(x$levelout$train[,train.tree]), 2)


train.auc <- round(x$auc$FFTrees[1], 2)
train.acc <- round((x$tree.stats$train$hi[train.tree] + x$tree.stats$train$cr[train.tree]) / x$tree.stats$train$n[train.tree], 2)

if(is.null(x$tree.stats$test) == FALSE) {

n.test.ex <- x$data.desc$test$cases
test.sens <- round(x$tree.stats$test$sens[train.tree], 2)
test.far <- round(x$tree.stats$test$far[train.tree], 2)
test.spec <- 1 - round(x$tree.stats$test$far[train.tree], 2)
test.bacc <- round(x$tree.stats$test$bacc[train.tree], 2)
test.frugality <- round(x$tree.stats$test$frugality[train.tree], 2)
test.mcpc <- round(mean(x$levelout$test[,train.tree]), 2)


test.auc <- round(x$auc$FFTrees[2], 2)
test.acc <- round((x$tree.stats$test$hi[train.tree] + x$tree.stats$test$cr[train.tree]) / x$tree.stats$test$n[train.tree], 2)

summary.df <- data.frame("train" = c(n.train.ex,
                                     train.frugality,
                                     train.mcpc,
                                     train.acc,
                                     train.bacc,
                                     train.sens,
                                     train.spec),
                         "test" = c(n.test.ex,
                                    test.frugality,
                                    test.mcpc,
                                    test.acc,
                                    test.bacc,
                                    test.sens,
                                    test.spec)
)

}

if(is.null(x$tree.stats$test)) {

  n.test.ex <- 0
  test.frugality <- "--"
  test.mcpc <- "--"
  test.sens <- "--"
  test.far <- "--"
  test.spec <- "--"
  test.auc <- "--"
  test.acc <- "--"
  test.bacc <- "--"

  summary.df <- data.frame("train" = c(n.train.ex,
                                       train.frugality,
                                       train.mcpc,
                                       train.acc,
                                       train.bacc,
                                       train.sens,
                                       train.spec)
  )


}

rownames(summary.df) <- c("n", "frugality", "mcpc", "acc", "bacc", "sens", "spec")


summary.text <- paste("FFTrees object containing ", n.trees, " trees using up to ", all.cues.n,
                      " predictors of an original ", n.cues.total, sep = "")

if(is.null(test.auc)) {

auc.text <- paste("FFTrees AUC: (Train = ", train.auc, ")", sep = "")

}

if(is.null(test.auc) == F) {

  auc.text <- paste("FFTrees AUC: (Train = ", train.auc, ", Test = ", test.auc, ")", sep = "")

}

accuracy.text <- paste("Best training tree: #", train.tree, ", using ", train.cues.n, " cues {", train.cues, "}:", sep = "")

print(summary.text)
#print(auc.text)
print(accuracy.text)
print(summary.df)

}
