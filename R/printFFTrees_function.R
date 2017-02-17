#' Prints summary information from an FFTrees x
#'
#' @description Printing function for an FFTrees x
#' @param x FFTrees. A FFTrees x created from FFTrees()
#' @param ... additional arguments passed to print.
#' @export

print.FFTrees <- function(
  x = NULL,
  ...
) {

goal <- x$params$goal

n.trees <- nrow(x$tree.stats$train)
n.cues.total <- x$data.desc$train$features

tree <- min(x$tree.stats$train$tree[x$tree.stats$train[[goal]] == max(x$tree.stats$train[[goal]])])[1]

train.cues <- paste(unique(unlist(strsplit(x$tree.stats$train$cues[tree], ";"))), collapse = ",")
train.cues.n <- length(unique(unlist(strsplit(train.cues, ","))))

all.cues <- paste(unique(unlist(strsplit(x$tree.stats$train$cues, ";"))), collapse = ",")
all.cues.n <- length(unique(unlist(strsplit(x$tree.stats$train$cues, ";"))))

train.n <- x$data.desc$train$cases
train.sens <- round(x$tree.stats$train$sens[tree], 2)
train.far <- round(x$tree.stats$train$far[tree], 2)
train.spec <- 1 - round(x$tree.stats$train$far[tree], 2)
train.dp <- round(x$tree.stats$train$dprime[tree], 2)
train.bacc <- round(x$tree.stats$train$bacc[tree], 2)
train.acc <- round(x$tree.stats$train$acc[tree], 2)
train.pci <-  round(x$tree.stats$train$pci[tree], 2)
train.mcu <- round(x$tree.stats$train$mcu[tree], 2)
train.auc <- round(x$auc$FFTrees[1], 2)

if(is.null(x$tree.stats$test) == FALSE) {

test.n <- x$data.desc$test$cases
test.sens <- round(x$tree.stats$test$sens[tree], 2)
test.far <- round(x$tree.stats$test$far[tree], 2)
test.spec <- 1 - round(x$tree.stats$test$far[tree], 2)
test.dp <- round(x$tree.stats$test$dprime[tree], 2)
test.bacc <- round(x$tree.stats$test$bacc[tree], 2)
test.acc <- round(x$tree.stats$test$acc[tree], 2)
test.pci <-  round(x$tree.stats$test$pci[tree], 2)
test.mcu <- round(x$tree.stats$test$mcu[tree], 2)
test.auc <- round(x$auc$FFTrees[1], 2)

summary.df <- data.frame("train" = c(train.n,
                                     train.pci,
                                     train.mcu,
                                     train.acc,
                                     train.bacc,
                                     train.sens,
                                     train.spec),
                         "test" = c(test.n,
                                    test.pci,
                                    test.mcu,
                                    test.acc,
                                    test.bacc,
                                    test.sens,
                                    test.spec)
)

}

if(is.null(x$tree.stats$test)) {

  test.n <- 0
  test.pci <- "--"
  test.mcu <- "--"
  test.sens <- "--"
  test.far <- "--"
  test.spec <- "--"
  test.auc <- "--"
  test.acc <- "--"
  test.bacc <- "--"

  summary.df <- data.frame("train" = c(train.n,
                                       train.pci,
                                       train.mcu,
                                       train.acc,
                                       train.bacc,
                                       train.sens,
                                       train.spec)
  )


}

rownames(summary.df) <- c("n", "pci", "mcu", "acc", "bacc", "sens", "spec")


summary.text <- paste(n.trees, " FFTs using up to ", all.cues.n,
                      " of ", n.cues.total, " cues", sep = "")

if(is.null(test.auc)) {

auc.text <- paste("FFTrees AUC: (Train = ", train.auc, ")", sep = "")

}

if(is.null(test.auc) == F) {

  auc.text <- paste("FFTrees AUC: (Train = ", train.auc, ", Test = ", test.auc, ")", sep = "")

}

accuracy.text <- paste("FFT #", tree, " uses ", train.cues.n, " cues {", train.cues, "} with the following performance:", sep = "")

print(summary.text)
print(accuracy.text)
print(summary.df)

}
