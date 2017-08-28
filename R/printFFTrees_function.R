#' Prints summary information from an FFTrees object
#'
#' @description Printing function for an FFTrees object
#' @param x FFTrees. A FFTrees x created from FFTrees()
#' @param ... additional arguments passed to print.
#' @export

print.FFTrees <- function(
  x = NULL,
  ...
) {

goal <- x$params$goal
sens.w <- x$params$sens.w
criterion.name <- paste(x$formula)[2]

n.trees <- nrow(x$tree.stats$train)
n.cues.total <- x$data.desc$train$features

if(("tree.max" %in% names(x)) == FALSE) {

tree <- min(x$tree.stats$train$tree[x$tree.stats$train[[goal]] == max(x$tree.stats$train[[goal]])])[1]

} else {tree <- x$tree.max}

train.cues <- paste(unique(unlist(strsplit(x$tree.definitions$cues[tree], ";"))), collapse = ",")
train.cues.n <- length(unique(unlist(strsplit(train.cues, ","))))

all.cues <- paste(unique(unlist(strsplit(x$tree.definitions$cues, ";"))), collapse = ",")
all.cues.n <- length(unique(unlist(strsplit(x$tree.definitions$cues, ";"))))

train.n <- x$data.desc$train$cases
train.nodes <- train.cues.n
train.sens <- round(x$tree.stats$train$sens[tree], 2)
train.far <- round(x$tree.stats$train$far[tree], 2)
train.spec <- 1 - round(x$tree.stats$train$far[tree], 2)
train.dp <- round(x$tree.stats$train$dprime[tree], 2)
train.bacc <- round(x$tree.stats$train$bacc[tree], 2)
train.wacc <- round(x$tree.stats$train$wacc[tree], 2)
train.acc <- round(x$tree.stats$train$acc[tree], 2)
train.pci <-  round(x$tree.stats$train$pci[tree], 2)
train.mcu <- round(x$tree.stats$train$mcu[tree], 2)
train.auc <- round(x$auc$FFTrees[1], 2)

if(is.null(x$tree.stats$test) == FALSE) {

test.n <- x$data.desc$test$cases
test.nodes <- train.cues.n
test.sens <- round(x$tree.stats$test$sens[tree], 2)
test.far <- round(x$tree.stats$test$far[tree], 2)
test.spec <- 1 - round(x$tree.stats$test$far[tree], 2)
test.dp <- round(x$tree.stats$test$dprime[tree], 2)
test.bacc <- round(x$tree.stats$test$bacc[tree], 2)
test.wacc <- round(x$tree.stats$test$wacc[tree], 2)
test.acc <- round(x$tree.stats$test$acc[tree], 2)
test.pci <-  round(x$tree.stats$test$pci[tree], 2)
test.mcu <- round(x$tree.stats$test$mcu[tree], 2)
test.auc <- round(x$auc$FFTrees[1], 2)

summary.df <- data.frame("train" = c(train.n,
                                     train.mcu,

                                     train.pci,
                                     train.acc,
                                     # train.bacc,
                                     train.wacc,
                                     train.sens,
                                     train.spec),
                         "test" = c(test.n,
                                    test.mcu,

                                    test.pci,
                                    test.acc,
                                    # test.bacc,
                                    test.wacc,
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
  test.wacc <- "--"

  summary.df <- data.frame("train" = c(train.n,
                                       train.mcu,
                                       train.pci,
                                       train.acc,
                                       # train.bacc,
                                       train.wacc,
                                       train.sens,
                                       train.spec)
  )


}

rownames(summary.df) <- c("cases       :n",
                          "speed       :mcu",
                          "frugality   :pci",
                          "accuracy    :acc",
                          # "balanced    :bacc",
                          "weighted    :wacc",
                          "sensitivity :sens",
                          "specificity :spec")


n.cues <- x$tree.definitions$nodes[tree]

if(n.trees == 1) {summary.text <- paste(x$params$algorithm, " FFT predicting ", criterion.name, " with up to ", n.cues, " nodes", sep = "")}
if(n.trees > 1) {summary.text <- paste(n.trees, " FFTs predicting ", criterion.name, " (", x$params$decision.labels[1], " v ", x$params$decision.labels[2], ")", sep = "")}

params.text <- paste0("pars: algorithm = '", x$params$algorithm, "', goal = '", x$params$goal, "', goal.chase = '", x$params$goal.chase, "', sens.w = ", x$params$sens.w, ", max.levels = ", x$params$max.levels)

if(is.null(test.auc)) {

auc.text <- paste("FFTrees AUC: (Train = ", train.auc, ")", sep = "")

}

if(is.null(test.auc) == FALSE) {

  auc.text <- paste("FFTrees AUC: (Train = ", train.auc, ", Test = ", test.auc, ")", sep = "")

}


accuracy.text <- paste("FFT #", tree, " predicts ", criterion.name," using ", train.cues.n, " cues: {", train.cues, "}", sep = "")


# Confusion table





if(is.null(x$params$main) == FALSE) {

cat(x$params$main)
cat("\n")
}



cat(accuracy.text)
cat("\n")

cat("\n")
sapply(1:length(FFTrees::inwords(x = x)$v1), FUN = function(i) {cat(paste0("[", i, "] ", FFTrees::inwords(x)$v1[i], ".\n"))})
cat("\n")
print(summary.df)
cat("\n")

cat(params.text)


}
