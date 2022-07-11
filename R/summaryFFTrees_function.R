#' Returns summary information about an FFTrees x
#' @param x An FFTrees x
#' @param tree integer. The tree to summarise
#' @param ... additional arguments (currently ignored)
#' @export
#'

summary.FFTrees <- function(x,
                            tree = 1,
                            ...) {
  train.cues <- paste(unique(unlist(strsplit(x$trees$definitions$cues[tree], ";"))), collapse = ",")
  train.cues.n <- length(unique(unlist(strsplit(train.cues, ","))))

  all.cues <- paste(unique(unlist(strsplit(x$trees$definitions$cues, ";"))), collapse = ",")
  all.cues.n <- length(unique(unlist(strsplit(x$trees$definitions$cues, ";"))))

  train.n <- nrow(x$data$train)
  train.hi <- x$trees$results$train$stats$hi[tree]
  train.mi <- x$trees$results$train$stats$mi[tree]
  train.cr <- x$trees$results$train$stats$cr[tree]
  train.fa <- x$trees$results$train$stats$fa[tree]
  train.nodes <- train.cues.n
  train.sens <- x$trees$results$train$stats$sens[tree]
  train.far <- x$trees$results$train$stats$far[tree]
  train.spec <- 1 - x$trees$results$train$stats$far[tree]
  train.bacc <- x$trees$results$train$stats$bacc[tree]
  train.wacc <- x$trees$results$train$stats$wacc[tree]
  train.cost <- x$trees$results$train$stats$cost[tree]
  train.acc <- x$trees$results$train$stats$acc[tree]
  train.pci <- x$trees$results$train$stats$pci[tree]
  train.mcu <- x$trees$results$train$stats$mcu[tree]

  if (is.null(x$trees$results$test$stats) == FALSE) {
    test.n <- nrow(x$data$test)
    test.hi <- x$trees$results$test$stats$hi[tree]
    test.mi <- x$trees$results$test$stats$mi[tree]
    test.cr <- x$trees$results$test$stats$cr[tree]
    test.fa <- x$trees$results$test$stats$fa[tree]

    test.nodes <- train.cues.n
    test.sens <- x$trees$results$test$stats$sens[tree]
    test.far <- x$trees$results$test$stats$far[tree]
    test.spec <- 1 - x$trees$results$test$stats$far[tree]
    test.bacc <- x$trees$results$test$stats$bacc[tree]
    test.wacc <- x$trees$results$test$stats$wacc[tree]
    test.cost <- x$trees$results$test$stats$cost[tree]
    test.acc <- x$trees$results$test$stats$acc[tree]
    test.pci <- x$trees$results$test$stats$pci[tree]
    test.mcu <- x$trees$results$test$stats$mcu[tree]

    summary.df <- data.frame(
      "train" = c(
        train.n,
        train.hi,
        train.mi,
        train.fa,
        train.cr,
        train.mcu,
        train.pci,
        train.cost,
        train.acc,
        train.bacc,
        train.sens,
        train.spec
      ),
      "test" = c(
        test.n,
        test.hi,
        test.mi,
        test.fa,
        test.cr,
        test.mcu,
        test.pci,
        test.cost,
        test.acc,
        test.bacc,
        test.sens,
        test.spec
      )
    )
  }

  if (is.null(x$trees$results$test$stats)) {
    test.n <- NA
    test.hi <- NA
    test.mi <- NA
    test.fa <- NA
    test.cr <- NA
    test.pci <- NA
    test.mcu <- NA
    test.sens <- NA
    test.far <- NA
    test.spec <- NA
    test.acc <- NA
    test.cost <- NA
    test.bacc <- NA
    test.wacc <- NA

    summary.df <- data.frame(
      "train" = c(
        train.n,
        train.hi,
        train.mi,
        train.fa,
        train.cr,
        train.mcu,
        train.pci,
        train.cost,
        train.acc,
        train.bacc,
        train.sens,
        train.spec
      ),
      "test" = c(
        test.n,
        test.hi,
        test.mi,
        test.fa,
        test.cr,
        test.mcu,
        test.pci,
        test.cost,
        test.acc,
        test.bacc,
        test.sens,
        test.spec
      )
    )
  }

  rownames(summary.df) <- c(
    "n",
    "hi",
    "mi",
    "fa",
    "cr",
    "mcu",
    "pci",
    "cost",
    "acc",
    "bacc",
    "sens",
    "spec"
  )



  return(summary.df)
}
