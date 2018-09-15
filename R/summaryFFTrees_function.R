#' Returns summary information about an FFTrees object
#' @param object An FFTrees object
#' @param tree integer. The tree to summarise
#' @param ... additional arguments (currently ignored)
#' @export
#'

summary.FFTrees <- function(object,
                            tree = 1,
                            ...) {


  train.cues <- paste(unique(unlist(strsplit(object$tree.definitions$cues[tree], ";"))), collapse = ",")
  train.cues.n <- length(unique(unlist(strsplit(train.cues, ","))))

  all.cues <- paste(unique(unlist(strsplit(object$tree.definitions$cues, ";"))), collapse = ",")
  all.cues.n <- length(unique(unlist(strsplit(object$tree.definitions$cues, ";"))))

  train.n <- object$data.desc$train$cases
  train.hi <- object$tree.stats$train$hi[tree]
  train.mi <- object$tree.stats$train$mi[tree]
  train.cr <- object$tree.stats$train$cr[tree]
  train.fa <- object$tree.stats$train$fa[tree]
  train.nodes <- train.cues.n
  train.sens <- object$tree.stats$train$sens[tree]
  train.far <- object$tree.stats$train$far[tree]
  train.spec <- 1 - object$tree.stats$train$far[tree]
  train.bacc <- object$tree.stats$train$bacc[tree]
  train.wacc <- object$tree.stats$train$wacc[tree]
  train.cost <- object$tree.stats$train$cost[tree]
  train.acc <- object$tree.stats$train$acc[tree]
  train.pci <-  object$tree.stats$train$pci[tree]
  train.mcu <- object$tree.stats$train$mcu[tree]

  if(is.null(object$tree.stats$test) == FALSE) {

    test.n <- object$data.desc$test$cases
    test.hi <- object$tree.stats$test$hi[tree]
    test.mi <- object$tree.stats$test$mi[tree]
    test.cr <- object$tree.stats$test$cr[tree]
    test.fa <- object$tree.stats$test$fa[tree]

    test.nodes <- train.cues.n
    test.sens <- object$tree.stats$test$sens[tree]
    test.far <- object$tree.stats$test$far[tree]
    test.spec <- 1 - object$tree.stats$test$far[tree]
    test.bacc <- object$tree.stats$test$bacc[tree]
    test.wacc <- object$tree.stats$test$wacc[tree]
    test.cost <- object$tree.stats$test$cost[tree]
    test.acc <- object$tree.stats$test$acc[tree]
    test.pci <-  object$tree.stats$test$pci[tree]
    test.mcu <- object$tree.stats$test$mcu[tree]

    summary.df <- data.frame("train" = c(train.n,
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
                                         train.spec),
                             "test" = c(test.n,
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
                                        test.spec)
    )

  }

  if(is.null(object$tree.stats$test)) {

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

    summary.df <- data.frame("train" = c(train.n,
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
                                         train.spec),
                                         "test" = c(test.n,
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
                                                    test.spec))


  }

  rownames(summary.df) <- c("n",
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
                            "spec")



  return(summary.df)

}
