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
  train.sens <- round(object$tree.stats$train$sens[tree], 2)
  train.far <- round(object$tree.stats$train$far[tree], 2)
  train.spec <- 1 - round(object$tree.stats$train$far[tree], 2)
  train.bacc <- round(object$tree.stats$train$bacc[tree], 2)
  train.wacc <- round(object$tree.stats$train$wacc[tree], 2)
  train.cost <- round(object$tree.stats$train$cost[tree], 2)
  train.acc <- round(object$tree.stats$train$acc[tree], 2)
  train.pci <-  round(object$tree.stats$train$pci[tree], 2)
  train.mcu <- round(object$tree.stats$train$mcu[tree], 2)

  if(is.null(object$tree.stats$test) == FALSE) {

    test.n <- object$data.desc$test$cases
    test.hi <- object$tree.stats$test$hi[tree]
    test.mi <- object$tree.stats$test$mi[tree]
    test.cr <- object$tree.stats$test$cr[tree]
    test.fa <- object$tree.stats$test$fa[tree]


    test.nodes <- train.cues.n
    test.sens <- round(object$tree.stats$test$sens[tree], 2)
    test.far <- round(object$tree.stats$test$far[tree], 2)
    test.spec <- 1 - round(object$tree.stats$test$far[tree], 2)
    test.bacc <- round(object$tree.stats$test$bacc[tree], 2)
    test.wacc <- round(object$tree.stats$test$wacc[tree], 2)
    test.cost <- round(object$tree.stats$test$cost[tree], 2)
    test.acc <- round(object$tree.stats$test$acc[tree], 2)
    test.pci <-  round(object$tree.stats$test$pci[tree], 2)
    test.mcu <- round(object$tree.stats$test$mcu[tree], 2)

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

    test.n <- 0
    test.hi <- 0
    test.mi <- 0
    test.fa <- 0
    test.cr <- 0
    test.pci <- "--"
    test.mcu <- "--"
    test.sens <- "--"
    test.far <- "--"
    test.spec <- "--"
    test.acc <- "--"
    test.cost <- "--"
    test.bacc <- "--"
    test.wacc <- "--"

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
                                         train.spec)
    )


  }

  rownames(summary.df) <- c("cases       n",
                            "hits        hi",
                            "misses      mi",
                            "false al    fa",
                            "corr rej    cr",
                            "speed       mcu",
                            "frugality   pci",
                            "cost        cost",
                            "accuracy    acc",
                            "balanced    bacc",
                            "sensitivity sens",
                            "specificity spec")



  return(summary.df)

}
