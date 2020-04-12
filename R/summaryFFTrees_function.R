#' Returns summary information about an FFTrees x
#' @param object FFTrees.
#' @param tree integer. The tree to summarise
#' @param ... additional arguments (currently ignored)
#' @export
#'

summary.FFTrees <- function(object,
                            tree = 1,
                            ...) {


  train.cues <- paste(unique(unlist(strsplit(object$trees$definitions$cues[tree], ";"))), collapse = ",")
  train.cues.n <- length(unique(unlist(strsplit(train.cues, ","))))

  all.cues <- paste(unique(unlist(strsplit(object$trees$definitions$cues, ";"))), collapse = ",")
  all.cues.n <- length(unique(unlist(strsplit(object$trees$definitions$cues, ";"))))

  train.n <- nrow(object$data$train)
  train.hi <- object$trees$results$train$stats$hi[tree]
  train.mi <- object$trees$results$train$stats$mi[tree]
  train.cr <- object$trees$results$train$stats$cr[tree]
  train.fa <- object$trees$results$train$stats$fa[tree]
  train.nodes <- train.cues.n
  train.sens <- object$trees$results$train$stats$sens[tree]
  train.far <- object$trees$results$train$stats$far[tree]
  train.spec <- 1 - object$trees$results$train$stats$far[tree]
  train.bacc <- object$trees$results$train$stats$bacc[tree]
  train.wacc <- object$trees$results$train$stats$wacc[tree]
  train.cost <- object$trees$results$train$stats$cost[tree]
  train.acc <- object$trees$results$train$stats$acc[tree]
  train.pci <-  object$trees$results$train$stats$pci[tree]
  train.mcu <- object$trees$results$train$stats$mcu[tree]

  if(is.null(object$trees$results$test$stats) == FALSE) {

    test.n <- nrow(object$data$test)
    test.hi <- object$trees$results$test$stats$hi[tree]
    test.mi <- object$trees$results$test$stats$mi[tree]
    test.cr <- object$trees$results$test$stats$cr[tree]
    test.fa <- object$trees$results$test$stats$fa[tree]

    test.nodes <- train.cues.n
    test.sens <- object$trees$results$test$stats$sens[tree]
    test.far <- object$trees$results$test$stats$far[tree]
    test.spec <- 1 - object$trees$results$test$stats$far[tree]
    test.bacc <- object$trees$results$test$stats$bacc[tree]
    test.wacc <- object$trees$results$test$stats$wacc[tree]
    test.cost <- object$trees$results$test$stats$cost[tree]
    test.acc <- object$trees$results$test$stats$acc[tree]
    test.pci <-  object$trees$results$test$stats$pci[tree]
    test.mcu <- object$trees$results$test$stats$mcu[tree]

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

  if(is.null(object$trees$results$test$stats)) {

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
