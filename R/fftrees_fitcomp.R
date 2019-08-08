#' Fit competitive algorithms
#'
#' @param x
#'
#'
#'
fftrees_fitcomp <- function(x) {

  do.lr <- x$params$do.lr
  do.svm <- x$params$do.svm
  do.cart <- x$params$do.cart
  do.rf <- x$params$do.rf

  if(x$params$do.comp == FALSE) {

    do.lr <- FALSE
    do.cart <- FALSE
    do.svm <- FALSE
    do.rf <- FALSE

  }

  if(do.lr | do.cart | do.rf | do.svm) {if(!x$params$quiet) {message("Fitting other algorithms for comparison (disable with do.comp = FALSE) ...")}}

  # LR
  {

    if(do.lr) {

      lr.acc <- FFTrees:::comp.pred(formula = x$formula,
                                    data.train = x$data$train,
                                    data.test = x$data$test,
                                    algorithm = "lr",
                                    model = NULL)

      lr.stats <- lr.acc$accuracy
      lr.model <- lr.acc$model

      x$comp$lr$model <- lr.model
      x$comp$lr$results <- lr.stats

    }

  }

  # CART
  {

    if(do.cart) {

      cart.acc <- FFTrees:::comp.pred(formula = x$formula,
                                      data.train = x$data$train,
                                      data.test = x$data$test,
                                      algorithm = "cart",
                                      model = NULL)

      cart.stats <- cart.acc$accuracy
      cart.model <- cart.acc$model

      x$comp$cart$model <- cart.model
      x$comp$cart$results <- cart.stats

    }

  }

  # rf
  {

    if(do.rf) {

      rf.acc <- FFTrees:::comp.pred(formula = x$formula,
                                    data.train = x$data$train,
                                    data.test = x$data$test,
                                    algorithm = "rf",
                                    model = NULL)

      rf.stats <- rf.acc$accuracy
      rf.model <- rf.acc$model

      x$comp$rf$model <- rf.model
      x$comp$rf$results <- rf.stats

    }
  }

  # svm
  {

    if(do.svm) {

      svm.acc <- FFTrees:::comp.pred(formula = x$formula,
                                     data.train = x$data$train,
                                     data.test = x$data$test,
                                     algorithm = "svm",
                                     model = NULL)

      svm.stats <- svm.acc$accuracy
      svm.model <- svm.acc$model

      x$comp$svm$model <- svm.model
      x$comp$svm$results <- svm.stats
  }


  }

return(x)

}
