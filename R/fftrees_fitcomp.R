#' Fit competitive algorithms
#'
#' @description \code{fftrees_fitcomp} fits competitive algorithms for binary classification tasks
#' (e.g., LR, CART, RF, SVM) to the data and parameters specified in an \code{FFTrees} object.
#'
#' \code{fftrees_fitcomp} is called by the main \code{\link{FFTrees}} function
#' when creating FFTs from and applying them to data (unless \code{do.comp = FALSE}).
#'
#' @param x An \code{FFTrees} object.
#'
#' @seealso
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.

fftrees_fitcomp <- function(x) {

  # Parameters: ------

  do.lr   <- x$params$do.lr
  do.svm  <- x$params$do.svm
  do.cart <- x$params$do.cart
  do.rf   <- x$params$do.rf

  if (x$params$do.comp == FALSE) {
    do.lr   <- FALSE
    do.cart <- FALSE
    do.svm  <- FALSE
    do.rf   <- FALSE
  }

  sens.w <- x$params$sens.w  # required for wacc


  # Set the measures/columns to select from stats (computed by classtable() helper): ----
  my_cols <- c("algorithm",
               "n", "hi", "fa", "mi", "cr",
               "sens", "spec", "far", "ppv", "npv",
               "acc", "bacc", "wacc",  # ToDo: Add dprime?
               "cost", "cost_decisions", "cost_cues"
  )


  # A. FFTrees: ----

  x$competition$train <- x$trees$stats$train %>%
    dplyr::filter(tree == 1) %>%
    dplyr::mutate(algorithm = "fftrees") %>%
    dplyr::select(tidyselect::all_of(my_cols))

  if (!is.null(x$trees$stats$test)) {
    x$competition$test <- x$trees$stats$test %>%
      dplyr::filter(tree == 1) %>%
      dplyr::mutate(algorithm = "fftrees") %>%
      dplyr::select(tidyselect::all_of(my_cols))
  }


  # B. Competition: ------

  if (do.lr | do.cart | do.rf | do.svm) {
    if (!x$params$quiet) {
      message("Fitting other algorithms for comparison (disable with do.comp = FALSE) ...")
    }
  }


  # - LR: ----

  {
    if (do.lr) {

      lr.acc <- comp_pred(
        formula = x$formula,
        data.train = x$data$train,
        data.test = x$data$test,
        algorithm = "lr",
        model = NULL,
        sens.w = sens.w
      )

      lr.stats <- lr.acc$accuracy
      lr.model <- lr.acc$model

      x$competition$models$lr <- lr.model

      x$competition$train <- x$competition$train %>%
        dplyr::bind_rows(lr.stats[["train"]] %>%
                           dplyr::mutate(algorithm = "lr") %>%
                           dplyr::mutate(cost_cues = NA) %>%
                           dplyr::select(tidyselect::all_of(my_cols)))

      x$competition$test <- x$competition$test %>%
        dplyr::bind_rows(lr.stats[["test"]] %>%
                           dplyr::mutate(algorithm = "lr") %>%
                           dplyr::mutate(cost_cues = NA) %>%
                           dplyr::select(tidyselect::all_of(my_cols)))
    }
  }


  # - CART: ----

  {
    if (do.cart) {

      cart.acc <- comp_pred(
        formula = x$formula,
        data.train = x$data$train,
        data.test = x$data$test,
        algorithm = "cart",
        model = NULL,
        sens.w = sens.w
      )

      cart.stats <- cart.acc$accuracy
      cart.model <- cart.acc$model

      x$competition$models$cart <- cart.model

      x$competition$train <- x$competition$train %>%
        dplyr::bind_rows(cart.stats[["train"]] %>%
                           dplyr::mutate(algorithm = "cart") %>%
                           dplyr::mutate(cost_cues = NA) %>%
                           dplyr::select(tidyselect::all_of(my_cols)))

      x$competition$test <- x$competition$test %>%
        dplyr::bind_rows(cart.stats[["test"]] %>%
                           dplyr::mutate(algorithm = "cart") %>%
                           dplyr::mutate(cost_cues = NA) %>%
                           dplyr::select(tidyselect::all_of(my_cols)))
    }
  }


  # - RF: ----

  {
    if (do.rf) {

      rf.acc <- comp_pred(
        formula = x$formula,
        data.train = x$data$train,
        data.test = x$data$test,
        algorithm = "rf",
        model = NULL,
        sens.w = sens.w
      )


      rf.stats <- rf.acc$accuracy
      rf.model <- rf.acc$model

      x$competition$models$rf <- rf.model

      x$competition$train <- x$competition$train %>%
        dplyr::bind_rows(rf.stats[["train"]] %>%
                           dplyr::mutate(algorithm = "rf") %>%
                           dplyr::mutate(cost_cues = NA) %>%
                           dplyr::select(tidyselect::all_of(my_cols)))

      x$competition$test <- x$competition$test %>%
        dplyr::bind_rows(rf.stats[["test"]] %>%
                           dplyr::mutate(algorithm = "rf") %>%
                           dplyr::mutate(cost_cues = NA) %>%
                           dplyr::select(tidyselect::all_of(my_cols)))
    }
  }


  # - SVM: ----

  {
    if (do.svm) {

      svm.acc <- comp_pred(
        formula = x$formula,
        data.train = x$data$train,
        data.test = x$data$test,
        algorithm = "svm",
        model = NULL,
        sens.w = sens.w
      )

      svm.stats <- svm.acc$accuracy
      svm.model <- svm.acc$model

      x$competition$models$svm <- svm.model

      x$competition$train <- x$competition$train %>%
        dplyr::bind_rows(svm.stats[["train"]] %>%
                           dplyr::mutate(algorithm = "svm") %>%
                           dplyr::mutate(cost_cues = NA) %>%
                           dplyr::select(tidyselect::all_of(my_cols)))

      x$competition$test <- x$competition$test %>%
        dplyr::bind_rows(svm.stats[["test"]] %>%
                           dplyr::mutate(algorithm = "svm") %>%
                           dplyr::mutate(cost_cues = NA) %>%
                           dplyr::select(tidyselect::all_of(my_cols)))
    }
  }


  # Output: ------

  return(x)

} # fftrees_fitcomp().

# eof.
