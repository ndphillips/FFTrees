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


  # Set the measures/columns to select from stats (as computed by the classtable() helper): ----
  my_cols <- c("algorithm",
               "n", "hi", "fa", "mi", "cr",
               "sens", "spec", "far", "ppv", "npv",
               "acc", "bacc", "wacc",
               "dprime",
               "cost_dec", "cost_cue", "cost"
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


  # Provide user feedback: ----

  if (do.lr | do.cart | do.rf | do.svm) {
    if (!x$params$quiet) {
      msg <- "Aiming to fit comparative algorithms (disable by do.comp = FALSE):\n"
      cat(u_f_ini(msg))
    }
  }


  # - LR: ----

  {
    if (do.lr) {

      lr_acc <- comp_pred(
        formula = x$formula,
        data.train = x$data$train,
        data.test = x$data$test,
        algorithm = "lr",
        model = NULL,
        sens.w = sens.w
      )

      lr_stats <- lr_acc$accuracy
      lr_model <- lr_acc$model

      x$competition$models$lr <- lr_model

      x$competition$train <- x$competition$train %>%
        dplyr::bind_rows(lr_stats[["train"]] %>%
                           dplyr::mutate(algorithm = "lr") %>%
                           dplyr::mutate(cost_cue = NA) %>%
                           dplyr::select(tidyselect::all_of(my_cols)))

      x$competition$test <- x$competition$test %>%
        dplyr::bind_rows(lr_stats[["test"]] %>%
                           dplyr::mutate(algorithm = "lr") %>%
                           dplyr::mutate(cost_cue = NA) %>%
                           dplyr::select(tidyselect::all_of(my_cols)))
    }
  }


  # - CART: ----

  {
    if (do.cart) {

      cart_acc <- comp_pred(
        formula = x$formula,
        data.train = x$data$train,
        data.test = x$data$test,
        algorithm = "cart",
        model = NULL,
        sens.w = sens.w
      )

      cart_stats <- cart_acc$accuracy
      cart_model <- cart_acc$model

      x$competition$models$cart <- cart_model

      x$competition$train <- x$competition$train %>%
        dplyr::bind_rows(cart_stats[["train"]] %>%
                           dplyr::mutate(algorithm = "cart") %>%
                           dplyr::mutate(cost_cue = NA) %>%
                           dplyr::select(tidyselect::all_of(my_cols)))

      x$competition$test <- x$competition$test %>%
        dplyr::bind_rows(cart_stats[["test"]] %>%
                           dplyr::mutate(algorithm = "cart") %>%
                           dplyr::mutate(cost_cue = NA) %>%
                           dplyr::select(tidyselect::all_of(my_cols)))
    }
  }


  # - RF: ----

  {
    if (do.rf) {

      rf_acc <- comp_pred(
        formula = x$formula,
        data.train = x$data$train,
        data.test = x$data$test,
        algorithm = "rf",
        model = NULL,
        sens.w = sens.w
      )


      rf_stats <- rf_acc$accuracy
      rf_model <- rf_acc$model

      x$competition$models$rf <- rf_model

      x$competition$train <- x$competition$train %>%
        dplyr::bind_rows(rf_stats[["train"]] %>%
                           dplyr::mutate(algorithm = "rf") %>%
                           dplyr::mutate(cost_cue = NA) %>%
                           dplyr::select(tidyselect::all_of(my_cols)))

      x$competition$test <- x$competition$test %>%
        dplyr::bind_rows(rf_stats[["test"]] %>%
                           dplyr::mutate(algorithm = "rf") %>%
                           dplyr::mutate(cost_cue = NA) %>%
                           dplyr::select(tidyselect::all_of(my_cols)))
    }
  }


  # - SVM: ----

  {
    if (do.svm) {

      svm_acc <- comp_pred(
        formula = x$formula,
        data.train = x$data$train,
        data.test = x$data$test,
        algorithm = "svm",
        model = NULL,
        sens.w = sens.w
      )

      svm_stats <- svm_acc$accuracy
      svm_model <- svm_acc$model

      x$competition$models$svm <- svm_model

      x$competition$train <- x$competition$train %>%
        dplyr::bind_rows(svm_stats[["train"]] %>%
                           dplyr::mutate(algorithm = "svm") %>%
                           dplyr::mutate(cost_cue = NA) %>%
                           dplyr::select(tidyselect::all_of(my_cols)))

      x$competition$test <- x$competition$test %>%
        dplyr::bind_rows(svm_stats[["test"]] %>%
                           dplyr::mutate(algorithm = "svm") %>%
                           dplyr::mutate(cost_cue = NA) %>%
                           dplyr::select(tidyselect::all_of(my_cols)))
    }
  }


  # Provide user feedback: ----

  if (do.lr | do.cart | do.rf | do.svm) {
    if (!x$params$quiet) {
      cat(u_f_fin("Successfully fitted comparative algorithms.\n"))
    }
  }


  # Output: ------

  return(x)

} # fftrees_fitcomp().

# eof.
