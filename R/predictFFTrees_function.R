#' Predict classification outcomes or probabilities from data
#'
#' @description \code{predict.FFTrees} predicts binary classification outcomes or their probabilities from \code{newdata}
#' for an \code{FFTrees} object.
#'
#' @param object An \code{FFTrees} object created by the \code{\link{FFTrees}} function.
#' @param newdata dataframe. A data frame of test data.
#' @param tree integer. Which tree in the object should be used? By default, \code{tree = 1} is used.
#' @param type string. What should be predicted? Can be \code{"class"}, which returns a vector of class predictions, \code{"prob"} which returns a matrix of class probabilities,
#'  or \code{"both"} which returns a matrix with both class and probability predictions.
#' @param method string. Method of calculating class probabilities. Either 'laplace', which applies the Laplace correction, or 'raw' which applies no correction.
#' @param ... Additional arguments passed on to \code{predict}.
#'
#' @param sens.w,data deprecated
#'
#' @return Either a logical vector of predictions, or a matrix of class probabilities.
#'
#' @examples
#' # Create training and test data:
#' set.seed(100)
#' breastcancer <- breastcancer[sample(nrow(breastcancer)), ]
#' breast.train <- breastcancer[1:150, ]
#' breast.test  <- breastcancer[151:303, ]
#'
#' # Create an FFTrees object from the training data:
#' breast.fft <- FFTrees(
#'   formula = diagnosis ~ .,
#'   data = breast.train
#' )
#'
#' # Predict classes for test data:
#' breast.fft.pred <- predict(breast.fft,
#'   newdata = breast.test
#' )
#'
#' # Predict class probabilities for test data:
#' breast.fft.pred <- predict(breast.fft,
#'   newdata = breast.test,
#'   type = "prob"
#' )
#'
#' @seealso
#' \code{\link{print.FFTrees}} for printing FFTs;
#' \code{\link{plot.FFTrees}} for plotting FFTs;
#' \code{\link{summary.FFTrees}} for summarizing FFTs;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export

predict.FFTrees <- function(object = NULL,
                            newdata = NULL,
                            tree = 1,
                            type = "class",
                            sens.w = NULL,
                            method = "laplace",
                            data = NULL,
                            ...) {

  # Prepare: ------
  testthat::expect_true(is.null(data), info = "data is deprecated. Please use newdata instead")
  testthat::expect_true(!is.null(newdata))

  if (is.null(sens.w) == FALSE) {
    stop("sens.w is deprecated and will be ignored.")
  }

  testthat::expect_true(type %in% c("both", "class", "prob"))

  goal <- object$params$goal

  new.apply.tree <- fftrees_apply(
    x = object,
    mydata = "test",
    newdata = newdata
  )

  # Calculate predictions from tree: ------

  predictions <- new.apply.tree$trees$decisions$test[[tree]]$decision

  # Get classification output: ----

  class_output <- predictions

  # Get probability output: ----

  # For each case, determine where it is decided in the tree:
  prob_output <- matrix(NA, nrow = nrow(newdata), ncol = 2)

  # Get cumulative level stats for current tree in training data
  levelstats.c <- new.apply.tree$trees$level_stats$train[new.apply.tree$trees$level_stats$train$tree == tree, ]
  levels.n <- nrow(levelstats.c)

  # Get marginal counts for levels:

  n.m <- levelstats.c$n - c(0, levelstats.c$n[1:(levels.n - 1)])
  hi.m <- levelstats.c$hi - c(0, levelstats.c$hi[1:(levels.n - 1)])
  mi.m <- levelstats.c$mi - c(0, levelstats.c$mi[1:(levels.n - 1)])
  fa.m <- levelstats.c$fa - c(0, levelstats.c$fa[1:(levels.n - 1)])
  cr.m <- levelstats.c$cr - c(0, levelstats.c$cr[1:(levels.n - 1)])

  npv.m <- cr.m / (cr.m + mi.m)
  ppv.m <- hi.m / (hi.m + fa.m)

  # Laplace correction:
  npv.lp.m <- (cr.m + 1) / (cr.m + mi.m + 2)
  ppv.lp.m <- (hi.m + 1) / (hi.m + fa.m + 2)

  # Loop over levels:
  for (level.i in 1:levels.n) {

    # Which cases are classified as FALSE in this level?
    decide.now.0.log <- new.apply.tree$trees$decisions$test[[paste0("tree_", tree)]]$levelout == level.i &
      new.apply.tree$trees$decisions$test[[paste0("tree_", tree)]]$decision == FALSE

    # Which cases are classified as TRUE in this level?
    decide.now.1.log <- new.apply.tree$trees$decisions$test[[paste0("tree_", tree)]]$levelout == level.i &
      new.apply.tree$trees$decisions$test[[paste0("tree_", tree)]]$decision == TRUE

    if (method == "laplace") {

      # Assign the npv for those assigned 0
      prob_output[decide.now.0.log, 1] <- npv.lp.m[level.i]
      prob_output[decide.now.0.log, 2] <- 1 - npv.lp.m[level.i]

      # Assign the ppv for those assigned 1
      prob_output[decide.now.1.log, 2] <- ppv.lp.m[level.i]
      prob_output[decide.now.1.log, 1] <- 1 - ppv.lp.m[level.i]
    }

    if (method == "raw") {
      prob_output[decide.now.0.log, 1] <- npv.m[level.i]
      prob_output[decide.now.0.log, 2] <- 1 - npv.m[level.i]

      prob_output[decide.now.1.log, 2] <- ppv.m[level.i]
      prob_output[decide.now.1.log, 1] <- 1 - ppv.m[level.i]
    }
  }

  if (type == "prob") {
    colnames(prob_output) <- c("prob_0", "prob_1")
    output <- tibble::as_tibble(prob_output)
  }

  if (type == "class") {
    output <- class_output
  }

  if (type == "both") {
    output <- tibble::as_tibble(data.frame(
      class = class_output,
      prob_0 = prob_output[, 1],
      prob_1 = prob_output[, 2]
    ))
  }


  # Output: ------
  return(output)

} # predict.FFTrees().

# eof.
