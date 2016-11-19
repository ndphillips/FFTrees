#' Applies an existing FFTrees object to a new (test) data set
#'
#' @param object An FFTrees object created from the FFTrees() function.
#' @param data A dataframe of test data
#' @param tree Which tree in the FFTrees object should be used? Can be an integer or "best.train" (the default) to use the tree with the best training statistics.
#' @param ... Additional arguments passed on to \code{predict()}
#' @return An FFTrees object
#' @export
#' @examples
#'
#'
#'   # Create an FFTrees object from 200 cases from thethe breastcancer dataset
#'
#'   breastcancer.fft <- FFTrees(formula = diagnosis ~.,
#'                               data = breastcancer[1:300,])
#'
#'  # Predict results for remaining data
#'   predict(breastcancer.fft,
#'   data = breastcancer[301:nrow(breastcancer),])
#'
#'
#'
#'

predict.FFTrees <- function(
  object = NULL,
  data = NULL,
  tree = "best.train",
  ...
) {

  if (tree == "best.train") {

    tree <- which(object$tree.stats$train$v == max(object$tree.stats$train$v))

  }

  predictions <- apply.tree(formula = object$formula,
                            data = data,
                            tree.definitions = object$tree.definitions)

  predictions <- predictions$decision[,tree]

  return(predictions)

}
