#' Predict new data from an FFTrees x
#'
#' @param object An FFTrees object created from the FFTrees() function.
#' @param data A dataframe of test data
#' @param tree Which tree in the FFTrees x should be used? Can be an integer or "best.train" (the default) to use the tree with the best training statistics.
#' @param ... Additional arguments passed on to \code{predict()}
#' @return A logical vector of predictions
#' @export
#' @examples
#'
#'
#'   # Create training and test data
#'
#'   set.seed(100)
#'   breastcancer <- breastcancer[sample(nrow(breastcancer)),]
#'   breast.train <- breastcancer[1:150,]
#'   breast.test <- breastcancer[151:303,]
#'
#'   # Create an FFTrees x from the training data
#'
#'   breast.fft <- FFTrees(formula = diagnosis ~.,
#'                               data = breast.train)
#'
#'  # Predict results for test data
#'   breast.fft.pred <- predict(breast.fft,
#'                              data = breast.test)
#'

predict.FFTrees <- function(
  object = NULL,
  data = NULL,
  tree = "best.train",
  ...
) {

  # x <- result.i
  # data = data
  # tree <- "best.train"
  #
  goal <- object$params$goal

  if (tree == "best.train") {

    tree <- which(object$tree.stats$train[[goal]] == max(object$tree.stats$train[[goal]]))
    if(length(tree) > 1) {tree <- sample(tree, 1)}

  }

  predictions <- apply.tree(formula = object$formula,
                            data = data,
                            tree.definitions = object$tree.definitions)

  predictions <- predictions$decision[,tree]

  return(predictions)

}
