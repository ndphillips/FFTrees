#' Predict new data from an FFTrees x
#'
#' @param object An FFTrees object created from the FFTrees() function.
#' @param data dataframe. A dataframe of test data
#' @param tree Which tree in the FFTrees x should be used? Can be an integer or "best.train" (the default) to use the tree with the best training statistics (according the goal specified in tree construction).
#' @param sens.w numeric.  A number from 0 to 1 indicating how to weight sensitivity relative to specificity. If specified, the tree with the highest weighted accuracy (wacc) given the specified value of sens.w will be selected
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
  sens.w = NULL,
  ...
) {


  goal <- object$params$goal

  if (tree == "best.train" & is.null(sens.w)) {

    tree <- which(object$tree.stats$train[[goal]] == max(object$tree.stats$train[[goal]]))

  }


  if (is.null(sens.w) == FALSE) {

    if(sens.w < 0 | sens.w > 1) {stop("sens.w must be a number between 0 and 1")}

    # Get the sensitivities and specificities

    sens.v <- object$tree.stats$train$sens
    spec.v <- object$tree.stats$train$spec

    # Calculate new wacc values

    wacc.v <- sens.v * sens.w + spec.v * (1 - sens.w)

    # Select tree with highest wacc value
    tree <- which(wacc.v == max(wacc.v))

  }

  # If there is more than one best tree, take the first
  if(length(tree) > 1) {tree <- tree[1]}

  # Calculate predictions across all trees
  predictions <- apply.tree(formula = object$formula,
                            data = data,
                            tree.definitions = object$tree.definitions)

  # Get the predictions for the selected tree
  predictions <- predictions$decision[,tree]

  return(predictions)

}
