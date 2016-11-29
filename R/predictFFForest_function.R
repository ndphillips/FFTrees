#' Predict outcoms from a test dataset using an FFForest object
#'
#' @param object FFForest. An FFForest object created from the FFForest() function.
#' @param data dataframe. A dataframe of test data
#' @param threshold numeric. A threshold value
#' @param ... Additional arguments passed on to \code{predict()}
#' @return A logical vector of predictions
#' @export
#'

predict.FFForest <- function(object = NULL,
  data = NULL,
  threshold = .5,
  ...
) {

  n.trees <- nrow(object$tree.sim)
  predictions <- matrix(NA, nrow = nrow(data), ncol = n.trees)

  for(i in 1:n.trees) {

    pred.i <- apply.tree(data = data,
                         formula = object$formula,
                         tree.definitions = object$tree.sim[i,])

    pred.i <- pred.i$decision

    predictions[,i] <- pred.i

  }

  # Convert prediction matrix to point predictions

  pred.means <- rowMeans(predictions)

  output <- pred.means >= threshold

  return(output)

}
