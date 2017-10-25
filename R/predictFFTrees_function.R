#' Predict new data from an FFTrees x
#'
#' @param object An FFTrees object created from the FFTrees() function.
#' @param data dataframe. A dataframe of test data
#' @param tree integer. Which tree in the object should be used?
#' @param type string. What should be predicted? Can be \code{"class"}, which returns a vector of class predictions, or \code{"prob"} which returns a matrix of class probabilities.
#' @param ... Additional arguments passed on to \code{predict()}
#' @param sens.w depricated
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
  newdata = NULL,
  data = NULL,
  tree = 1,
  type = "class",
  sens.w = NULL,
  ...
) {

  if(is.null(data)) {

    if(is.null(newdata) == FALSE) {

      data <- newdata} else {

      stop("Either data or newdata must be specified.")

      }

  }

  if(is.null(sens.w) == FALSE) {stop("sens.w is depricated and will be ignored.")}

  goal <- object$params$goal

  # Calculate predictions across all trees
  predictions <- apply.tree(formula = object$formula,
                            data = data,
                            tree.definitions = object$tree.definitions)

  # Get the predictions for the selected tree
  predictions <- predictions$decision[,tree]


  return(predictions)

}
