#' Applies an existing FFTrees object to a new dataset
#'
#' @param object (M) An FFTrees object created from the FFTrees() function.
#' @param data.test (M) A dataframe of test data
#' @param ... Additional arguments passed on to predict()
#' @return An FFTrees object
#' @export

predict.FFTrees <- function(
  object = NULL,
  data.test = NULL,
  ...
) {

  new.result <- FFTrees(data.test = data.test,
                        data = object$data$train,
                        object = object
  )

  return(new.result)

}
