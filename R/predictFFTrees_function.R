#' Applies an existing FFTrees object to a new dataset
#'
#' @param object (M) An FFTrees object created from the FFTrees() function.
#' @param data (M) An m x n dataframe containing n cue values for each of the m exemplars.
#' @param ... Additional arguments passed on to predict()
#' @return An FFTrees object
#' @export

predict.FFTrees <- function(
  object = NULL,
  data = NULL,
  ...
) {

  new.result <- FFTrees(data.test = data,
                        data = object$data$train,
                        object = object
  )

  return(new.result)

}
