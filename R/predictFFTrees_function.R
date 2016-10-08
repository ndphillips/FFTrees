#' Applies an existing FFTrees object to a new (test) data set
#'
#' @param object (M) An FFTrees object created from the FFTrees() function.
#' @param data.test (M) A dataframe of test data
#' @param ... Additional arguments passed on to predict()
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
#'   # Currently the object only contains training data
#'   breastcancer.fft
#'
#'   # Now add the rest of the dataset as test data using predict
#'   #
#'   breastcancer.fft <- predict(breastcancer.fft,
#'   data.test = breastcancer[301:nrow(breastcancer),])
#'
#'   # Now the new data are stored as test data
#'   breastcancer.fft
#'
#'
#'

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
