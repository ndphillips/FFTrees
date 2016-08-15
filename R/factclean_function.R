#' Calculates predictions from CART
#' @param formula a formula
#' @param data.train A training dataset
#' @param data.test A testing dataset
#' @importFrom stats model.frame formula glm
#' @importFrom rpart rpart
#' @export
#'


factclean <- function(
  data.train,
  data.test,
  show.warning = T
) {


  data.train <- data.frame("a" = c("a", "b", "a", "b"),
                           "b" = c(1, 1, 1, 1), stringsAsFactors= F)

  data.test <- data.frame("a" = c("a", "b", "c", "b"),
                           "b" = c(1, 1, 1, 1), stringsAsFactors= F)

  # Look for new factor values in test set not in training set

  orig.vals.ls <- lapply(1:ncol(data.train), FUN = function(x) {unique(data.train[,x])})

  can.predict.mtx <- matrix(1, nrow = nrow(data.test), ncol = ncol(data.test))

  for(i in 1:ncol(can.predict.mtx)) {

    test.vals.i <- data.test[,i]

    if(is.numeric(test.vals.i)) {
      can.predict.mtx[,i] <- 1} else {

        can.predict.mtx[,i] <- paste(test.vals.i) %in% paste(orig.vals.ls[[i]])


      }
  }

  model.can.predict <- rowMeans(can.predict.mtx) == 1

  if(mean(model.can.predict) != 1 & show.warning == T) {

    warning(paste(sum(model.can.predict), " out of ",
                  nrow(data.test), " cases (", round(sum(model.can.predict == 0) / length(model.can.predict), 2) * 100,
                  "%) were removed from the test dataset.", sep = ""))

  }

  output <- data.test[model.can.predict,]

  return(output)

}
