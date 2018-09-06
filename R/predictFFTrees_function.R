#' Predict classifications from newdata using an FFTrees object
#'
#' @param object An FFTrees object created from the FFTrees() function.
#' @param newdata dataframe. A dataframe of test data
#' @param tree integer. Which tree in the object should be used? By default, tree = 1 is used
#' @param type string. What should be predicted? Can be \code{"class"}, which returns a vector of class predictions, or \code{"prob"} which returns a matrix of class probabilities.
#' @param method string. Method of calculating class probabilities. Either 'laplace', which applies the Laplace correction, or 'raw' which applies no correction.
#' @param ... Additional arguments passed on to \code{predict()}
#' @param sens.w,data depricated
#' @return Either a logical vector of predictions, or a matrix of class probabilities.
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
#'  # Predict classes of test data
#'   breast.fft.pred <- predict(breast.fft,
#'                              data = breast.test)
#'
#'  # Predict class probabilities
#'   breast.fft.pred <- predict(breast.fft,
#'                              data = breast.test,
#'                              type = "prob")
#'

predict.FFTrees <- function(
  object = NULL,
  newdata = NULL,
  data = NULL,
  tree = 1,
  type = "class",
  sens.w = NULL,
  method = "laplace",
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

  new.apply.tree <-  apply.tree(formula = object$formula,
                                data = data,
                                tree.definitions = object$tree.definitions)

  # Calculate predictions from tree
  predictions <- new.apply.tree$decision[,tree]

   if(type == "class") {output <- predictions}

if(type == "prob") {

output <- matrix(NA, nrow = nrow(data), ncol = 2)

# Get cumulative level stats for current tree in training data
levelstats.c <- object$level.stats$train[object$level.stats$train$tree == tree,]
levels.n <- nrow(levelstats.c)

# Get marginal counts for levels

n.m <- levelstats.c$n - c(0, levelstats.c$n[1:(levels.n - 1)])
hi.m <- levelstats.c$hi - c(0, levelstats.c$hi[1:(levels.n - 1)])
mi.m <- levelstats.c$mi - c(0, levelstats.c$mi[1:(levels.n - 1)])
fa.m <- levelstats.c$fa - c(0, levelstats.c$fa[1:(levels.n - 1)])
cr.m <- levelstats.c$cr - c(0, levelstats.c$cr[1:(levels.n - 1)])


npv.m <- cr.m / (cr.m + mi.m)
ppv.m <- hi.m / (hi.m + fa.m)

# Laplace correction
npv.lp.m <- (cr.m + 1) / (cr.m + mi.m + 2)
ppv.lp.m <- (hi.m + 1) / (hi.m + fa.m + 2)

# Loop over levels
for(level.i in 1:levels.n) {

  decide.now.0.log <- new.apply.tree$levelout[,tree] == level.i &
                      new.apply.tree$decision[,tree] == 0

  decide.now.1.log <- new.apply.tree$levelout[,tree] == level.i &
                      new.apply.tree$decision[,tree] == 1

  if(method == "laplace") {

 output[decide.now.0.log, 1] <- npv.lp.m[level.i]
 output[decide.now.0.log, 2] <- 1 - npv.lp.m[level.i]

 output[decide.now.1.log, 2] <- ppv.lp.m[level.i]
 output[decide.now.1.log, 1] <- 1 - ppv.lp.m[level.i]

  }

  if(method == "raw") {

  output[decide.now.0.log, 1] <- npv.m[level.i]
  output[decide.now.0.log, 2] <- 1 - npv.m[level.i]

  output[decide.now.1.log, 2] <- ppv.m[level.i]
  output[decide.now.1.log, 1] <- 1 - ppv.m[level.i]

  }

}


}

  return(output)

}
