#' Wrapper for classfication algorithms
#'
#' This function is a wrapper for many classification algorithms such as CART (rpart::rpart), regularised logistic regression (glmnet::glmnet), support vector machines (svm::svm) and random forests (randomForest::randomForest)
#'
#' @param formula a formula
#' @param data.train dataframe. A training dataset
#' @param data.test dataframe. A testing dataset
#' @param algorithm string. An algorithm in the set "cart" -- decision trees, "lr" -- regularised logistic regression, "svm" -- support vector machines, "rf" -- random forests
#' @importFrom stats model.frame formula glm model.matrix
#' @importFrom e1071 svm
#' @importFrom rpart rpart
#' @importFrom glmnet glmnet
#' @importFrom randomForest randomForest
#' @export
#' @examples
#'
#' # Fit many alternative algorithms to the mushrooms dataset
#'
#' mushrooms.lr.pred <- comp.pred(formula = poisonous ~.,
#'                                data.train = mushrooms[1:100,],
#'                                data.test = mushrooms[101:nrow(mushrooms),],
#'                                algorithm = "lr")
#'
#' mushrooms.cart.pred <- comp.pred(formula = poisonous ~.,
#'                                data.train = mushrooms[1:100,],
#'                                data.test = mushrooms[101:nrow(mushrooms),],
#'                                algorithm = "cart")
#'
#' mushrooms.rf.pred <- comp.pred(formula = poisonous ~.,
#'                                data.train = mushrooms[1:100,],
#'                                data.test = mushrooms[101:nrow(mushrooms),],
#'                                algorithm = "rf")
#'
#' mushrooms.svm.pred <- comp.pred(formula = poisonous ~.,
#'                                data.train = mushrooms[1:100,],
#'                                data.test = mushrooms[101:nrow(mushrooms),],
#'                                algorithm = "svm")
#'
#'
#'

comp.pred <- function(formula,
                     data.train,
                     data.test = NULL,
                     algorithm = NULL) {


  if(is.null(formula)) {stop("You must enter a valid formula")}
  if(is.null(algorithm)) {stop("You must specify one of the following models: 'lr', 'cart', 'svm', 'rf'")}

  # SETUP
  {

  if(is.null(data.test)) {

    data.all <- data.train
    train.cases <- 1:nrow(data.train)
    test.cases <- c()

  }

  if(is.null(data.test) == FALSE) {

    data.all <- rbind(data.train, data.test)
    train.cases <- 1:nrow(data.train)
    test.cases <- (nrow(data.train) + 1):nrow(data.all)

  }

  data.all <- model.frame(formula = formula,
                          data = data.all)

  # Remove columns with no variance in training data

  if(length(unique(data.all[train.cases,1])) == 1) {do.test <- FALSE} else {do.test <- TRUE}
  }
  
  if(do.test) {
  
  ok.cols <- sapply(1:ncol(data.all), FUN = function(x) {length(unique(data.all[train.cases,x])) > 1})
  data.all <- data.all[,ok.cols]

  # Convert character columnns to factors
  for(col.i in 1:ncol(data.all)) {

    if(any(c("logical", "character", "factor") %in% class(data.all[,col.i]))) {

      data.all[,col.i] <- factor(data.all[,col.i])}

  }

  # Get data, cue, crit objects

  data.train <- data.all[train.cases,]
  cue.train <- data.all[train.cases, -1]
  crit.train <- data.all[train.cases, 1]

  if(is.null(data.test) == FALSE) {

    data.test <- data.all[test.cases,]
    cue.test <- data.all[test.cases,-1]
    crit.test <- data.all[test.cases,1]

  }
  

  # Get pred.train, pred.test from each model

  if(algorithm == "svm") {

    train.mod <- e1071::svm(formula,
                            data = data.train, type = "C")

    pred.train <- predict(train.mod,
                          data = data.train)


    if(is.null(data.test) == FALSE) {

    # Get training decisions
    pred.test <- predict(train.mod,
                             data.test)


    } else {pred.test <- NULL}

  }

  if(algorithm == "lr") {

    # Set up training data with factor matrix

    col.classes <- sapply(1:ncol(data.train), FUN = function(x) {class(data.train[,x])})
    fact.names <- names(data.train)[col.classes == "factor"]
    crit.name <- names(data.train)[1]

    fact.formula <- as.formula(paste(crit.name, "~", paste(fact.names, collapse = "+")))
    xfactors <- suppressWarnings(model.matrix(object = fact.formula, data = data.train)[,-1])
    x.train <- suppressWarnings(as.matrix(data.frame(data.train[,col.classes %in% c("numeric", "integer", "logical")], xfactors))[,-1])

    # Create new LR model
    train.mod <- try(train.mod <-  suppressWarnings(glmnet::cv.glmnet(x.train,
                                                        y = as.factor(data.train[,1]),
                                                        family = "binomial"
    )), silent = TRUE)


    if(class(train.mod) == "try-error") {

      pred.train <- NULL

      }

    if(class(train.mod) == "cv.glmnet") {

    pred.train <- round(as.vector(predict(train.mod,
                                          x.train,
                                          type = "response",
                                          a = train.mod$lambda.min)), 0)

   }

    if(is.null(data.test) == FALSE & class(train.mod) == "cv.glmnet") {

      # Set up test data with factor matrix

      col.classes <- sapply(1:ncol(data.test), FUN = function(x) {class(data.test[,x])})
      fact.names <- names(data.test)[col.classes == "factor"]
      crit.name <- names(data.test)[1]

      fact.formula <- as.formula(paste(crit.name, "~", paste(fact.names, collapse = "+")))
      xfactors <- suppressWarnings(model.matrix(object = fact.formula, data = data.test)[,-1])
      x.test <- suppressWarnings(as.matrix(data.frame(data.test[,col.classes %in% c("numeric", "integer", "logical")], xfactors))[,-1])

      pred.test <- round(as.vector(predict(train.mod,
                                           x.test,
                                           type = "response",
                                           a = train.mod$lambda.min)), 0)


    } else {pred.test <- NULL}


  }

  if(algorithm == "cart") {

    # Create new CART model
    train.mod <- rpart::rpart(formula,
                                   data = data.train,
                                   method = "class"
    )

    pred.train <- predict(train.mod,
                          data.train,
                          type = "class")



    if(is.null(data.test) == FALSE) {

      pred.test <- predict(train.mod,
                           data.test,
                           type = "class")



      } else {pred.test <- NULL}


  }

  if(algorithm == "rf") {

    data.train[,1] <- factor(data.train[,1])

    train.mod <- randomForest::randomForest(formula,
                                            data = data.train
    )

    # Get training decisions
    pred.train <- predict(train.mod,
                          data.train)


    if(is.null(data.test) == FALSE) {

      crit.test <- factor(data.test[,1])

      pred.test <- predict(train.mod,
                           data.test)

    } else {pred.test <- NULL}

  }
}
  
  # Convert predictions to logical if necessary

  if(is.null(pred.train) == FALSE) {

  if("TRUE" %in% paste(pred.train)) {pred.train <- as.logical(paste(pred.train))}
  if("1" %in% paste(pred.train)) {pred.train <- as.logical(as.numeric(paste(pred.train)))}

  if("TRUE" %in% paste(pred.test)) {pred.test <- as.logical(paste(pred.test))}
  if("1" %in% paste(pred.test)) {pred.test <- as.logical(as.numeric(paste(pred.test)))}

    # Calculate training accuracy stats

    acc.train <- classtable(prediction.v = pred.train,
                           criterion.v = crit.train)

  }

  if(is.null(pred.train)) {

    acc.train <- classtable(c(1, 1, 0), c(0, 0, 1))
    acc.train[1,] <- NA

  }

  if(is.null(pred.train) == FALSE & is.null(pred.test) == FALSE) {

    acc.test <- classtable(prediction.v = pred.test,
                           criterion.v = crit.test)
  } else {

    acc.test <- classtable(prediction.v = 1,
                               criterion.v = 1)
    acc.test[1,] <- NA

  }


  if(do.test == FALSE) {
    
    acc.train <- classtable(c(1, 0, 1), c(0, 1, 1))
    acc.train[1, ] <- NA
    
    acc.test <- classtable(c(1, 0, 1), c(0, 1, 1))
    acc.test[1, ] <- NA
  }
    
  # ORGANIZE

  names(acc.train) <- paste(names(acc.train), ".train", sep = "")
  names(acc.test) <- paste(names(acc.test), ".test", sep = "")

  acc <- cbind(acc.train, acc.test)
  acc <- acc[order(acc$far.train),]

  # CALCULATE AUC

  if(is.null(data.train) == FALSE & do.test == TRUE) {

    auc.train <- FFTrees::auc(hr.v = acc$hr.train, far.v = acc$far.train)

  } else {

    auc.train <- NA

  }

  if(is.null(data.test) == FALSE & do.test == TRUE) {

    auc.test <- FFTrees::auc(hr.v = acc$hr.test, far.v = acc$far.test)

  } else {

    auc.test <- NA

  }

  model.auc <- matrix(c(auc.train, auc.test), nrow = 2, ncol = 1)
  colnames(model.auc) <- algorithm
  rownames(model.auc) <- c("train", "test")

  

  output <- list("accuracy" = acc,
                 "auc" = model.auc,
                 "model" = train.mod,
                 "algorithm" = algorithm)

  return(output)

}
