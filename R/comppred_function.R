#' Wrapper for classfication algorithms
#'
#' This function is a wrapper for many classification algorithms such as CART (rpart::rpart), logistic regression (glm), support vector machines (svm::svm) and random forests (randomForest::randomForest)
#'
#' @param formula a formula
#' @param data.train dataframe. A training dataset
#' @param data.test dataframe. A testing dataset
#' @param algorithm string. An algorithm in the set "lr" -- logistic regression, cart" -- decision trees, "rlr" -- regularised logistic regression, "svm" -- support vector machines, "rf" -- random forests
#' @param model model. An optional existing model applied to test data
#' @param new.factors string. What should be done if new factor values are discovered in the test set? "exclude" = exclude (i.e.; remove these cases), "base" = predict the base rate of the criterion.
#' @importFrom stats model.frame formula glm model.matrix
#' @importFrom e1071 svm
#' @importFrom rpart rpart
#' @importFrom randomForest randomForest
#' @export
#' @examples
#'
#' # Fit many alternative algorithms to the mushrooms dataset
#'
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
                     algorithm = NULL,
                     model = NULL,
                     new.factors = "exclude") {


#
#   formula = formula
#   data.train = data.train
#   data.test = data.test
#   algorithm = "lr"
#   model = model
#   new.factors = "exclude"

  if(is.null(formula)) {stop("You must enter a valid formula")}
  if(is.null(algorithm)) {stop("You must specify one of the following models: 'rlr', 'lr', 'cart', 'svm', 'rf'")}

  # SETUP
  {

  if(is.null(data.test) & (is.null(data.train) == FALSE)) {

    data.all <- data.train
    train.cases <- 1:nrow(data.train)
    test.cases <- c()

  }

  if(is.null(data.test) == FALSE & (is.null(data.train) == FALSE)) {

    data.all <- rbind(data.train, data.test)
    train.cases <- 1:nrow(data.train)
    test.cases <- (nrow(data.train) + 1):nrow(data.all)

  }

  if(is.null(data.train) & is.null(data.test) == FALSE) {

    data.all <- data.test
    train.cases <- c()
    test.cases <- 1:nrow(data.all)
  }


  data.all <- model.frame(formula = formula,
                          data = data.all)

  train.crit <- data.all[train.cases,1]

  # Remove columns with no variance in training data
if(is.null(data.train) == FALSE) {

  if(length(unique(data.all[train.cases,1])) == 1) {do.test <- FALSE} else {do.test <- TRUE}

} else {do.test <- TRUE}

  }

  if(do.test) {

    if(is.null(train.cases) == FALSE) {

  ok.cols <- sapply(1:ncol(data.all), FUN = function(x) {length(unique(data.all[train.cases,x])) > 1})
  data.all <- data.all[,ok.cols]

    }

  # Convert character columns to factors
  for(col.i in 1:ncol(data.all)) {

    if(any(c("logical", "character", "factor") %in% class(data.all[,col.i]))) {

      data.all[,col.i] <- factor(data.all[,col.i])}

  }
  }

  # Get data, cue, crit objects

  if(is.null(train.cases) == FALSE) {
  data.train <- data.all[train.cases,]
  cue.train <- data.all[train.cases, -1]
  crit.train <- data.all[train.cases, 1]

  } else {

    data.train <- NULL
    cue.train <- NULL
    crit.train <- NULL
  }


  # Build models and get training data

  if(algorithm == "lr") {

    if(is.null(model)) {

    train.mod <- suppressWarnings(glm(formula, data.train, family = "binomial"))

    } else {train.mod <- model}

    if(is.null(data.train) == FALSE) {
    pred.train <- suppressWarnings(round(1 / (1 + exp(-predict(train.mod, data.train))), 0))

    } else {pred.train <- NULL}

  }

  if(algorithm == "svm") {

    if(is.null(model)) {
    train.mod <- e1071::svm(formula,
                            data = data.train, type = "C")
    } else {train.mod <- model}

    if(is.null(data.train) == FALSE) {
    pred.train <- predict(train.mod,
                          data = data.train)

    } else {pred.train <- NULL}


  }

  if(algorithm == "cart") {

    if(is.null(model)) {
    # Create new CART model
    train.mod <- rpart::rpart(formula,
                              data = data.train,
                              method = "class"
    ) } else {train.mod <- model}


    if(is.null(data.train) == FALSE) {
    pred.train <- predict(train.mod,
                          data.train,
                          type = "class")} else {
                            pred.train <- NULL
                          }


  }

  if(algorithm == "rf") {


    if(is.null(model)) {
      data.train[,1] <- factor(data.train[,1])

    train.mod <- randomForest::randomForest(formula,
                                            data = data.train)
    } else {train.mod <- model}

    if(is.null(data.train) == FALSE) {
    # Get training decisions
    pred.train <- predict(train.mod,
                          data.train)

    } else {pred.train <- NULL}
  }

  # Get testing data

  if(is.null(data.test) == FALSE) {

    data.test <- data.all[test.cases, ]
    cue.test <- data.all[test.cases, -1]
    crit.test <- data.all[test.cases, 1]

    # Check for new factor values
    {
if(is.null(train.cases) == FALSE) {
    factor.ls <- lapply(1:ncol(data.train), FUN = function(x) {unique(data.train[,x])})

} else {

  factor.ls <- lapply(1:ncol(data.test), FUN = function(x) {

    cue.x <- names(data.test)[x]

    if(cue.x %in% names(model$xlevels)) {return(model$xlevels[[cue.x]])} else {

      return(unique(data.test[,x]))

    }
  })
}

    cannot.pred.mtx <- matrix(0, nrow = nrow(data.test), ncol = ncol(data.test))

    for(i in 1:ncol(cannot.pred.mtx)) {

      if(any(class(data.test[,i]) %in% c("factor", "character"))) {

        cannot.pred.mtx[,i] <- data.test[,i] %in% factor.ls[[i]] == FALSE

      }

    }

    cannot.pred.v <- rowSums(cannot.pred.mtx) > 0

    if(any(cannot.pred.v)) {

    if(substr(new.factors, 1, 1) == "e") {

      warning(paste(sum(cannot.pred.v), "cases in the test data could not be predicted by due to new factor values. These cases will be excluded"))

      data.test <- data.test[cannot.pred.v == FALSE,]
      cue.test <- cue.test[cannot.pred.v == FALSE,]
      crit.test <- crit.test[cannot.pred.v == FALSE]

    }

   if(substr(new.factors, 1, 1) == "b") {

     warning(paste(sum(cannot.pred.v), "cases in the test data could not be predicted by  due to new factor values. They will be predicted to be", mean(train.crit) > .5))

   }
    }
}

  # Get pred.test from each model

  if(algorithm == "lr") {

    if(is.null(data.test) == FALSE) {

      if(any(cannot.pred.v) & substr(new.factors, 1, 1) == "b") {

        pred.test <- rep(0, nrow(data.test))
        pred.test[cannot.pred.v] <- mean(train.crit) > .5
        pred.test[cannot.pred.v == FALSE] <- round(1 / (1 + exp(-predict(train.mod, data.test[cannot.pred.v == FALSE,]))), 0)

      } else {

        pred.test <- round(1 / (1 + exp(-predict(train.mod, data.test[cannot.pred.v == FALSE,]))), 0)

      }


    } else {pred.test <- NULL}


  }

  if(algorithm == "svm") {

    if(is.null(data.test) == FALSE) {

      # See if we can do any predictions

      try.pred <- try(predict(train.mod, data.test), silent = TRUE)

      if(class(try.pred) == "try-error") {
        warning("svm crashed predicting new data. That's all I can say")

        pred.test <- NULL

      } else {

        pred.test <- predict(train.mod, data.test)
      }


    } else {pred.test <- NULL}

  }

  # if(algorithm == "rlr") {
  #
  #   # Set up training data with factor matrix
  #
  #   col.classes <- sapply(1:ncol(data.train), FUN = function(x) {class(data.train[,x])})
  #   fact.names <- names(data.train)[col.classes == "factor"]
  #   crit.name <- names(data.train)[1]
  #
  #   fact.formula <- as.formula(paste(crit.name, "~", paste(fact.names, collapse = "+")))
  #   xfactors <- suppressWarnings(model.matrix(object = fact.formula, data = data.train)[,-1])
  #   x.train <- suppressWarnings(as.matrix(data.frame(data.train[,col.classes %in% c("numeric", "integer", "logical")], xfactors))[,-1])
  #
  #   # Create new rLR model
  #   train.mod <- try(train.mod <-  suppressWarnings(glmnet::cv.glmnet(x.train,
  #                                                       y = as.factor(data.train[,1]),
  #                                                       family = "binomial"
  #   )), silent = TRUE)
  #
  #
  #   if(class(train.mod) == "try-error") {
  #
  #     pred.train <- NULL
  #
  #     }
  #
  #   if(class(train.mod) == "cv.glmnet") {
  #
  #   pred.train <- round(as.vector(predict(train.mod,
  #                                         x.train,
  #                                         type = "response",
  #                                         a = train.mod$lambda.min)), 0)
  #
  #  }
  #
  #   if(is.null(data.test) == FALSE & class(train.mod) == "cv.glmnet") {
  #
  #     # Set up test data with factor matrix
  #
  #     col.classes <- sapply(1:ncol(data.test), FUN = function(x) {class(data.test[,x])})
  #     fact.names <- names(data.test)[col.classes == "factor"]
  #     crit.name <- names(data.test)[1]
  #
  #     fact.formula <- as.formula(paste(crit.name, "~", paste(fact.names, collapse = "+")))
  #     xfactors <- suppressWarnings(model.matrix(object = fact.formula, data = data.test)[,-1])
  #     x.test <- suppressWarnings(as.matrix(data.frame(data.test[,col.classes %in% c("numeric", "integer", "logical")], xfactors))[,-1])
  #
  #     pred.test <- round(as.vector(predict(train.mod,
  #                                          x.test,
  #                                          type = "response",
  #                                          a = train.mod$lambda.min)), 0)
  #
  #
  #   } else {pred.test <- NULL}
  #
  #
  # }

  if(algorithm == "cart") {


    if(is.null(data.test) == FALSE) {

      if(any(cannot.pred.v) & substr(new.factors, 1, 1) == "b") {

        pred.test <- rep(0, nrow(data.test))
        pred.test[cannot.pred.v] <- mean(train.crit) > .5
        pred.test[cannot.pred.v == FALSE] <- predict(train.mod, data.test[cannot.pred.v == FALSE,], type = "class")

      } else {

        pred.test <- predict(train.mod, data.test[cannot.pred.v == FALSE,], type = "class")

      }



      } else {pred.test <- NULL}


  }

  if(algorithm == "rf") {

    if(is.null(data.test) == FALSE) {

      crit.test <- as.factor(crit.test)

      # Get levels of training criterion
      train.crit.levels <- levels(train.mod$y)

      # convert test crit to factor
      crit.name <- paste(formula)[2]

      data.test.2 <- data.test
      data.test.2[crit.name] <- factor(data.test.2[[crit.name]], levels = train.crit.levels)


      # See if we can do any predictions

      try.pred <- try(predict(train.mod, data.test.2), silent = TRUE)

      if(class(try.pred) == "try-error") {
        warning("randomForest crashed predicting new data. That's all I can say")

        pred.test <- NULL

      } else {

        pred.test <- predict(train.mod, data.test)
      }

      # if(any(cannot.pred.v) & substr(new.factors, 1, 1) == "b") {
      #
      #   pred.test <- rep(0, nrow(data.test))
      #   pred.test[cannot.pred.v] <- mean(train.crit) > .5
      #   pred.test[cannot.pred.v == FALSE] <- predict(train.mod, data.test[cannot.pred.v == FALSE,])
      #
      # } else {
      #
      #   pred.test <- predict(train.mod, data.test[cannot.pred.v == FALSE,])
      #
      # }



    } else {pred.test <- NULL}
  }

} else {pred.test <- NULL}

  # Convert predictions to logical if necessary

  if(is.null(pred.train) == FALSE) {

  if("TRUE" %in% paste(pred.train)) {pred.train <- as.logical(paste(pred.train))}
  if("1" %in% paste(pred.train)) {pred.train <- as.logical(as.numeric(paste(pred.train)))}

    # Calculate training accuracy stats

    acc.train <- classtable(prediction.v = pred.train,
                           criterion.v = crit.train)

  }

  if(is.null(pred.train)) {

    acc.train <- classtable(c(1, 1, 0), c(0, 0, 1))
    acc.train[1,] <- NA

  }

  if( is.null(pred.test) == FALSE) {

    if("TRUE" %in% paste(pred.test)) {pred.test <- as.logical(paste(pred.test))}
    if("1" %in% paste(pred.test)) {pred.test <- as.logical(as.numeric(paste(pred.test)))}


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
  acc <- acc[order(acc$spec.train),]

  # CALCULATE AUC

  if(is.null(data.train) == FALSE & do.test == TRUE) {

    auc.train <- FFTrees::auc(sens.v = acc$sens.train,
                              spec.v = acc$spec.train)

  } else {

    auc.train <- NA

  }

  if(is.null(data.test) == FALSE & do.test == TRUE) {

    auc.test <- FFTrees::auc(sens.v = acc$sens.test, spec.v = acc$spec.test)

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
