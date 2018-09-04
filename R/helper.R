
# Apply break function
#   Takes a direction, threshold value, and cue vector, and returns a vector of decisions
apply.break <- function(direction,
                        threshold.val,
                        cue.v,
                        cue.class
) {


  if(is.character(threshold.val)) {threshold.val <- unlist(strsplit(threshold.val, ","))}

  if(cue.class %in% c("numeric", "integer")) {threshold.val <- as.numeric(threshold.val)}


  if(direction == "!=") {output <- (cue.v %in% threshold.val) == FALSE}
  if(direction == "=") {output <- cue.v %in% threshold.val}
  if(direction == "<") {output <- cue.v < threshold.val}
  if(direction == "<=") {output <- cue.v <= threshold.val}
  if(direction == ">") {output <- cue.v > threshold.val}
  if(direction == ">=") {output <- cue.v >= threshold.val}


  return(output)

}


# Create cost.cues
cost.cues.append <- function(formula,
                             data,
                             cost.cues = NULL) {


  criterion_name <- paste(formula)[2]

  data_mf <- model.frame(formula = formula,
                         data = data)

  cue_df <- data_mf[,2:ncol(data_mf)]
  cue_name_v <- names(cue_df)


  if(is.null(cost.cues) == FALSE) {

    # Make sure all named cues in cost.cues are in data
    {
      cue.not.in.data <- sapply(names(cost.cues), FUN = function(x) {x %in% cue_name_v == FALSE})

      if(any(cue.not.in.data)) {

        missing.cues <- paste(cost.cues[cue.not.in.data, 1], collapse = ",")

        warning(paste0("The cue(s) {",missing.cues, "} specified in cost.cues are not present in the data."))

      }

    }

    # Add any missing cue costs as 0
    {
      cost.cues.o <- cost.cues

      cost.cues <- lapply(1:ncol(cue_df), FUN = function(x) {0})
      names(cost.cues) <- names(cue_df)

      for(i in 1:length(cost.cues)) {

        cue_name_i <- names(cost.cues)[i]

        if(names(cost.cues)[i] %in% names(cost.cues.o)) {

          cost.cues[[i]] <- cost.cues.o[[cue_name_i]]

        }

      }
    }

  }

  if(is.null(cost.cues)) {

    cost.cues <- lapply(1:ncol(cue_df), FUN = function(x) {0})
    names(cost.cues) <- names(cue_df)

  }

  return(cost.cues)


}

# text.outline
# adds text with a white background - taken from Dirk Wulff www.dirkwulff.org
text.outline <- function(x, y,
                         labels = 'test',
                         col = 'black',
                         font = 1,
                         bg = 'white',
                         r = 0.02,
                         h = 1,
                         w = 1,
                         cex = 1,
                         adj = .5,
                         pos = NULL){

  # Draw background
  is <- seq(0, 2 * pi, length = 72)
  for(i in is){
    xn = x + cos(i) * r * w
    yn = y + sin(i) * r * h
    text(xn, yn, labels = labels, col = bg, cex = cex, adj = adj, pos = pos, font = font)
  }

  # Foreground
  text(x, y, labels = labels, col = col, cex = cex, adj = adj, pos = pos, font = font)
}


# transparent
#  make text transparent
transparent <- function(orig.col = "red",
                        trans.val = .5)
{
  n.cols <- length(orig.col)
  orig.col <- col2rgb(orig.col)
  final.col <- rep(NA, n.cols)
  for (i in 1:n.cols) {
    final.col[i] <- rgb(orig.col[1, i], orig.col[2, i], orig.col[3,
                                                                 i], alpha = (1 - trans.val) * 255, maxColorValue = 255)
  }
  return(final.col)
}


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

    acc.train <- classtable(prediction.v = as.logical(pred.train),
                            criterion.v = as.logical(crit.train))

  }

  if(is.null(pred.train)) {

    acc.train <- classtable(c(TRUE, TRUE, FALSE), c(FALSE, FALSE, TRUE))
    acc.train[1,] <- NA

  }

  if( is.null(pred.test) == FALSE) {

    if("TRUE" %in% paste(pred.test)) {pred.test <- as.logical(paste(pred.test))}
    if("1" %in% paste(pred.test)) {pred.test <- as.logical(as.numeric(paste(pred.test)))}


    acc.test <- classtable(prediction.v = as.logical(pred.test),
                           criterion.v =  as.logical(crit.test))
  } else {

    acc.test <- classtable(prediction.v = c(TRUE, FALSE, TRUE),
                           criterion.v = c(TRUE, TRUE, FALSE))
    acc.test[1,] <- NA

  }


  if(do.test == FALSE) {

    acc.train <- classtable(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, TRUE))
    acc.train[1, ] <- NA

    acc.test <- classtable(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, TRUE))
    acc.test[1, ] <- NA
  }

  # ORGANIZE

  names(acc.train) <- paste(names(acc.train), ".train", sep = "")
  names(acc.test) <- paste(names(acc.test), ".test", sep = "")

  acc <- cbind(acc.train, acc.test)
  acc <- acc[order(acc$spec.train),]

  output <- list("accuracy" = acc,
                 "model" = train.mod,
                 "algorithm" = algorithm)

  return(output)

}


#' Does miscellaneous cleaning of prediction datasets
#' @param data.train A training dataset
#' @param data.test A testing dataset
#' @param show.warning ...
#'
factclean <- function(
  data.train,
  data.test,
  show.warning = T
) {


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

