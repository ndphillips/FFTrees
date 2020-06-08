
# Apply break function
#   Takes a direction, threshold value, and cue vector, and returns a vector of decisions
apply.break <- function(direction,
                        threshold.val,
                        cue.v,
                        cue.class
) {

  testthat::expect_true(direction %in% c("!=", "=", "<", "<=", ">", ">="))
  testthat::expect_true(length(threshold.val) == 1)

  # direction = cue_direction_new
  # threshold.val = cue_threshold_new
  # cue.v = data_current[[cues_name_new]]
  # cue.class = cue_class_new
  #

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

  cue_df <- data_mf[,2:ncol(data_mf), drop = FALSE]
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
#   formula = x$formula
#   data.train = x$data$train
#   data.test = x$data$test
#   algorithm = "lr"
#   model = NULL
#

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

  pred.test <- NULL

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

        pred.test <- rep(0, nrow(data.test))

        if(any(cannot.pred.v) & substr(new.factors, 1, 1) == "b") {

          pred.test[cannot.pred.v] <- mean(train.crit) > .5
          pred.test[cannot.pred.v == FALSE] <- round(1 / (1 + exp(-predict(train.mod, data.test[cannot.pred.v == FALSE,]))), 0)

        } else {

          pred.test[!cannot.pred.v] <- round(1 / (1 + exp(-predict(train.mod, data.test[cannot.pred.v == FALSE,]))), 0)

        }


      }

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


      }

    }

    if(algorithm == "cart") {


      if(is.null(data.test) == FALSE) {

        if(any(cannot.pred.v) & substr(new.factors, 1, 1) == "b") {

          pred.test <- rep(0, nrow(data.test))
          pred.test[cannot.pred.v] <- mean(train.crit) > .5
          pred.test[cannot.pred.v == FALSE] <- predict(train.mod, data.test[cannot.pred.v == FALSE,], type = "class")

        } else {

          pred.test <- predict(train.mod, data.test[cannot.pred.v == FALSE,], type = "class")

        }



      }


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

      }
    }

  }

  # Convert predictions to logical if necessary

  if(is.null(pred.train) == FALSE) {

    if("TRUE" %in% paste(pred.train)) {pred.train <- as.logical(paste(pred.train))}
    if("1" %in% paste(pred.train)) {pred.train <- as.logical(as.numeric(paste(pred.train)))}

    # Calculate training accuracy stats

    acc.train <- classtable(prediction_v = as.logical(pred.train),
                            criterion_v = as.logical(crit.train))

  }

  if(is.null(pred.train)) {

    acc.train <- classtable(c(TRUE, TRUE, FALSE), c(FALSE, FALSE, TRUE))
    acc.train[1,] <- NA

  }

  if(is.null(pred.test) == FALSE) {

    if("TRUE" %in% paste(pred.test)) {pred.test <- as.logical(paste(pred.test))}
    if("1" %in% paste(pred.test)) {pred.test <- as.logical(as.numeric(paste(pred.test)))}


    acc.test <- classtable(prediction_v = as.logical(pred.test),
                                    criterion_v =  as.logical(crit.test))
  } else {

    acc.test <- classtable(prediction_v = c(TRUE, FALSE, TRUE),
                                     criterion_v = c(TRUE, TRUE, FALSE))
    acc.test[1,] <- NA

  }


  if(do.test == FALSE) {

    acc.train <- classtable(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, TRUE))
    acc.train[1, ] <- NA

    acc.test <- classtable(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, TRUE))
    acc.test[1, ] <- NA
  }

  # ORGANIZE

  output <- list("accuracy" = list(train = acc.train, test = acc.test),
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

#' Adds decision statistics to a dataframe containing hr, cr, mi and fa
#'
#' @param data dataframe. With named (integer) columns hi, cr, mi, fa
#' @param sens.w numeric. Sensitivity weight
#' @param cost.each numeric. An optional fixed cost added to all outputs (e.g.; the cost of the cue)
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying the costs of a hit, false alarm, miss, and correct rejection rspectively. E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a false alarm and miss cost 10 and 20 respectively while correct decisions have no cost.
Add_Stats <- function(data,
                      sens.w = .5,
                      cost.each = NULL,
                      cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0)) {

  if(is.null(cost.each)) {cost.each <- 0}

  # Accuracy
  data$acc <- with(data, (hi + cr) / (hi + cr + fa + mi))

  # Sensitivity
  data$sens <- with(data, hi / (hi + mi))

  # Specificity
  data$spec <- with(data, cr / (cr + fa))

  # Negative Predictive Value
  data$npv <- with(data, cr / (cr + mi))

  # Positive Predictive Value
  data$ppv <- with(data, hi / (hi + fa))

  # False alarm rate
  data$far <- with(data, 1 - spec)

  # Balanced Accuracy
  data$bacc <- with(data, sens * .5 + spec * .5)

  # Weighted Accuracy
  data$wacc <- with(data, sens * sens.w + spec * (1 - sens.w))

  # Outcome Cost
  data$cost_decisions <- with(data, -1 * (hi * cost.outcomes$hi + fa * cost.outcomes$fa + mi * cost.outcomes$mi + cr * cost.outcomes$cr)) / data$n

  # Total Cost
  data$cost <- data$cost_decisions - cost.each

  # reorder
  data <- data[, c("sens", "spec", "far", "ppv", "npv", "acc", "bacc", "wacc", "cost_decisions", "cost")]

  return(data)

}


#' Calculates several classification statistics from binary prediction and criterion (e.g.; truth) vectors
#' @param prediction_v logical. A logical vector of predictions
#' @param criterion_v logical A logical vector of criterion (true) values
#' @param sens.w numeric. Weight given to sensitivity, must range from 0 to 1.
#' @param cost.v list. An optional list of additional costs to be added to each case.
#' @param correction numeric. Correction added to all counts for calculating dprime
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying the costs of a hit, false alarm, miss, and correct rejection rspectively. E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a false alarm and miss cost 10 and 20 respectively while correct decisions have no cost.
#' @param na_prediction_action not sure.
#' @importFrom stats qnorm
#' @importFrom caret confusionMatrix

classtable <- function(prediction_v = NULL,
                       criterion_v,
                       sens.w = .5,
                       cost.v = NULL,
                       correction = .25,
                       cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0),
                       na_prediction_action = "ignore") {
  #
  #
  #   prediction_v <- sample(c(TRUE, FALSE), size = 20, replace = TRUE)
  #   criterion_v <- sample(c(TRUE, FALSE), size = 20, replace = TRUE)
  #   sens.w = .5
  #   cost.v = NULL
  #   correction = .25
  #   cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0)

  if(is.null(cost.v)) {cost.v <- rep(0, length(prediction_v))}

  if(any(c("FALSE", "TRUE") %in% paste(prediction_v))) {

    prediction_v <- as.logical(paste(prediction_v))

  }

  if(any(c("FALSE", "TRUE") %in% paste(criterion_v))) {

    criterion_v <- as.logical(paste(criterion_v))

  }

  if(((class(prediction_v) != "logical") | class(criterion_v) != "logical") & !is.null(prediction_v)) {stop("prediction_v and criterion_v must be logical")}



  # Remove NA criterion values
  prediction_v <- prediction_v[is.finite(criterion_v)]
  criterion_v <- criterion_v[is.finite(criterion_v)]

  # Remove NA prediction values

  # if(na_prediction_action == "ignore") {
  #
  #   bad_index <- !is.finite(prediction_v)
  #
  #   prediction_v <- prediction_v[-bad_index]
  #   criterion_v <- criterion_v[-bad_index]
  #
  #
  # }

  N <- min(length(criterion_v), length(prediction_v))

  if(N > 0) {

    if(var(prediction_v) > 0 & var(criterion_v) > 0) {


      if(length(prediction_v) != length(criterion_v)) {

        stop("length of prediction_v is", length(prediction_v), "and length of criterion_v is ",
             length(criterion_v))
      }

      cm <- caret::confusionMatrix(table(prediction_v, criterion_v),
                                   positive = "TRUE")

      cm_byClass <- data.frame(as.list(cm$byClass))
      cm_overall <- data.frame(as.list(cm$overall))

      hi <- cm$table[2, 2]
      mi <- cm$table[1, 2]
      fa <- cm$table[2, 1]
      cr <- cm$table[1, 1]

      # Corrected values
      hi_c <- hi + correction
      mi_c <- mi + correction
      fa_c <- fa + correction
      cr_c <- cr + correction

      # Statistics
      sens <- cm_byClass$Sensitivity
      spec <- cm_byClass$Specificity
      far <- 1 - spec
      acc <-  cm_overall$Accuracy
      acc_p <- cm_overall$AccuracyPValue
      ppv <- cm_byClass$Pos.Pred.Value
      npv <- cm_byClass$Neg.Pred.Value
      bacc <- cm_byClass$Balanced.Accuracy
      wacc <- cm_byClass$Sensitivity * sens.w + cm_byClass$Specificity * (1 - sens.w)
      dprime <- qnorm(hi_c / (hi_c + mi_c)) - qnorm(cr_c / (cr_c + fa_c))

      # cost per case
      cost_decisions <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr))) / N
      cost <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr)) + sum(cost.v)) / N

      # auc
      #
      #     auc <- as.numeric(pROC::roc(response = as.numeric(criterion_v),
      #                                 predictor = as.numeric(prediction_v))$auc)

    } else {

      hi <- sum(prediction_v == TRUE & criterion_v == TRUE)
      mi <- sum(prediction_v == FALSE & criterion_v == TRUE)
      fa <- sum(prediction_v == TRUE & criterion_v == FALSE)
      cr <- sum(prediction_v == FALSE & criterion_v == FALSE)

      # Corrected values
      hi_c <- hi + correction
      mi_c <- mi + correction
      fa_c <- fa + correction
      cr_c <- cr + correction

      # Statistics
      sens <- hi / (hi + mi)
      spec <- cr / (cr + fa)
      far <- 1 - spec
      acc <-  (hi + cr) / c(hi + cr + mi + fa)
      acc_p <- NA
      ppv <- hi / (hi + fa)
      npv <- cr / (cr + mi)
      bacc <- sens * .5 + spec * .5
      wacc <- sens * sens.w + spec * (1 - sens.w)
      dprime <- qnorm(hi_c / (hi_c + mi_c)) - qnorm(cr_c / (cr_c + fa_c))

      # cost per case
      cost_decisions <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr))) / N
      cost <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr)) + sum(cost.v)) / N

      # auc
      # auc <- as.numeric(pROC::roc(response = as.numeric(criterion_v),
      #                             predictor = as.numeric(prediction_v))$auc)

    }

  } else {

    hi <- NA
    mi <- NA
    fa <- NA
    cr <- NA
    sens <- NA
    spec <- NA
    far <- NA
    ppv <- NA
    npv <- NA
    far <- NA
    acc <- NA
    acc_p <- NA
    bacc <- NA
    wacc <- NA
    dprime <- NA
    cost_decisions <- NA
    cost <- NA
    # auc <- NA

  }

  result <- data.frame(
    n = N,
    hi = hi,
    mi = mi,
    fa = fa,
    cr = cr,
    sens = sens,
    spec = spec,
    far = far,
    ppv = ppv,
    npv = npv,
    acc = acc,
    acc_p = acc_p,
    # auc = auc,
    bacc = bacc,
    wacc = wacc,
    dprime = dprime,
    cost_decisions = cost_decisions,
    cost = cost)

  return(result)

}

console_confusionmatrix <- function(hi, mi, fa, cr, cost) {
#
# hi <- 6534
# mi <- 5
# fa <- 765
# cr <- 54
#   cost <- 0
#

num_space <- function(x) {

  if(x == 0) {return(1)}

  ceiling(log10(x)) + floor(log(x, base = 1000))

  }

col_width <- max(c(8, floor(log10(max(c(hi, mi, fa, cr)))) + floor(log(max(c(hi, mi, fa, cr)), base = 1000)) + 6))

# Header Row
cat("|", rep(" ", times = 9),
    "| True +",
    rep(" ", col_width - 7),
    "| True -",
    rep(" ", col_width - 7),
    "|\n", sep = "")

# Line
cat("|", rep("-", times = 9), "|", rep("-", times = col_width), "|", rep("-", times = col_width), "|\n", sep = "")

# Decide + Row
cat("|Decide +",
  " ",
    "| ",
    crayon::silver("hi "),
    crayon::green(scales::comma(hi)),
    rep(" ", max(0, col_width - num_space(hi) - 4)),
    "| ",
    crayon::silver("fa "),
    crayon::red(scales::comma(fa)),
    rep(" ", max(0, col_width - num_space(fa) - 4)),
    "| ", sep = "")
cat(scales::comma(hi + fa))

cat("\n")


# Decide - Row
cat("|Decide -",
    " ",
    "| ",
    crayon::silver("mi "),
    crayon::red(scales::comma(mi)),
    rep(" ", max(0, col_width - num_space(mi) - 4)),
    "| ",
    crayon::silver("cr "),
    crayon::green(scales::comma(cr)),
    rep(" ", max(0, col_width - num_space(cr) - 4)),
    "| ", sep = "")
cat(scales::comma(mi + cr))
cat("\n")

# Bottom line
cat("|-", rep("-", times = 8), "|", rep("-", times = col_width), "|", rep("-", times = col_width), "|\n", sep = "")

# Bottom total
cat(rep(" ", times = 12), sep = "")
cat(scales::comma(hi + mi))
cat(rep(" ", times = col_width - num_space(hi + mi)), " ", sep = "")
cat(scales::comma(cr + fa))

cat(rep(" ", col_width - num_space(cr + fa) + 1), sep = "")
cat("N = ")
cat(crayon::underline(scales::comma(hi + mi + fa + cr), sep = ""), sep = "")
cat("\n\n")

cat("acc  =", scales::percent((hi + cr) / (hi + mi + cr + fa), accuracy = .1), sep = " ")

cat("  ppv  =", scales::percent(hi / (hi + fa), accuracy = .1), sep = " ")
cat("  npv  =", scales::percent(cr / (cr + mi), accuracy = .1), sep = " ")
cat("\n")

cat("bacc =", scales::percent((hi / (hi + mi) + cr / (cr + fa)) / 2, accuracy = .1), sep = " ")

cat("  sens =", scales::percent(hi / (hi + mi), accuracy = .1), sep = " ")
cat("  spec =", scales::percent(cr / (cr + fa), accuracy = .1), sep = " ")
cat("\n")
cat("E(cost) =", scales::comma(cost, accuracy = .001), sep = " ")

# cat("br   =", scales::percent((hi + mi) / (hi + cr + mi + fa), accuracy = .1), sep = " ")
# cat("\n")

}


#' \code{FFTrees} package
#'
#' Create fast and frugal trees
#'
#' @docType package
#' @name FFTrees
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "tree", "tree_new", "tree", "level"))








