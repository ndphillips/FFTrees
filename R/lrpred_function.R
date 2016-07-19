#' Calculates predictions from logistic regression
#' @param formula a formula
#' @param data.train A training dataset
#' @param data.test A testing dataset
#' @param thresholds A vector of decision thresholds
#' @importFrom stats model.frame formula glm
#' @export
#'


lr.pred <- function(
  formula,
  data.train,
  data.test = NULL,
  thresholds = seq(.9, .1, -.1)
) {
  correction <- .25

  # formula = formula
  # data.train = data
  # data.test = data.test
  #


  data.mf.train <- model.frame(formula = formula, data = data.train)
  crit.train <- data.mf.train[,1]

  if(is.null(data.test) == F) {

    data.mf.test <- model.frame(formula = formula, data = data.test)
    crit.test <- data.mf.test[,1]

  }

  # Training data
  {

    # Remove cues with only one value
    train.df.ex <- sapply(1:ncol(data.train), FUN = function(x) {length(unique(data.train[,x]))})
    data.train <- data.train[,train.df.ex > 1]

    # Run logistic regression on training set (can be time consuming....)

    lr.train.mod <- suppressWarnings(glm(formula,
                                         family = "binomial",
                                         data = data.train
    ))


    lr.train.predictions <- suppressWarnings(predict(lr.train.mod,
                                                     newdata = data.train))

    lr.train.predictions <- 1 / (1 + exp(-lr.train.predictions))
    lr.train.predictions.bin <- rep(0, length(lr.train.predictions))
    lr.train.predictions.bin[lr.train.predictions >= .5] <- 1


    lr.train.stats <- classtable(prediction.v = lr.train.predictions.bin,
                                 criterion.v = crit.train)


    lr.train.far.v <- rep(NA, length(thresholds))
    lr.train.hr.v <- rep(NA, length(thresholds))

    for(threshold.i in thresholds) {

      lr.train.stats.i <- classtable(prediction.v = lr.train.predictions >= threshold.i,
                                     criterion.v = crit.train)

      lr.train.hr.v[thresholds == threshold.i] <- lr.train.stats.i$hr
      lr.train.far.v[thresholds == threshold.i] <- lr.train.stats.i$far


    }

  }



  # Test data
  {
    if(is.null(data.test) == F) {


      # Look for new factor values in test set not in training set

      orig.vals.ls <- lapply(2:ncol(data.mf.train), FUN = function(x) {unique(data.mf.train[,x])})

      can.predict.mtx <- matrix(1, nrow = nrow(data.test), ncol = ncol(data.test) - 1)

      for(i in 1:ncol(can.predict.mtx)) {

        test.vals.i <- data.mf.test[,i + 1]

        if(is.numeric(test.vals.i)) {
          can.predict.mtx[,i] <- 1} else {

            can.predict.mtx[,i] <- paste(test.vals.i) %in% paste(orig.vals.ls[[i]])


          }
      }

      model.can.predict <- rowMeans(can.predict.mtx) == 1

      if(mean(model.can.predict) != 1) {

        warning(paste("Linear regression couldn't fit some testing data.", sum(model.can.predict), "out of",
                      nrow(data.test), "cases (", round(sum(model.can.predict == 0) / length(model.can.predict), 2) * 100,
                      "%) had to be ignored"))

      }

      lr.test.predictions <- suppressWarnings(predict(lr.train.mod,
                                                      newdata = data.test[model.can.predict,]))

      lr.test.predictions <- 1 / (1 + exp(-lr.test.predictions))
      lr.test.predictions.bin <- rep(0, length(lr.test.predictions))
      lr.test.predictions.bin[lr.test.predictions >= .5] <- 1

      lr.test.stats <- classtable(prediction.v = lr.test.predictions.bin,
                                  criterion.v = crit.test[model.can.predict])

      lr.test.stats$n.exemplars <- length(model.can.predict)
      lr.test.stats$n.exemplars.can.pred <- sum(model.can.predict)
      lr.test.stats$p.exemplars.can.pred <- sum(model.can.predict) / length(model.can.predict)



      lr.test.far.v <- rep(NA, length(thresholds))
      lr.test.hr.v <- rep(NA, length(thresholds))

      for(threshold.i in thresholds) {

        lr.test.stats.i <- classtable(prediction.v = lr.test.predictions >= threshold.i,
                                      criterion.v = crit.test[model.can.predict])

        lr.test.hr.v[thresholds == threshold.i] <- lr.test.stats.i$hr
        lr.test.far.v[thresholds == threshold.i] <- lr.test.stats.i$far


      }


    }

    if(is.null(data.test)) {

      lr.test.hr.v <- NA
      lr.test.far.v <- NA
      lr.auc.test <- NA

    }
  }


  # Get AUC

  lr.train.auc <- auc(lr.train.hr.v, lr.train.far.v)

  if(is.null(data.test) == F) {

    lr.test.auc <- auc(lr.test.hr.v, lr.test.far.v)

  } else {

    lr.test.auc <- NA

  }

  lr.auc <- data.frame("train" = lr.train.auc, "test" = lr.test.auc)

  # Get summary vectors of FAR and HR for all thresholds

  lr.acc <- data.frame("threshold" = thresholds,
                       "hr.train" = lr.train.hr.v,
                       "far.train" = lr.train.far.v,
                       "hr.test" = lr.test.hr.v,
                       "far.test" = lr.test.far.v
  )

  output <- list(lr.acc, lr.auc)


  return(output)


}
