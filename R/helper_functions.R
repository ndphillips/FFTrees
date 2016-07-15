lr.pred <- function(
  formula,
  data.train,
  data.test = NULL,
  thresholds = seq(.9, .1, -.1),
  correction = .25
) {

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
                               criterion.v = crit.train,
                               correction = correction,
                               hr.weight = .5
  )


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



    lr.test.predictions <- suppressWarnings(predict(lr.train.mod,
                                                    newdata = data.test))

    lr.test.predictions <- 1 / (1 + exp(-lr.test.predictions))
    lr.test.predictions.bin <- rep(0, length(lr.test.predictions))
    lr.test.predictions.bin[lr.test.predictions >= .5] <- 1

    lr.test.stats <- classtable(prediction.v = lr.test.predictions.bin,
                                criterion.v = crit.test,
                                correction = correction,
                                hr.weight = .5
    )

    lr.test.stats$n.exemplars <- length(model.can.predict)
    lr.test.stats$n.exemplars.can.pred <- sum(model.can.predict)
    lr.test.stats$p.exemplars.can.pred <- sum(model.can.predict) / length(model.can.predict)



    lr.test.far.v <- rep(NA, length(thresholds))
    lr.test.hr.v <- rep(NA, length(thresholds))

    for(threshold.i in thresholds) {

      lr.test.stats.i <- classtable(prediction.v = lr.test.predictions >= threshold.i,
                                    criterion.v = crit.test)

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



auc <- function(hr.v, far.v) {


  hr.order <- order(hr.v)

  hr.v <- hr.v[hr.order]
  far.v <- far.v[hr.order]

  hr.v <- c(0, hr.v, 1)
  far.v <- c(0, far.v, 1)


  # Remove bad (i.e.. non-increasing values)

  hr.v.n <- hr.v[1]
  far.v.n <- far.v[1]

  for(i in 2:length(hr.v)) {

    if(hr.v[i] > hr.v.n[length(hr.v.n)] & far.v[i] >= far.v.n[length(far.v.n)]) {

      hr.v.n <- c(hr.v.n, hr.v[i])
      far.v.n <- c(far.v.n, far.v[i])

    }


  }

  hr.v <- hr.v.n
  far.v <- far.v.n


  far.d.v <- far.v[2:length(far.v)] - far.v[1:(length(far.v) - 1)]
  hr.d.v <- hr.v[2:length(hr.v)] - hr.v[1:(length(hr.v) - 1)]

  aoc.i <- 1- sum(far.d.v * hr.d.v)

  return(aoc.i)

}


cart.pred <- function(
  formula,
  data.train,
  data.test = NULL,
  correction = .25
) {


  # formula <- diagnosis ~.
  # data.train <- heartdisease[1:50,]
  # data.test <- NULL


  data.mf.train <- model.frame(formula = formula, data = data.train)
  crit.train <- data.mf.train[,1]

  if(is.null(data.test) == F) {
  data.mf.test <- model.frame(formula = formula, data = data.test)
  crit.test <- data.mf.test[,1]
  } else {data.mf.test <- NULL}




  # Calculate loss df (for ROC curve)

  loss.df <- expand.grid(miss.cost = seq(1, 5, length.out = 10),
                         fa.cost = seq(1, 5, length.out = 10)
  )

  # Remove some cases where costs are identical
  loss.df <- loss.df[loss.df$miss.cost != loss.df$fa.cost | loss.df$miss.cost == 1,]


  cart.train.acc.ls <- vector("list", length = nrow(loss.df))

  if(is.null(data.test) == F) {cart.test.acc.ls <- vector("list", length = nrow(loss.df))}


  cart.cues.vec <- c()

  for(row.i in 1:nrow(loss.df)) {

    miss.cost.i <- loss.df$miss.cost[row.i]
    fa.cost.i <- loss.df$fa.cost[row.i]

    # Train cart model

    cart.train.mod.i <- rpart::rpart(formula,
                                     data = data.train,
                                     method = "class",
                                     parms = list(loss = matrix(c(0, miss.cost.i, fa.cost.i, 0), byrow = T, nrow = 2))
    )

    # Determine the cues used by cart model

    cart.cues.used <- as.character(cart.train.mod.i$frame$var)
    cart.cues.used <- paste(cart.cues.used[cart.cues.used != "<leaf>"], collapse = ";")
    cart.cues.vec <- c(cart.cues.vec, cart.cues.used)


    # Get training decisions

    cart.train.pred.i <- predict(cart.train.mod.i,
                                 data.train,
                                 type = "class")

    # Recode to logical

    if("TRUE" %in% paste(cart.train.pred.i)) {cart.train.pred.i <- as.logical(paste(cart.train.pred.i))}
    if("1" %in% paste(cart.train.pred.i)) {cart.train.pred.i <- as.logical(as.numeric(paste(cart.train.pred.i)))}


    # Calculate training accuracy stats

    cart.train.acc.i <- classtable(prediction.v = cart.train.pred.i,
                                   criterion.v = crit.train,
                                   correction = correction,
                                   hr.weight = .5
    )

    cart.train.acc.ls[[row.i]] <- cart.train.acc.i

    if(is.null(data.test) == F) {

      # Get test decisions

      cart.test.pred.i <- predict(cart.train.mod.i,
                                  data.test,
                                  type = "class")

      # Recode to logical

      if("TRUE" %in% paste(cart.test.pred.i)) {cart.test.pred.i <- as.logical(paste(cart.test.pred.i))}
      if("1" %in% paste(cart.test.pred.i)) {cart.test.pred.i <- as.logical(as.numeric(paste(cart.test.pred.i)))}


      cart.test.acc.i <- classtable(prediction.v = cart.test.pred.i,
                                    criterion.v = crit.test,
                                    correction = correction,
                                    hr.weight = .5)

      cart.test.acc.ls[[row.i]] <- cart.test.acc.i


    }

  }

  # Combine training statistics into a df

  cart.train.acc.df <- do.call(rbind, cart.train.acc.ls)
  cart.train.acc.df <- cbind(cart.train.acc.df, loss.df)

  ## Combine cart.train and cart.test

  cart.train.acc.df <- cart.train.acc.df[c("miss.cost", "fa.cost", "hr", "far", "v", "dprime")]
  names(cart.train.acc.df) <- c("miss.cost", "fa.cost", "hr.train", "far.train", "v.train", "dprime.train")


  cart.acc <- cbind(cart.train.acc.df,
                    cart.cues.vec)


  if(is.null(data.test) == F) {
    # Combine test statistics into a df
    cart.test.acc.df <- do.call(rbind, cart.test.acc.ls)
    cart.test.acc.df <- cbind(cart.test.acc.df, loss.df)

    cart.test.acc.df <- cart.test.acc.df[c("hr", "far", "v", "dprime")]
    names(cart.test.acc.df) <- c("hr.test", "far.test", "v.test", "dprime.test")
    cart.auc.test <- auc(cart.test.acc.df$hr.test, cart.test.acc.df$far.test)

    cart.acc <- cbind(cart.acc, cart.test.acc.df)
  }


  # Sort in ascending order of FAR

  cart.acc <- cart.acc[order(cart.acc$far.train),]

  # Remove cases with the same result

  dup.rows <- duplicated.data.frame(cart.acc[,-c(1, 2)])
  cart.acc <- cart.acc[dup.rows == F,]

# Remove cases without cues

  cart.acc <- cart.acc[cart.acc$cart.cues.vec != "",]

# Calculate auc

  cart.auc.train <- auc(hr.v = cart.acc$hr.train, far.v = cart.acc$far.train)


  if(is.null(data.test) == F) {
    cart.auc.test <- auc(hr.v = cart.acc$hr.test, far.v = cart.acc$far.test)
  } else {

    cart.auc.test <- NA
  }

  cart.auc.df <- data.frame(train = cart.auc.train, test = cart.auc.test)

  output <- list(cart.acc = cart.acc,
                 cart.auc = cart.auc.df
  )

  return(output)


}
