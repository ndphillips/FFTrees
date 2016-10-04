#' Calculates predictions from logistic regression
#' @param formula a formula
#' @param data.train A training dataset
#' @param data.test A testing dataset
#' @param thresholds A vector of decision thresholds
#' @param lr.model An optional logistic regression model created from glm(family = "binomial")
#' @importFrom stats model.frame formula glm
#' @export
#'


lr.pred <- function(formula,
                    data.train,
                    data.test = NULL,
                    lr.model = NULL,
                    thresholds = .5     #seq(.9, .1, -.1)
) {

  correction <- .25

if(is.null(data.train) == F) {

data.train <- model.frame(formula = formula,
                             data = data.train)

# Remove columns with no variance
data.train <- data.train[,sapply(1:ncol(data.train), FUN = function(x) {length(unique(data.train[,x]))}) > 1]

crit.train <- data.train[,1]
cue.train <- data.train[,2:ncol(data.train)]

if(ncol(data.train) == 2) {

  cue.train <- data.frame(cue.train)
  names(cue.train) <- names(data.train)[2]

}

}

if(is.null(data.test) == F) {

  data.test <- model.frame(formula = formula, data = data.test)

  # Restrict testing data to columns in training data
  data.test <- data.test[,names(data.test) %in% names(data.train)]
  crit.test <- data.test[,1]
  cue.test <- data.test[,2:ncol(data.test)]

}

# DETERMINE LR MODEL

if(is.null(lr.model) == T) {

  # Create new LR model
  lr.train.mod <-  suppressWarnings(glm(formula,
                                        family = "binomial",
                                        data = data.train
                                    ))


} else {

  lr.train.mod <- lr.model

}

## GET FACTOR VALUES

lr.factor.values <- lr.train.mod$xlevels

# LR TRAINING PREDICTIONS

if(is.null(data.train) == F) {

# Look for new factor values

can.predict.mtx <- matrix(NA, nrow = nrow(data.train), ncol = ncol(cue.train))

for(i in 1:ncol(cue.train)) {

  if(class(cue.train[,i]) %in% c("numeric", "logical", "integer")) {can.predict.mtx[,i] <- T}

  if(class(cue.train[,i]) %in% c("factor", "character")) {

    can.predict.mtx[,i] <- paste(cue.train[,i]) %in% lr.factor.values[[which(names(lr.factor.values) == names(cue.train[i]))]]

  }

}

  can.predict.vec <- rowMeans(can.predict.mtx) == 1

  if(any(can.predict.vec == F)) {

    warning(paste("LR couldn't predict ",  sum(can.predict.vec == FALSE),
                  " cases because they contained new factor values. These cases will be predicted to be FALSE",
                  sep = ""))
  }

  lr.train.pred <- rep(FALSE, nrow(data.train))

  # Get training decisions
  lr.train.pred.t <- suppressWarnings( predict(lr.train.mod,
                               newdata = data.train[can.predict.vec == T,]
                              ))

  lr.train.pred.t <- 1 / (1 + exp(-lr.train.pred.t))
  lr.train.pred.t <- lr.train.pred.t > thresholds

  lr.train.pred[can.predict.vec == T] <- lr.train.pred.t

  # Calculate training accuracy stats

  lr.train.acc <- classtable(prediction.v = lr.train.pred,
                               criterion.v = crit.train)

}

if(is.null(data.train) == T) {

  lr.train.acc <- classtable(prediction.v = 1, criterion.v = 1)
  lr.train.acc[1,] <- NA

}

# LR TEST PREDICTIONS

if(is.null(data.test) == F) {

  # Look for new factor values

  can.predict.mtx <- matrix(NA, nrow = nrow(cue.test), ncol = ncol(cue.test))

  for(i in 1:ncol(cue.test)) {

    if(class(cue.test[,i]) %in% c("numeric", "logical", "integer")) {can.predict.mtx[,i] <- T}

    if(class(cue.test[,i]) %in% c("factor", "character")) {

      can.predict.mtx[,i] <- paste(cue.test[,i]) %in% lr.factor.values[[which(names(lr.factor.values) == names(cue.test)[i])]]

    }

  }

  can.predict.vec <- rowMeans(can.predict.mtx) == 1

  if(any(can.predict.vec == F)) {

    warning(paste("LR couldn't predict ",  sum(can.predict.vec == FALSE),
                  " cases because they contained new factor values. These cases will be predicted to be FALSE",
                  sep = ""))
  }

  lr.test.pred <- rep(FALSE, nrow(data.test))

  # Get training decisions
  lr.test.pred.t <- suppressWarnings(predict(object = lr.train.mod,
                             newdata = data.test[can.predict.vec == T,])
  )

  lr.test.pred.t <- 1 / (1 + exp(-lr.test.pred.t))
  lr.test.pred.t <- lr.test.pred.t > thresholds

  lr.test.pred[can.predict.vec == T] <- lr.test.pred.t

  # Calculate training accuracy stats

  lr.test.acc <- classtable(prediction.v = lr.test.pred,
                             criterion.v = crit.test)


}

if(is.null(data.test)) {

  lr.test.acc <- classtable(prediction.v = 1, criterion.v = 1)
  lr.test.acc[1,] <- NA

}

## ORGANIZE

stat.names <- c("hi", "mi", "fa", "cr", "hr", "far", "v", "dprime")
names(lr.train.acc)[names(lr.train.acc) %in% stat.names] <- paste(names(lr.train.acc)[names(lr.train.acc) %in% stat.names], ".train", sep = "")
names(lr.test.acc)[names(lr.test.acc) %in% stat.names] <- paste(names(lr.test.acc)[names(lr.test.acc) %in% stat.names], ".test", sep = "")

lr.acc <- cbind(lr.train.acc, lr.test.acc)
lr.acc <- lr.acc[order(lr.acc$far.train),]
lr.acc$threshold <- thresholds

# AUC

if(is.null(data.train) == F) {

lr.train.auc <- auc(lr.train.acc$hr.train, lr.train.acc$far.train)

} else {lr.train.auc <- NA}

if(is.null(data.test) == F & all(is.finite(lr.test.acc$hr.test))) {

 lr.test.auc <- auc(lr.test.acc$hr.test, lr.test.acc$far.test)

} else {lr.test.auc <- NA}

lr.auc <- matrix(c(lr.train.auc, lr.test.auc), nrow = 2, ncol = 1)
rownames(lr.auc) <- c("train", "test")
colnames(lr.auc) <- "lr"

# Get summary vectors of FAR and HR for all thresholds

output <- list("accuracy" = lr.acc,
               "auc" = lr.auc,
               "model" = lr.train.mod)


return(output)


}
