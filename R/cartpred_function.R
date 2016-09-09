#' Calculates predictions from CART using the rpart package
#' @param formula a formula
#' @param data.train A training dataset
#' @param data.test A testing dataset
#' @param cart.model An optional existing cart model
#' @param cost.mi,cost.fa Optional costs for misses and false alarms
#' @importFrom stats model.frame formula glm
#' @importFrom rpart rpart
#' @export
#'


cart.pred <- function(
  formula,
  data.train,
  data.test = NULL,
  cart.model = NULL,
  cost.mi = 1,
  cost.fa = 1
) {


if(is.null(data.train) == F) {

  data.mf.train <- model.frame(formula = formula, data = data.train)
  cue.train <- data.mf.train[,2:ncol(data.mf.train)]
  crit.train <- data.mf.train[,1]

}

if(is.null(data.test) == F) {

  data.mf.test <- model.frame(formula = formula, data = data.test)
  cue.test <- data.mf.test[,2:ncol(data.mf.test)]
  crit.test <- data.mf.test[,1]

}

# DETERMINE CART MODEL

if(is.null(cart.model) == T) {

# Create new CART model
cart.train.mod <- rpart::rpart(formula,
                               data = data.train,
                               method = "class",
                               parms = list(loss = matrix(c(0, cost.mi, cost.fa, 0), byrow = T, nrow = 2))
)

} else {

 cart.train.mod <- cart.model

}

# Determine the cues used by cart model
cart.cues.used <- as.character(cart.train.mod$frame$var)
cart.cues.used <- paste(cart.cues.used[cart.cues.used != "<leaf>"], collapse = ";")
cart.factor.values <- attributes(cart.train.mod)$xlevels

# CART TRAINING PREDICTIONS
if(is.null(data.train) == F) {

# Look for new factor values

can.predict.mtx <- matrix(NA, nrow = nrow(data.train), ncol = ncol(cue.train))

for(i in 1:ncol(cue.train)) {

  if(class(cue.train[,i]) %in% c("numeric", "logical", "integer")) {can.predict.mtx[,i] <- T}

  if(class(cue.train[,i]) %in% c("factor", "character")) {

    can.predict.mtx[,i] <- paste(cue.train[,i]) %in% cart.factor.values[[which(names(cart.factor.values) == names(cue.train[i]))]]

  }

}

can.predict.vec <- rowMeans(can.predict.mtx) == 1

if(any(can.predict.vec == F)) {

warning(paste("CART couldn't predict ",  sum(can.predict.vec == FALSE),
              " cases because they contained new factor values. These cases will be predicted to be FALSE",
              sep = ""))
}

cart.train.pred <- rep(FALSE, nrow(data.train))

# Get training decisions
cart.train.pred.t <- predict(cart.train.mod,
                             data.train[can.predict.vec == T,],
                             type = "class")

# Recode to logical

if("TRUE" %in% paste(cart.train.pred.t)) {cart.train.pred.t <- as.logical(paste(cart.train.pred.t))}
if("1" %in% paste(cart.train.pred.t)) {cart.train.pred.t <- as.logical(as.numeric(paste(cart.train.pred.t)))}

cart.train.pred[can.predict.vec == T] <- cart.train.pred.t

# Calculate training accuracy stats

cart.train.acc <- classtable(prediction.v = cart.train.pred,
                               criterion.v = crit.train)

} else {

cart.train.acc <- classtable(prediction.v = 1, criterion.v = 1)
cart.train.acc[1,] <- NA

}

# CART TESTING PREDICTIONS

if(is.null(data.test) == F) {

  if(is.null(cart.model) == F) {

    cart.train.mod <- cart.model

  }

# Get test decisions


# Look for new factor values

can.predict.mtx <- matrix(NA, nrow = nrow(data.test), ncol = ncol(cue.test))

for(i in 1:ncol(cue.test)) {

  if(class(cue.test[,i]) %in% c("numeric", "logical", "integer")) {can.predict.mtx[,i] <- T}

  if(class(cue.test[,i]) %in% c("factor", "character")) {

    can.predict.mtx[,i] <- paste(cue.test[,i]) %in% cart.factor.values[[which(names(cart.factor.values) == names(cue.test[i]))]]

  }

}

can.predict.vec <- rowMeans(can.predict.mtx) == 1

if(any(can.predict.vec == F)) {

  warning(paste("CART couldn't predict ",  sum(can.predict.vec == FALSE),
                " test cases because they contained new factor values. These cases will be predicted to be FALSE",
                sep = ""))
}

cart.test.pred <- rep(FALSE, nrow(data.test))

# Get training decisions
cart.test.pred.t <- predict(cart.train.mod,
                             data.test[can.predict.vec == T,],
                             type = "class")

# Recode to logical

if("TRUE" %in% paste(cart.test.pred.t)) {cart.test.pred.t <- as.logical(paste(cart.test.pred.t))}
if("1" %in% paste(cart.test.pred.t)) {cart.test.pred.t <- as.logical(as.numeric(paste(cart.test.pred.t)))}

cart.test.pred[can.predict.vec == T] <- cart.test.pred.t


cart.test.acc <- classtable(prediction.v = cart.test.pred,
                              criterion.v = crit.test)


} else {

  cart.test.acc <- classtable(prediction.v = 1, criterion.v = 1)
  cart.test.acc[1,] <- NA

}

# ORGANIZE

names(cart.train.acc) <- paste(names(cart.train.acc), ".train", sep = "")
names(cart.test.acc) <- paste(names(cart.test.acc), ".test", sep = "")

cart.acc <- cbind(cart.train.acc, cart.test.acc, cart.cues.used)
cart.acc <- cart.acc[order(cart.acc$far.train),]

# CALCULATE AUC

if(is.null(data.train) == F) {

cart.auc.train <- auc(hr.v = cart.acc$hr.train, far.v = cart.acc$far.train)

} else {

  cart.auc.train <- NA
}

if(is.null(data.test) == F) {
  cart.auc.test <- auc(hr.v = cart.acc$hr.test, far.v = cart.acc$far.test)
} else {

  cart.auc.test <- NA
}

cart.auc <- matrix(c(cart.auc.train, cart.auc.test), nrow = 2, ncol = 1)
colnames(cart.auc) <- "cart"
rownames(cart.auc) <- c("train", "test")

# SETUP OUTPUT

output <- list("accuracy" = cart.acc,
               "auc" = cart.auc,
               "model" = cart.train.mod
)

return(output)

}
