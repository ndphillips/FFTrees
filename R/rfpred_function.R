#' Calculates predictions from Random Forests using the randomForest package
#' @param formula a formula
#' @param data.train A training dataset
#' @param data.test A testing dataset
#' @param rf.model An optional existing random forests model
#' @importFrom stats model.frame formula glm
#' @importFrom randomForest randomForest
#' @export
#' @examples
#'
#' # Fit rf for the mushrooms dataset
#'
#' mushrooms.rf <- rf.pred(formula = poisonous ~.,
#'                             data.train = mushrooms)
#'
#'
#'

rf.pred <- function(formula,
                    data.train,
                    data.test = NULL,
                    rf.model = NULL,
                    cost.mi = 1,
                    cost.fa = 1) {

  formula = diagnosis ~.

  heartdisease.2 <- heartdisease[sample(nrow(heartdisease)),]
  data.train = heartdisease.2[1:150,]
  data.test = heartdisease.2[151:303,]
  rf.model = NULL

  cost.mi = 1
  cost.fa = 1

  # formula = formula
  # data.train = data.train
  # data.test = data.test
  #
  # rf.model = NULL
  # cost.mi = 1
  # cost.fa = 1

if(is.null(data.train) == FALSE) {

  data.mf.train <- model.frame(formula = formula,
                               data = data.train,
                               na.action = NULL)


  data.mf.train[,1] <- as.factor(data.mf.train[,1])

  # Convert character cues to factors

  for(i in 1:ncol(data.mf.train)) {

    if(class(data.mf.train[,i]) == "character") {

      data.mf.train[,i] <- as.factor(data.mf.train[,i])
    }

  }


  cue.train <- data.mf.train[,2:ncol(data.mf.train)]
  crit.train <- data.mf.train[,1]

  if(ncol(data.mf.train) == 2) {

    cue.train <- data.frame(cue.train)
    names(cue.train) <- names(data.mf.train)[2]

  }



}

if(is.null(data.test) == FALSE) {

  data.mf.test <- model.frame(formula = formula,
                              data = data.test,
                              na.action = NULL)

  data.mf.test[,1] <- as.factor(data.mf.test[,1])


  for(i in 1:ncol(data.mf.test)) {

    if(class(data.mf.test[,i]) == "character") {

      data.mf.test[,i] <- as.factor(data.mf.test[,i])

    }

  }

  cue.test <- data.mf.test[,2:ncol(data.mf.test)]
  crit.test <- data.mf.test[,1]

  if(ncol(data.mf.test) == 2) {

    cue.test <- data.frame(cue.test)
    names(cue.test) <- names(data.mf.test)[2]

  }


}

# DETERMINE rf MODEL

if(is.null(rf.model) == TRUE) {

# Create new rf model
rf.train.mod <- randomForest::randomForest(formula,
                               data = data.mf.train
                               )

} else {

 rf.train.mod <- rf.model

}

# rf TRAINING PREDICTIONS
if(is.null(data.train) == FALSE) {

# Get training decisions
rf.train.pred <- predict(rf.train.mod,
                           data = data.mf.train)

# Recode to logical

if("TRUE" %in% paste(rf.train.pred)) {rf.train.pred <- as.logical(paste(rf.train.pred))}
if("1" %in% paste(rf.train.pred)) {rf.train.pred <- as.logical(as.numeric(paste(rf.train.pred)))}

# Calculate training accuracy stats

rf.train.acc <- classtable(prediction.v = rf.train.pred,
                               criterion.v = crit.train)

} else {

rf.train.acc <- classtable(prediction.v = 1, criterion.v = 1)
rf.train.acc[1,] <- NA

}

# rf TESTING PREDICTIONS

if(is.null(data.test) == FALSE) {

  if(is.null(rf.model) == FALSE) {

    rf.train.mod <- rf.model

  }

# Get test decisions

# Get training decisions
rf.test.pred <- predict(rf.train.mod,
                        data.mf.test)

# Recode to logical

if("TRUE" %in% paste(rf.test.pred)) {rf.test.pred <- as.logical(paste(rf.test.pred))}
if("1" %in% paste(rf.test.pred)) {rf.test.pred <- as.logical(as.numeric(paste(rf.test.pred)))}


rf.test.acc <- classtable(prediction.v = rf.test.pred,
                              criterion.v = crit.test)


} else {

  rf.test.acc <- classtable(prediction.v = 1, criterion.v = 1)
  rf.test.acc[1,] <- NA

}

# ORGANIZE

names(rf.train.acc) <- paste(names(rf.train.acc), ".train", sep = "")
names(rf.test.acc) <- paste(names(rf.test.acc), ".test", sep = "")

rf.acc <- cbind(rf.train.acc, rf.test.acc)
rf.acc <- rf.acc[order(rf.acc$far.train),]

# CALCULATE AUC

if(is.null(data.train) == FALSE) {

rf.auc.train <- auc(hr.v = rf.acc$hr.train, far.v = rf.acc$far.train)

} else {

  rf.auc.train <- NA

}

if(is.null(data.test) == FALSE) {

  rf.auc.test <- auc(hr.v = rf.acc$hr.test, far.v = rf.acc$far.test)
} else {

  rf.auc.test <- NA

}

rf.auc <- matrix(c(rf.auc.train, rf.auc.test), nrow = 2, ncol = 1)
colnames(rf.auc) <- "rf"
rownames(rf.auc) <- c("train", "test")

# SETUP OUTPUT

output <- list("accuracy" = rf.acc,
               "auc" = rf.auc,
               "model" = rf.train.mod
)

return(output)

}
