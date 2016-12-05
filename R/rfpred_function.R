#' Calculates predictions from Random Forests using the randomForest package
#' @param formula a formula
#' @param data.train dataframe. A training dataset
#' @param data.test dataframe. A testing dataset
#' @param rf.model An optional existing random forests model
#' @importFrom stats model.frame formula glm
#' @importFrom randomForest randomForest
#' @export
#' @examples
#'
#' # Fit rf for the mushrooms dataset
#'
#' mushrooms.rf <- rf.pred(formula = poisonous ~.,
#'                         data.train = mushrooms)
#'
#'
#'

rf.pred <- function(formula,
                    data.train,
                    data.test = NULL,
                    rf.model = NULL) {


  # formula = poisonous ~.
  # data.train = mushrooms[1:10,]
  # data.test = mushrooms[11:nrow(mushrooms),]
  # rf.model <- NULL

if(is.null(data.train) == FALSE) {

  data.train <- model.frame(formula = formula,
                               data = data.train,
                               na.action = NULL)

  crit.train <- data.train[,1]
}

if(is.null(data.test) == FALSE) {

    data.test <- model.frame(formula = formula,
                                 data = data.test,
                                 na.action = NULL)

    crit.test <- data.test[,1]
}

# Convert character cues to factors and ensure that
#  both training and test data columns have the same factor
#  values.

  # Ensure training and test data have complete factor levels
  for(col.i in 1:ncol(data.train)) {

    if(any(c("factor", "character") %in% class(data.train[,col.i]))) {

      levels.i <- paste(unique(data.train[,col.i]))

      if(is.null(data.test) == FALSE) {

        test.index <- names(data.test) == names(data.train)[col.i]

        levels.i <- c(levels.i, paste(unique(data.test[,test.index])))

      }

      levels.i <- unique(levels.i)

      data.train[,col.i] <- factor(data.train[,col.i], levels = levels.i)

      if(is.null(data.test) == FALSE) {

        data.test[,test.index] <- factor(data.test[,test.index], levels = levels.i)

      }

    }

  }

# Convert criterion to factor

dv.vals <- unique(data.train[,1])

data.train[,1] <- factor(data.train[,1], levels = dv.vals)

if(is.null(data.test) == FALSE) {

  data.test[,1] <- factor(data.test[,1], levels = dv.vals)

}

# DETERMINE rf MODEL

if(is.null(rf.model) == TRUE) {

# Create new rf model
rf.train.mod <- randomForest::randomForest(formula,
                               data = data.train
                               )

} else {

 rf.train.mod <- rf.model

}

# rf TRAINING PREDICTIONS
if(is.null(data.train) == FALSE) {

# Get training decisions
rf.train.pred <- predict(rf.train.mod,
                           data = data.train)

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
                        data.test)

# Recode to logical

if("TRUE" %in% paste(rf.test.pred)) {rf.test.pred <- as.logical(paste(rf.test.pred))}
if("1" %in% paste(rf.test.pred)) {rf.test.pred <- as.logical(as.numeric(paste(rf.test.pred)))}


rf.test.acc <- classtable(prediction.v = rf.test.pred,
                          criterion.v = crit.test)


} else {

  rf.test.acc <- classtable(prediction.v = 1,
                            criterion.v = 1)
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

output <- list("accuracy" = rf.acc,
               "auc" = rf.auc,
               "model" = rf.train.mod
)

return(output)

}
