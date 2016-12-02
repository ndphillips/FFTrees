#' Calculates predictions from support vector machines using the e1071 package
#' @param formula a formula
#' @param data.train dataframe. A training dataset
#' @param data.test dataframe. A testing dataset
#' @param svm.model An optional existing svm model
#' @importFrom stats model.frame formula glm
#' @importFrom e1071 svm
#' @export
#' @examples
#'
#' # Fit svm for the mushrooms dataset
#'
#' mushrooms.svm <- svm.pred(formula = poisonous ~.,
#'                           data.train = mushrooms)
#'
#'
#'

svm.pred <- function(formula,
                     data.train,
                     data.test = NULL,
                     svm.model = NULL) {

if(is.null(data.train) == FALSE) {

  data.mf.train <- model.frame(formula = formula,
                               data = data.train,
                               na.action = NULL)

  crit.train <- data.mf.train[,1]
}

if(is.null(data.test) == FALSE) {

    data.mf.test <- model.frame(formula = formula,
                                 data = data.test,
                                 na.action = NULL)

    crit.test <- data.mf.test[,1]
}

# Convert character cues to factors and ensure that
#  both training and test data columns have the same factor
#  values.

levels.ls <- lapply(1:ncol(data.mf.train),
                    FUN = function(x) {

if(is.null(data.test)) {
  return(unique(data.mf.train[,x]))
}


if(is.null(data.test) == FALSE) {
  return(unique(c(data.mf.train[,x], data.mf.test[,x])))
}

})

for(i in 1:ncol(data.mf.train)) {

    if(class(data.mf.train[,i]) %in% c("character", "factor", "logical")) {

levels.i <- levels.ls[[i]]

      data.mf.train[,i] <- factor(paste(data.mf.train[,i]),
                                  levels = levels.i)

    }

  }

if(is.null(data.test) == FALSE) {

  data.mf.test <- model.frame(formula = formula,
                              data = data.test,
                              na.action = NULL)

  for(i in 1:ncol(data.mf.test)) {

    if(class(data.mf.test[,i]) %in% c("character", "factor", "logical")) {

      levels.i <- levels.ls[[i]]

      data.mf.test[,i] <- factor(data.mf.test[,i],
                                 levels = levels.i)

    }

  }

}

# Remove factor values with only one level

ok.cols <- sapply(1:ncol(data.mf.train), FUN = function(x) {

  length(unique(data.mf.train[,x])) > 1

})

data.mf.train <- data.mf.train[,ok.cols]

# Convert criterion to factor

dv.vals <- unique(data.mf.train[,1])

data.mf.train[,1] <- factor(data.mf.train[,1], levels = dv.vals)

if(is.null(data.test) == FALSE) {

  data.mf.test[,1] <- factor(data.mf.test[,1], levels = dv.vals)

}

# DETERMINE svm MODEL

if(is.null(svm.model) == TRUE) {

# Create new svm model
svm.train.mod <- e1071::svm(formula,
                            data = data.mf.train)

} else {

 svm.train.mod <- svm.model

}

# svm TRAINING PREDICTIONS
if(is.null(data.train) == FALSE) {

# Get training decisions
svm.train.pred <- predict(svm.train.mod,
                          data = data.mf.train)

# Recode to logical

if("TRUE" %in% paste(svm.train.pred)) {svm.train.pred <- as.logical(paste(svm.train.pred))}
if("1" %in% paste(svm.train.pred)) {svm.train.pred <- as.logical(as.numeric(paste(svm.train.pred)))}

# Calculate training accuracy stats

svm.train.acc <- classtable(prediction.v = svm.train.pred,
                           criterion.v = crit.train)

} else {

svm.train.acc <- classtable(prediction.v = 1, criterion.v = 1)
svm.train.acc[1,] <- NA

}

# svm TESTING PREDICTIONS

if(is.null(data.test) == FALSE) {

  if(is.null(svm.model) == FALSE) {

    svm.train.mod <- svm.model

  }

# Get test decisions

# Get training decisions
svm.test.pred <- predict(svm.train.mod,
                        data.mf.test)

# Recode to logical

if("TRUE" %in% paste(svm.test.pred)) {svm.test.pred <- as.logical(paste(svm.test.pred))}
if("1" %in% paste(svm.test.pred)) {svm.test.pred <- as.logical(as.numeric(paste(svm.test.pred)))}


svm.test.acc <- classtable(prediction.v = svm.test.pred,
                          criterion.v = crit.test)


} else {

  svm.test.acc <- classtable(prediction.v = 1,
                            criterion.v = 1)
  svm.test.acc[1,] <- NA

}

# ORGANIZE

names(svm.train.acc) <- paste(names(svm.train.acc), ".train", sep = "")
names(svm.test.acc) <- paste(names(svm.test.acc), ".test", sep = "")

svm.acc <- cbind(svm.train.acc, svm.test.acc)
svm.acc <- svm.acc[order(svm.acc$far.train),]

# CALCULATE AUC

if(is.null(data.train) == FALSE) {

svm.auc.train <- auc(hr.v = svm.acc$hr.train, far.v = svm.acc$far.train)

} else {

  svm.auc.train <- NA

}

if(is.null(data.test) == FALSE) {

  svm.auc.test <- auc(hr.v = svm.acc$hr.test, far.v = svm.acc$far.test)
} else {

  svm.auc.test <- NA

}

svm.auc <- matrix(c(svm.auc.train, svm.auc.test), nrow = 2, ncol = 1)
colnames(svm.auc) <- "svm"
rownames(svm.auc) <- c("train", "test")

output <- list("accuracy" = svm.acc,
               "auc" = svm.auc,
               "model" = svm.train.mod
)

return(output)

}
