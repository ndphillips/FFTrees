#' Splits a dataset into two partitions
#'
#' @param data dataframe.
#' @param formula formula. A formula specifying the criterion
#' @param train.p numeric. The percentage of data used to create the training set
#' @importFrom stats model.frame
#' @export
#' @return
#'
#' @examples
#'
#'  library(FFTrees) # For the heartdisease data
#'
#'  heartdisease.split <- createPartition(data = heartdisease,
#'                                  formula = diagnosis ~.,
#'                                  train.p = .1)
#'
#'
#'  heartdisease.split$part1   # The training data
#'  heartdisease.split$part2    # The testing data
#'
#'
#'
#'
#'
createPartition <- function(data,       # Original dataset to be split
                            formula,    # Formula specifying the criterion
                            train.p) {  # Percent of data used in first partition

    # UPDATE

    # randomly sort the original data
    # loop over the rows. Assign the first row to the training data and the second row to the test data
    # Look at the next row, would it fill an empty factor slot for the training data? if yes, then assign to training. If no, then assign to test.
    # repeat until all slots are filled for both training and test.
    # assign the remaining cases proportionally to training and test


    data.train.o <- data

    data.train <- model.frame(formula = formula,
                              data = data.train.o,
                              na.action = NULL)

    cue.train <- data.train[,2:ncol(data.train)]
    crit.train <- data.train[,1]
    cue.names <- names(cue.train)
    n.cues <- length(cue.names)

    # create 'training testing' sets of size train.p * nrow(data.train)

    continue <- TRUE
    run.i <- 0

    # Create train and test set

    # Test set must not have new factor values
    # training set must have at least one positive and one negative case

    while(continue) {

      run.i <- run.i + 1

      n.train <- floor(train.p * nrow(data.train))
      train.exemplars.i <- sample(1:nrow(data.train), size = n.train)
      test.exemplars.i <- setdiff(1:nrow(data.train), train.exemplars.i)
      crit.train.i <- data.train[train.exemplars.i, 1]
      crit.test.i <- data.train[test.exemplars.i, 1]

      data.train.i <- data.train[train.exemplars.i,]
      data.test.i <- data.train[test.exemplars.i,]

      ## FORCE TEST AND TRAINING SET TO HAVE SAME FACTOR VALUES
      force.factor <- FALSE

      if(force.factor == TRUE) {
        orig.vals.ls <- lapply(1:ncol(data.train.i), FUN = function(x) {unique(data.train.i[,x])})

        can.predict.mtx <- matrix(1, nrow = nrow(data.test.i), ncol = ncol(data.test.i))

        for(i in 1:ncol(can.predict.mtx)) {

          test.vals.i <- data.test.i[,i]

          if(is.numeric(test.vals.i)) {
            can.predict.mtx[,i] <- 1} else {

              can.predict.mtx[,i] <- paste(test.vals.i) %in% paste(orig.vals.ls[[i]])


            }
        }

        model.can.predict <- rowMeans(can.predict.mtx) == 1
      }

      if(force.factor == FALSE) {model.can.predict <- TRUE}

      # # Do the training and test data valid?
      # if(mean(crit.train.i) > 0 & mean(crit.train.i) < 1 &
      #    mean(crit.test.i > 0) & mean(crit.test.i < 1) & all(model.can.predict)) {continue <- FALSE}

      # # Do the training and test data valid?
      if(all(model.can.predict)) {continue <- FALSE}

      if(run.i == 100) {stop("I could not create valid training and test data sets. This is likely due to sparse data. You'll have to create them manually.")}

    }

    data.train.full <- data.train

    data.train <- data.train.full[train.exemplars.i,]
    cue.train <- data.train[,2:ncol(data.train)]
    crit.train <- data.train[,1]

    data.test <- data.train.full[test.exemplars.i,]
    cue.test <- data.test[,2:ncol(data.test)]
    crit.test <- data.test[,1]

    return(list("part1"= data.train,    # Training data
                "part2" = data.test,     # Testing data
                "data" = data))              # All data

  }
