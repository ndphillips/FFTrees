# helper.R:
# Collection of various utility functions.
# ----------------------------------------


# valid_train_test_data: ------

# Goal: Ensure that train and test data are sufficiently similar (e.g., contain the same variables)
#       and provide feedback on any existing differences.
#
# Currently, it is only verified that both DFs have some cases and
# contain the SAME names (but the content or order of variables is not checked or altered).
# Future versions may want to verify that 'test' data is valid, given current 'train' data
# (e.g., "test" contains all required variables of "train" to create the current FFTs).
#
# Output: Boolean.

valid_train_test_data <- function(train_data, test_data){

  # Initialize:
  valid <- FALSE

  train_names <- names(train_data)
  test_names  <- names(test_data)

  train_names_not_in_test <- setdiff(train_names, test_names)
  test_names_not_in_train <- setdiff(test_names,  train_names)

  # Conditions:
  if (nrow(train_data) < 1){

    msg <- paste("The 'train' data contains no cases (rows).")
    warning(msg)

  } else if (nrow(test_data) < 1){

    msg <- paste("The 'test' data contains no cases (rows).")
    warning(msg)

  } else if (length(train_names_not_in_test) > 0){

    msg <- paste("Some variables occur in 'train' data, but not in 'test' data:",
                 paste(train_names_not_in_test, collapse = ", "))
    warning(msg)

  } else if (length(test_names_not_in_train) > 0){

    msg <- paste("Some variables occur in 'test' data, but not in 'train' data:",
                 paste(test_names_not_in_train, collapse = ", "))
    warning(msg)

    same_names <- FALSE

  } else { # all tests passed:

    valid <- TRUE

  }

  # Output:
  return(valid)

} # valid_train_test_data().

# # Check:
# (df1 <- data.frame(matrix( 1:9,  nrow = 3)))
# (df2 <- data.frame(matrix(11:22, ncol = 3)))
# (df3 <- data.frame(matrix(31:45, nrow = 3)))
# (df0 <- df1[-(1:3), ])
#
# # FALSE cases:
# valid_train_test_data(df0, df1)
# valid_train_test_data(df1, df0)
# valid_train_test_data(df1, df3)
# valid_train_test_data(df3, df1)
# # TRUE cases:
# valid_train_test_data(df1, df2)
# valid_train_test_data(df1, df2[ , 3:1])



# select_best_tree: ------

#' Select the best tree (from the current set)
#'
#' \code{select_best_tree} selects (looks up and identifies) the best tree
#' from the set (or \dQuote{fan}) of FFTs contained in the current \code{FFTrees} object \code{x},
#' an existing type of \code{data} ('train' or 'test'), and
#' a \code{goal} for which corresponding statistics are available
#' in the designated \code{data} type (in \code{x$trees$stats}).
#'
#' Importantly, \code{select_best_tree} only identifies and selects from the set of
#' \emph{existing} trees with known statistics,
#' rather than creating new trees or computing new cue thresholds.
#' More specifically, \code{goal} is used for identifying and selecting the best of
#' an existing set of FFTs, but not for
#' computing new cue thresholds (see \code{goal.threshold} and \code{fftrees_cuerank()}) or
#' creating new trees (see \code{goal.chase} and \code{fftrees_ranktrees()}).
#'
#' @param x An \code{FFTrees} object.
#'
#' @param data character. Must be either "train" or "test".
#'
#' @param goal character. A goal to maximize or minimize when selecting a tree from an existing \code{x}
#' (for which values exist in \code{x$trees$stats}).
#'
#' @return An integer denoting the \code{tree} that maximizes/minimizes \code{goal} in \code{data}.
#'
#' @seealso
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.

select_best_tree <- function(x, data, goal){

  # Verify inputs: ------

  # x: ----

  testthat::expect_true(inherits(x, "FFTrees"),
                        info = "Argument x is no FFTrees object")

  # data: ----

  testthat::expect_true(data %in% c("train", "test"))

  if (is.null(x$trees$stats$test) & (data == "test")){
    message("You asked for 'test' data, but x only contains training statistics. I'll use data = 'train' instead...")
    data <- "train"
  }

  # goal: ----

  # # (a) narrow goal range:
  #
  # goal_valid <- c("acc", "bacc", "wacc", "dprime", "cost")  # ToDo: Is "dprime" being computed?
  # testthat::expect_true(goal %in% goal_valid)

  # (b) wide goal range:

  # Goals to maximize (more is better):
  max_goals <- c("hi", "cr",
                 "sens", "spec",
                 "ppv", "npv",
                 "acc", "bacc", "wacc", "dprime",
                 "pci")

  # Goals to minimize (less is better):
  min_goals <- c("mi", "fa",
                 "cost", "cost_decisions", "cost_cues",
                 "mcu")

  goal_valid <- c(max_goals, min_goals)
  testthat::expect_true(goal %in% goal_valid)


  # Get tree stats (from x given data): ------

  cur_stats <- x$trees$stats[[data]]
  cur_names <- names(cur_stats)

  ix_goal <- which(cur_names == goal)
  cur_goal_vals <- as.vector(cur_stats[[ix_goal]])

  if (goal %in% max_goals){ # more is better:

    cur_ranks <- rank(-cur_goal_vals, ties.method = "first")  # low ranks indicate higher/better values

  } else { # goal %in% min_goals / less is better:

    cur_ranks <- rank(+cur_goal_vals, ties.method = "first")  # low rank indicate lower/better values
  }

  tree <- cur_stats$tree[cur_ranks == min(cur_ranks)]  # tree with minimum rank


  # Output: -----

  testthat::expect_true(is.integer(tree))  # verify output

  return(tree) # as number

} # select_best_tree().



# apply.break: ------

# Takes a direction, threshold value, and cue vector, and returns a vector of decisions.

apply.break <- function(direction,
                        threshold.val,
                        cue.v,
                        cue.class) {

  testthat::expect_true(direction %in% c("!=", "=", "<", "<=", ">", ">="))
  testthat::expect_length(threshold.val, 1)

  # direction = cue_direction_new
  # threshold.val = cue_threshold_new
  # cue.v = data_current[[cues_name_new]]
  # cue.class = cue_class_new

  if (is.character(threshold.val)) {
    threshold.val <- unlist(strsplit(threshold.val, ","))
  }

  if (cue.class %in% c("numeric", "integer")) {
    threshold.val <- as.numeric(threshold.val)
  }

  if (direction == "!=") {
    output <- (cue.v %in% threshold.val) == FALSE
  }

  if (direction == "=") {
    output <- cue.v %in% threshold.val
  }

  if (direction == "<") {
    output <- cue.v < threshold.val
  }

  if (direction == "<=") {
    output <- cue.v <= threshold.val
  }

  if (direction == ">") {
    output <- cue.v > threshold.val
  }

  if (direction == ">=") {
    output <- cue.v >= threshold.val
  }

  return(output)

} # apply.break().



# cost.cues.append: ------

# Create cost.cues:

cost.cues.append <- function(formula,
                             data,
                             cost.cues = NULL) {

  criterion_name <- paste(formula)[2]

  data_mf <- model.frame(
    formula = formula,
    data = data
  )

  cue_df <- data_mf[, 2:ncol(data_mf), drop = FALSE]
  cue_name_v <- names(cue_df)


  if (is.null(cost.cues) == FALSE) {

    # Make sure all named cues in cost.cues are in data:
    {
      cue.not.in.data <- sapply(names(cost.cues), FUN = function(x) {
        x %in% cue_name_v == FALSE
      })

      if (any(cue.not.in.data)) {

        missing.cues <- paste(cost.cues[cue.not.in.data, 1], collapse = ",")

        warning(paste0("The cue(s) {", missing.cues, "} specified in cost.cues are not present in the data."))
      }
    }

    # Add any missing cue costs as 0:
    {
      cost.cues.o <- cost.cues

      cost.cues <- lapply(1:ncol(cue_df), FUN = function(x) {
        0
      })
      names(cost.cues) <- names(cue_df)

      for (i in 1:length(cost.cues)) {
        cue_name_i <- names(cost.cues)[i]

        if (names(cost.cues)[i] %in% names(cost.cues.o)) {
          cost.cues[[i]] <- cost.cues.o[[cue_name_i]]
        }
      }
    }
  }

  if (is.null(cost.cues)) {
    cost.cues <- lapply(1:ncol(cue_df), FUN = function(x) {
      0
    })
    names(cost.cues) <- names(cue_df)
  }

  return(cost.cues)

} # cost.cues.append().



# comp.pred: ------

#' A wrapper for competing classification algorithms.
#'
#' \code{comp.pred} provides a wrapper for many classification algorithms --- such as CART (\code{rpart::rpart}),
#' logistic regression (\code{glm}), support vector machines (\code{svm::svm}), and random forests (\code{randomForest::randomForest}).
#'
#' @param formula a formula
#' @param data.train dataframe. A training dataset.
#' @param data.test dataframe. A testing dataset.
#' @param algorithm string. An algorithm in the set
#' "lr" -- logistic regression, "cart" -- decision trees, "rlr" -- regularized logistic regression,
#' "svm" -- support vector machines, "rf" -- random forests
#' @param model model. An optional existing model applied to test data
#' @param new.factors string. What should be done if new factor values are discovered in the test set?
#' "exclude" = exclude (i.e.; remove these cases), "base" = predict the base rate of the criterion.
#'
#' @importFrom dplyr bind_rows
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

  #   formula = x$formula
  #   data.train = x$data$train
  #   data.test = x$data$test
  #   algorithm = "lr"
  #   model = NULL

  if (is.null(formula)) {
    stop("You must enter a valid formula")
  }

  if (is.null(algorithm)) {
    stop("You must specify one of the following models: 'rlr', 'lr', 'cart', 'svm', 'rf'")
  }

  # SETUP: ----
  {
    if (is.null(data.test) & (is.null(data.train) == FALSE)) {
      data.all <- data.train
      train.cases <- 1:nrow(data.train)
      test.cases <- c()
    }

    if (is.null(data.test) == FALSE & (is.null(data.train) == FALSE)) {
      # data.all <- rbind(data.train, data.test)  # Note: fails when both dfs have different variables!
      data.all <- dplyr::bind_rows(data.train, data.test)  # fills any non-matching columns with NAs.
      train.cases <- 1:nrow(data.train)
      test.cases <- (nrow(data.train) + 1):nrow(data.all)
    }

    if (is.null(data.train) & is.null(data.test) == FALSE) {
      data.all <- data.test
      train.cases <- c()
      test.cases <- 1:nrow(data.all)
    }

    data.all <- model.frame(
      formula = formula,
      data = data.all
    )

    train.crit <- data.all[train.cases, 1]

    # Remove columns with no variance in training data:
    if (is.null(data.train) == FALSE) {
      if (isTRUE(all.equal(length(unique(data.all[train.cases, 1])), 1))) {
        do.test <- FALSE
      } else {
        do.test <- TRUE
      }
    } else {
      do.test <- TRUE
    }
  }

  if (do.test) {
    if (is.null(train.cases) == FALSE) {
      ok.cols <- sapply(1:ncol(data.all), FUN = function(x) {
        length(unique(data.all[train.cases, x])) > 1
      })
      data.all <- data.all[, ok.cols]
    }

    # Convert character columns to factors:
    for (col.i in 1:ncol(data.all)) {
      if (inherits(data.all[,col.i], c("logical", "character", "factor"))) {
        data.all[, col.i] <- factor(data.all[, col.i])
      }
    }
  }

  # Get data, cue, crit objects: ----

  if (is.null(train.cases) == FALSE) {
    data.train <- data.all[train.cases, ]
    cue.train <- data.all[train.cases, -1]
    crit.train <- data.all[train.cases, 1]
  } else {
    data.train <- NULL
    cue.train <- NULL
    crit.train <- NULL
  }


  # Build models and get training data: ----

  if (algorithm == "lr") {
    if (is.null(model)) {
      train.mod <- suppressWarnings(glm(formula, data.train, family = "binomial"))
    } else {
      train.mod <- model
    }

    if (is.null(data.train) == FALSE) {
      pred.train <- suppressWarnings(round(1 / (1 + exp(-predict(train.mod, data.train))), 0))
    } else {
      pred.train <- NULL
    }
  }

  if (algorithm == "svm") {
    if (is.null(model)) {
      train.mod <- e1071::svm(formula,
                              data = data.train, type = "C"
      )
    } else {
      train.mod <- model
    }

    if (is.null(data.train) == FALSE) {
      pred.train <- predict(train.mod,
                            data = data.train
      )
    } else {
      pred.train <- NULL
    }
  }

  if (algorithm == "cart") {
    if (is.null(model)) {
      # Create new CART model
      train.mod <- rpart::rpart(formula,
                                data = data.train,
                                method = "class"
      )
    } else {
      train.mod <- model
    }

    if (is.null(data.train) == FALSE) {
      pred.train <- predict(train.mod,
                            data.train,
                            type = "class"
      )
    } else {
      pred.train <- NULL
    }
  }

  if (algorithm == "rf") {

    if (is.null(model)) {
      data.train[, 1] <- factor(data.train[, 1])

      train.mod <- randomForest::randomForest(formula,
                                              data = data.train
      )

    } else {

      train.mod <- model

    }

    if (is.null(data.train) == FALSE) {
      # Get training decisions
      pred.train <- predict(
        train.mod,
        data.train
      )

    } else {

      pred.train <- NULL
    }
  }

  # Get testing data: ----
  pred.test <- NULL

  if (is.null(data.test) == FALSE) {
    data.test <- data.all[test.cases, ]
    cue.test <- data.all[test.cases, -1]
    crit.test <- data.all[test.cases, 1]

    # Check for new factor values:
    {

      if (is.null(train.cases) == FALSE) {
        factor.ls <- lapply(1:ncol(data.train), FUN = function(x) {
          unique(data.train[, x])
        })

      } else {

        factor.ls <- lapply(1:ncol(data.test), FUN = function(x) {
          cue.x <- names(data.test)[x]

          if (cue.x %in% names(model$xlevels)) {

            return(model$xlevels[[cue.x]])

          } else {

            return(unique(data.test[, x]))
          }
        })
      }

      cannot.pred.mtx <- matrix(0, nrow = nrow(data.test), ncol = ncol(data.test))

      for (i in 1:ncol(cannot.pred.mtx)) {

        if (inherits(data.test[, i], c("factor", "character"))) {
          cannot.pred.mtx[, i] <- data.test[, i] %in% factor.ls[[i]] == FALSE
        }

      } # for().

      cannot.pred.v <- rowSums(cannot.pred.mtx) > 0

      if (any(cannot.pred.v)) {

        if (substr(new.factors, 1, 1) == "e") {

          warning(paste(sum(cannot.pred.v), "cases in the test data could not be predicted by 'e' due to new factor values. These cases will be excluded"))

          data.test <- data.test[cannot.pred.v == FALSE, ]
          cue.test <- cue.test[cannot.pred.v == FALSE, ]
          crit.test <- crit.test[cannot.pred.v == FALSE]
        }

        if (substr(new.factors, 1, 1) == "b") {

          warning(paste(sum(cannot.pred.v), "cases in the test data could not be predicted by 'b' due to new factor values. They will be predicted to be", mean(train.crit) > .5))
        }

      }
    }

    # Get pred.test from each model: ----

    # LR:
    if (algorithm == "lr") {
      if (is.null(data.test) == FALSE) {

        pred.test <- rep(0, nrow(data.test))

        if (any(cannot.pred.v) & substr(new.factors, 1, 1) == "b") {

          pred.test[cannot.pred.v] <- mean(train.crit) > .5
          pred.test[cannot.pred.v == FALSE] <- round(1 / (1 + exp(-predict(train.mod, data.test[cannot.pred.v == FALSE, ]))), 0)

        } else {

          pred.test[!cannot.pred.v] <- round(1 / (1 + exp(-predict(train.mod, data.test[cannot.pred.v == FALSE, ]))), 0)
        }
      }
    }

    # SVM:
    if (algorithm == "svm") {
      if (is.null(data.test) == FALSE) {

        # See if we can do any predictions

        try.pred <- try(predict(train.mod, data.test), silent = TRUE)

        if (inherits(try.pred, "try-error")) {

          warning("svm crashed predicting new data. That's all I can say")

          pred.test <- NULL

        } else {

          pred.test <- predict(train.mod, data.test)
        }
      }
    }

    # CART:
    if (algorithm == "cart") {
      if (is.null(data.test) == FALSE) {
        if (any(cannot.pred.v) & substr(new.factors, 1, 1) == "b") {

          pred.test <- rep(0, nrow(data.test))
          pred.test[cannot.pred.v] <- mean(train.crit) > .5
          pred.test[cannot.pred.v == FALSE] <- predict(train.mod, data.test[cannot.pred.v == FALSE, ], type = "class")

        } else {

          pred.test <- predict(train.mod, data.test[cannot.pred.v == FALSE, ], type = "class")
        }
      }
    }

    # RF:
    if (algorithm == "rf") {
      if (is.null(data.test) == FALSE) {

        crit.test <- as.factor(crit.test)

        # Get levels of training criterion
        train.crit.levels <- levels(train.mod$y)

        # convert test crit to factor
        crit.name <- paste(formula)[2]

        data.test.2 <- data.test
        data.test.2[crit.name] <- factor(data.test.2[[crit.name]], levels = train.crit.levels)


        # See if we can do any predictions:
        try.pred <- try(predict(train.mod, data.test.2), silent = TRUE)

        if (inherits(try.pred, "try-error")) {
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

  # Convert predictions to logical if necessary: ----

  if (is.null(pred.train) == FALSE) {

    if ("TRUE" %in% paste(pred.train)) {
      pred.train <- as.logical(paste(pred.train))
    }

    if ("1" %in% paste(pred.train)) {
      pred.train <- as.logical(as.numeric(paste(pred.train)))
    }

    # Calculate training accuracy stats:

    acc.train <- classtable(
      prediction_v = as.logical(pred.train),
      criterion_v = as.logical(crit.train)
    )
  }

  if (is.null(pred.train)) {
    acc.train <- classtable(c(TRUE, TRUE, FALSE), c(FALSE, FALSE, TRUE))
    acc.train[1, ] <- NA
  }

  if (is.null(pred.test) == FALSE) {

    if ("TRUE" %in% paste(pred.test)) {
      pred.test <- as.logical(paste(pred.test))
    }

    if ("1" %in% paste(pred.test)) {
      pred.test <- as.logical(as.numeric(paste(pred.test)))
    }

    acc.test <- classtable(
      prediction_v = as.logical(pred.test),
      criterion_v = as.logical(crit.test)
    )

  } else {

    acc.test <- classtable(
      prediction_v = c(TRUE, FALSE, TRUE),
      criterion_v = c(TRUE, TRUE, FALSE)
    )
    acc.test[1, ] <- NA
  }

  if (do.test == FALSE) {

    acc.train <- classtable(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, TRUE))
    acc.train[1, ] <- NA

    acc.test <- classtable(c(TRUE, FALSE, TRUE), c(FALSE, TRUE, TRUE))
    acc.test[1, ] <- NA
  }

  # ORGANIZE output: ----

  output <- list(
    "accuracy" = list(train = acc.train, test = acc.test),
    "model" = train.mod,
    "algorithm" = algorithm
  )

  return(output)

} # comp.pred().



# factclean: ------

#' Clean factor variables in prediction data
#'
#' @param data.train A training dataset
#' @param data.test A testing dataset
#' @param show.warning logical

factclean <- function(data.train,
                      data.test,
                      show.warning = T) {


  # 1. Look for new factor values in test set that are not in training set: ----

  orig.vals.ls <- lapply(1:ncol(data.train), FUN = function(x) {
    unique(data.train[, x])
  })

  # 2. can.predict.mtx: ----
  can.predict.mtx <- matrix(1, nrow = nrow(data.test), ncol = ncol(data.test))

  for (i in 1:ncol(can.predict.mtx)) {

    test.vals.i <- data.test[, i]

    if (is.numeric(test.vals.i)) {

      can.predict.mtx[, i] <- 1

    } else {

      can.predict.mtx[, i] <- paste(test.vals.i) %in% paste(orig.vals.ls[[i]])

    }
  }

  # 3. model.can.predict: ----
  model.can.predict <- isTRUE(all.equal(rowMeans(can.predict.mtx), 1))

  if (identical(mean(model.can.predict), 1) == FALSE & show.warning == TRUE) {

    warning(paste(sum(model.can.predict), " out of ",
                  nrow(data.test), " cases (", round(sum(model.can.predict == 0) / length(model.can.predict), 2) * 100,
                  "%) were removed from the test dataset.",
                  sep = ""
    ))
  }

  # Output: ----

  output <- data.test[model.can.predict, ]

  return(output)

} # factclean().



# add_stats: ------

#' Add decision statistics to data (containing counts of a 2x2 contingency table)
#'
#' \code{add_stats} assumes the input of essential 2x2 frequency counts
#' (as a data frame \code{data} with variable names \code{"hi"}, \code{"fa"}, \code{"mi"}, and \code{"cr"})
#' and uses them to compute various decision accuracy measures.
#'
#' Providing numeric values for \code{cost.each} (as a vector) and \code{cost.outcomes} (as a named list)
#' allows computing cost information for the counts of corresponding classification decisions.
#'
#' @param data A data frame with (integer) values named \code{"hi"}, \code{"fa"}, \code{"mi"}, and \code{"cr"}.
#' @param sens.w numeric. Sensitivity weight (for computing weighted accuracy, \code{wacc}).
#' @param cost.each numeric. An optional fixed cost added to all outputs (e.g.; the cost of the cue).
#' @param cost.outcomes list. A list of length 4 named \code{"hi"}, \code{"fa"}, \code{"mi"}, \code{"cr"}, and
#' specifying the costs of a hit, false alarm, miss, and correct rejection, respectively.
#' E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a
#' false alarm and miss cost 10 and 20 units, respectively, while correct decisions incur no costs.
#'
#' @return A data frame with variables of computed accuracy and cost measures (but dropping inputs).

add_stats <- function(data,
                      sens.w = .50,
                      cost.each = NULL,
                      cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0)) {

  # Prepare: ----

  if (is.null(cost.each)) {
    cost.each <- 0
  }


  # Compute measures: ----

  N <- with(data, (hi + cr + fa + mi))

  # Sensitivity:
  data$sens <- with(data, hi / (hi + mi))

  # Specificity:
  data$spec <- with(data, cr / (cr + fa))

  # False alarm rate:
  data$far <- with(data, 1 - spec)


  # Positive predictive value (PPV):
  data$ppv <- with(data, hi / (hi + fa))

  # Negative predictive value (NPV):
  data$npv <- with(data, cr / (cr + mi))


  # Accuracy:
  data$acc <- with(data, (hi + cr) / N)

  # Balanced accuracy:
  data$bacc <- with(data, (sens + spec) / 2)  # = (sens * .50) + (spec * .50)

  # Weighted accuracy:
  data$wacc <- with(data, (sens * sens.w) + (spec * (1 - sens.w)))


  # Outcome cost:
  data$cost_decisions <- with(data, -1 * ((hi * cost.outcomes$hi) + (fa * cost.outcomes$fa)
                                          + (mi * cost.outcomes$mi) + (cr * cost.outcomes$cr))) / data$n

  # Total cost:
  data$cost <- data$cost_decisions - cost.each


  # Output: ----

  # Drop inputs and order columns (of df):
  data <- data[, c("sens", "spec",  "far",  "ppv", "npv",
                   "acc", "bacc", "wacc",   "cost_decisions", "cost")]

  return(data)

} # add_stats().

# # Check:
# (freq <- data.frame(hi = 2, mi = 3, fa = 1, cr = 4))
# add_stats(freq)
# add_stats(freq, sens.w = 3/4, cost.each = 1,
#           cost.outcomes = list(hi = 0, mi = 3, fa = 2, cr = 0))
# dim(add_stats(freq))  # 1 x 10



# classtable: ------

#' Compute classification statistics for binary prediction and criterion (e.g.; truth) vectors
#'
#' The primary confusion matrix is computed by \code{\link{confusionMatrix}} of the \strong{caret} package.
#'
#' @param prediction_v logical. A logical vector of predictions.
#' @param criterion_v logical. A logical vector of (TRUE) criterion values.
#' @param sens.w numeric. Sensitivity weight parameter (from 0 to 1, for computing \code{wacc}).
#' Default: \code{sens.w = .50}.
#' @param cost.v list. An optional list of additional costs to be added to each case.
#' @param correction numeric. Correction added to all counts for calculating dprime.
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying
#' the costs of a hit, false alarm, miss, and correct rejection, respectively.
#' For instance, \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that
#' a false alarm and miss cost 10 and 20, respectively, while correct decisions have no cost.
#' @param na_prediction_action not sure.
#'
#' @importFrom stats qnorm
#' @importFrom caret confusionMatrix

classtable <- function(prediction_v = NULL,
                       criterion_v,
                       sens.w = .50,
                       cost.v = NULL,
                       correction = .25,
                       cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0),
                       na_prediction_action = "ignore") {

  #   prediction_v <- sample(c(TRUE, FALSE), size = 20, replace = TRUE)
  #   criterion_v <- sample(c(TRUE, FALSE), size = 20, replace = TRUE)
  #   sens.w = .50
  #   cost.v = NULL
  #   correction = .25
  #   cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0)

  if (is.null(cost.v)) {
    cost.v <- rep(0, length(prediction_v))
  }

  if (any(c("FALSE", "TRUE") %in% paste(prediction_v))) {
    prediction_v <- as.logical(paste(prediction_v))
  }

  if (any(c("FALSE", "TRUE") %in% paste(criterion_v))) {
    criterion_v <- as.logical(paste(criterion_v))
  }

  if (!inherits(prediction_v, "logical") | !inherits(criterion_v, "logical") & !is.null(prediction_v)) {
    stop("prediction_v and criterion_v must be logical")
  }

  # Remove NA criterion values:
  prediction_v <- prediction_v[is.finite(criterion_v)]
  criterion_v <- criterion_v[is.finite(criterion_v)]


  # Remove NA prediction values:

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

  if (N > 0) { # use vectors: ----

    if ((var(prediction_v) > 0) & (var(criterion_v) > 0)) { # use caret: ----

      if (length(prediction_v) != length(criterion_v)) {

        stop("length of prediction_v is", length(prediction_v),
             "and length of criterion_v is ", length(criterion_v))
      }

      # Use caret::confusionMatrix:
      cm <- caret::confusionMatrix(table(prediction_v, criterion_v),
                                   positive = "TRUE")

      cm_byClass <- data.frame(as.list(cm$byClass))
      cm_overall <- data.frame(as.list(cm$overall))

      # Get freq counts:
      hi <- cm$table[2, 2]
      mi <- cm$table[1, 2]
      fa <- cm$table[2, 1]
      cr <- cm$table[1, 1]

      N <- (hi + mi + fa + cr)

      # Corrected freq values:
      hi_c <- hi + correction
      mi_c <- mi + correction
      fa_c <- fa + correction
      cr_c <- cr + correction

      # Get or compute statistics:
      sens <- cm_byClass$Sensitivity
      spec <- cm_byClass$Specificity
      far  <- (1 - spec)

      ppv <- cm_byClass$Pos.Pred.Value
      npv <- cm_byClass$Neg.Pred.Value

      acc <- cm_overall$Accuracy
      acc_p <- cm_overall$AccuracyPValue
      bacc <- cm_byClass$Balanced.Accuracy
      wacc <- (cm_byClass$Sensitivity * sens.w) + (cm_byClass$Specificity * (1 - sens.w))

      # dprime (corrected):
      # hi_rate <- hi_c / (hi_c + mi_c)
      # fa_rate <- fa_c / (fa_c + cr_c)
      # dprime <- qnorm(hi_rate) - qnorm(fa_rate)
      dprime <- qnorm(hi_c / (hi_c + mi_c)) - qnorm(fa_c / (fa_c + cr_c))
      # ToDo: Use raw values, rather than aggregate counts (so that qnorm() makes sense)?

      # AUC:
      # auc <- as.numeric(pROC::roc(response = as.numeric(criterion_v),
      #                             predictor = as.numeric(prediction_v))$auc)

      # Cost per case:
      cost_decisions <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr))) / N
      cost <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr)) + sum(cost.v)) / N


    } else { # Compute stats from freq combinations: ----

      # Compute freqs as sum of T/F combinations:
      hi <- sum(prediction_v == TRUE  & criterion_v == TRUE)
      mi <- sum(prediction_v == FALSE & criterion_v == TRUE)
      fa <- sum(prediction_v == TRUE  & criterion_v == FALSE)
      cr <- sum(prediction_v == FALSE & criterion_v == FALSE)

      N <- (hi + mi + fa + cr)

      # Corrected values:
      hi_c <- hi + correction
      mi_c <- mi + correction
      fa_c <- fa + correction
      cr_c <- cr + correction

      # Compute statistics:
      sens <- hi / (hi + mi)
      spec <- cr / (cr + fa)
      far  <- (1 - spec)

      ppv <- hi / (hi + fa)
      npv <- cr / (cr + mi)

      acc <- (hi + cr) / N
      acc_p <- NA
      bacc <- (sens + spec) / 2  # = (sens * .50) + (spec * .50)
      wacc <- (sens * sens.w) + (spec * (1 - sens.w))

      # dprime (corrected):
      dprime <- qnorm(hi_c / (hi_c + mi_c)) - qnorm(fa_c / (fa_c + cr_c))
      # ToDo: Use raw values, rather than aggregate counts (so that qnorm() makes sense)?

      # AUC:
      # auc <- as.numeric(pROC::roc(response = as.numeric(criterion_v),
      #                             predictor = as.numeric(prediction_v))$auc)

      # Cost per case:
      cost_decisions <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr))) / N
      cost <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr)) + sum(cost.v)) / N

    } # else if ((var(prediction_v) > 0) & (var(criterion_v) > 0)).


  } else { # (N > 0) failed: Assign NAs ----

    hi <- NA
    mi <- NA
    fa <- NA
    cr <- NA

    N <- NA

    sens <- NA
    spec <- NA
    far <- NA

    ppv <- NA
    npv <- NA

    acc <- NA
    acc_p <- NA
    bacc <- NA
    wacc <- NA

    dprime <- NA
    # auc <- NA

    cost_decisions <- NA
    cost <- NA

  }

  # Output: ----

  # Collect result (as df):
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
    bacc = bacc,
    wacc = wacc,

    dprime = dprime,
    # auc = auc,

    cost_decisions = cost_decisions,
    cost = cost

  )

  return(result)

} # classtable().



# sens.w_epsion: ------

# A constant/threshold: Minimum required difference
# from the sens.w default value (sens.w = 0.50):

sens.w_epsilon <- 10^-4



# enable_wacc: ------

# Test whether wacc makes sense (iff sens.w differs from its default of 0.50).
# Output: Boolean value.

enable_wacc <- function(sens.w){

  out <- FALSE

  if (abs(sens.w - .50) >= sens.w_epsilon){
    out <- TRUE
  }

  return(out)

} # enable_wacc().



# get_bacc_wacc: ------

# Obtain either bacc or wacc (for displays in print and plot functions).
# Output: Named vector (with name specifying the current type of measure).

get_bacc_wacc <- function(sens, spec,  sens.w){

  if (enable_wacc(sens.w)){ # wacc:

    value <- (sens * sens.w) + (spec * (1 - sens.w))
    names(value) <- "wacc"

  } else { # bacc:

    value <- (sens + spec) / 2  # = (sens * .50) + (spec * .50)
    names(value) <- "bacc"

  }

  return(value)

} # get_bacc_wacc().

# # Check:
# get_bacc_wacc(1, .80, .500)
# get_bacc_wacc(1, .80, .501)
# get_bacc_wacc(1, .80, 0)



# num_space: ------

# \code{num_space} computes the width of a representation of \code{x}
# (as number of digits, in base 10)
# when using scales::comma(x) (as in \code{console_confusionmatrix} below).

num_space <- function(x) {

  if (isTRUE(all.equal(x, 0))) { return(1) }

  # else:
  nchar(scales::comma(x))

} # num_space().



# console_confusionmatrix: ------

console_confusionmatrix <- function(hi, mi, fa, cr,  sens.w,  cost) {

  # hi <- 6534  # 4debugging
  # mi <-    5
  # fa <-  765
  # cr <-   54

  # sens.w <- .70

  # cost <- 0


  # Parameters: ----

  # Labels:
  sum_lbl <- "Totals:"  # or "Sums:"

  # Number of digits in N:
  N <- (hi + mi + fa + cr)
  ndg_N <- num_space(N)

  # Column width (of columns 2 & 3):
  col_width <- max(c(8, ndg_N + 5))


  # Header row: ----

  cat("|",
      rep(" ", times = 10),
      "| True +",
      rep(" ", col_width - 7),
      "| True -",
      rep(" ", col_width - 7),
      "|",
      rep(" ", nchar(" N = ") + ndg_N - nchar(sum_lbl)),
      sum_lbl,
      "\n",
      sep = ""
  )

  # Top line:
  cat("|", rep("-", times = 10), "|", rep("-", times = col_width), "|", rep("-", times = col_width), "|\n", sep = "")


  # Decide + row: ----

  cat("| Decide +",
      " | ",
      crayon::silver("hi"),
      rep(" ", max(1, col_width - 4 - num_space(hi))),
      crayon::green(scales::comma(hi)),
      # rep(" ", max(0, col_width - num_space(hi) - 4)),
      " | ",
      crayon::silver("fa"),
      rep(" ", max(1, col_width - 4 - num_space(fa))),
      crayon::red(scales::comma(fa)),
      # rep(" ", max(0, col_width - num_space(fa) - 4)),
      " | ",
      sep = ""
  )

  cat(rep(" ", max(1, 4 + ndg_N - num_space(hi + fa))), sep = "")
  cat(scales::comma(hi + fa))

  cat("\n")


  # Decide - row: ----

  cat("| Decide -",
      " | ",
      crayon::silver("mi"),
      rep(" ", max(1, col_width - 4 - num_space(mi))),
      crayon::red(scales::comma(mi)),
      # rep(" ", max(0, col_width - num_space(mi) - 4)),
      " | ",
      crayon::silver("cr"),
      rep(" ", max(1, col_width - 4 - num_space(cr))),
      crayon::green(scales::comma(cr)),
      # rep(" ", max(0, col_width - num_space(cr) - 4)),
      " | ",
      sep = ""
  )

  cat(rep(" ", max(1, 4 + ndg_N - num_space(mi + cr))), sep = "")
  cat(scales::comma(mi + cr))

  cat("\n")

  # Bottom line:
  cat("|-", rep("-", times = 9), "|", rep("-", times = col_width), "|", rep("-", times = col_width), "|\n", sep = "")


  # Bottom totals: ----

  cat(rep(" ", times = (9 - nchar(sum_lbl))), sep = "")
  cat(sum_lbl)
  cat("    ")  #  w/o |
  # cat("  | ")  # with |

  cat(rep(" ", max(1, col_width - 2 - num_space(hi + mi))), sep = "")
  cat(scales::comma(hi + mi))
  cat("   ")  #  w/o |
  # cat(" | ")  # with |

  cat(rep(" ", max(1, col_width - 2 - num_space(cr + fa))), sep = "")
  cat(scales::comma(cr + fa))
  cat("   ")  #  w/o |
  # cat(" | ")  # with |

  cat("N = ")
  cat(crayon::underline(scales::comma(N), sep = ""), sep = "")

  cat("\n\n")


  # Accuracy info: ----

  # Compute acc statistics (or use add_stats() above):
  acc <- (hi + cr) / N

  ppv <- hi / (hi + fa)
  npv <- cr / (cr + mi)

  sens <- hi / (hi + mi)
  spec <- cr / (cr + fa)

  # bacc <- (sens + spec) / 2  # = (sens * .50) + (spec * .50)
  # wacc <- (sens * sens.w) + (spec * (1 - sens.w))

  # Get either bacc OR wacc (based on sens.w):
  bacc_wacc <- get_bacc_wacc(sens, spec, sens.w)


  # Print labels and values:

  cat("acc  =", scales::percent(acc, accuracy = .1), sep = " ")

  cat("   ppv  =", scales::percent(ppv, accuracy = .1), sep = " ")
  cat("   npv  =", scales::percent(npv, accuracy = .1), sep = " ")

  cat("\n")

  # if (enable_wacc(sens.w)){ # print wacc:
  #   cat("wacc =", scales::percent(wacc, accuracy = .1), sep = " ")
  # } else { # print bacc:
  #   cat("bacc =", scales::percent(bacc, accuracy = .1), sep = " ")
  # }
  cat(names(bacc_wacc), "=", scales::percent(bacc_wacc, accuracy = .1), sep = " ")

  cat("   sens =", scales::percent(sens, accuracy = .1), sep = " ")
  cat("   spec =", scales::percent(spec, accuracy = .1), sep = " ")

  cat("\n")

  if (enable_wacc(sens.w)){ # print sens.w (as round percentage value):
    cat("sens.w = ", scales::percent(sens.w, accuracy = 1), sep = "")
    cat("\n")
  }

  # Baseline info: Rate of positive criterion values / "True +" cases: ----
  # cat("br   =", scales::percent((hi + mi) / N, accuracy = .1), sep = " ")

  # Bias info: Rate of positive decisions / "Decide +" cases: ----
  # cat("   bias =", scales::percent((hi + fa) / N, accuracy = .1), sep = " ")


  # cat("\n")


  # Cost info: ----

  # Moved cost info to speed & frugality feedback (in printFFTrees_function.R),
  # as cost is NOT an accuracy measure:
  # cat("E(cost) =", scales::comma(cost, accuracy = .001), sep = " ")


  # Output: none. ------

} # console_confusionmatrix().


# text.outline: ------

# Adds text with a white background (taken from Dirk Wulff www.dirkwulff.org).

text.outline <- function(x, y,
                         labels = "test",
                         col = "black",
                         font = 1,
                         bg = "white",
                         r = 0.02,
                         h = 1,
                         w = 1,
                         cex = 1,
                         adj = .5,
                         pos = NULL) {

  # Draw background:
  is <- seq(0, 2 * pi, length = 72)

  for (i in is) {
    xn <- x + cos(i) * r * w
    yn <- y + sin(i) * r * h
    text(xn, yn, labels = labels, col = bg, cex = cex, adj = adj, pos = pos, font = font)
  }

  # Foreground:
  text(x, y, labels = labels, col = col, cex = cex, adj = adj, pos = pos, font = font)

} # text.outline().



# transparent: ------

# Make text color transparent.

transparent <- function(orig.col = "red",
                        trans.val = .5) {

  n.cols <- length(orig.col)
  orig.col <- col2rgb(orig.col)
  final.col <- rep(NA, n.cols)

  for (i in 1:n.cols) {
    final.col[i] <- rgb(orig.col[1, i], orig.col[2, i], orig.col[
      3,
      i
      ], alpha = (1 - trans.val) * 255, maxColorValue = 255)
  }

  return(final.col)

} # transparent().



# FFTrees package: ------

#' \code{FFTrees} package.
#'
#' Create fast-and-frugal trees (FFTs).
#'
#' @docType package
#' @name FFTrees
#' @importFrom dplyr %>%

NULL


# R version check: ------

## quiets concerns of R CMD check re: the .'s that appear in pipelines:
if (getRversion() >= "2.15.1") utils::globalVariables(c(".", "tree", "tree_new", "tree", "level"))



# ToDo: ------

# - Bring back dprime as goal and goal.chase (and verify its computation).

# - Consider re-using add_stats() rather than re-computing stats in classtable(),
#   or when printing (by console_confusionmatrix()) or plotting (by plot.FFTrees()) FFTs.

# eof.
