# util_stats.R:
# Statistical helper/utility functions.
# -------------------------------------

# Statistical calculations (based on classification outcomes in 2x2 matrix and models): ------


# add_stats (from frequency of 4 classification outcomes): ------

# Outcome statistics based on frequency counts (of 4 classification outcomes)
# [used to set cue thresholds in fftrees_threshold_factor_grid() and fftrees_threshold_numeric_grid()]:


#' Add decision statistics to data (based on frequency counts of a 2x2 classification outcomes)
#'
#' \code{add_stats} assumes the input of the 4 essential classification outcomes
#' (as frequency counts in a data frame \code{"data"} with variable names \code{"hi"}, \code{"fa"}, \code{"mi"}, and \code{"cr"})
#' and uses them to compute various decision accuracy measures.
#'
#' Providing numeric values for \code{cost.each} (as a vector) and \code{cost.outcomes} (as a named list)
#' allows computing cost information for the counts of corresponding classification decisions.
#'
#' @param data A data frame with 4 frequency counts (as integer values, named \code{"hi"}, \code{"fa"}, \code{"mi"}, and \code{"cr"}).
#'
#' @param correction numeric. Correction added to all counts for calculating \code{dprime}.
#' Default: \code{correction = .25}.
#' @param sens.w numeric. Sensitivity weight (for computing weighted accuracy, \code{wacc}).
#' Default: \code{sens.w = NULL} (to enforce that value is passed from calling function).
#'
#' @param my.goal Name of an optional, user-defined goal (as character string). Default: \code{my.goal = NULL}.
#' @param my.goal.fun User-defined goal function (with 4 arguments \code{hi fa mi cr}). Default: \code{my.goal.fun = NULL}.
#'
#' @param cost.each numeric. An optional fixed cost added to all outputs (e.g., the cost of using the cue).
#' @param cost.outcomes list. A list of length 4 named \code{"hi"}, \code{"fa"}, \code{"mi"}, \code{"cr"}, and
#' specifying the costs of a hit, false alarm, miss, and correct rejection, respectively.
#' E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a
#' false alarm and miss cost 10 and 20 units, respectively, while correct decisions incur no costs.
#'
#'
#' @return A data frame with variables of computed accuracy and cost measures (but dropping inputs).

add_stats <- function(data,  # df with frequency counts of classification outcomes ('hi fa mi cr', as integers)
                      #
                      correction = .25,  # used to compute dprime
                      sens.w = NULL,     # used to compute wacc
                      #
                      my.goal = NULL,
                      my.goal.fun = NULL,
                      #
                      cost.each = NULL,
                      cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0)
) {

  # Prepare: ----

  if (is.null(cost.each)) {
    cost.each <- 0
  }

  # Get the 4 key freq counts (from data):

  hi <- data$hi
  fa <- data$fa
  mi <- data$mi
  cr <- data$cr


  # Compute measures: ----

  N <- hi + cr + fa + mi

  # Sensitivity:
  data$sens <- hi / (hi + mi)

  # Specificity:
  data$spec <- cr / (cr + fa)

  # False alarm rate:
  data$far <- 1 - data$spec


  # Positive predictive value (PPV):
  data$ppv <- hi / (hi + fa)

  # Negative predictive value (NPV):
  data$npv <- cr / (cr + mi)


  # Accuracy:
  data$acc <- (hi + cr) / N

  # Balanced accuracy:
  data$bacc <- with(data, (sens + spec) / 2)  # = (sens * .50) + (spec * .50)

  # Weighted accuracy:
  data$wacc <- with(data, (sens * sens.w) + (spec * (1 - sens.w)))


  # dprime:

  # a. Corrected freq values:
  hi_c <- hi + correction
  fa_c <- fa + correction
  mi_c <- mi + correction
  cr_c <- cr + correction

  # b. dprime (corrected):
  data$dprime <- qnorm(hi_c / (hi_c + mi_c)) - qnorm(fa_c / (fa_c + cr_c))


  # Cost:

  # Outcome cost (using NEGATIVE costs, to allow maximizing value to minimize cost):
  data$cost_dec <- -1 * ((hi * cost.outcomes$hi) + (fa * cost.outcomes$fa)
                         + (mi * cost.outcomes$mi) + (cr * cost.outcomes$cr)) / data$n  # Why data$n, not N?

  # Total cost:
  data$cost <- data$cost_dec - cost.each  # Note: cost.each is a constant and deducted (i.e., negative cost).

  if (!is.null(my.goal)){

    # Compute my.goal value (by my.goal.fun):
    my_goal_value <- mapply(FUN = my.goal.fun, hi = hi, fa = fa, mi = mi, cr = cr)
    # print(paste0(my.goal, " = ", round(my_goal_value, 3)))  # 4debugging

    # Add to data (df, by name):
    data[[my.goal]] <- my_goal_value
    # print(paste0(my.goal, " = ", round(data[[my.goal]], 3)))  # 4debugging

  }


  # Output: ----

  # Define the set of critical stats [add_stats_v]: ----

  if (!is.null(my.goal)){ # include my.goal (name and value):

    add_stats_v <- c("sens", "spec",
                     "far",  "ppv", "npv",
                     "dprime",
                     "acc", "bacc", "wacc",
                     my.goal,  # (+)
                     "cost_dec", "cost")


  } else { # default set of critical stats:

    add_stats_v <- c("sens", "spec",
                     "far",  "ppv", "npv",
                     "dprime",
                     "acc", "bacc", "wacc",
                     "cost_dec", "cost")

  }

  # Drop inputs and re-arrange columns (of df): ----
  data <- data[ , add_stats_v]


  return(data)

} # add_stats().

# # Check:
# (freq <- data.frame(hi = 2, fa = 1, mi = 3, cr = 4))
# add_stats(freq)
# add_stats(freq, sens.w = 3/4, cost.each = 1,
#           cost.outcomes = list(hi = 0, fa = 2, mi = 3, cr = 0))
# dim(add_stats(freq))  # 1 x 11 (with dprime)



# classtable (from 2 binary vectors of decisions/predictions): ------

# Outcome statistics based on 2 binary vectors (of logical values)
# [called by fftrees_grow_fan(), fftrees_apply(), and comp_pred() below]:


#' Compute classification statistics for binary prediction and criterion (e.g.; truth) vectors
#'
#' The main input are 2 logical vectors of prediction and criterion values.
#'
#' The primary confusion matrix is computed by \code{\link{confusionMatrix}} of the \strong{caret} package.
#'
#' @param prediction_v logical. A logical vector of predictions.
#' @param criterion_v logical. A logical vector of (TRUE) criterion values.
#'
#' @param correction numeric. Correction added to all counts for calculating \code{dprime}.
#' Default: \code{correction = .25}.
#' @param sens.w numeric. Sensitivity weight parameter (from 0 to 1, for computing \code{wacc}).
#' Default: \code{sens.w = NULL} (to enforce that the current value is passed by the calling function).
#'
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying
#' the costs of a hit, false alarm, miss, and correct rejection, respectively.
#' For instance, \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that
#' a false alarm and miss cost 10 and 20, respectively, while correct decisions have no cost.
#' @param cost_v numeric. Additional cost value of each decision (as an optional vector of numeric values).
#' Typically used to include the cue cost of each decision (as a constant for the current level of an FFT).
#'
#' @param my.goal Name of an optional, user-defined goal (as character string). Default: \code{my.goal = NULL}.
#' @param my.goal.fun User-defined goal function (with 4 arguments \code{hi fa mi cr}). Default: \code{my.goal.fun = NULL}.
#'
#' @param na_prediction_action What happens when no prediction is possible? (experimental).
#'
#' @importFrom stats qnorm
#' @importFrom caret confusionMatrix

classtable <- function(prediction_v = NULL,
                       criterion_v  = NULL,
                       #
                       correction = .25,       # used for dprime calculation
                       sens.w = NULL,          # sens.w (to allow passing by calling function)
                       #
                       cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0),
                       cost_v = NULL,          # cost value of each decision (at current level, as a constant)
                       #
                       my.goal = NULL,
                       my.goal.fun = NULL,
                       #
                       na_prediction_action = "ignore"
){

  #   prediction_v <- sample(c(TRUE, FALSE), size = 20, replace = TRUE)
  #   criterion_v  <- sample(c(TRUE, FALSE), size = 20, replace = TRUE)
  #   sens.w = .50
  #   cost_v = NULL
  #   correction = .25
  #   cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0)

  if (is.null(cost_v)) {
    cost_v <- rep(0, length(prediction_v))
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

  # Remove NA and infinite values (from prediction AND criterion):
  prediction_v <- prediction_v[is.finite(criterion_v)]
  criterion_v  <- criterion_v[is.finite(criterion_v)]

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

    # Note 2 special cases:

    var_pred_v <- var(prediction_v)
    var_crit_v <- var(criterion_v)

    if (is.na(var_pred_v)){
      message("Variance of prediction_v is NA. See print(prediction_v) =")
      print(prediction_v)
    }

    if (is.na(var_crit_v)){
      message("Variance of criterion_v is NA. See print(criterion_v) =")
      print(criterion_v)
    }


    # Main: Compute statistics:

    if (((!is.na(var_pred_v)) & (!is.na(var_crit_v))) &&  # Pre-condition &
        ((var_pred_v > 0) & (var_crit_v > 0))) {          # Case 1. Use caret: ----

      if (length(prediction_v) != length(criterion_v)) {

        stop("Different lengths of prediction_v and criterion_v.\nLength of prediction_v is ", length(prediction_v),
             "and length of criterion_v is ", length(criterion_v))
      }

      # Use caret::confusionMatrix:
      cm <- caret::confusionMatrix(table(prediction_v, criterion_v),
                                   positive = "TRUE")

      cm_byClass <- data.frame(as.list(cm$byClass))
      cm_overall <- data.frame(as.list(cm$overall))

      # Get 4 freq counts:
      hi <- cm$table[2, 2]
      fa <- cm$table[2, 1]
      mi <- cm$table[1, 2]
      cr <- cm$table[1, 1]

      N <- (hi + mi + fa + cr)

      # Corrected freq values:
      hi_c <- hi + correction
      fa_c <- fa + correction
      mi_c <- mi + correction
      cr_c <- cr + correction

      # Get or compute statistics:
      sens <- cm_byClass$Sensitivity
      spec <- cm_byClass$Specificity
      far  <- (1 - spec)

      ppv <- cm_byClass$Pos.Pred.Value
      npv <- cm_byClass$Neg.Pred.Value

      acc   <- cm_overall$Accuracy
      acc_p <- cm_overall$AccuracyPValue
      bacc  <- cm_byClass$Balanced.Accuracy
      wacc  <- (cm_byClass$Sensitivity * sens.w) + (cm_byClass$Specificity * (1 - sens.w))

      # dprime (corrected):
      # hi_rate <- hi_c / (hi_c + mi_c)
      # fa_rate <- fa_c / (fa_c + cr_c)
      # dprime <- qnorm(hi_rate) - qnorm(fa_rate)
      dprime <- qnorm(hi_c / (hi_c + mi_c)) - qnorm(fa_c / (fa_c + cr_c))

      # AUC:
      # auc <- as.numeric(pROC::roc(response = as.numeric(criterion_v),
      #                             predictor = as.numeric(prediction_v))$auc)

      # Costs (per classification outcome):
      # print(cost_v)  # 4debugging: Cost of each decision (cue cost at current level, as a constant)
      cost_dec <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr))) / N
      cost     <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr)) + sum(cost_v)) / N

      # Compute my.goal value (by my.goal.fun):
      if (!is.null(my.goal)){

        my_goal_value <- mapply(FUN = my.goal.fun, hi = hi, fa = fa, mi = mi, cr = cr)
        # print(paste0(my.goal, " = ", round(my_goal_value, 3)))  # 4debugging

      }

    } else { # Case 2. Compute stats from freq combinations: ----

      # Compute freqs as sum of T/F combinations:
      hi <- sum(prediction_v == TRUE  & criterion_v == TRUE)
      fa <- sum(prediction_v == TRUE  & criterion_v == FALSE)
      mi <- sum(prediction_v == FALSE & criterion_v == TRUE)
      cr <- sum(prediction_v == FALSE & criterion_v == FALSE)

      N <- (hi + fa + mi + cr)

      # Corrected values:
      hi_c <- hi + correction
      fa_c <- fa + correction
      mi_c <- mi + correction
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

      # AUC:
      # auc <- as.numeric(pROC::roc(response = as.numeric(criterion_v),
      #                             predictor = as.numeric(prediction_v))$auc)

      # Cost per case:
      cost_dec <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr))) / N
      cost <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr)) + sum(cost_v)) / N

      # Compute my.goal value (by my.goal.fun):
      if (!is.null(my.goal)){

        my_goal_value <- mapply(FUN = my.goal.fun, hi = hi, fa = fa, mi = mi, cr = cr)
        # print(paste0(my.goal, " = ", round(my_goal_value, 3)))  # 4debugging

      }

    } # else if ((var(prediction_v) > 0) & (var(criterion_v) > 0)).


  } else { # (N > 0) failed: Assign NAs ----

    hi <- NA
    fa <- NA
    mi <- NA
    cr <- NA

    N <- NA

    sens <- NA
    spec <- NA
    far  <- NA

    ppv <- NA
    npv <- NA

    acc   <- NA
    acc_p <- NA
    bacc  <- NA
    wacc  <- NA

    dprime <- NA

    # auc  <- NA

    cost_dec <- NA
    cost     <- NA

    my_goal_val <- NA

  }


  # Output: ----

  # Collect result (as df):
  result <- data.frame(

    n = N,

    hi = hi,
    fa = fa,
    mi = mi,
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

    cost_dec = cost_dec,
    cost = cost

  )

  if (!is.null(my.goal)){ # include my.goal (name and value):

    result[[my.goal]] <- my_goal_value  # (+)
    # print(result) # 4debugging

  }

  return(result)

} # classtable().



# comp_pred: ------

#' A wrapper for competing classification algorithms
#'
#' \code{comp_pred} provides the main wrapper for running alternative classification algorithms, such as CART (\code{rpart::rpart}),
#' logistic regression (\code{glm}), support vector machines (\code{svm::svm}), and random forests (\code{randomForest::randomForest}).
#'
#' @param formula A formula (usually \code{x$formula}, for an \code{FFTrees} object \code{x}).
#' @param data.train A training dataset (as data frame).
#' @param data.test A testing dataset (as data frame).
#'
#' @param algorithm character string. An algorithm in the set:
#' "lr" -- logistic regression;
#' "rlr" -- regularized logistic regression;
#' "cart" -- decision trees;
#' "svm" -- support vector machines;
#' "rf" -- random forests.
#'
#' @param model model. An optional existing model, applied to the test data.
#' @param sens.w Sensitivity weight parameter (from 0 to 1, required to compute \code{wacc}).
#' @param new.factors string. What should be done if new factor values are discovered in the test set?
#' "exclude" = exclude (i.e.; remove these cases), "base" = predict the base rate of the criterion.
#'
#' @importFrom dplyr bind_rows
#' @importFrom stats model.frame formula glm model.matrix
#' @importFrom e1071 svm
#' @importFrom rpart rpart
#' @importFrom randomForest randomForest

comp_pred <- function(formula,
                      data.train,
                      data.test = NULL,
                      algorithm = NULL,
                      model = NULL,
                      sens.w = NULL,
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
    stop("Please specify one of the following models: 'lr', 'rlr', 'cart', 'svm', 'rf'")
    # ToDo: 'rlr' does currently not seem to be implemented (see below).
  }

  if (inherits(algorithm, "character")) {
    algorithm <- tolower(algorithm)  # 4robustness
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

        do_test <- FALSE

      } else {

        do_test <- TRUE

      }

    } else {

      do_test <- TRUE

    }
  }


  if (do_test) {

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

  } # if (do_test).


  # Get data, cue, crit objects: ----

  if (is.null(train.cases) == FALSE) {

    data.train <- data.all[train.cases,   ]
    cue.train  <- data.all[train.cases, -1]
    crit.train <- data.all[train.cases,  1]

  } else {

    data.train <- NULL
    cue.train  <- NULL
    crit.train <- NULL

  }


  # Build models for training data: ------

  # 1. LR: ----

  if (algorithm == "lr") {

    if (is.null(model)) {

      # Create new LR model:
      train.mod <- suppressWarnings(glm(formula, data.train, family = "binomial"))

    } else {

      train.mod <- model

    }

    if (is.null(data.train) == FALSE) {

      pred.train <- suppressWarnings(round(1 / (1 + exp(-predict(train.mod, data.train))), 0))

    } else {

      pred.train <- NULL

    }

  } # if (algorithm == "lr").


  # 2. RLR: ToDo ----

  if (algorithm == "rlr") {

    stop("The 'rlr' algorithm is currently not supported. Please specify one of the following models: 'lr', 'cart', 'svm', 'rf'")

  } # if (algorithm == "rlr").


  # 3. SVM: ----

  if (algorithm == "svm") {

    if (is.null(model)) {

      # Create new SVM model:
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

  } # if (algorithm == "svm").


  # 4. CART: ----

  if (algorithm == "cart") {

    if (is.null(model)) {

      # Create new CART model:
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

  } # if (algorithm == "cart").


  # 5. RF: ----

  if (algorithm == "rf") {

    if (is.null(model)) {

      # Create new RF model:
      data.train[, 1] <- factor(data.train[, 1])

      train.mod <- randomForest::randomForest(formula,
                                              data = data.train
      )

    } else {

      train.mod <- model

    }

    if (is.null(data.train) == FALSE) {

      # Get training decisions:
      pred.train <- predict(
        train.mod,
        data.train
      )

    } else {

      pred.train <- NULL

    }

  } # if (algorithm == "rf").



  # Get testing data: ------

  pred.test <- NULL

  if (is.null(data.test) == FALSE) {

    data.test <- data.all[test.cases, ]
    cue.test  <- data.all[test.cases, -1]
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


    # Get predictions (pred.test) from each model: ------

    # 1. LR: ----

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

    } # if (algorithm == "lr").


    # 2. RLR: ToDo ----

    if (algorithm == "rlr") {

      stop("The 'rlr' algorithm is currently not supported.")

    } # if (algorithm == "rlr").


    # 3. SVM: ----

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

    } # if (algorithm == "svm").


    # 4. CART: ----

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

    } # if (algorithm == "cart").


    # 5. RF: ----

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

    } # if (algorithm == "rf").

  } # if (is.null(data.test) == FALSE)).


  # Convert predictions to logical if necessary: ----

  if (is.null(pred.train) == FALSE) {

    if ("TRUE" %in% paste(pred.train)) {
      pred.train <- as.logical(paste(pred.train))
    }

    if ("1" %in% paste(pred.train)) {
      pred.train <- as.logical(as.numeric(paste(pred.train)))
    }


    # Calculate training accuracy stats: ----

    acc.train <- classtable(
      prediction_v = as.logical(pred.train),
      criterion_v = as.logical(crit.train),
      sens.w = sens.w
    )
  }

  if (is.null(pred.train)) {

    acc.train <- classtable(
      prediction_v = c(TRUE, TRUE, FALSE),
      criterion_v =  c(FALSE, FALSE, TRUE),
      sens.w = sens.w
    )

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
      criterion_v = as.logical(crit.test),
      sens.w = sens.w
    )

  } else {

    acc.test <- classtable(
      prediction_v = c(TRUE, FALSE, TRUE),
      criterion_v = c(TRUE, TRUE, FALSE),
      sens.w = sens.w
    )

    acc.test[1, ] <- NA

  }


  if (do_test == FALSE) {

    acc.train <- classtable(
      prediction_v = c(TRUE, FALSE, TRUE),
      criterion_v = c(FALSE, TRUE, TRUE),
      sens.w = sens.w
    )
    acc.train[1, ] <- NA

    acc.test <- classtable(
      prediction_v = c(TRUE, FALSE, TRUE),
      criterion_v = c(FALSE, TRUE, TRUE),
      sens.w = sens.w
    )
    acc.test[1, ] <- NA
  }


  # ORGANIZE output: ----

  output <- list(
    "accuracy" = list(train = acc.train, test = acc.test),
    "model" = train.mod,
    "algorithm" = algorithm
  )

  return(output)

} # comp_pred().




# ToDo: ------

# Reduce redundancies:
# - Avoid repeated computation of stats in add_stats() and classtable().
# - Consider re-using stats from add_stats() or classtable()
#   when printing (by console_confusionmatrix()) or plotting (by plot.FFTrees()) FFTs.

# eof.