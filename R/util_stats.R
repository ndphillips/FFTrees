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
#'
#' @param sens.w numeric. Sensitivity weight (for computing weighted accuracy, \code{wacc}).
#' Default: \code{sens.w = NULL} (to ensure that values are passed by calling function).
#'
#' @param my.goal Name of an optional, user-defined goal (as character string).
#' Default: \code{my.goal = NULL}.
#' @param my.goal.fun User-defined goal function (with 4 arguments \code{hi fa mi cr}).
#' Default: \code{my.goal.fun = NULL}.
#'
#' @param cost.outcomes list. A list of length 4 named \code{"hi"}, \code{"fa"}, \code{"mi"}, \code{"cr"}, and
#' specifying the costs of a hit, false alarm, miss, and correct rejection, respectively.
#' E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a
#' false alarm and miss cost 10 and 20 units, respectively, while correct decisions incur no costs.
#' Default: \code{cost.outcomes = NULL} (to ensure that values are passed by calling function).
#'
#' @param cost.each numeric. An optional fixed cost added to all outputs (e.g., the cost of using the cue).
#' Default: \code{cost.each = NULL} (to ensure that values are passed by calling function).
#'
#' @return A data frame with variables of computed accuracy and cost measures (but dropping inputs).

add_stats <- function(data, # df with frequency counts of classification outcomes ('hi fa mi cr', as integers)
                      #
                      correction = .25,  # used for dprime
                      sens.w = NULL,     # used for wacc
                      #
                      my.goal = NULL,
                      my.goal.fun = NULL,
                      #
                      cost.outcomes = NULL,  # (to ensure that values are passed by calling function), WAS: list(hi = 0, fa = 1, mi = 1, cr = 0)
                      cost.each = NULL
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

  # AUC?


  # my.goal:
  if (!is.null(my.goal)){

    # Compute my.goal value (by my.goal.fun):
    my_goal_value <- mapply(FUN = my.goal.fun, hi = hi, fa = fa, mi = mi, cr = cr)
    # print(paste0(my.goal, " = ", round(my_goal_value, 3)))  # 4debugging

    # Add to data (df, by name):
    data[[my.goal]] <- my_goal_value
    # print(paste0(my.goal, " = ", round(data[[my.goal]], 3)))  # 4debugging

  }


  # Cost:

  if (!is.null(cost.outcomes)){

    # a. Outcome cost (using NEGATIVE values, so that maximizing value will minimize costs):
    data$cost_dec <- -1 * ((hi * cost.outcomes$hi) + (fa * cost.outcomes$fa)
                           + (mi * cost.outcomes$mi) + (cr * cost.outcomes$cr)) / data$n  # Why data$n, not N?

    # b. Total cost:
    data$cost <- data$cost_dec - cost.each  # Subtract constant cost.each (due to negative cost).

  } else { # no cost.outcomes:

    data$cost_dec <- NA
    data$cost     <- NA

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
# [called by fftrees_grow_fan(), fftrees_apply(), and comp_pred() utility function below]:


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
#' Default: \code{sens.w = NULL} (to ensure that values are passed by calling function).
#'
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying
#' the costs of a hit, false alarm, miss, and correct rejection, respectively.
#' For instance, \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that
#' a false alarm and miss cost 10 and 20, respectively, while correct decisions have no cost.
#' Default: \code{cost.outcomes = NULL} (to ensure that values are passed by calling function).
#'
#' @param cost_v numeric. Additional cost value of each decision (as an optional vector of numeric values).
#' Typically used to include the cue cost of each decision (as a constant for the current level of an FFT).
#' Default: \code{cost_v = NULL} (to ensure that values are passed by calling function).
#'
#' @param my.goal Name of an optional, user-defined goal (as character string). Default: \code{my.goal = NULL}.
#' @param my.goal.fun User-defined goal function (with 4 arguments \code{hi fa mi cr}). Default: \code{my.goal.fun = NULL}.
#'
#' @param quiet_mis A logical value passed to hide/show \code{NA} user feedback
#' (usually \code{x$params$quiet$mis} of the calling function).
#' Default: \code{quiet_mis = FALSE} (i.e., show user feedback).
#' @param na_prediction_action What happens when no prediction is possible? (Experimental and currently unused.)
#'
#' @importFrom stats qnorm
#' @importFrom caret confusionMatrix


classtable <- function(prediction_v = NULL,
                       criterion_v  = NULL,
                       #
                       correction = .25,       # used for dprime calculation
                       sens.w = NULL,          # sens.w (to ensure that values are passed by calling function)
                       #
                       cost.outcomes = NULL,   # (to ensure that values are passed by calling function), WAS: list(hi = 0, fa = 1, mi = 1, cr = 0),
                       cost_v = NULL,          # cost value of each decision (at current level, as a constant)
                       #
                       my.goal = NULL,
                       my.goal.fun = NULL,
                       #
                       quiet_mis = FALSE,               # logical arg passed to hide/show NA user feedback
                       na_prediction_action = "ignore"  # is NOT used anywhere?  (see fin_NA_pred for options)
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


  # Handle NA values: ------

  # Note: As NA values in predictors of type character / factor / logical were handled in handle_NA(),
  #       only NA values in numeric predictors or the criterion variable appear here.

  if ( allow_NA_pred | allow_NA_crit ){

    # Detect NA values: ----

    ix_NA_pred <- is.na(prediction_v)  # 1. NA in prediction_v
    ix_NA_crit <- is.na(criterion_v)   # 2. NA in criterion_v


    # Report NA values (prior to removing them): ----

    if (!quiet_mis) { # Provide user feedback:

      # 1. Report NA in prediction_v:
      if (allow_NA_pred & any(ix_NA_pred)){

        sum_NA_pred <- sum(ix_NA_pred)

        # Which corresponding values in criterion_v will be removed?
        rem_criterion_v <- criterion_v[ix_NA_pred]
        rem_criterion_s <- paste0(rem_criterion_v, collapse = ", ")

        cli::cli_alert_warning("2x2: Ignoring {sum_NA_pred} NA value{?s} in prediction_v and corresponding criterion_v = c({rem_criterion_s}).")

      }

      # 2. Report NA in criterion_v:
      if (allow_NA_crit & any(ix_NA_crit)){

        sum_NA_crit <- sum(ix_NA_crit)

        # Which values in prediction_v will be removed?
        rem_prediction_v <- prediction_v[ix_NA_crit]
        rem_prediction_s <- paste0(rem_prediction_v, collapse = ", ")

        cli::cli_alert_warning("2x2: Ignoring {sum_NA_crit} NA value{?s} in criterion_v and corresponding prediction_v = c({rem_prediction_s}).")

      }

    } # if (!quiet_mis).


    # Main: Filter vectors ----

    # # A. Remove NA and infinite values (from both):
    # both_finite <- is.finite(prediction_v) & is.finite(criterion_v)
    #
    # prediction_v <- prediction_v[both_finite]
    # criterion_v  <- criterion_v[both_finite]


    # B. Remove only NA cases (from both):
    both_not_NA  <- !ix_NA_pred & !ix_NA_crit

    prediction_v <- prediction_v[both_not_NA]
    criterion_v  <- criterion_v[both_not_NA]


  } # Handle NA: if ( allow_NA_pred | allow_NA_crit ).


  N <- min(length(criterion_v), length(prediction_v))


  if (N > 0) { # use 2 vectors: ----

    # Note 2 special cases:

    var_pred_v <- var(prediction_v)
    var_crit_v <- var(criterion_v)


    quiet_na_var <- TRUE # FALSE  # HACK: as local constant (as object x or quiet list are not passed)

    if (!quiet_na_var) { # Provide user feedback:

      if (is.na(var_pred_v)){

        # Provide user feedback:
        prediction_v_s <- paste(prediction_v, collapse = ", ")

        # msg_1a <- "A prediction vector has no variance (NA):"
        # msg_2a <- paste0("prediction_v = ", prediction_v_s, ".")
        msg_3a <- paste0("\u2014 No variance in 'prediction_v = c(", prediction_v_s, ")'.")

        # cat(u_f_hig(msg_1a, "\n"))  # highlighted and non-optional
        # cat(u_f_hig(msg_2a, "\n"))
        cat(u_f_hig(msg_3a, "\n"))

        # cli::cli_alert_warning(msg_1a)
        # cli::cli_alert_warning(msg_2a)

      }

      if (is.na(var_crit_v)){

        # Provide user feedback:
        criterion_v_s <- paste(criterion_v, collapse = ", ")

        # msg_1b <- "A criterion vector has no variance (NA):"
        # msg_2b <- paste0("criterion_v = ", criterion_v_s, ".")
        msg_3b <- paste0("\u2014 No variance in 'criterion_v = c(", criterion_v_s, ")'.")

        # cat(u_f_hig(msg_1b, "\n"))  # highlighted and non-optional
        # cat(u_f_hig(msg_2b, "\n"))
        cat(u_f_hig(msg_3b, "\n"))

        # cli::cli_alert_warning(msg_1b)
        # cli::cli_alert_warning(msg_2b)

      }

    } # if (!quiet_na_var).


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

      # Get (or compute) statistics:
      sens <- cm_byClass$Sensitivity
      spec <- cm_byClass$Specificity
      far  <- (1 - spec)

      ppv <- cm_byClass$Pos.Pred.Value
      npv <- cm_byClass$Neg.Pred.Value

      acc   <- cm_overall$Accuracy
      acc_p <- cm_overall$AccuracyPValue
      bacc  <- cm_byClass$Balanced.Accuracy
      wacc  <- (sens * sens.w) + (spec * (1 - sens.w))  # (use values from above)
      # wacc  <- (cm_byClass$Sensitivity * sens.w) + (cm_byClass$Specificity * (1 - sens.w))


      # dprime:

      # a. Corrected freq values:
      hi_c <- hi + correction
      fa_c <- fa + correction
      mi_c <- mi + correction
      cr_c <- cr + correction

      # b. dprime (corrected):
      # hi_rate <- hi_c / (hi_c + mi_c)
      # fa_rate <- fa_c / (fa_c + cr_c)
      # dprime <- qnorm(hi_rate) - qnorm(fa_rate)
      dprime <- qnorm(hi_c / (hi_c + mi_c)) - qnorm(fa_c / (fa_c + cr_c))

      # AUC:
      # auc <- as.numeric(pROC::roc(response = as.numeric(criterion_v),
      #                             predictor = as.numeric(prediction_v))$auc)

      # my.goal:
      if (!is.null(my.goal)){

        my_goal_value <- mapply(FUN = my.goal.fun, hi = hi, fa = fa, mi = mi, cr = cr)
        # print(paste0(my.goal, " = ", round(my_goal_value, 3)))  # 4debugging

      }

      # Cost:

      # Costs (per classification outcome case):

      # print(unlist(cost.outcomes))  # 4debugging
      # print(cost_v)  # 4debugging: Cost of each decision (cue cost at current level, as a constant)

      if (!is.null(cost.outcomes)){

        cost_dec <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr))) / N
        cost     <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr)) + sum(cost_v)) / N

      } else { # no cost.outcomes:

        cost_dec <- NA
        cost     <- NA

      }


    } else { # Case 2. Compute stats from freq combinations: ----

      if (!quiet_na_var){ # Provide user feedback:

        msg_c2 <- ("\u2014 Computing stats from freq counts, rather than caret::confusionMatrix()")
        cat(u_f_hig(msg_c2, "\n"))

      }

      # Compute freqs as sum of T/F combinations:
      hi <- sum(prediction_v == TRUE  & criterion_v == TRUE)
      fa <- sum(prediction_v == TRUE  & criterion_v == FALSE)
      mi <- sum(prediction_v == FALSE & criterion_v == TRUE)
      cr <- sum(prediction_v == FALSE & criterion_v == FALSE)

      N <- (hi + fa + mi + cr)

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

      # dprime:

      # a. Corrected freq values:
      hi_c <- hi + correction
      fa_c <- fa + correction
      mi_c <- mi + correction
      cr_c <- cr + correction

      # b. dprime (corrected):
      dprime <- qnorm(hi_c / (hi_c + mi_c)) - qnorm(fa_c / (fa_c + cr_c))

      # AUC:
      # auc <- as.numeric(pROC::roc(response = as.numeric(criterion_v),
      #                             predictor = as.numeric(prediction_v))$auc)

      # my.goal:
      if (!is.null(my.goal)){

        my_goal_value <- mapply(FUN = my.goal.fun, hi = hi, fa = fa, mi = mi, cr = cr)
        # print(paste0(my.goal, " = ", round(my_goal_value, 3)))  # 4debugging

      }


      # Cost:

      # Costs (per classification outcome case):

      # print(unlist(cost.outcomes))  # 4debugging
      # print(cost_v)  # 4debugging: Cost of each decision (cue cost at current level, as a constant)

      if (!is.null(cost.outcomes)){

        cost_dec <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr))) / N
        cost     <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr)) + sum(cost_v)) / N

      } else { # no cost.outcomes:

        cost_dec <- NA
        cost     <- NA

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

    my_goal_val <- NA

    cost_dec <- NA
    cost     <- NA

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


#' Fit and predict competing classification algorithms
#'
#' \code{comp_pred} provides a wrapper for running (i.e., fit or predict)
#' alternative classification algorithms to data
#' (i.e., \code{data.train} or \code{data.test}, respectively).
#'
#' The range of competing algorithms currently available includes
#' logistic regression (\code{stats::glm}),
#' CART (\code{rpart::rpart}),
#' support vector machines (\code{e1071::svm}), and
#' random forests (\code{randomForest::randomForest}).
#'
#' The current support for handling missing data (or \code{NA} values) is only rudimentary.
#' When enabled (via the global options \code{allow_NA_pred} or \code{allow_NA_crit}),
#' any rows in \code{data.train} or \code{data.test} with incomplete cases are being removed
#' prior to fitting or predicting a model (by using \code{na.omit} from \strong{stats}).
#' See the specifications of each model for more sophisticated ways of handling missing data.
#'
#' @param formula A formula (usually \code{x$formula}, for an \code{FFTrees} object \code{x}).
#' @param data.train A training dataset (as a data frame).
#' @param data.test A testing dataset (as a data frame).
#'
#' @param algorithm A character string specifying an algorithm in the set:
#' \itemize{
#'   \item{\code{"lr"}: Logistic regression (using \code{\link{glm}} from \strong{stats} with \code{family = "binomial"});}
#'   \item{\code{"rlr"}: Regularized logistic regression (currently not supported);}
#'   \item{\code{"cart"}: Decision trees (using \code{rpart} from \strong{rpart});}
#'   \item{\code{"svm"}: Support vector machines (using \code{svm} from \strong{e1071});}
#'   \item{\code{"rf"}: Random forests (using \code{randomForest} from \strong{randomForest}.}
#' }
#'
#' @param model An optional existing model (as a \code{model}), to be applied to the test data.
#'
#' @param sens.w Sensitivity weight parameter (numeric, from \code{0} to \code{1}), required to compute \code{wacc}.
#'
#' @param new.factors What should be done if new factor values are discovered in the test set (as a character string)?
#' Available options:
#' \itemize{
#'   \item{\code{"exclude"}: exclude case (i.e., remove these cases, used by default);}
#'   \item{\code{"base"}: predict the base rate of the criterion.}
#' }
#'
#' @param quiet_mis A logical value passed to hide/show \code{NA} user feedback
#' (usually \code{x$params$quiet$mis} of the calling function).
#' Default: \code{quiet_mis = FALSE} (i.e., show user feedback).
#'
#' @importFrom dplyr bind_rows
#' @importFrom stats formula glm model.frame model.matrix na.omit
#' @importFrom e1071 svm
#' @importFrom rpart rpart
#' @importFrom randomForest randomForest

comp_pred <- function(formula,
                      data.train,
                      data.test = NULL,
                      algorithm = NULL,
                      model = NULL,
                      sens.w = NULL,
                      new.factors = "exclude",
                      quiet_mis = FALSE         # logical arg passed to hide/show NA user feedback
) {

  #   formula = x$formula
  #   data.train = x$data$train
  #   data.test = x$data$test
  #   algorithm = "lr"
  #   model = NULL

  if (is.null(formula)) {
    stop("A valid formula is required")
  }

  if (is.null(algorithm)) {
    stop("Please specify one of the following models: 'lr', 'cart', 'svm', 'rf'")
    # Note: 'rlr' is currently not supported (see below).
  }

  if (inherits(algorithm, "character")) {
    algorithm <- tolower(algorithm)  # 4robustness
  }


  # SETUP: ----

  # Get data_all, train_cases, and test_cases:

  if (is.null(data.test) & (is.null(data.train) == FALSE)) { # only train data:
    data_all <- data.train
    train_cases <- 1:nrow(data.train)
    test_cases <- c()
  }

  if (is.null(data.test) == FALSE & (is.null(data.train) == FALSE)) { # both train + test data:
    # data_all <- rbind(data.train, data.test)  # Note: fails when both dfs have different variables!
    data_all <- dplyr::bind_rows(data.train, data.test)  # fills any non-matching columns with NAs.
    train_cases <- 1:nrow(data.train)
    test_cases  <- (nrow(data.train) + 1):nrow(data_all)
  }

  if (is.null(data.train) & (is.null(data.test) == FALSE)) { # only test data:
    data_all <- data.test
    train_cases <- c()
    test_cases  <- 1:nrow(data_all)
  }

  # print(data_all)  # 4debugging

  # Turn data_all into model.frame:
  data_all <- model.frame(
    formula = formula,
    data = data_all,
    na.action = NULL  # keeps NA cases
  )

  # print(data_all)  # 4debugging



  train_crit <- data_all[train_cases, 1]


  # Set a flag:
  do_test <- TRUE  # default

  # Remove columns with no variance in training data:
  if (!is.null(data.train)) {

    if (isTRUE(all.equal(length(unique(data_all[train_cases, 1])), 1))) { # no variance in train_cases:

      do_test <- FALSE  # unflag

    }
  }


  # Pre-process columns in data_all:

  if (do_test) {

    # Identify the columns with variance in data_all:
    if (is.null(train_cases) == FALSE) {

      ok_cols <- sapply(1:ncol(data_all), FUN = function(x) {
        length(unique(data_all[train_cases, x])) > 1
      })

      data_all <- data_all[ , ok_cols]  # select columns

    }

    # Convert character columns to factors:
    for (col_i in 1:ncol(data_all)) {

      if (inherits(data_all[ , col_i], c("logical", "character", "factor"))) {
        data_all[ , col_i] <- factor(data_all[ , col_i])
      }

    } # for (col_i).

  } # if (do_test).


  # print(data_all)  # 4debugging: NA cases are still present?
  # print(data.train)  # 4debugging: NA cases are still present.


  # Get training data (data.train): ----

  if (!is.null(train_cases)) {

    data.train <- data_all[train_cases, ]
    # cues_train  <- data_all[train_cases, -1]  # is NOT used anywhere?
    crit_train <- data_all[train_cases, 1]

  } else {

    data.train <- NULL
    # cues_train  <- NULL  # is NOT used anywhere?
    crit_train <- NULL

  }


  # Build models for training data: ------


  # Handle NA values (in data.train): ----

  if (any(is.na(data.train))){ # NAs in data.train:

    nr_NA <- sum(is.na(data.train))  # count (before removal)
    # ix_NA_row <- rowSums(is.na(data.train)) > 0  # rows with NA values

    if ( allow_NA_pred | allow_NA_crit ){ # Only remove cases with NA values (rather than doing anything fancy):

      # Remove incomplete cases:
      data.train <- stats::na.omit(data.train)

      # Adjust crit_train accordingly:
      crit_train <- data.train[ , 1]  # 1st column

      if (!quiet_mis) { # Provide user feedback:

        n_train <- nrow(data.train)

        cli::cli_alert_warning("Fitting {toupper(algorithm)}: Found {nr_NA} NA{?s} in 'data.train'. Removing incomplete cases left {n_train} case{?s}.")

      }

    } else { # do nothing, but warn:

      if (!quiet_mis) { # Provide user feedback:

        n_train <- nrow(data.train)

        cli::cli_alert_warning("Fitting {toupper(algorithm)}: Found {nr_NA} NA{?s} in 'data.train': Keeping all {n_train} case{?s}, but applying default algorithms may yield errors.")

      }
    }
  }

  # print(data.train)  # 4debugging: Have NA cases been removed?
  # print(crit_train)


  # 1. LR: binomial LR ----

  if (algorithm == "lr") {

    if (is.null(model)) {

      # Create new LR model:
      train_mod <- suppressWarnings(glm(formula, data.train, family = "binomial"))

    } else {

      train_mod <- model

    }

    if (is.null(data.train) == FALSE) {

      pred_train <- suppressWarnings(round(1 / (1 + exp(-predict(train_mod, data.train))), 0))

    } else {

      pred_train <- NULL

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
      train_mod <- e1071::svm(formula,
                              data = data.train, type = "C"
      )

    } else {

      train_mod <- model
    }

    if (is.null(data.train) == FALSE) {

      pred_train <- predict(train_mod,
                            data = data.train
      )

    } else {

      pred_train <- NULL
    }

  } # if (algorithm == "svm").


  # 4. CART: ----

  if (algorithm == "cart") {

    if (is.null(model)) {

      # Create new CART model:
      train_mod <- rpart::rpart(formula,
                                data = data.train,
                                method = "class"
      )

    } else {

      train_mod <- model
    }

    if (is.null(data.train) == FALSE) {

      pred_train <- predict(train_mod,
                            data.train,
                            type = "class"
      )

    } else {

      pred_train <- NULL
    }

  } # if (algorithm == "cart").


  # 5. RF: ----

  if (algorithm == "rf") {

    if (is.null(model)) {

      # Create new RF model:
      data.train[ , 1] <- factor(data.train[ , 1])

      train_mod <- randomForest::randomForest(formula,
                                              data = data.train
      )

    } else {

      train_mod <- model

    }

    if (is.null(data.train) == FALSE) {

      # Get training decisions:
      pred_train <- predict(
        train_mod,
        data.train
      )

    } else {

      pred_train <- NULL

    }

  } # if (algorithm == "rf").



  # Get testing data (data.test): ------

  pred_test <- NULL

  if (!is.null(data.test)) {

    data.test <- data_all[test_cases, ]
    # cues_test <- data_all[test_cases, -1]  # is NOT used anywhere?
    crit_test <- data_all[test_cases,  1]


    # Handle NA values (in data.test): ----

    if (any(is.na(data.test))){ # NAs in data.test:

      nr_NA <- sum(is.na(data.test))  # count (before removal)

      if ( allow_NA_pred | allow_NA_crit ){ # Only remove cases with NA values (rather than doing anything fancy):

        # Remove incomplete cases:
        data.test <- stats::na.omit(data.test)

        # Adjust crit_test accordingly:
        crit_test <- data.test[ , 1]  # 1st column

        if (!quiet_mis) { # Provide user feedback:

          n_test <- nrow(data.test)

          cli::cli_alert_warning("Predicting {toupper(algorithm)}: Found {nr_NA} NA{?s} in 'data.test'. Removing incomplete cases left {n_test} case{?s}.")

        }

      } else { # do nothing, but warn:

        if (!quiet_mis) { # Provide user feedback:

          n_test <- nrow(data.test)

          cli::cli_alert_warning("Aiming to predict {toupper(algorithm)}: Found {nr_NA} NA{?s} in 'data.test': Keeping all {n_test} case{?s}, but applying default algorithms may yield errors.")

        }
      }
    }

    # print(data.test)  # 4debugging: Have NA cases been removed?


    # Check for new factor values: ----
    {

      if (!is.null(train_cases)) {
        factor_ls <- lapply(1:ncol(data.train), FUN = function(x) {
          unique(data.train[ , x])
        })

      } else {

        factor_ls <- lapply(1:ncol(data.test), FUN = function(x) {
          cue_x <- names(data.test)[x]

          if (cue_x %in% names(model$xlevels)) {

            return(model$xlevels[[cue_x]])

          } else {

            return(unique(data.test[ , x]))

          }
        })
      }


      cannot_pred_mtx <- matrix(0, nrow = nrow(data.test), ncol = ncol(data.test))

      for (i in 1:ncol(cannot_pred_mtx)) {

        if (inherits(data.test[, i], c("factor", "character"))) {
          cannot_pred_mtx[ , i] <- data.test[ , i] %in% factor_ls[[i]] == FALSE
        }

      } # for().


      cannot_pred_vec <- rowSums(cannot_pred_mtx) > 0

      if (any(cannot_pred_vec)) {

        if (substr(new.factors, 1, 1) == "e") { # "exclude":

          warning(paste(sum(cannot_pred_vec), "cases in the test data could not be predicted by 'e' due to new factor values. These cases will be excluded"))

          data.test <- data.test[cannot_pred_vec == FALSE, ]
          # cues_test <- cues_test[cannot_pred_vec == FALSE, ]  # is NOT used anywhere?
          crit_test <- crit_test[cannot_pred_vec == FALSE]
        }

        if (substr(new.factors, 1, 1) == "b") { # "base" rate:

          warning(paste(sum(cannot_pred_vec), "cases in the test data could not be predicted by 'b' due to new factor values. They will be predicted to be", mean(train_crit) > .5))
        }

      }

    } # Check for new factor values.



    # Get predictions (pred_test) from each model: ------

    # 1. LR: ----

    if (algorithm == "lr") {

      if (is.null(data.test) == FALSE) {

        pred_test <- rep(0, nrow(data.test))

        if (any(cannot_pred_vec) & substr(new.factors, 1, 1) == "b") { # "base" rate:

          pred_test[cannot_pred_vec] <- mean(train_crit) > .5
          pred_test[cannot_pred_vec == FALSE] <- round(1 / (1 + exp(-predict(train_mod, data.test[cannot_pred_vec == FALSE, ]))), 0)

        } else {

          pred_test[!cannot_pred_vec] <- round(1 / (1 + exp(-predict(train_mod, data.test[cannot_pred_vec == FALSE, ]))), 0)
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

        try_pred <- try(predict(train_mod, data.test), silent = TRUE)

        if (inherits(try_pred, "try-error")) {

          warning("svm crashed predicting new data. That's all I can say")

          pred_test <- NULL

        } else {

          pred_test <- predict(train_mod, data.test)
        }
      }

    } # if (algorithm == "svm").


    # 4. CART: ----

    if (algorithm == "cart") {

      if (is.null(data.test) == FALSE) {

        if (any(cannot_pred_vec) & substr(new.factors, 1, 1) == "b") { # "base" rate:

          pred_test <- rep(0, nrow(data.test))
          pred_test[cannot_pred_vec] <- mean(train_crit) > .5
          pred_test[cannot_pred_vec == FALSE] <- predict(train_mod, data.test[cannot_pred_vec == FALSE, ], type = "class")

        } else {

          pred_test <- predict(train_mod, data.test[cannot_pred_vec == FALSE, ], type = "class")
        }
      }

    } # if (algorithm == "cart").


    # 5. RF: ----

    if (algorithm == "rf") {

      if (is.null(data.test) == FALSE) {

        crit_test <- as.factor(crit_test)

        # Get levels of training criterion
        train_crit_levels <- levels(train_mod$y)

        # convert test crit to factor
        crit_name <- paste(formula)[2]

        data_test_2 <- data.test
        data_test_2[crit_name] <- factor(data_test_2[[crit_name]], levels = train_crit_levels)


        # See if we can do any predictions:
        try_pred <- try(predict(train_mod, data_test_2), silent = TRUE)

        if (inherits(try_pred, "try-error")) {

          warning("randomForest crashed predicting new data. That's all I can say, unfortunately")

          pred_test <- NULL

        } else {

          pred_test <- predict(train_mod, data.test)
        }

        # if(any(cannot_pred_vec) & substr(new.factors, 1, 1) == "b") { # "base" rate:
        #
        #   pred_test <- rep(0, nrow(data.test))
        #   pred_test[cannot_pred_vec] <- mean(train_crit) > .5
        #   pred_test[cannot_pred_vec == FALSE] <- predict(train_mod, data.test[cannot_pred_vec == FALSE,])
        #
        # } else {
        #
        #   pred_test <- predict(train_mod, data.test[cannot_pred_vec == FALSE,])
        #
        # }
      }

    } # if (algorithm == "rf").

  } # if (is.null(data.test) == FALSE)).


  # Convert predictions to logical (if necessary): ----

  if (is.null(pred_train) == FALSE) {

    if ("TRUE" %in% paste(pred_train)) {
      pred_train <- as.logical(paste(pred_train))
    }

    if ("1" %in% paste(pred_train)) {
      pred_train <- as.logical(as.numeric(paste(pred_train)))
    }


    # Calculate training accuracy stats: ----

    acc_train <- classtable(
      prediction_v = as.logical(pred_train),
      criterion_v = as.logical(crit_train),
      sens.w = sens.w,
      quiet_mis = quiet_mis
    )
  }

  if (is.null(pred_train)) {

    acc_train <- classtable(
      prediction_v = c(TRUE, TRUE, FALSE),
      criterion_v =  c(FALSE, FALSE, TRUE),
      sens.w = sens.w,
      quiet_mis = quiet_mis
    )

    acc_train[1, ] <- NA

  }

  if (is.null(pred_test) == FALSE) {

    if ("TRUE" %in% paste(pred_test)) {
      pred_test <- as.logical(paste(pred_test))
    }

    if ("1" %in% paste(pred_test)) {
      pred_test <- as.logical(as.numeric(paste(pred_test)))
    }

    acc_test <- classtable(
      prediction_v = as.logical(pred_test),
      criterion_v = as.logical(crit_test),
      sens.w = sens.w,
      quiet_mis = quiet_mis
    )

  } else {

    acc_test <- classtable(
      prediction_v = c(TRUE, FALSE, TRUE),
      criterion_v = c(TRUE, TRUE, FALSE),
      sens.w = sens.w,
      quiet_mis = quiet_mis
    )

    acc_test[1, ] <- NA

  }


  if (do_test == FALSE) {

    acc_train <- classtable(
      prediction_v = c(TRUE, FALSE, TRUE),
      criterion_v = c(FALSE, TRUE, TRUE),
      sens.w = sens.w,
      quiet_mis = quiet_mis
    )

    acc_train[1, ] <- NA

    acc_test <- classtable(
      prediction_v = c(TRUE, FALSE, TRUE),
      criterion_v = c(FALSE, TRUE, TRUE),
      sens.w = sens.w,
      quiet_mis = quiet_mis
    )

    acc_test[1, ] <- NA

  }


  # OUTPUT: ------

  # Organize output:
  output <- list(
    "accuracy" = list(train = acc_train, test = acc_test),
    "model" = train_mod,
    "algorithm" = algorithm
  )

  return(output)

} # comp_pred().



# ToDo: ------

# Reduce redundancy:
# - Avoid repeated computation of same stats in add_stats() and classtable().
# - Consider re-using the stats from add_stats() or classtable()
#   when printing (by console_confusionmatrix()) or plotting (by plot.FFTrees()) FFTs.

# eof.
