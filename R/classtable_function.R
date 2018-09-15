#' Calculates several classification statistics from binary prediction and criterion (e.g.; truth) vectors
#' @param prediction.v logical. A logical vector of predictions
#' @param criterion.v logical A logical vector of criterion (true) values
#' @param sens.w numeric. Weight given to sensitivity, must range from 0 to 1.
#' @param cost.v list. An optional list of additional costs to be added to each case.
#' @param correction numeric. Correction added to all counts for calculating dprime
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying the costs of a hit, false alarm, miss, and correct rejection rspectively. E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a false alarm and miss cost 10 and 20 respectively while correct decisions have no cost.
#' @importFrom stats qnorm
#' @importFrom caret confusionMatrix

classtable <- function(prediction.v = NULL,
                       criterion.v,
                       sens.w = .5,
                       cost.v = NULL,
                       correction = .25,
                       cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0)) {
#
#
#   prediction.v <- sample(c(TRUE, FALSE), size = 20, replace = TRUE)
#   criterion.v <- sample(c(TRUE, FALSE), size = 20, replace = TRUE)
#   sens.w = .5
#   cost.v = NULL
#   correction = .25
#   cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0)

if(is.null(cost.v)) {cost.v <- rep(0, length(prediction.v))}

if(any(c("FALSE", "TRUE") %in% paste(prediction.v))) {

  prediction.v <- as.logical(paste(prediction.v))

}

if(any(c("FALSE", "TRUE") %in% paste(criterion.v))) {

  criterion.v <- as.logical(paste(criterion.v))

}

if(((class(prediction.v) != "logical") | class(criterion.v) != "logical") & !is.null(prediction.v)) {stop("prediction.v and criterion.v must be logical")}

  # Remove NA criterion values
  prediction.v <- prediction.v[is.finite(criterion.v)]
  criterion.v <- criterion.v[is.finite(criterion.v)]

  N <- min(length(criterion.v), length(prediction.v))

  if(N > 0) {

    if(var(prediction.v) > 0 & var(criterion.v) > 0) {

    cm <- caret::confusionMatrix(table(prediction.v, criterion.v),
                                      positive = "TRUE")

    cm_byClass <- data.frame(as.list(cm$byClass))
    cm_overall <- data.frame(as.list(cm$overall))

    hi <- cm$table[2, 2]
    mi <- cm$table[1, 2]
    fa <- cm$table[2, 1]
    cr <- cm$table[1, 1]

    # Corrected values
    hi_c <- hi + correction
    mi_c <- mi + correction
    fa_c <- fa + correction
    cr_c <- cr + correction

    # Statistics
    sens <- cm_byClass$Sensitivity
    spec <- cm_byClass$Specificity
    far <- 1 - spec
    acc <-  cm_overall$Accuracy
    acc_p <- cm_overall$AccuracyPValue
    ppv <- cm_byClass$Pos.Pred.Value
    npv <- cm_byClass$Neg.Pred.Value
    bacc <- cm_byClass$Balanced.Accuracy
    wacc <- cm_byClass$Sensitivity * sens.w + cm_byClass$Specificity * (1 - sens.w)
    dprime <- qnorm(hi_c / (hi_c + mi_c)) - qnorm(cr_c / (cr_c + fa_c))

    # cost per case
    costout <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr))) / N
    cost <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr)) + sum(cost.v)) / N

    # auc
#
#     auc <- as.numeric(pROC::roc(response = as.numeric(criterion.v),
#                                 predictor = as.numeric(prediction.v))$auc)

    } else {

      hi <- sum(prediction.v == TRUE & criterion.v == TRUE)
      mi <- sum(prediction.v == FALSE & criterion.v == TRUE)
      fa <- sum(prediction.v == TRUE & criterion.v == FALSE)
      cr <- sum(prediction.v == FALSE & criterion.v == FALSE)

      # Corrected values
      hi_c <- hi + correction
      mi_c <- mi + correction
      fa_c <- fa + correction
      cr_c <- cr + correction

      # Statistics
      sens <- hi / (hi + mi)
      spec <- cr / (cr + fa)
      far <- 1 - spec
      acc <-  (hi + cr) / c(hi + cr + mi + fa)
      acc_p <- NA
      ppv <- hi / (hi + fa)
      npv <- cr / (cr + mi)
      bacc <- sens * .5 + spec * .5
      wacc <- sens * sens.w + spec * (1 - sens.w)
      dprime <- qnorm(hi_c / (hi_c + mi_c)) - qnorm(cr_c / (cr_c + fa_c))

      # cost per case
      costout <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr))) / N
      cost <- (as.numeric(c(hi, fa, mi, cr) %*% c(cost.outcomes$hi, cost.outcomes$fa, cost.outcomes$mi, cost.outcomes$cr)) + sum(cost.v)) / N

      # auc
      # auc <- as.numeric(pROC::roc(response = as.numeric(criterion.v),
      #                             predictor = as.numeric(prediction.v))$auc)

    }

    } else {

    hi <- NA
    mi <- NA
    fa <- NA
    cr <- NA
    sens <- NA
    spec <- NA
    far <- NA
    ppv <- NA
    npv <- NA
    far <- NA
    acc <- NA
    acc_p <- NA
    bacc <- NA
    wacc <- NA
    dprime <- NA
    costout <- NA
    cost <- NA
    # auc <- NA

  }

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
    # auc = auc,
    bacc = bacc,
    wacc = wacc,
    dprime = dprime,
    costout = costout,
    cost = cost)

  return(result)

}
