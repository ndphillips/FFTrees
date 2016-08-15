#' Calculates predictions from CART
#' @param formula a formula
#' @param data.train A training dataset
#' @param data.test A testing dataset
#' @importFrom stats model.frame formula glm
#' @importFrom rpart rpart
#' @export
#'


cart.pred <- function(
  formula,
  data.train,
  data.test = NULL
) {


  data.mf.train <- model.frame(formula = formula, data = data.train)
  crit.train <- data.mf.train[,1]

  if(is.null(data.test) == F) {
    data.mf.test <- model.frame(formula = formula, data = data.test)
    crit.test <- data.mf.test[,1]
  } else {data.mf.test <- NULL}




  # Calculate loss df (for ROC curve)

  loss.df <- expand.grid(miss.cost = 1,
                         fa.cost = 1)

  # loss.df <- expand.grid(miss.cost = seq(1, 5, length.out = 3),
  #                        fa.cost = seq(1, 5, length.out = 3)
  # )

  # Remove some cases where costs are identical
  loss.df <- loss.df[loss.df$miss.cost != loss.df$fa.cost | loss.df$miss.cost == 1,]


  cart.train.acc.ls <- vector("list", length = nrow(loss.df))

  if(is.null(data.test) == F) {cart.test.acc.ls <- vector("list", length = nrow(loss.df))}


  cart.cues.vec <- c()

  for(row.i in 1:nrow(loss.df)) {

    miss.cost.i <- loss.df$miss.cost[row.i]
    fa.cost.i <- loss.df$fa.cost[row.i]

    # Train cart model

    cart.train.mod.i <- rpart::rpart(formula,
                                     data = data.train,
                                     method = "class",
                                     parms = list(loss = matrix(c(0, miss.cost.i, fa.cost.i, 0), byrow = T, nrow = 2))
    )

    # Determine the cues used by cart model

    cart.cues.used <- as.character(cart.train.mod.i$frame$var)
    cart.cues.used <- paste(cart.cues.used[cart.cues.used != "<leaf>"], collapse = ";")
    cart.cues.vec <- c(cart.cues.vec, cart.cues.used)


    # Get training decisions

    cart.train.pred.i <- predict(cart.train.mod.i,
                                 data.train,
                                 type = "class")

    # Recode to logical

    if("TRUE" %in% paste(cart.train.pred.i)) {cart.train.pred.i <- as.logical(paste(cart.train.pred.i))}
    if("1" %in% paste(cart.train.pred.i)) {cart.train.pred.i <- as.logical(as.numeric(paste(cart.train.pred.i)))}


    # Calculate training accuracy stats

    cart.train.acc.i <- classtable(prediction.v = cart.train.pred.i,
                                   criterion.v = crit.train)

    cart.train.acc.ls[[row.i]] <- cart.train.acc.i

    if(is.null(data.test) == F) {

      # Get test decisions

      cart.test.pred.i <- predict(cart.train.mod.i,
                                  data.test,
                                  type = "class")

      # Recode to logical

      if("TRUE" %in% paste(cart.test.pred.i) | "FALSE" %in% paste(cart.test.pred.i)) {cart.test.pred.i <- as.logical(paste(cart.test.pred.i))}
      if("1" %in% paste(cart.test.pred.i) | "0" %in% paste(cart.test.pred.i)) {cart.test.pred.i <- as.logical(as.numeric(paste(cart.test.pred.i)))}


      cart.test.acc.i <- classtable(prediction.v = cart.test.pred.i,
                                    criterion.v = crit.test)

      cart.test.acc.ls[[row.i]] <- cart.test.acc.i


    }

  }

  # SAVE BASIC CART MODEL

  cart.model <- rpart::rpart(formula,
                             data = data.train,
                             method = "class"
  )


  # Combine training statistics into a df

  cart.train.acc.df <- do.call(rbind, cart.train.acc.ls)
  cart.train.acc.df <- cbind(cart.train.acc.df, loss.df)

  ## Combine cart.train and cart.test

  cart.train.acc.df <- cart.train.acc.df[c("miss.cost", "fa.cost", "hr", "far", "v", "dprime")]
  names(cart.train.acc.df) <- c("miss.cost", "fa.cost", "hr.train", "far.train", "v.train", "dprime.train")


  cart.acc <- cbind(cart.train.acc.df,
                    cart.cues.vec)


  if(is.null(data.test) == F) {
    # Combine test statistics into a df
    cart.test.acc.df <- do.call(rbind, cart.test.acc.ls)
    cart.test.acc.df <- cbind(cart.test.acc.df, loss.df)

    cart.test.acc.df <- cart.test.acc.df[c("hr", "far", "v", "dprime")]
    names(cart.test.acc.df) <- c("hr.test", "far.test", "v.test", "dprime.test")
    cart.auc.test <- auc(cart.test.acc.df$hr.test, cart.test.acc.df$far.test)

  }

  if(is.null(data.test) == T) {
    # Combine test statistics into a df

    cart.test.acc.df <- as.data.frame(matrix(NA, nrow = nrow(cart.train.acc.df), ncol =4))
    names(cart.test.acc.df) <- c("hr.test", "far.test", "v.test", "dprime.test")

  }

  cart.acc <- cbind(cart.acc, cart.test.acc.df)



  # Sort in ascending order of FAR

  cart.acc <- cart.acc[order(cart.acc$far.train),]

  # Remove cases with the same result

  dup.rows <- duplicated.data.frame(cart.acc[,-c(1, 2)])
  cart.acc <- cart.acc[dup.rows == F | (cart.acc$miss.cost == 1 & cart.acc$fa.cost == 1),]

  # Remove cases without cues

  #cart.acc <- cart.acc[cart.acc$cart.cues.vec != "",]

  # Calculate auc

  cart.auc.train <- auc(hr.v = cart.acc$hr.train, far.v = cart.acc$far.train)


  if(is.null(data.test) == F) {
    cart.auc.test <- auc(hr.v = cart.acc$hr.test, far.v = cart.acc$far.test)
  } else {

    cart.auc.test <- NA
  }

  cart.auc <- matrix(c(cart.auc.train, cart.auc.test), nrow = 2, ncol = 1)
  colnames(cart.auc) <- "cart"
  rownames(cart.auc) <- c("train", "test")

  output <- list("accuracy" = cart.acc,
                 "auc" = cart.auc,
                 "model" = cart.model
  )

  return(output)


}
