#' Applies an existing FFTrees object to a new dataset
#'
#' @param object (M) An FFTrees object created from the FFTrees() function.
#' @param data (M) An m x n dataframe containing n cue values for each of the m exemplars.
#' @param formula a formula
#' @param tree An integer indicating which tree to plot (only valid when the tree argument is non-empty). To plot the best training (or test) tree with respect to v (HR - FAR), use "best.train" or "best.test"
#' @param level.name.v A character indicating the names of the levels in the tree separated by ;. For example "age;sex;occupation"
#' @param level.threshold.v (M) A character indicating the level thresholds separated by ;. For example "25;female;occupation"
#' @param level.sigdirection.v (M) A character vector of length n indicating the direction for which exemplars are classified as signals for each cue. Values must be in the set "<" (strictly less than), "<=" (less than or equal to), "=" (equal), "!=" (unequal), ">=" (greater than or equal to), or ">" (strictly greater than)/
#' @param level.exit.v (B) A numeric vector of length n indicating the exit direction for each level. 0 = noise clasification, 1 = signal decision, .5 = both.
#' @param level.class.v (B) A character vector of length n indicating the class of the cues for each level. "F" = factor, "N" = numeric, "L" = logical.
#' @param ... Additional arguments passed on to predict()
#' @return A list of length 3. The first element "decision.df" is a dataframe with the decisions (and level of decisions) for each exemplar. The second element, "final.df" is a dataframe showing final tree accuracy statistics. The third element "level.df" shows tree accuracy statistics at each level.
#' @export

predict.FFTrees <- function(
  object = NULL,
  data = NULL,
  formula = NULL,
  tree = NULL,
  level.name.v = NULL,
  level.threshold.v = NULL,
  level.sigdirection.v = NULL,
  level.exit.v = NULL,
  level.class.v = NULL,
  ...
) {
#
#
#   object = x
#   data = data.mf
#   formula = x$formula
#   tree = tree
#
#
#   level.name.v = NULL
#   level.threshold.v = NULL
#   level.sigdirection.v = NULL
#   level.exit.v = NULL
#   level.class.v = NULL



#
#   object = x
#   data = data.mf
#   formula = x$formula
#   tree = tree
#
#   level.name.v = NULL
#   level.threshold.v = NULL
#   level.sigdirection.v = NULL
#   level.exit.v = NULL
#   level.class.v = NULL


  if(is.null(tree)) {

    if(is.null(object) == F) {tree <- 1:nrow(object$tree.stats)}
    if(is.null(object) == T) {tree <- 1}

  }

  if(is.null(object) == F) {

    level.name.v <- object$tree.stats$level.name[tree]
    level.class.v <- object$tree.stats$level.class[tree]
    level.exit.v <- object$tree.stats$level.exit[tree]
    level.threshold.v <- object$tree.stats$level.threshold[tree]
    level.sigdirection.v <- object$tree.stats$level.sigdirection[tree]

    formula <- object$formula

    data.mf <- model.frame(formula = formula, data = data)
    cue.train <- data.mf[,2:ncol(data.mf)]
    crit.train <- data.mf[,1]

  }

  if(is.null(object)) {

    data.mf <- model.frame(formula = formula, data = data)
    cue.train <- data.mf[,2:ncol(data.mf)]
    crit.train <- data.mf[,1]

  }

  # Loop over trees

  n.exemplars <- length(crit.train)
  n.trees <- length(tree)
  tree.stats <- NULL

  decision.df <- as.data.frame(matrix(NA, nrow = n.exemplars, ncol = n.trees))
  levelout.df <- as.data.frame(matrix(NA, nrow = n.exemplars, ncol = n.trees))

  for(tree.i in tree) {

    tree.index <- which(tree.i == tree)

    level.name.i <-  unlist(strsplit(level.name.v[tree.index], ";"))
    level.class.i <-  unlist(strsplit(level.class.v[tree.index], ";"))
    level.exit.i <-  unlist(strsplit(level.exit.v[tree.index], ";"))
    level.threshold.i <-  unlist(strsplit(level.threshold.v[tree.index], ";"))
    level.sigdirection.i <-  unlist(strsplit(level.sigdirection.v[tree.index], ";"))

    n.levels <- length(level.name.i)

    decision <- rep(NA, n.exemplars)
    levelout <- rep(NA, n.exemplars)

    apply.break <- function(direction,
                            threshold.val,
                            cue.vec
    ) {


      if(is.character(threshold.val)) {threshold.val <- unlist(strsplit(threshold.val, ","))}


      if(direction == "!=") {output <- (cue.vec %in% threshold.val) == F}
      if(direction == "=") {output <- cue.vec %in% threshold.val}
      if(direction == "<") {output <- cue.vec < threshold.val}
      if(direction == "<=") {output <- cue.vec <= threshold.val}
      if(direction == ">") {output <- cue.vec > threshold.val}
      if(direction == ">=") {output <- cue.vec >= threshold.val}


      return(output)

    }

    for(level.j in 1:n.levels) {

      level.name.j <- level.name.i[level.j]
      level.class.j <- level.class.i[level.j]
      level.exit.j <- level.exit.i[level.j]
      level.threshold.j <- level.threshold.i[level.j]
      level.sigdirection.j <- level.sigdirection.i[level.j]

      if(level.class.j %in% c("numeric", "integer", "N")) {

        level.threshold.j <- as.numeric(level.threshold.j)

      }

      cue.vec <- unlist(cue.train[level.name.j])

      if(level.exit.j == 0) {

        class.now.index <- apply.break(level.sigdirection.j,
                                       level.threshold.j,
                                       cue.vec) == F & is.na(decision)

        decision[class.now.index] <- 0

      }

      if(level.exit.j == 1) {

        class.now.index <- apply.break(level.sigdirection.j,
                                       level.threshold.j,
                                       cue.vec) & is.na(decision)
        decision[class.now.index] <- 1

      }

      if(level.exit.j == .5 | level.exit.j == ".5") {

        class.now.index <- is.na(decision)
        decision[class.now.index] <- apply.break(level.sigdirection.j,
                                                 level.threshold.j,
                                                 cue.vec = cue.vec[class.now.index])

      }

      levelout[class.now.index] <- level.j

    }

    # get final stats

    tree.i.finalstats <- classtable(prediction.v = decision,
                                    criterion.v = crit.train)


    tree.i.finalstats$n.levels <- n.levels
    tree.i.finalstats$level.name <- level.name.v[tree.index]
    tree.i.finalstats$level.exit <- level.exit.v[tree.index]
    tree.i.finalstats$level.threshold <- level.threshold.v[tree.index]
    tree.i.finalstats$level.sigdirection <- level.sigdirection.v[tree.index]

    tree.i.finalstats$tree.num <- tree.i

    if(tree.index == 1) {tree.stats <- tree.i.finalstats}
    if(tree.index > 1) {tree.stats <- rbind(tree.stats, tree.i.finalstats)}

    # Update decision.df and levelout.df

    decision.df[,tree.index] <- decision
    levelout.df[,tree.index] <- levelout

  }

  names(levelout.df) <- paste("tree.", tree, sep = "")
  names(decision.df) <- paste("tree.", tree, sep = "")


tree.auc <- matrix(c(NA, auc(hr.v = tree.stats$hr, far.v = tree.stats$far)), nrow = 2, ncol = 1)
rownames(tree.auc) <- c("train", "test")
colnames(tree.auc) <- "fft"


# Calculate LR stats
if(is.null(object$lr.model) == F) {

# Remove cues with no variance

train.df.ex <- sapply(1:ncol(data), FUN = function(x) {length(unique(data[[x]]))})
data.lr <- data[,train.df.ex > 1]

# Remove cases with new factor values

orig.vals.ls <- lapply(1:ncol(data.lr), FUN = function(x) {unique(data.lr[,x])})

can.predict.mtx <- matrix(1, nrow = nrow(data.lr), ncol = ncol(data.lr))

for(i in 1:ncol(can.predict.mtx)) {

  test.vals.i <- data.lr[,1]

  if(is.numeric(test.vals.i)) {
    can.predict.mtx[,i] <- 1} else {

      can.predict.mtx[,i] <- paste(test.vals.i) %in% paste(orig.vals.ls[[i]])


    }
}

lr.can.predict <- rowMeans(can.predict.mtx) == 1

if(mean(lr.can.predict) != 1) {

  # warning(paste("Linear regression couldn't fit some testing data.", sum(model.can.predict), "out of",
  #               nrow(data.test), "cases (", round(sum(model.can.predict == 0) / length(model.can.predict), 2) * 100,
  #               "%) had to be ignored"))

}


lr.pred <- suppressWarnings(predict(object$lr.model,
                                    newdata = data.lr))

lr.pred <- 1 / (1 + exp(-lr.pred))
lr.pred.bin <- rep(0, length(lr.pred))
lr.pred.bin[lr.pred >= .5] <- 1

lr.stats <- classtable(prediction.v = lr.pred.bin,
                       criterion.v = crit.train)

lr.auc <- matrix(c(NA, auc(hr.v = lr.stats$hr, far.v = lr.stats$far)), nrow = 2, ncol = 1)
colnames(lr.auc) <- "lr"
rownames(lr.auc) <- c("train", "test")
lr.model <- object$lr.model

}
if(is.null(object$lr.model)) {

lr.stats <- NULL
lr.model <- NULL

lr.auc <- matrix(NA, nrow = 2, ncol = 1)
rownames(lr.auc) <- c("train", "test")
colnames(lr.auc) <- "lr"

}


# Calculate CART stats
if(is.null(object$cart.model) == F) {

cart.pred <- predict(object$cart.model,
                     data,
                     type = "class")

# Recode to logical

if("TRUE" %in% paste(cart.pred)) {cart.pred <- as.logical(paste(cart.pred))}
if("1" %in% paste(cart.pred)) {cart.pred <- as.logical(as.numeric(paste(cart.pred)))}


# Calculate training accuracy stats

cart.stats <- classtable(prediction.v = cart.pred,
                         criterion.v = crit.train)

cart.auc <- matrix(c(NA, auc(hr.v = cart.stats$hr, far.v = cart.stats$far)), nrow = 2, ncol = 1)
colnames(cart.auc) <- "cart"
rownames(cart.auc) <- c("train", "test")
cart.model <- object$cart.model

}
if(is.null(object$cart.model)) {

  cart.stats <- NULL
  cart.model <- NULL

  cart.auc <- matrix(NA, nrow = 2, ncol =1)
  rownames(cart.auc) <- c("train", "test")
  colnames(cart.auc) <- "cart"

}

# AUC

auc <- cbind(tree.auc, lr.auc, cart.auc)



  # Create output

  output <- list(
                  "formula" = formula,
                  "data.train" = data,
                  "data.test" = data,
                  "cue.accuracies" = NULL,
                  "tree.stats" = tree.stats,
                  "lr.stats" = lr.stats,
                  "cart.stats" = cart.stats,
                  "auc" = auc,
                  "decision" = decision.df,
                  "levelout" = levelout.df
  )

  return(output)

}
