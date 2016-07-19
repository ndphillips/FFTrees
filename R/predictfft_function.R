#' Applies an existing fft to a new dataset
#'
#' @param object (M) An fft object created from the fft() function.
#' @param data (M) An m x n dataframe containing n cue values for each of the m exemplars.
#' @param formula a formula
#' @param which.tree which.tree An integer indicating which tree to plot (only valid when the tree argument is non-empty). To plot the best training (or test) tree with respect to v (HR - FAR), use "best.train" or "best.test"
#' @param level.name.v A character indicating the names of the levels in the tree separated by ;. For example "age;sex;occupation"
#' @param level.threshold.v (M) A character indicating the level thresholds separated by ;. For example "25;female;occupation"
#' @param level.sigdirection.v (M) A character vector of length n indicating the direction for which exemplars are classified as signals for each cue. Values must be in the set "<" (strictly less than), "<=" (less than or equal to), "=" (equal), "!=" (unequal), ">=" (greater than or equal to), or ">" (strictly greater than)/
#' @param level.exit.v (B) A numeric vector of length n indicating the exit direction for each level. 0 = noise clasification, 1 = signal decision, .5 = both.
#' @param level.class.v (B) A character vector of length n indicating the class of the cues for each level. "F" = factor, "N" = numeric, "L" = logical.
#' @param ... Additional arguments passed on to predict()
#' @return A list of length 3. The first element "decision.df" is a dataframe with the decisions (and level of decisions) for each exemplar. The second element, "final.df" is a dataframe showing final tree accuracy statistics. The third element "level.df" shows tree accuracy statistics at each level.
#' @export

predict.fft <- function(
  object = NULL,
  data = NULL,
  formula = NULL,
  which.tree = NULL,
  level.name.v = NULL,
  level.threshold.v = NULL,
  level.sigdirection.v = NULL,
  level.exit.v = NULL,
  level.class.v = NULL,
  ...
) {


#
#   x <- a
#   data <- heartdisease

  if(is.null(which.tree)) {

    if(is.null(object) == F) {which.tree <- 1:nrow(object$trees)}
    if(is.null(object) == T) {which.tree <- 1}

  }

  if(is.null(object) == F) {

    level.name.v <- object$trees$level.name[which.tree]
    level.class.v <- object$trees$level.class[which.tree]
    level.exit.v <- object$trees$level.exit[which.tree]
    level.threshold.v <- object$trees$level.threshold[which.tree]
    level.sigdirection.v <- object$trees$level.sigdirection[which.tree]

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
  n.trees <- length(which.tree)
  tree.stats <- NULL

  decision.df <- as.data.frame(matrix(NA, nrow = n.exemplars, ncol = n.trees))
  levelout.df <- as.data.frame(matrix(NA, nrow = n.exemplars, ncol = n.trees))

  for(tree.i in which.tree) {

    tree.index <- which(tree.i == which.tree)

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

  names(levelout.df) <- paste("tree.", which.tree, sep = "")
  names(decision.df) <- paste("tree.", which.tree, sep = "")

  # Calculate AUC

  tree.auc.df <- data.frame("test" = auc(tree.stats$hr, tree.stats$far))

  # Calculate LR stats


  # Create output

  output <- list(
                  "formula" = formula,
                  "data.train" = NULL,
                  "data.test" = data,
                  "cue.accuracies" = NULL,
                  "trees" = tree.stats,
                  "trees.auc" = tree.auc.df,
                  "lr" = NULL,
                  "cart" = NULL,
                  "decision" = decision.df,
                  "levelout" = levelout.df
  )

  return(output)

}
