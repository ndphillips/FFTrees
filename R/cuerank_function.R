#' Calculate the marginal accuracy of all cues in a dataframe. For each cue, the threshold that maximizes the criterion is selected.
#'
#' @param formula formula. A formula specifying a binary criterion as a function of multiple variables
#' @param data dataframe. A dataframe containing variables in formula
#' @param goal character. A string indicating the statistic to maximize: "v" = HR - FAR, "d" = d-prime, "c" = correct decisions
#' @param numthresh.method character. A string indicating how to calculate cue splitting thresholds. "m" = median split, "o" = split that maximizes the tree criterion.
#' @param rounding integer. An integer indicating digit rounding for non-integer numeric cue thresholds. The default is NULL which means no rounding. A value of 0 rounds all possible thresholds to the nearest integer, 1 rounds to the nearest .1 (etc.).
#' @param verbose logical. A logical value indicating whether or not to print ongoing diagnostics
#' @param cue.rules dataframe. An optional df specifying how to make decisions for each cue. Must contain columns "cue", "class", "threshold" and "direction"
#' @importFrom stats median
#' @return A dataframe containing best thresholds and marginal classification statistics for each cue
#' @export
#' @examples
#'
#'  # What are the best thresholds for each cue in the mushrooms dataset?
#'  mushrooms.cues <- cuerank(formula = poisonous ~.,
#'                            data = mushrooms)
#'
#'
#'

cuerank <- function(formula = NULL,
                    data = NULL,
                    goal = "v",
                    numthresh.method = "o",
                    rounding = NULL,
                    verbose = F,
                    cue.rules = NULL

) {


  # formula = formula
  # data = data
  # goal = criterion
  # numthresh.method = numthresh.method
  # rounding = rounding
  # verbose = verbose
  # cue.rules = NULL

# TESTING GROUNDS
# -----
#
# formula <- diagnosis ~.
# data = heartdisease
# goal = "v"
# numthresh.method = "o"
# rounding = NULL
# verbose = F
# cue.rules = NULL


# EXTRACT FORMULA VARIABLES
data.mf <- model.frame(formula = formula,
                       data = data)

criterion.v <- data.mf[,1]
cue.df <- data.mf[,2:ncol(data.mf)]

if(class(cue.df) != "data.frame") {

  cue.df <- data.frame(cue.df)
  names(cue.df) <- names(data.mf)[2]

}


# GLOBAL VARIABLES (could be updated later)

correction <- .25
max.numcat <- 20        # Maximum number of numeric thresholds to consider
n.cues <- ncol(cue.df)


# Adjust inputs
if(substr(goal, 1, 1) == "c") {goal <- "cor"}
if(substr(goal, 1, 1) == "v") {goal <- "v"}
if(substr(goal, 1, 1) == "d") {goal <- "dprime"}

# CHECK cue.rules

if(is.null(cue.rules) == FALSE) {

if(all(c("cue", "class", "threshold", "direction") %in% names(cue.rules)) == FALSE) {

  stop("You specified a cue.rules object but the object does not contain critical columns. Look at the descriptions for cue.rules and try again")

}
}

for(cue.i in 1:n.cues) {

  cue <- names(cue.df)[cue.i]
  class <- class(cue.df[,cue.i])
  cue.v <- unlist(cue.df[,cue.i])

  if(setequal(class, c("ordered", "factor"))) {

    class <- "integer"
    cue.v <- as.numeric(cue.v)

  }

  if("factor" %in% class(cue.v)) {cue.v <- paste(cue.v)}

  # Step 0B: Determine cue levels

  if(substr(class, 1, 1) %in% c("n", "i")) {

    if(numthresh.method == "o" & is.null(cue.rules)) {

      cue.levels <- sort(unique(unlist(cue.v)))
      if(length(cue.levels) > max.numcat) {

        indicies <- round(seq(1, length(cue.levels), length.out = max.numcat), 0)
        cue.levels <- cue.levels[indicies]

      }


    }

    if(numthresh.method == "m" & is.null(cue.rules)) {

      cue.levels <- median(unlist(cue.v))

    }

    if(is.null(cue.rules) == FALSE) {

      cue.levels <- as.numeric(cue.rules$threshold[cue.rules$cue == cue])

    }

    if(!is.null(rounding)) {cue.levels <- round(cue.levels, digits = rounding)}

    cue.levels <- cue.levels[duplicated(cue.levels) == FALSE]

  }

  if((substr(class, 1, 1) %in% c("n", "i")) == FALSE) {

    if(is.null(cue.rules)) {

      cue.levels <- sort(unique(unlist(cue.v)))

    }

    if(is.null(cue.rules) == FALSE) {

      cue.levels <- cue.rules$threshold[cue.rules$cue == cue]

    }

  }

  cue.n <- length(cue.levels)
  accuracy.names <- names(classtable(c(1, 0, 1), c(1, 1, 0)))

  # Step 1A: Determine best direction for all cue levels

  # FACTORS

  if(substr(class, 1, 1) %in% c("f", "c", "l")) {

    if(is.null(cue.rules)) {

    direction.vec <- c("=", "!=")

    }

    if(is.null(cue.rules) == F) {

    direction.vec <- cue.rules$direction[cue.rules$cue == cue]

    }

    for(direction.i in direction.vec) {

      # cue.stats.o
      # Get accuracy of all individual cue levels
      {

        cue.stats.o <- as.data.frame(matrix(NA, nrow = length(cue.levels),
                                            ncol = 4))

        names(cue.stats.o) <- c("cue", "class", "threshold", "direction")

        cue.stats.o$cue <- cue
        cue.stats.o$class <- class
        cue.stats.o$threshold <- cue.levels
        cue.stats.o$direction <- direction.i

        cue.stats.o[accuracy.names] <- NA

        # Get stats for each cue.level

        for(cue.level.i in cue.levels) {

          if(direction.i == "=") {pred.vec <- cue.v == cue.level.i}
          if(direction.i == "!=") {pred.vec <- cue.v != cue.level.i}

          classtable.temp <- classtable(prediction.v = pred.vec,
                                        criterion.v = criterion.v)


          cue.stats.o[cue.stats.o$threshold == cue.level.i, names(classtable.temp)] <- classtable.temp

        }

        # Rank cues
        cue.stats.o <- cue.stats.o[order(cue.stats.o[goal], decreasing = TRUE),]
        cue.stats.o$rank <- 1:nrow(cue.stats.o)

      }

      # cue.stats
      # Accuracy of threshold based criterion
      {

      cue.stats <- as.data.frame(matrix(NA, nrow = length(cue.levels),
                                        ncol = 4))

      names(cue.stats) <- c("cue", "class", "threshold", "direction")
      cue.stats$cue <- cue
      cue.stats$class <- class
      cue.stats$direction <- direction.i

      cue.stats[accuracy.names] <- NA

      for (row.index.i in 1:nrow(cue.stats.o)) {

        levels.i <- cue.stats.o$threshold[1:row.index.i]

        cue.stats$threshold[row.index.i] <- paste(levels.i, collapse = ",")

        joint.cue.vec <- cue.v %in% levels.i

        if(direction.i == "=") {pred.vec <- joint.cue.vec == TRUE}
        if(direction.i == "!=") {pred.vec <- joint.cue.vec == FALSE}


        classtable.temp <- classtable(prediction.v = pred.vec,
                                      criterion.v = criterion.v)


        cue.stats[row.index.i, accuracy.names] <- classtable.temp


      }

      if(direction.i == direction.vec[1]) {cue.stats.all <- cue.stats}
      if(direction.i != direction.vec[1]) {cue.stats.all <- rbind(cue.stats.all, cue.stats)}

      }
    }

    cue.stats <- cue.stats.all

  }

  # NUMERIC

  if(substr(class, 1, 1) %in% c("n", "i")) {

    if(is.null(cue.rules)) {

    direction.vec <- c("<", ">")

    }

    if(is.null(cue.rules) == FALSE) {

    direction.vec <- cue.rules$direction[cue.rules$cue == cue]

    }


    cue.stats <- as.data.frame(matrix(NA, nrow = cue.n, ncol = 4))

    names(cue.stats) <- c("cue",
                          "class",
                          "threshold",
                          "direction")

    cue.stats[accuracy.names] <- NA
    cue.stats$cue <- cue
    cue.stats$class <- class
    cue.stats$threshold <- cue.levels

    # Loop over all possible cue levels

    for(level.i in cue.levels) {

      accuracy.names <- names(classtable(c(1, 1, 0), c(0, 1, 0)))

      direction.accuracy.df <- as.data.frame(matrix(NA,
                                                       nrow = length(direction.vec),
                                                       ncol = length(accuracy.names)))

      names(direction.accuracy.df) <- accuracy.names

      direction.accuracy.df$direction <- direction.vec

      # Loop over 4 directions: <, <=, >, >=

      for(direction.i in direction.vec) {

        if(direction.i == "<") {pred.vec <- cue.v < level.i}
        if(direction.i == "<=") {pred.vec <- cue.v <= level.i}
        if(direction.i == ">") {pred.vec <- cue.v > level.i}
        if(direction.i == ">=") {pred.vec <- cue.v >= level.i}

        classtable.temp <- classtable(prediction.v = pred.vec,
                                      criterion.v = criterion.v)

        direction.accuracy.df[which(direction.i == direction.vec), names(classtable.temp)] <- classtable.temp

      }

      # Determine best direction for level.i

      best.acc <- max(direction.accuracy.df[goal], na.rm = TRUE)
      best.acc.index <- which(direction.accuracy.df[goal] == best.acc)

      if(length(best.acc.index) > 1) {best.acc.index <- sample(best.acc.index, size = 1)}

      best.level.stats <- direction.accuracy.df[best.acc.index,]

      cue.stats[cue.levels == level.i, names(best.level.stats)] <- best.level.stats

    }

  }

  if((substr(class, 1, 1) %in% c("f", "c", "l", "n", "i")) == FALSE) {

    direction.i <- NA
    v.i <-  NA
    hr.i <-  NA
    far.i <-  NA
    dprime.i <-  NA
    hi.i <- NA
    mi.i <- NA
    fa.i <- NA
    cr.i <- NA

  }


  # Get best threshold

  best.result.index <- which(cue.stats[goal] == max(cue.stats[goal]))

  if(length(best.result.index) > 1) {

    best.result.index <- sample(best.result.index, size = 1)

  }

  best.result <- cue.stats[best.result.index,]

  if(cue.i == 1) {cuerank.df <- best.result}
  if(cue.i > 1) {cuerank.df <- rbind(cuerank.df, best.result)}

}

rownames(cuerank.df) <- sapply(cuerank.df$cue, FUN = function(x) {which(names(data) == x)})

return(cuerank.df)

}
