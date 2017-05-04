#' Calculate the marginal accuracy of all cues in a dataframe. For each cue, the threshold that maximizes the criterion is selected.
#'
#' @param formula formula. A formula specifying a binary criterion as a function of multiple variables
#' @param data dataframe. A dataframe containing variables in formula
#' @param goal character. A string indicating the statistic to maximize: "acc" = overall accuracy, "bacc" = balanced accuracy, "wacc" = weighted accuracy, "d" = dprime
#' @param sens.w numeric. A number from 0 to 1 indicating how to weight sensitivity relative to specificity.
#' @param numthresh.method character. A string indicating how to calculate cue splitting thresholds. "m" = median split, "o" = split that maximizes the goal,
#' @param rounding integer. An integer indicating digit rounding for non-integer numeric cue thresholds. The default is NULL which means no rounding. A value of 0 rounds all possible thresholds to the nearest integer, 1 rounds to the nearest .1 (etc.).
#' @param verbose logical. A logical value indicating whether or not to print ongoing diagnostics
#' @param cue.rules dataframe. An optional df specifying how to make decisions for each cue. Must contain columns "cue", "class", "threshold" and "direction"
#' @importFrom stats median var
#' @return A dataframe containing best thresholds and marginal classification statistics for each cue
#' @export
#' @examples
#'
#'\dontrun{
#'  # What are the best thresholds for each cue in the mushrooms dataset?
#'  mushrooms.cues <- cuerank(formula = poisonous ~.,
#'                            data = mushrooms)
#'}
#'
#'

cuerank <- function(formula = NULL,
                    data = NULL,
                    goal = "bacc",
                    sens.w = .5,
                    numthresh.method = "o",
                    rounding = NULL,
                    verbose = FALSE,
                    cue.rules = NULL

) {



# GLOBAL VARIABLES (could be updated later)
max.numcat <- 20        # Maximum number of numeric thresholds to consider

# Adjust inputs
if(substr(goal, 1, 1) == "a") {goal <- "acc"}
if(substr(goal, 1, 1) == "b") {goal <- "bacc"}
if(substr(goal, 1, 1) == "d") {goal <- "dprime"}
if(substr(goal, 1, 1) == "w") {goal <- "wacc"}

# Extract variables in formula
data.mf <- model.frame(formula = formula,
                       data = data)

# Define criterion (vector) and cues (dataframe)
criterion.v <- data.mf[,1]
cue.df <- data.mf[,2:ncol(data.mf), drop = FALSE]
n.cues <- ncol(cue.df)

# Convert unordered factors to character, and ordered factors to integer
for(i in 1:n.cues) {

  if(setequal(class(cue.df[,i]),c("ordered", "factor"))) {

    cue.df[,i] <- as.numeric(cue.df[,i])

  }

  if(class(cue.df[,i]) == "factor") {

    cue.df[,i] <- paste(cue.df[,i])

  }
}

# Validation: Make sure there is variance in the criterion!
if(var(criterion.v) == 0) {

  stop("There is no variance in the criterion!")

}

# Validation: If cue.rules are specified, make sure they are valid
if(is.null(cue.rules) == FALSE) {

if(all(c("cue", "class", "threshold", "direction") %in% names(cue.rules)) == FALSE) {

  stop("You specified a cue.rules object but the object does not contain critical columns. Look at the descriptions for cue.rules and try again")

}
}

# Loop over cues
for(cue.i in 1:n.cues) {

  # Get main information about current cue
  cue <- names(cue.df)[cue.i]
  class <- class(cue.df[,cue.i])
  cue.v <- unlist(cue.df[,cue.i])

  # Step 0: Determine possible cue levels

  # Numeric and integer cues
  if(substr(class, 1, 1) %in% c("n", "i")) {

    # If cue.cules exists, then get the cue.levels from the dataframe
    if(is.null(cue.rules) == FALSE) {

      cue.levels <- as.numeric(cue.rules$threshold[cue.rules$cue == cue])

    }

    # "optimize" method
    if(numthresh.method == "o" & is.null(cue.rules)) {

      # Get all possible (sorted) cue values
      cue.levels <- sort(unique(unlist(cue.v)))

      # If too long, reduce to max.numcat
      if(length(cue.levels) > max.numcat) {

        indicies <- round(seq(1, length(cue.levels), length.out = max.numcat), 0)
        cue.levels <- cue.levels[indicies]

      }

    }

    # "median" method
    if(numthresh.method == "m" & is.null(cue.rules)) {

      cue.levels <- median(unlist(cue.v))

    }

    # Round cue levels
    if(!is.null(rounding)) {cue.levels <- round(cue.levels, digits = rounding)}

    # Remove potential duplicates
    cue.levels <- cue.levels[duplicated(cue.levels) == FALSE]

  }

  # Non-numeric and integer cues
  if((substr(class, 1, 1) %in% c("n", "i")) == FALSE) {

    if(is.null(cue.rules) == FALSE) {

      cue.levels <- cue.rules$threshold[cue.rules$cue == cue]

    }

    # Use all unique cue values
    if(is.null(cue.rules)) {

      cue.levels <- sort(unique(unlist(cue.v)))

    }

  }

  cue.n <- length(cue.levels)

  # Names of accuracy measures
  accuracy.names <- names(classtable(c(1, 0, 1), c(1, 1, 0)))

  # Step 1A: Determine best direction and threshold for all cues

  # factor, character, and logical
  if(substr(class, 1, 1) %in% c("f", "c", "l")) {

    if(is.null(cue.rules)) {

    direction.vec <- c("=", "!=")

    }

    if(is.null(cue.rules) == FALSE) {

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
                                        criterion.v = criterion.v,
                                        sens.w = sens.w)


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
                                      criterion.v = criterion.v,
                                      sens.w = sens.w)


        cue.stats[row.index.i, accuracy.names] <- classtable.temp


      }

      if(direction.i == direction.vec[1]) {cue.stats.all <- cue.stats}
      if(direction.i != direction.vec[1]) {cue.stats.all <- rbind(cue.stats.all, cue.stats)}

      }
    }

    cue.stats <- cue.stats.all

  }

  # Numeric, integer
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

      direction.accuracy.df <- as.data.frame(matrix(NA,
                                                       nrow = length(direction.vec),
                                                       ncol = length(accuracy.names)))

      names(direction.accuracy.df) <- accuracy.names

      direction.accuracy.df$direction <- direction.vec

      # Loop over directions: <, <=, >, >=
      for(direction.i in direction.vec) {

        if(direction.i == "<") {pred.vec <- cue.v < level.i}
        if(direction.i == "<=") {pred.vec <- cue.v <= level.i}
        if(direction.i == ">") {pred.vec <- cue.v > level.i}
        if(direction.i == ">=") {pred.vec <- cue.v >= level.i}

        classtable.temp <- classtable(prediction.v = pred.vec,
                                      criterion.v = criterion.v,
                                      sens.w = sens.w)

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

  # Other classes
  if((substr(class, 1, 1) %in% c("f", "c", "l", "n", "i")) == FALSE) {

    direction.i <- NA
    v.i <-  NA
    spec.i <-  NA
    sens.i <- NA
    far.i <-  NA
    dprime.i <-  NA
    hi.i <- NA
    mi.i <- NA
    fa.i <- NA
    cr.i <- NA

  }

  # Get thresholds that maximizes goal
  best.result.index <- which(cue.stats[goal] == max(cue.stats[goal]))[1]

  best.result <- cue.stats[best.result.index,]
  if(cue.i == 1) {cuerank.df <- best.result}
  if(cue.i > 1) {cuerank.df <- rbind(cuerank.df, best.result)}

}

rownames(cuerank.df) <- 1:nrow(cuerank.df)

return(cuerank.df)

}
