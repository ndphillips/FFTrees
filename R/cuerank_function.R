#' Calculates thresholds that maximize a statistic (goal) for cues.
#'
#' @param formula formula. A formula specifying a binary criterion as a function of multiple variables
#' @param data dataframe. A dataframe containing variables in formula
#' @param goal character. A string indicating the statistic to maximize: "acc" = overall accuracy, "bacc" = balanced accuracy, "wacc" = weighted accuracy, "d" = dprime
#' @param sens.w numeric. A number from 0 to 1 indicating how to weight sensitivity relative to specificity.
#' @param cost.outcomes numeric. A vector of length 4 specifying the costs of a hit, false alarm, miss, and correct rejection rspectively. E.g.; \code{cost.outcomes = c(0, 10, 20, 0)} means that a false alarm and miss cost 10 and 20 respectively while correct decisions have no cost.
#' @param cost.cues dataframe. A dataframe with two columns specifying the cost of each cue. The first column should be a vector of cue names, and the second column should be a numeric vector of costs. Cues in the dataset not present in \code{cost.cues} are assume to have 0 cost.
#' @param numthresh.method character. A string indicating how to calculate cue splitting thresholds. "m" = median split, "o" = split that maximizes the goal,
#' @param rounding integer. An integer indicating digit rounding for non-integer numeric cue thresholds. The default is NULL which means no rounding. A value of 0 rounds all possible thresholds to the nearest integer, 1 rounds to the nearest .1 (etc.).
#' @param factor.directions character. A vector of possible directions for factor values. \code{c("=", "!=")} allows both equality and inequality, while \code{"="} only allows for equality.
#' @param numeric.directions character. A vector of possible directions for numeric values. \code{c(">", "<")} allows only strict inequalities while \code{c("<=", "<", ">=", ">")} is more flexible.
#' @param considerFALSE logical. Should FALSE logical values be considered as potential thresholds? This is only relevant for very special algorithms.
#' @param progress logical. Should ongoing diagnostics be printed?
#' @param cue.rules dataframe. An optional df specifying existing cue thresholds, directions, names, and classes
#' @importFrom stats median var
#' @importFrom progress progress_bar
#' @return A dataframe containing thresholds and marginal classification statistics for each cue
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
                    cost.outcomes = c(0, 1, 1, 0),
                    cost.cues = NULL,
                    numthresh.method = "o",
                    rounding = NULL,
                    factor.directions = c("=", "!="),
                    numeric.directions = c(">", "<"),
                    considerFALSE = TRUE,
                    progress = FALSE,
                    cue.rules = NULL

) {

  
  # formula = formula
  # data = data.mf.r
  # goal = goal.chase
  # numthresh.method = numthresh.method
  # rounding = rounding
  # sens.w = sens.w
  # cost.outcomes = cost.outcomes
  # cost.cues = cost.cues


  # GLOBAL VARIABLES (could be updated later)
  max.numcat <- 20        # Maximum number of numeric thresholds to consider

  # Adjust inputs
  if(substr(goal, 1, 1) == "a") {goal <- "acc"}
  if(substr(goal, 1, 1) == "b") {goal <- "bacc"}
  if(substr(goal, 1, 1) == "d") {goal <- "dprime"}
  if(substr(goal, 1, 1) == "w") {goal <- "wacc"}

  # Extract variables in formula
  data.mf <- model.frame(formula = formula,
                         data = data, na.action = NULL)

  # Define criterion (vector) and cues (dataframe)
  criterion.v <- data.mf[,1]
  cases.n <- length(criterion.v)
  cue.df <- data.mf[,2:ncol(data.mf), drop = FALSE]
  n.cues <- ncol(cue.df)
  cue.names <- names(cue.df)
  N <- length(criterion.v)

  # Names of accuracy measures
  accuracy.names <- names(classtable(c(1, 0, 1), c(1, 1, 0)))

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

  if(progress) {pb <- progress::progress_bar$new(total = n.cues, clear = FALSE, show_after = .5)}

  if(is.null(cue.rules) == FALSE) {cuesToLoop <- nrow(cue.rules)} else {cuesToLoop <- n.cues}

  # Make sure cost.cues is full
  {
    if(is.null(cost.cues)) {

      cost.cues <- data.frame("cue" = cue.names,
                              "cost" = rep(0, n.cues),
                              stringsAsFactors = FALSE)

    } else {

      names(cost.cues) <- c("cue", "cost")

      # Which cues are missing in cost.cues?

      missing.cues <- setdiff(cue.names, cost.cues[,1])

      if(length(missing.cues) > 0) {

        cost.cues.missing <- data.frame("cue" = missing.cues,
                                        "cost" = rep(0, length(missing.cues)))

        cost.cues <- rbind(cost.cues, cost.cues.missing)


      }

    }
  }

  # Loop over cues
  for(cue.i in 1:cuesToLoop) {

    # Progress update
    if(progress) {

      pb$tick()
      Sys.sleep(1 / n.cues)

    }


    if(is.null(cue.rules)) {

      # Get main information about current cue
      cue <- names(cue.df)[cue.i]
      class <- class(cue.df[,cue.i])
      cue.v <- unlist(cue.df[,cue.i])

    } else {

      cue <- cue.rules$cue[cue.i]
      class <- cue.rules$class[cue.i]
      cue.v <- unlist(cue.df[cue])

    }

    cue.cost.i <- cost.cues$cost[cost.cues$cue == cue]

    if(all(is.na(cue.v)) == FALSE) {

      # Step 0: Determine possible cue levels [cue.levels]
      {

        if(is.null(cue.rules)) {

          # Numeric and integer cues
          if(substr(class, 1, 1) %in% c("n", "i")) {

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

              if(length(unique(unlist(cue.v))) == 2) {cue.levels <- unique(unlist(cue.v))} else {

                cue.levels <- median(unlist(cue.v))

              }

            }

            # Round cue levels
            if(!is.null(rounding)) {cue.levels <- round(cue.levels,
                                                        digits = rounding)}

            # Remove potential duplicates
            cue.levels <- cue.levels[duplicated(cue.levels) == FALSE]

          }

          # Non-numeric and integer cues
          if((substr(class, 1, 1) %in% c("n", "i")) == FALSE) {


            # Use all unique cue values
            if(is.null(cue.rules)) {

              if(is.logical(unique(unlist(cue.v))) & considerFALSE == FALSE) {

                cue.levels <- TRUE

              } else {

                cue.levels <- sort(unique(unlist(cue.v)))

              }

            }

          }

        }
        if(is.null(cue.rules) == FALSE) {

          cue.levels <- cue.rules$threshold[cue.i]
          if(grepl(",", cue.levels)) {cue.levels <- unlist(strsplit(cue.levels, ","))}

        }

      }

      # Step 1A: Determine possible directions [direction.vec]
      {
        if(is.null(cue.rules)) {

          if(substr(class, 1, 1) %in% c("n", "i")) {

            direction.vec <- numeric.directions

          }

          if(substr(class, 1, 1) %in% c("f", "c", "l")) {

            direction.vec <- factor.directions

          }

        }

        if(is.null(cue.rules) == FALSE) {

          direction.vec <- cue.rules$direction[cue.i]

        }
      }

      # Step 1A: Determine best direction and threshold for cue

      # factor, character, and logical
      if(substr(class, 1, 1) %in% c("f", "c", "l")) {

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
                                            sens.w = sens.w,
                                            cost.v = rep(cue.cost.i, N),
                                            cost.outcomes = cost.outcomes)


              cue.stats.o[cue.stats.o$threshold == cue.level.i, names(classtable.temp)] <- classtable.temp

            }

            # Rank cues
            if(goal != "cost") {
              cue.stats.o <- cue.stats.o[order(cue.stats.o[goal], decreasing = TRUE),]

            }

            if(goal == "cost") {
              cue.stats.o <- cue.stats.o[order(cue.stats.o["cost"], decreasing = FALSE),]

            }
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
                                            sens.w = sens.w,
                                            cost.v = rep(cue.cost.i, N),
                                            cost.outcomes = cost.outcomes)


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

        cue.stats <- as.data.frame(matrix(NA, nrow = length(cue.levels), ncol = 4))

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
                                          cost.v = rep(cue.cost.i, cases.n),
                                          sens.w = sens.w,
                                          cost.outcomes = cost.outcomes)

            direction.accuracy.df[which(direction.i == direction.vec), names(classtable.temp)] <- classtable.temp

          }

          # Determine best direction for level.i

          if(goal == "cost") {

            best.acc <- min(direction.accuracy.df["cost"], na.rm = TRUE)
            best.acc.index <- which(direction.accuracy.df["cost"] == best.acc)


          } else {

            if(any(is.finite(unlist(direction.accuracy.df[goal])))) {
            
            best.acc <- max(direction.accuracy.df[goal], na.rm = TRUE)
            best.acc.index <- which(direction.accuracy.df[goal] == best.acc)
            
            
            } else {
              
              best.acc <- 0
              best.acc.index <- 1
              }

          }


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
      if(goal != "cost") {

        best.result.index <- which(cue.stats[goal] == max(cue.stats[goal]))[1]

      }

      if(goal == "cost") {

        best.result.index <- which(cue.stats["cost"] == min(cue.stats["cost"]))[1]

      }
      
      if(is.na(best.result.index)) {best.result.index <- 1}

      best.result <- cue.stats[best.result.index,]

    } else {

      # If all cue values are NA, then return empy results

      best.result <- data.frame("cue" = cue, class = class, threshold = NA, direction = NA)
      best.result <- cbind(best.result, classtable(c(0, 0, 0), c(1, 1, 1)))
      best.result[names(classtable(c(0, 1, 0), c(1, 0, 0)))] <-0

    }



    if(cue.i == 1) {cuerank.df <- best.result}
    if(cue.i > 1) {cuerank.df <- rbind(cuerank.df, best.result)}

  }

  rownames(cuerank.df) <- 1:nrow(cuerank.df)

  # Add cue costs
  cuerank.df$cost.cue <- cost.cues$cost[match(cuerank.df$cue, cost.cues$cue)]

  return(cuerank.df)

}
