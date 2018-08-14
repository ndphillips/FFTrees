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

  # formula = diagnosis ~.
  # data = heartdisease
  # goal = "bacc"
  # sens.w = .5
  # cost.outcomes = c(0, 1, 1, 0)
  # cost.cues = NULL
  # numthresh.method = "o"
  # rounding = NULL
  # factor.directions = c("=", "!=")
  # numeric.directions = c(">", "<")
  # considerFALSE = TRUE
  # progress = FALSE
  # cue.rules = NULL


  # GLOBAL VARIABLES (could be updated later)
  max.numcat <- 20        # Maximum number of numeric thresholds to consider

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

  if(progress) {pb <- progress::progress_bar$new(total = n.cues, clear = FALSE, show_after = .5)}

    cuesToLoop <- n.cues

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

    cue.name <- names(cue.df)[cue.i]
    cue.class <- class(cue.df[,cue.i])
    cue.v <- unlist(cue.df[,cue.i])
    cue.cost.i <- cost.cues$cost[cost.cues$cue == cue]


    if(all(is.na(cue.v)) == FALSE) {

      # Step 0: Determine possible cue levels [cue.levels]
      {

          # Numeric and integer cues
          if(substr(cue.class, 1, 1) %in% c("n", "i")) {

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
          if((substr(cue.class, 1, 1) %in% c("n", "i")) == FALSE) {


            # Use all unique cue values
            if(is.null(cue.rules)) {

              if(is.logical(unique(unlist(cue.v))) & considerFALSE == FALSE) {

                cue.levels <- TRUE

              } else {

                cue.levels <- sort(unique(unlist(cue.v)))

              }

            }


        # If there are > 50% unique cue.levels, send a warning

            if(length(cue.levels) > .5 * nrow(data)) {


              warning(paste0("The cue ", cue.names[cue.i], " is nominal and contains mostly unique values. This could lead to dramatic overfitting. You should probably exclude this cue or reduce the number of unique values."))}


          }






        # Check for clue levels containing protected characters (;)

        if(any(sapply(cue.levels, FUN = function(x) {grepl(";", x = x)}))) {

          stop(paste0("The cue ", cue.names[cue.i], " contains the character ; which is not allowed. Please replace this value in the data and try again."))

          }



      }

      # Step 1A: Determine best direction and threshold for cue

      # factor, character, and logical
      if(substr(cue.class, 1, 1) %in% c("f", "c", "l")) {

        cue.stats <- threshold_factor_grid(thresholds = cue.levels,
                                               cue.v = cue.v,
                                               criterion.v = criterion.v,
                                               sens.w = sens.w,
                                               cost.outcomes = cost.outcomes,
                                               goal.chase = goal.chase)

      }

      # Numeric, integer
      if(substr(cue.class, 1, 1) %in% c("n", "i")) {


        cue.stats <- threshold_numeric_grid(thresholds = cue.levels,
                                               cue.v = cue.v,
                                               criterion.v = criterion.v,
                                               sens.w = sens.w,
                                               cost.outcomes = cost.outcomes,
                                               goal.chase = goal.chase)

      }

      cue.stats$cue <- cue.name

      best.result.index <- which(cue.stats[goal] == max(cue.stats[goal], na.rm = TRUE))[1]

      if(is.na(best.result.index)) {best.result.index <- 1}

      best.result <- cue.stats[best.result.index,]

    } else {

      # If all cue values are NA, then return empty results

      best.result <- data.frame("cue" = cue, class = cue.class, threshold = NA, direction = NA)
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
