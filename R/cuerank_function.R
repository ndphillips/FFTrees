#' Calculates thresholds that maximize a statistic (goal) for cues.
#'
#' @param formula formula. A formula specifying a binary criterion as a function of multiple variables
#' @param data dataframe. A dataframe containing variables in formula
#' @param goal.threshold character. A string indicating the statistic to maximize when calculting cue thresholds: "acc" = overall accuracy, "wacc" = weighted accuracy, "bacc" = balanced accuracy
#' @param sens.w numeric. A number from 0 to 1 indicating how to weight sensitivity relative to specificity.
#' @param cost.outcomes list. A list of length 4 with names 'hi', 'fa', 'mi', and 'cr' specifying the costs of a hit, false alarm, miss, and correct rejection rspectively. E.g.; \code{cost.outcomes = listc("hi" = 0, "fa" = 10, "mi" = 20, "cr" = 0)} means that a false alarm and miss cost 10 and 20 respectively while correct decisions have no cost.
#' @param cost.cues dataframe. A dataframe with two columns specifying the cost of each cue. The first column should be a vector of cue names, and the second column should be a numeric vector of costs. Cues in the dataset not present in \code{cost.cues} are assume to have 0 cost.
#' @param numthresh.method character. A string indicating how to calculate cue splitting thresholds. "m" = median split, "o" = split that maximizes the goal,
#' @param numthresh.n integer. The maximum number of numeric thresholds to be considered.
#' @param rounding integer. An integer indicating digit rounding for non-integer numeric cue thresholds. The default is NULL which means no rounding. A value of 0 rounds all possible thresholds to the nearest integer, 1 rounds to the nearest .1 (etc.).
#' @param cue.rules dataframe. Optional existing dataframe of previously defined cue rules.
#' @param quiet logical. Should ongoing diagnostics be printed?
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
                    goal.threshold = "bacc",
                    sens.w = .5,
                    cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0),
                    cost.cues = NULL,
                    numthresh.method = "o",
                    numthresh.n = 20,
                    rounding = NULL,
                    cue.rules = NULL,
                    quiet = FALSE

) {

  #
  #
  # formula = diagnosis ~.
  # data = heartdisease
  # goal = "bacc"
  # sens.w = .5
  # cost.outcomes = list(hi = 0, fa = 1, mi = 1, cr = 0)
  # cost.cues = NULL
  # numthresh.method = "o"
  # numthresh.n = 20
  # rounding = NULL
  # cue.rules = NULL
  # progress = FALSE

  #
  # Extract variables in formula
  data.mf <- model.frame(formula = formula,
                         data = data,
                         na.action = NULL)

  # Define criterion (vector) and cues (dataframe)
  cue.df <- data.mf[,2:ncol(data.mf), drop = FALSE]
  criterion_v <- data.mf[,1]

  if(is.null(cost.cues)) {cost.cues <- cost.cues.append(formula, data)}

  # Get data information
  cases_n <- length(criterion_v)
  cue_n <- ncol(cue.df)
  cue_names <- names(cue.df)

  # Clean up and validation
  {
  # Convert unordered factors to character, and ordered factors to integer
  for(i in 1:cue_n) {


    if(setequal(class(cue.df[,i]),c("ordered", "factor"))) {

      cue.df[,i] <- as.numeric(cue.df[,i])

    }

    if(class(cue.df[,i]) == "factor") {

      cue.df[,i] <- paste(cue.df[,i])

    }
  }

  # Validation: Make sure there is variance in the criterion!
  if(var(criterion_v) == 0) {

    stop("There is no variance in the criterion!")

  }


}


  if(!quiet) {pb <- progress::progress_bar$new(total = cue_n, clear = FALSE, show_after = .5)}

  # Loop over cues
  for(cue_i in 1:cue_n) {

    # Progress update
    if(!quiet) {

      pb$tick()
      Sys.sleep(1 / cue_n)

    }

    if(is.null(cue.rules)) {

      # Get main information about current cue
      cue_i_name <- names(cue.df)[cue_i]
      cue_i_class <- class(cue.df[,cue_i])
      cue_i_v <- unlist(cue.df[,cue_i])
      cue_i_cost <- cost.cues[[cue_i_name]]

    } else {

      # Get main information about current cue
      cue_i_name <- cue.rules$cue[cue_i]
      cue_i_class <- cue.rules$class[cue_i]
      cue_i_v <- unlist(cue.df[cue_i_name])
      cue_i_cost <- cost.cues[[cue_i_name]]

    }

    if(all(is.na(cue_i_v)) == FALSE) {

      # Step 0: Determine possible cue levels [cue_i_levels]
      {

        if(is.null(cue.rules)) {

          # Numeric and integer cues
          if(substr(cue_i_class, 1, 1) %in% c("n", "i")) {

            # "optimize" method
            if(numthresh.method == "o") {

              # Get all possible (sorted) cue values
              cue_i_levels <- sort(unique(unlist(cue_i_v)))

              # If too long, reduce to numthresh.n
              if(length(cue_i_levels) > numthresh.n) {

                indicies <- round(seq(1, length(cue_i_levels), length.out = numthresh.n), 0)
                cue_i_levels <- cue_i_levels[indicies]

              }

            }

            # "median" method
            if(numthresh.method == "m") {

              if(length(unique(unlist(cue_i_v))) == 2) {cue_i_levels <- unique(unlist(cue_i_v))} else {

                cue_i_levels <- median(unlist(cue_i_v))

              }

            }

            # Round cue levels
            if(!is.null(rounding)) {cue_i_levels <- round(cue_i_levels,
                                                          digits = rounding)}

            # Remove potential duplicates
            cue_i_levels <- cue_i_levels[duplicated(cue_i_levels) == FALSE]

          }

          # Non-numeric and integer cues
          if(substr(cue_i_class, 1, 1) %in% c("f", "c", "l")) {

       # Use all unique cue values

            cue_i_levels <- sort(unique(unlist(cue_i_v)))

        # If there are > 50% unique cue.levels, send a warning

            if(length(cue_i_levels) > .5 * nrow(data)) {

              warning(paste0("The cue ", cue_names[cue_i], " is nominal and contains mostly unique values. This could lead to dramatic overfitting. You should probably exclude this cue or reduce the number of unique values."))}
          }

        } else {

          cue_i_levels <- cue.rules$threshold[cue_i]
          if(grepl(",", cue_i_levels)) {cue_i_levels <- unlist(strsplit(cue_i_levels, ","))}

        }


        # Check for cue levels containing protected characters (;)

        if(any(sapply(cue_i_levels, FUN = function(x) {grepl(";", x = x)}))) {

          stop(paste0("The cue ", cue_names[cue_i], " contains the character ';' which is not allowed. Please replace this value in the data and try again."))

          }

      }

      if(is.null(cue.rules) == FALSE) {

        directions <- cue.rules$direction[cue_i]

      } else {

        if(substr(cue_i_class, 1, 1) %in% c("n", "i")) {

          directions <- c(">", "<=")
        }

        if(substr(cue_i_class, 1, 1) %in% c("c", "f", "l")) {

          directions <- c("=", "!=")
        }

      }


      # Step 1: Determine best direction and threshold for cue [cue_i_best]
      {
      # Numeric, integer
      if(substr(cue_i_class, 1, 1) %in% c("n", "i")) {

        cue_i_stats <- threshold_numeric_grid(thresholds = cue_i_levels,
                                              cue.v = cue_i_v,
                                              criterion.v = criterion_v,
                                              sens.w = sens.w,
                                              directions = directions,
                                              cost.each = cue_i_cost,
                                              cost.outcomes = cost.outcomes,
                                              goal.threshold = goal.threshold)
      }

      # factor, character, and logical
      if(substr(cue_i_class, 1, 1) %in% c("f", "c", "l")) {

        cue_i_stats <- threshold_factor_grid(thresholds = cue_i_levels,
                                             cue.v = cue_i_v,
                                             criterion.v = criterion_v,
                                             directions = directions,
                                             sens.w = sens.w,
                                             cost.each = cue_i_cost,
                                             cost.outcomes = cost.outcomes,
                                             goal.threshold = goal.threshold)
      }

      # Get thresholds that maximizes goal.threshold

      best.result.index <- which(cue_i_stats[goal.threshold] == max(cue_i_stats[goal.threshold], na.rm = TRUE))

      # If there are two best indices, take the first
      #   Not sure if this is the best way to do it...

      if(length(best.result.index) > 1) {

        best.result.index <- best.result.index[1]

      }

      if(is.na(best.result.index)) {best.result.index <- 1}

      cue_i_best <- cue_i_stats[best.result.index,]
}

    } else {

      # If all cue values are NA, then return empty results

      cue_i_best <- threshold_factor_grid(thresholds = NULL,
                                           cue.v = NULL,
                                           criterion.v = NULL,
                                           sens.w = sens.w,
                                           cost.outcomes = cost.outcomes,
                                          goal.threshold = goal.threshold)

    }

      cue_i_best$cue <- cue_i_name
      cue_i_best$class <- cue_i_class

    if(cue_i == 1) {cuerank_df <- cue_i_best}
    if(cue_i > 1) {cuerank_df <- rbind(cuerank_df, cue_i_best)}

  }

  rownames(cuerank_df) <- 1:nrow(cuerank_df)


    # Add cue costs
  cuerank_df$costcue <- unlist(cost.cues[match(cuerank_df$cue, names(cost.cues))])

  # Re-order
  cuerank_df <- cuerank_df[,c("cue", "class", setdiff(names(cuerank_df), c("cue", "class")))]

  return(cuerank_df)

}
