# cuerank
#'   Calculate the accuracy of all cues in a dataframe
#'
#' @param cue.df A dataframe of cue values
#' @param criterion.v A logical vector of length m containing criterion values for exemplars in cue.df
#' @param tree.criterion A string indicating how to rank cues. "v" = HR - FAR, "d" = d-prime.
#' @param numthresh.method A string indicating how to calculate cue splitting thresholds. "m" = median split, "o" = split that maximizes the tree criterion.
#' @param hr.weight A number between 0 and 1 indicating how much weight to give to increasing hit rates versus avoiding false alarms. 1 means maximizing HR and ignoring FAR, while 0 does the opposite. The default of 0.5 gives equal weight to both.
#' @param rounding An integer indicating digit rounding for non-integer numeric cue thresholds. The default is NULL which means no rounding. A value of 0 rounds all possible thresholds to the nearest integer, 1 rounds to the nearest .1 (etc.).
#' @param correction If any classification cells are empty, this number is added to all cells when calculating d-prime. Default is 0.25.
#' @param verbose A logical value indicating whether or not to print ongoing diagnostics
#' @return A list with two elements. The first, "cue.stats" is a dataframe containing the classification statistics for every possible cue threshold. The second element, cue.best, contains data for the best performing cue threshold
#' @export
#'


cuerank <- function(cue.df = NULL,
                    criterion.v = NULL,
                    tree.criterion = "v",
                    numthresh.method = "o",
                    hr.weight = .5,
                    correction = .25,
                    rounding = NULL,
                    verbose = F

) {


# -----
# TESTING GROUNDS
# -----

  # cue.df <- heartdisease[,1:5]
  # criterion.v <- heartdisease$diagnosis
  # tree.criterion <- "v"
  # numthresh.method <- "o"
  # hr.weight <- .5
  # correction <- .25
  # rounding <- NULL
  #
  #
  #
  # cue.df = fds.63.cues
  # criterion.v = fds.63.crit
  # tree.criterion = "v"
  # hr.weight = .5
  # correction = 0
  #

max.numcat <- 20
n.cues <- ncol(cue.df)

for(cue.i in 1:n.cues) {

  cue.name <- names(cue.df)[cue.i]
  cue.class <- class(cue.df[,cue.i])
  cue.v <- unlist(cue.df[,cue.i])

  if(setequal(cue.class, c("ordered", "factor"))) {

    cue.class <- "integer"
    cue.v <- as.numeric(cue.v)

  }

  if("factor" %in% class(cue.v)) {cue.v <- paste(cue.v)}

  # Step 0B: Determine cue levels

  if(cue.class %in% c("numeric", "integer")) {

    if(numthresh.method == "o") {

      cue.levels <- sort(unique(unlist(cue.v)))

      if(length(cue.levels) > max.numcat) {

        cue.levels <- seq(min(cue.v), max(cue.v), length.out = max.numcat)

        }

    }

    if(numthresh.method == "m") {

      cue.levels <- median(unlist(cue.v))

    }

    if(!is.null(rounding)) {cue.levels <- round(cue.levels, digits = rounding)}

    cue.levels <- cue.levels[duplicated(cue.levels) == FALSE]

  }

  if((cue.class %in% c("numeric", "integer")) == FALSE) {

      cue.levels <- sort(unique(unlist(cue.v)))

  }


  cue.n <- length(cue.levels)
  accuracy.names <- names(classtable(1, 2, 3, 4))


  # Step 1A: Determine best sigdirection for all cue levels

  # FACTORS

  if(cue.class %in% c("factor", "character", "logical")) {

    level.sigdirection.vec <- c("=", "!=")

    for(level.sigdirection.i in level.sigdirection.vec) {

      # cue.stats.o
      # Get accuracy of all individual cue levels
      {

        cue.stats.o <- as.data.frame(matrix(NA, nrow = length(cue.levels),
                                            ncol = 4))

        names(cue.stats.o) <- c("cue.name", "cue.class", "level.threshold", "level.sigdirection")

        cue.stats.o$cue.name <- cue.name
        cue.stats.o$cue.class <- cue.class
        cue.stats.o$level.threshold <- cue.levels
        cue.stats.o$level.sigdirection <- level.sigdirection.i

        cue.stats.o[accuracy.names] <- NA

        # Get stats for each cue.level

        for(cue.level.i in cue.levels) {

          if(level.sigdirection.i == "=") {pred.vec <- cue.v == cue.level.i}
          if(level.sigdirection.i == "!=") {pred.vec <- cue.v != cue.level.i}

          classtable.temp <- classtable(prediction.v = pred.vec,
                                        criterion.v = criterion.v,
                                        correction = correction,
                                        hr.weight = hr.weight)


          cue.stats.o[cue.stats.o$level.threshold == cue.level.i, names(classtable.temp)] <- classtable.temp

        }


        # Rank cues
        cue.stats.o <- cue.stats.o[order(cue.stats.o[tree.criterion], decreasing = T),]
        cue.stats.o$rank <- 1:nrow(cue.stats.o)

      }

      # cue.stats
      # Accuracy of threshold based criterion
      {

      cue.stats <- as.data.frame(matrix(NA, nrow = length(cue.levels),
                                        ncol = 4))

      names(cue.stats) <- c("cue.name", "cue.class", "level.threshold", "level.sigdirection")
      cue.stats$cue.name <- cue.name
      cue.stats$cue.class <- cue.class
      cue.stats$level.sigdirection <- level.sigdirection.i

      cue.stats[accuracy.names] <- NA

      for (row.index.i in 1:nrow(cue.stats.o)) {

        levels.i <- cue.stats.o$level.threshold[1:row.index.i]

        cue.stats$level.threshold[row.index.i] <- paste(levels.i, collapse = ",")

        joint.cue.vec <- cue.v %in% levels.i

        if(level.sigdirection.i == "=") {pred.vec <- joint.cue.vec == T}
        if(level.sigdirection.i == "!=") {pred.vec <- joint.cue.vec == F}


        classtable.temp <- classtable(prediction.v = pred.vec,
                                      criterion.v = criterion.v,
                                      correction = correction,
                                      hr.weight = hr.weight)


        cue.stats[row.index.i, accuracy.names] <- classtable.temp


      }

      if(level.sigdirection.i == level.sigdirection.vec[1]) {cue.stats.all <- cue.stats}
      if(level.sigdirection.i != level.sigdirection.vec[1]) {cue.stats.all <- rbind(cue.stats.all, cue.stats)}

      }
    }

    cue.stats <- cue.stats.all


  }

  # NUMERIC

  if(cue.class %in% c("numeric", "integer")) {

    sigdirection.vec <- c("<", "<=", ">", ">=")

    cue.stats <- as.data.frame(matrix(NA, nrow = cue.n, ncol = 4))

    names(cue.stats) <- c("cue.name",
                          "cue.class",
                          "level.threshold",
                          "level.sigdirection")

    cue.stats[accuracy.names] <- NA
    cue.stats$cue.name <- cue.name
    cue.stats$cue.class <- cue.class
    cue.stats$level.threshold <- cue.levels

    # Loop over all possible cue levels

    for(level.i in cue.levels) {

      accuracy.names <- names(classtable(1, 2, 3, 4))

      sigdirection.accuracy.df <- as.data.frame(matrix(NA,
                                                       nrow = length(sigdirection.vec),
                                                       ncol = length(accuracy.names)))

      names(sigdirection.accuracy.df) <- accuracy.names

      sigdirection.accuracy.df$level.sigdirection <- sigdirection.vec

      # Loop over 4 directions: <, <=, >, >=

      for(sigdirection.i in sigdirection.vec) {

        if(sigdirection.i == "<") {pred.vec <- cue.v < level.i}
        if(sigdirection.i == "<=") {pred.vec <- cue.v <= level.i}
        if(sigdirection.i == ">") {pred.vec <- cue.v > level.i}
        if(sigdirection.i == ">=") {pred.vec <- cue.v >= level.i}

        classtable.temp <- classtable(prediction.v = pred.vec,
                                      criterion.v = criterion.v,
                                      correction = correction,
                                      hr.weight = hr.weight)

        sigdirection.accuracy.df[which(sigdirection.i == sigdirection.vec), names(classtable.temp)] <- classtable.temp

      }

      # Determine best direction for level.i

      best.acc <- max(sigdirection.accuracy.df[tree.criterion], na.rm = T)
      best.acc.index <- which(sigdirection.accuracy.df[tree.criterion] == best.acc)

      if(length(best.acc.index) > 1) {best.acc.index <- sample(best.acc.index, size = 1)}

      best.level.stats <- sigdirection.accuracy.df[best.acc.index,]


      cue.stats[cue.levels == level.i, names(best.level.stats)] <- best.level.stats

    }

  }

  if((cue.class %in% c("factor", "character", "logical", "numeric", "integer")) == F) {

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

  best.result.index <- which(cue.stats[tree.criterion] == max(cue.stats[tree.criterion]))

  if(length(best.result.index) > 1) {

    best.result.index <- sample(best.result.index, size = 1)

  }



  best.result <- cue.stats[best.result.index,]

  if(cue.i == 1) {cuerank.df <- best.result}
  if(cue.i > 1) {cuerank.df <- rbind(cuerank.df, best.result)}
}

return(cuerank.df)

}
