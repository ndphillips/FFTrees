
# Apply break function
#   Takes a direction, threshold value, and cue vector, and returns a vector of decisions
apply.break <- function(direction,
                        threshold.val,
                        cue.v,
                        cue.class
) {


  if(is.character(threshold.val)) {threshold.val <- unlist(strsplit(threshold.val, ","))}

  if(cue.class %in% c("numeric", "integer")) {threshold.val <- as.numeric(threshold.val)}


  if(direction == "!=") {output <- (cue.v %in% threshold.val) == F}
  if(direction == "=") {output <- cue.v %in% threshold.val}
  if(direction == "<") {output <- cue.v < threshold.val}
  if(direction == "<=") {output <- cue.v <= threshold.val}
  if(direction == ">") {output <- cue.v > threshold.val}
  if(direction == ">=") {output <- cue.v >= threshold.val}


  return(output)

}


# apply.tree takes a dataset, a formula, and tree definitions, and returns a vector of decisions
apply.tree <- function(data,
                       formula,
                       tree.definitions
) {

  criterion.v <- model.frame(formula = formula, data = data, na.action = NULL)[,1]

  n.exemplars <- nrow(data)
  n.trees <- nrow(tree.definitions)

  levelout <- matrix(NA, nrow = n.exemplars, ncol = n.trees)
  decision <- matrix(NA, nrow = n.exemplars, ncol = n.trees)

  level.stats.ls <- vector("list", length = n.trees)

  for(tree.i in 1:n.trees) {

    cue.v <- unlist(strsplit(tree.definitions$cue[tree.i], ";"))
    class.v <- unlist(strsplit(tree.definitions$classes[tree.i], ";"))
    exit.v <- unlist(strsplit(tree.definitions$exits[tree.i], ";"))
    threshold.v <- unlist(strsplit(tree.definitions$thresholds[tree.i], ";"))
    direction.v <-  unlist(strsplit(tree.definitions$directions[tree.i], ";"))

    n.levels <- length(cue.v)

    level.stats.df.i <- data.frame(tree = tree.i,
                                   level = 1:n.levels,
                                   cue = cue.v,
                                   class = class.v,
                                   threshold = threshold.v,
                                   direction = direction.v,
                                   exit = exit.v)

    level.stats.df.i[names(classtable(1, 1))] <- NA

    for(level.i in 1:n.levels) {

      cue.i <- cue.v[level.i]
      class.i <- class.v[level.i]
      direction.i <- direction.v[level.i]
      exit.i <- exit.v[level.i]
      threshold.i <- threshold.v[level.i]

      cue.values <- data[[cue.i]]

      unclassified.cases <- which(is.na(decision[,tree.i]))
      classified.cases <- which(is.na(decision[,tree.i]) == F)


      if(is.character(threshold.i)) {threshold.i <- unlist(strsplit(threshold.i, ","))}

      if(class.i %in% c("numeric", "integer")) {threshold.i <- as.numeric(threshold.i)}

      if(direction.i == "!=") {current.decisions <- (cue.values %in% threshold.i) == F}
      if(direction.i == "=") {current.decisions <- cue.values %in% threshold.i}
      if(direction.i == "<") {current.decisions <- cue.values < threshold.i}
      if(direction.i == "<=") {current.decisions <- cue.values <= threshold.i}
      if(direction.i == ">") {current.decisions <- cue.values > threshold.i}
      if(direction.i == ">=") {current.decisions <- cue.values >= threshold.i}


      if(exit.i == 0) {classify.now <- current.decisions == F & is.na(decision[,tree.i])}
      if(exit.i == 1) {classify.now <- current.decisions == T & is.na(decision[,tree.i])}
      if(exit.i == .5) {classify.now <- is.na(decision[,tree.i])}


      decision[classify.now, tree.i] <- current.decisions[classify.now]
      levelout[classify.now, tree.i] <- level.i

      # Get level stats

      level.i.stats <- classtable(prediction.v = decision[levelout[,tree.i] <= level.i & is.finite(levelout[,tree.i]), tree.i],
                                  criterion.v = criterion.v[levelout[,tree.i] <= level.i & is.finite(levelout[,tree.i])]
      )

      level.stats.df.i[level.i, names(level.i.stats)] <- level.i.stats



    }

    level.stats.ls[[tree.i]] <- level.stats.df.i

  }

  levelstats <- do.call("rbind", args = level.stats.ls)

  # CUMULATIVE TREE STATS

  treestats <- tree.definitions
  helper <- paste(levelstats$tree, levelstats$level, sep = ".")
  maxlevs <- paste(rownames(tapply(levelstats$level, levelstats$tree, FUN = which.max)), tapply(levelstats$level, levelstats$tree, FUN = which.max), sep = ".")
  treestats <- cbind(tree.definitions, levelstats[helper %in% maxlevs, names(level.i.stats)])
  rownames(treestats) <- 1:nrow(treestats)



  return(list("decision" = decision,
              "levelout" = levelout,
              "levelstats" = levelstats,
              "treestats" = treestats
  ))

}
