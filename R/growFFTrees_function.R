#' Grows fast and frugal trees
#' @param formula a formula
#' @param data.train A training dataset
#' @param data.test A testing dataset
#' @param max.levels The maximum number of levels in the tree(s)
#' @param verbose A logical value indicating whether or not to display progress
#' @param numthresh.method A string indicating how to calculate cue splitting thresholds. "m" = median split, "o" = split that maximizes the tree criterion.
#' @param rank.method A string indicating how to rank cues during tree construction. "m" (for marginal) means that cues will only be ranked once with the entire training dataset. "c" (conditional) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars.
#' @param stopping.rule A string indicating the method to stop growing trees. "levels" means the tree grows until a certain level. "exemplars" means the tree grows until a certain number of unclassified exemplars remain. "statdelta" means the tree grows until the change in the tree.criterion statistic is less than a specified level.
#' @param stopping.par A number indicating the parameter for the stopping rule. For stopping.rule == "levels", this is the number of levels. For stopping rule == "exemplars", this is the smallest percentage of examplars allowed in the last level.
#' @importFrom stats anova predict glm as.formula
#' @return A list of length 3. The first element "tree.acc" is a dataframe containing the final statistics of all trees. The second element "cue.accuracies" shows the accuracies of all cues. The third element "tree.class.ls" is a list with n.trees elements, where each element shows the final decisions for each tree for each exemplar.
#' @export
#'


grow.FFTrees <- function(
                     formula,
                     data.train,
                     data.test = NULL,
                     rank.method = "m",
                     numthresh.method = "o",
                     max.levels = 4,
                     stopping.rule = "exemplars",
                     stopping.par = .1,
                     verbose = F
) {

  tree.criterion <- "v"
  exit.method <- "fixed"
  correction <- .25
  rounding <- 2
  hr.weight <- .5


  # Set up dataframes

  datamf.train <- model.frame(formula = formula, data = data.train)

  cue.train <- datamf.train[,2:ncol(datamf.train)]
  crit.train <- datamf.train[,1]

  if(is.null(data.test) == F) {

    datamf.test <- model.frame(formula = formula, data = data.test)

    cue.test <- datamf.test[,2:ncol(datamf.test)]
    crit.test <- datamf.test[,1]

  } else{

    cue.test <- NULL
    crit.test <- NULL

  }

  n.cues <- ncol(cue.train)

  # ----------
  # INITIAL TRAINING CUE ACCURACIES
  # ----------

  if(verbose) {print("Calculating initial cue accuracies...")}

  cue.accuracies.train <- cuerank(cue.df = cue.train,
                                  criterion.v = crit.train,
                                  tree.criterion = tree.criterion,
                                  numthresh.method = numthresh.method,
                                  rounding = rounding,
                                  verbose = verbose
  )

  # ----------
  # GROW TREES
  # ----------
  {

    if(verbose) {print("Growing trees..")}


    # -----
    # Set up trees
    #   tree.dm
    #   decision.train (.test)
    #   levelout.train (.test)
    # ----

    # create tree.dm (exit values and n.levels)

    expand.ls <- lapply(1:(max.levels - 1), FUN = function(x) {return(c(0, 1))})

    expand.ls[[length(expand.ls) + 1]] <- .5


    names(expand.ls) <- c(paste("exit.", 1:(max.levels - 1), sep = ""),
                          paste("exit.", max.levels, sep = "")
    )


    tree.dm <- expand.grid(
      expand.ls,
      stringsAsFactors = F
    )

    tree.dm$tree.num <- 1:nrow(tree.dm)

    n.trees <- nrow(tree.dm)

    # Set up decision.df and levelout.df

    decision.train <- as.data.frame(matrix(NA,
                                           nrow = length(crit.train),
                                           ncol = n.trees))

    levelout.train <- as.data.frame(matrix(NA,
                                           nrow = length(crit.train),
                                           ncol = n.trees))

    names(decision.train) <- paste("tree", 1:n.trees, sep = ".")
    names(levelout.train) <- paste("tree", 1:n.trees, sep = ".")

    # Loop over trees
    for(tree.i in 1:n.trees) {

      # ----------
      # Step 3: Grow tree.i
      # ----------

      ## Determine exits for tree.i

      level.exits.v.i <- unlist(tree.dm[tree.i, grepl("exit.", names(tree.dm))])
      n.levels <- length(level.exits.v.i)


      ## Set up placeholders

      cue.train.i <- cue.train
      criterion.v.i <- crit.train
      cue.accuracies.original <- cue.accuracies.train
      decision.v <- rep(NA, length(crit.train))
      levelout.v <- rep(NA, length(crit.train))


      level.stats = data.frame("level.num" = NA,
                               "level.name" = NA,
                               "level.class" = NA,
                               "level.threshold" = NA,
                               "level.sigdirection" = NA,
                               "level.exitdirection" = NA
      )

      level.stat.names <- c("hi", "mi", "fa", "cr", "hr", "far", "v", "dprime", "correction", "hr.weight")
      level.stats[level.stat.names] <- NA


      # Starting values
      grow.tree <- T
      current.level <- 0

      # Apply break function
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

      # ------------------
      # Grow Tree!
      # --------------------

      while(grow.tree == T) {

        current.level <- current.level + 1

        # Step 1) Determine cue

        if(rank.method == "m") {

          cue.accuracies.current <- cue.accuracies.original[(cue.accuracies.original$cue.name %in% level.stats$level.name) == F,]

        }

        if(rank.method == "c") {

          remaining.exemplars <- is.na(decision.v)
          remaining.cues.index <- (names(cue.train.i) %in% level.stats$level.name) == F
          remaining.cues <- names(cue.train.i)[remaining.cues.index]

          if(length(remaining.cues) == 1) {

            cue.train.i.temp <- as.data.frame(cue.train.i[remaining.exemplars, remaining.cues])
            names(cue.train.i.temp) <- remaining.cues

          }

          if(length(remaining.cues) > 1) {

            cue.train.i.temp <- cue.train.i[remaining.exemplars, remaining.cues]

          }

          cue.accuracies.current <-  cuerank(cue.df = cue.train.i.temp,
                                             criterion.v = criterion.v.i[remaining.exemplars],
                                             tree.criterion = tree.criterion,
                                             numthresh.method = numthresh.method,
                                             rounding = rounding
          )

        }

        best.cue.index <- which(cue.accuracies.current[tree.criterion] == max(cue.accuracies.current[tree.criterion], na.rm = T))

        new.cue <- cue.accuracies.current$cue.name[best.cue.index]

        if(length(new.cue) > 1) {new.cue <- new.cue[sample(1:length(new.cue), size = 1)]}

        new.cue.stats <- cue.accuracies.current[cue.accuracies.current$cue.name == new.cue,]

        # Step 2) Make decisions If all remaining exemplars were classified

        current.decisions <- apply.break(direction = new.cue.stats$level.sigdirection,
                                         threshold.val = new.cue.stats$level.threshold,
                                         cue.v = cue.train.i[, new.cue],
                                         cue.class = new.cue.stats$cue.class
        )

        current.level.classtable <- classtable(prediction.v = current.decisions[is.na(decision.v)],
                                               criterion.v = criterion.v.i[is.na(decision.v)])


        current.exit <- level.exits.v.i[current.level]


        # Step 3) Classify participants in current level
        {

          if(current.exit == 1 | current.exit == .5) {

            decide.1.index <- is.na(decision.v) & current.decisions == T

            decision.v[decide.1.index] <- 1
            levelout.v[decide.1.index] <- current.level

          }


          if(current.exit == 0 | current.exit == .5) {

            decide.0.index <- is.na(decision.v) & current.decisions == F

            decision.v[decide.0.index] <- 0
            levelout.v[decide.0.index] <- current.level


          }
        }

        # Step X) Update Results
        {

          # Get cumulative stats of examplars currently classified

          cum.level.classtable <- classtable(
            prediction.v = decision.v[levelout.v <= current.level & is.na(levelout.v) == F],
            criterion.v = criterion.v.i[levelout.v <= current.level & is.na(levelout.v) == F])

          if(current.level > 1) {

            level.stats[nrow(level.stats) + 1, ] <- NA

          }

          # Update level stats

          level.stats[current.level, c("level.num", "level.name", "level.class", "level.threshold", "level.sigdirection", "level.exitdirection")] <- c(
            current.level, new.cue.stats[c("cue.name", "cue.class", "level.threshold", "level.sigdirection")], current.exit)


          level.stats[current.level, names(cum.level.classtable)] <- cum.level.classtable

        }

        # Step 4) Continue growing tree?
        {

          n.remaining <- sum(is.na(decision.v))

          if(n.remaining > 0 & current.level != n.cues & exit.method == "fixed") {

            if(current.level < n.levels) {grow.tree <- T}
            if(current.level == n.levels) {grow.tree <- F}

          }
          if(n.remaining == 0 | current.level == n.cues) {grow.tree <- F}
          if(stopping.rule == "exemplars" & n.remaining < stopping.par * nrow(cue.train.i)) {grow.tree <- F}
          if(stopping.rule == "levels" & current.level == stopping.par) {grow.tree <- F}

        }

      }  # STOP while(grow.tree) Loop

      # Step 5) No more growth. Turn last level into a bi-directional one
      {

        last.level <- max(level.stats$level.num)
        last.cue <- level.stats$level.name[last.level]

        last.exitdirection <- level.stats$level.exitdirection[level.stats$level.num == last.level]

        if(last.exitdirection != .5) {

          decision.v[levelout.v == last.level] <- NA

          new.cue.stats <- cue.accuracies.current[cue.accuracies.current$cue.name == last.cue,]

          decision.index <- is.na(decision.v)


          # Step 2) Determine accuracy of negative and positive classification

          current.decisions <- apply.break(direction = new.cue.stats$level.sigdirection,
                                           threshold.val = new.cue.stats$level.threshold,
                                           cue.v = cue.train.i[, last.cue],
                                           cue.class = new.cue.stats$cue.class
          )

          decide.0.index <- decision.index == T & current.decisions == 0
          decide.1.index <- decision.index == T & current.decisions == 1

          decision.v[decide.0.index] <- 0
          decision.v[decide.1.index] <- 1

          levelout.v[decide.0.index] <- current.level
          levelout.v[decide.1.index] <- current.level

          # up

          last.level.classtable <- classtable(prediction.v = decision.v,
                                              criterion.v = crit.train)




          level.stats$level.exitdirection[last.level] <- .5
          level.stats[last.level, names(current.level.classtable)] <- last.level.classtable

        }

      }

      # ------------------
      # Tree is finished!
      # --------------------

      # Set up final output

      decision.train[,tree.i] <- decision.v
      levelout.train[,tree.i] <- levelout.v

      level.stats$tree <- tree.i

      if(tree.i == 1) {level.stats.df <- level.stats}
      if(tree.i > 1) {level.stats.df <- rbind(level.stats.df, level.stats)}

    }

  }



  # -------------------------
  # SUMMARISE TREE DEFINITIONS AND STATISTICS
  #   trees
  # -------------------------
  {
    # Summarise tree definitions

    trees <- as.data.frame(matrix(NA, nrow = n.trees, ncol = 6))
    names(trees) <- c("tree.num", "level.name", "level.class", "level.exit", "level.threshold", "level.sigdirection")

    trees$n.train <- nrow(cue.train.i)
    trees$tree.num <- 1:n.trees

    for(i in 1:n.trees) {

      trees$level.name[i] <- paste(level.stats.df$level.name[level.stats.df$tree == i], collapse = ";")
      trees$level.class[i] <- paste(level.stats.df$level.class[level.stats.df$tree == i], collapse = ";")
      trees$level.exit[i] <- paste(level.stats.df$level.exitdirection[level.stats.df$tree == i], collapse = ";")
      trees$level.threshold[i] <- paste(level.stats.df$level.threshold[level.stats.df$tree == i], collapse = ";")
      trees$level.sigdirection[i] <- paste(level.stats.df$level.sigdirection[level.stats.df$tree == i], collapse = ";")

    }

    # Determine final tree train statistics

    for(tree.i in 1:n.trees) {

      tree.i.train.stats <- classtable(prediction.v = decision.train[,tree.i],
                                       criterion.v = crit.train)



      if(tree.i == 1) {tree.train.stats <- tree.i.train.stats}
      if(tree.i != 1) {tree.train.stats <- rbind(tree.train.stats,
                                                 tree.i.train.stats)}

    }

    tree.train.stats$tree <- 1:n.trees
    names(tree.train.stats)[1:8] <- paste(names(tree.train.stats)[1:8], ".train", sep = "")
    trees <- cbind(trees, tree.train.stats[, 1:8])

    # Add testing stats for all trees

    decision.test <- NULL
    levelout.test <- NULL


    if(is.null(cue.test)) {

      trees[paste(c("n", "hi", "mi", "fa", "cr", "hr", "far", "v", "dprime"), ".test", sep = "")] <- NA

    }

    if(is.null(cue.test) == F) {

      n.test <- length(crit.test)

      levelout.test <- as.data.frame(matrix(NA, nrow = n.test, ncol = n.trees))
      decision.test <- as.data.frame(matrix(NA, nrow = n.test, ncol = n.trees))

      names(levelout.test) <- paste("tree.", 1:n.trees, sep = "")
      names(decision.test) <- paste("tree.", 1:n.trees, sep = "")

      trees[paste(c("n", "hi", "mi", "fa", "cr", "hr", "far", "v", "dprime"), ".test", sep = "")] <- NA

      for(tree.i in 1:n.trees) {

        tree.i.pred <- predict.FFTrees(
                                  formula = formula,
                                  data = data.test,
                                  level.name.v = trees$level.name[tree.i],
                                  level.class.v = trees$level.class[tree.i],
                                  level.exit.v = trees$level.exit[tree.i],
                                  level.threshold.v = trees$level.threshold[tree.i],
                                  level.sigdirection.v = trees$level.sigdirection[tree.i]
        )

        levelout.test[,tree.i] <- unlist(tree.i.pred$levelout)
        decision.test[,tree.i] <- unlist(tree.i.pred$decision)

        trees[tree.i, paste(c("n", "hi", "mi", "fa", "cr", "hr", "far", "v", "dprime"), ".test", sep = "")] <- c(n.test, tree.i.pred$tree.stats[c("hi", "mi", "fa", "cr", "hr", "far", "v", "dprime")])
      }


    }


    # Remove duplicate trees...

    duplicate.trees <- duplicated(trees[c("level.name", "level.exit", "level.threshold", "level.sigdirection")])
    trees <- trees[duplicate.trees == F,]

    levelout.train <- levelout.train[,duplicate.trees == F]
    levelout.test <- levelout.test[,duplicate.trees == F]

    decision.train <- decision.train[,duplicate.trees == F]
    decision.test <- decision.test[,duplicate.trees == F]

    trees$tree.num <- 1:nrow(trees)

    # Calculate AUC

    fft.auc.train <- auc(trees$hr.train, trees$far.train)

    if(is.null(cue.test) == F) {
      fft.auc.test <- auc(trees$hr.test, trees$far.test)
    }

    if(is.null(cue.test)) {fft.auc.test <- NA}


  }



  # setup output


  # Summaraise all AUC stats
  auc.df <- matrix(c(fft.auc.train, fft.auc.test), nrow = 2, ncol = 1)
colnames(auc.df) <- "fft"
  rownames(auc.df) <- c("train", "test")

  output <- list(
    auc = auc.df,
    trees = trees,
    levelout.train = levelout.train,
    levelout.test = levelout.test,
    decision.train = decision.train,
    decision.test = decision.test
  )

  return(output)

}
