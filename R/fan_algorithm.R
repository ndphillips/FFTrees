#' Grows fast-and-frugal trees using the fan algorithm
#'
#' @param formula formula. A formula
#' @param data dataframe. A dataset
#' @param max.levels integer. The maximum number of levels in the tree(s)
#' @param algorithm character. A string indicating how to rank cues during tree construction. "ifan"  (independent fan) means that cues will only be ranked once with the entire training dataset "dfan" (dependent fan) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars.
#' @param goal character. A string indicating the statistic to maximize: "acc" = overall accuracy, "bacc" = balanced accuracy, "wacc" = weighted accuracy
#' @param sens.w numeric. A number from 0 to 1 indicating how to weight sensitivity relative to specificity.
#' @param numthresh.method character. How should thresholds for numeric cues be determined? \code{"o"} will optimize thresholds, while \code{"m"} will always use the median.
#' @param stopping.rule character. A string indicating the method to stop growing trees. \code{"levels"} means the tree grows until a certain level, \code{"exemplars"} means the tree grows until a certain number of unclassified exemplars remain. \code{"statdelta"} means the tree grows until the change in the criterion statistic is less than a specified level.
#' @param stopping.par numeric. A number indicating the parameter for the stopping rule. For stopping.rule == \code{"levels"}, this is the number of levels. For stopping rule \code{"exemplars"}, this is the smallest percentage of examplars allowed in the last level.
#' @param rounding integer. How much should threshold parameters be rounded? Default is
#' @param progress logical. Should tree growing progress be displayed?
#' @param ... Currently ignored
#' @importFrom stats anova predict glm as.formula var
#'
#' @return A definition of an FFT
#' @export
#'
fan.algorithm <- function(formula,
                          data,
                          max.levels = 5,
                          algorithm = "ifan",
                          goal = "bacc",
                          sens.w = .5,
                          numthresh.method = "o",
                          stopping.rule = "exemplars",
                          stopping.par = .1,
                          rounding = NULL,
                          progress = TRUE) {
#
#   formula = formula
#   data = data.mf
#   max.levels = max.levels
#   algorithm = algorithm
#   goal = "bacc"
#   sens.w = .8
#   numthresh.method = numthresh.method
#   stopping.rule = stopping.rule
#   stopping.par = stopping.par
#   progress = progress
# # Some global variables which could be changed later.


repeat.cues <- TRUE
exit.method <- "fixed"
correction <- .25


# Start with criterion
data.mf <- model.frame(formula, data)
criterion.name <- names(data.mf)[1]
criterion.v <- data.mf[,1]
n.cues <- ncol(data.mf) - 1
cue.df <- data.mf[,2:ncol(data.mf)]

# ----------
# INITIAL TRAINING CUE ACCURACIES
# ----------

cue.accuracies <- cuerank(formula = formula,
                          data = data,
                          goal = goal,
                          numthresh.method = numthresh.method,
                          rounding = rounding,
                          progress = progress,
                          sens.w = sens.w)

# ----------
# GROW TREES
# ----------
{

  # SETUP TREES
  # create tree.dm which contains exit values for max.levels

  if(max.levels > 1) {

    expand.ls <- lapply(1:(max.levels - 1),
                        FUN = function(x) {return(c(0, 1))})

    expand.ls[[length(expand.ls) + 1]] <- .5
    names(expand.ls) <- c(paste("exit.", 1:(max.levels - 1), sep = ""),
                          paste("exit.", max.levels, sep = ""))

    tree.dm <- expand.grid(
      expand.ls,
      stringsAsFactors = FALSE)

  }

  if(max.levels == 1) {

    tree.dm <- data.frame("exit.1" = .5)

  }

  tree.dm$tree.num <- 1:nrow(tree.dm)
  n.trees <- nrow(tree.dm)

  # Set up decision.df and levelout.df
  #  These contain each tree's decisions and the level at which
  #   classifications are made

  decision <- as.data.frame(matrix(NA,
                                   nrow = length(criterion.v),
                                   ncol = n.trees))

  levelout <- as.data.frame(matrix(NA,
                                   nrow = length(criterion.v),
                                   ncol = n.trees))

  names(decision) <- paste("tree", 1:n.trees, sep = ".")
  names(levelout) <- paste("tree", 1:n.trees, sep = ".")

  # Loop over trees
  for(tree.i in 1:n.trees) {

    ## Determine exits for tree.i

    level.exits.v.i <- unlist(tree.dm[tree.i, grepl("exit.", names(tree.dm))])
    n.levels <- length(level.exits.v.i)

    ## Set up placeholders
    cue.accuracies.original <- cue.accuracies
    decision.v <- rep(NA, length(criterion.v))
    levelout.v <- rep(NA, length(criterion.v))

    ## level.stats shows cumulative classification decisions statistics at each level
    level.stats = data.frame("level" = NA,
                             "cue" = NA,
                             "class" = NA,
                             "threshold" = NA,
                             "direction" = NA,
                             "exit" = NA)

    level.stat.names <- names(classtable(1, 1))
    level.stats[level.stat.names] <- NA

    ## asif.stats shows cumulative classification statistics as if all exemplars were
    #   classified at the current level (i.e; if the tree stopped here)

    asif.stats <- data.frame("level" = 1:n.levels,
                             "sens" = NA,
                             "spec" = NA,
                             "bacc" = NA,
                             "wacc" = NA,
                             "goal.change" = NA)

    # Starting values
    grow.tree <- TRUE
    current.level <- 0

    # ------------------
    # Grow Tree!
    # --------------------

    while(grow.tree == TRUE) {

      current.level <- current.level + 1
      current.exit <- level.exits.v.i[current.level]
      remaining.exemplars <- is.na(decision.v)


      # Step 1) Determine cue for current level

      # ifan algorithm
      if(algorithm == "ifan") {

        # Get accuracies of un-used cues
        cue.accuracies.current <- cue.accuracies.original[(cue.accuracies.original$cue %in% level.stats$cue) == FALSE,]

      }

      # Conditional algorithm
      if(algorithm == "dfan") {

        data.mf.r <- data.mf[remaining.exemplars, ]

        # If cues can NOT be repeated, then remove old cues as well
        if(repeat.cues == FALSE) {

          remaining.cues.index <- (names(cue.df) %in% level.stats$cue) == FALSE
          remaining.cues <- names(cue.df)[remaining.cues.index]
          data.mf.r <- data.mf.r[, c(criterion.name, remaining.cues)]

        }

        # If there is no variance in the criterion, then stop growth!
        if(var(data.mf.r[,1]) == 0) {grow.tree <- FALSE ; break}

        # Calculate cue accuracies with remaining exemplars
        cue.accuracies.current <-  cuerank(formula = formula,
                                           data = data.mf.r,
                                           goal = goal,
                                           numthresh.method = numthresh.method,
                                           rounding = rounding,
                                           sens.w = sens.w)

      }

      # Get next cue based on maximizing goal

      best.cue.index <- which(cue.accuracies.current[[goal]] == max(cue.accuracies.current[[goal]]))

      # If there is a tie, take the first
      if(length(best.cue.index) > 1) {best.cue.index <- best.cue.index[1]}

      new.cue <- cue.accuracies.current$cue[best.cue.index]


      new.cue.stats <- cue.accuracies.current[cue.accuracies.current$cue == new.cue,]
      new.class <- new.cue.stats$class
      new.threshold <- new.cue.stats$threshold
      new.direction <- new.cue.stats$direction

      # ADD CUE INFO TO LEVEL.STATS

      level.stats$level[current.level] <- current.level
      level.stats$cue[current.level] <- new.cue
      level.stats$class[current.level] <- new.class
      level.stats$threshold[current.level] <- new.threshold
      level.stats$direction[current.level] <- new.direction
      level.stats$exit[current.level] <- current.exit

      # Get decisions for current cue
      cue.decisions <- apply.break(direction = new.direction,
                                   threshold.val = new.threshold,
                                   cue.v = data.mf[[new.cue]],
                                   cue.class = new.cue.stats$class)

      # Statistics for current decisions
      cue.classtable <- classtable(prediction.v = cue.decisions,
                                   criterion.v = criterion.v,
                                   sens.w = sens.w)

      # How would classifications look if all remaining exemplars
      #   were classified at the current level?
      as.if.decision.v <- decision.v
      as.if.decision.v[remaining.exemplars] <- cue.decisions[remaining.exemplars]

      asif.classtable <- classtable(prediction.v = as.if.decision.v,
                                    criterion.v = criterion.v,
                                    sens.w = sens.w)

      asif.stats[current.level, c("sens", "spec", "bacc", "wacc")] <-  asif.classtable[1, c("sens", "spec", "bacc", "wacc")]

      # If ASIF classification is perfect, then stop!
      if(asif.stats[[goal]][current.level] == 1) {grow.tree <- FALSE}

      if(current.level == 1) {

        asif.stats$goal.change[1] <- asif.classtable[[goal]]

      }

      if(current.level > 1) {

        goal.change <- asif.stats[[goal]][current.level] - asif.stats[[goal]][current.level - 1]
        asif.stats$goal.change[current.level] <- goal.change

      }

      # Step 3) Classify exemplars in current level
      {

        if(current.exit == 1 | current.exit == .5) {

          decide.1.index <- remaining.exemplars & cue.decisions == TRUE

          decision.v[decide.1.index] <- 1
          levelout.v[decide.1.index] <- current.level

        }


        if(current.exit == 0 | current.exit == .5) {

          decide.0.index <- is.na(decision.v) & cue.decisions == FALSE

          decision.v[decide.0.index] <- 0
          levelout.v[decide.0.index] <- current.level


        }
      }

      remaining.exemplars <- is.na(decision.v)

      # Step 4) Update Results
      {

        # Get cumulative stats of examplars currently classified

        cum.classtable <- classtable(
          prediction.v = decision.v[remaining.exemplars == FALSE],
          criterion.v = criterion.v[remaining.exemplars == FALSE],
          sens.w = sens.w)

        # Update level stats

        level.stats[current.level, c("level", "cue", "class", "threshold", "direction", "exit")] <- c(
          current.level, new.cue.stats[c("cue", "class", "threshold", "direction")], current.exit)

        level.stats[current.level, names(cum.classtable)] <- cum.classtable

      }

      # Step 5) Continue growing tree?
      {

        n.remaining <- sum(remaining.exemplars)

        if(n.remaining > 0 & current.level != n.cues & exit.method == "fixed") {

          if(current.level < n.levels) {grow.tree <- TRUE}
          if(current.level == n.levels) {grow.tree <- FALSE ; break}

        }
        if(n.remaining == 0 | current.level == n.cues) {break}
        if(stopping.rule == "exemplars" & n.remaining < stopping.par * nrow(cue.df)) {break}
        if(stopping.rule == "levels" & current.level == stopping.par) {break}


        if(algorithm == "dfan" & sd(criterion.v[remaining.exemplars]) == 0) {break}

        # Set up next level stats
        level.stats[current.level + 1,] <- NA

      }

    }  # STOP while(grow.tree) Loop

    # Step 5) No more growth. Make sure last level is bidirectional
    {

      last.level <- max(level.stats$level)
      last.cue <- level.stats$cue[last.level]

      last.exitdirection <- level.stats$exit[level.stats$level == last.level]

      if(last.exitdirection != .5) {

        decision.v[levelout.v == last.level] <- NA

        new.cue.stats <- cue.accuracies.current[cue.accuracies.current$cue == last.cue,]

        decision.index <- is.na(decision.v)

        # Step 2) Determine accuracy of negative and positive classification

        current.decisions <- apply.break(direction = new.direction,
                                         threshold.val = new.threshold,
                                         cue.v = data.mf[[last.cue]],
                                         cue.class = new.class)

        decide.0.index <- decision.index == TRUE & current.decisions == 0
        decide.1.index <- decision.index == TRUE & current.decisions == 1

        decision.v[decide.0.index] <- 0
        decision.v[decide.1.index] <- 1

        levelout.v[decide.0.index] <- current.level
        levelout.v[decide.1.index] <- current.level

        # up

        last.classtable <- classtable(prediction.v = decision.v,
                                      criterion.v = criterion.v,
                                      sens.w = sens.w)

        level.stats$exit[last.level] <- .5
        level.stats[last.level, names(last.classtable)] <- last.classtable

      }

    }

    # ------------------
    # Tree is finished!
    # --------------------

    # Set up final output

    decision[,tree.i] <- decision.v
    levelout[,tree.i] <- levelout.v

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

  stat.names <- names(classtable(1, 1))

  tree.definitions <- as.data.frame(matrix(NA, nrow = n.trees, ncol = 7))
  names(tree.definitions) <- c("tree", "nodes", "classes", "cues", "directions", "thresholds", "exits")

  level.stats.df$class <- substr(level.stats.df$class, 1, 1)

  for(i in 1:n.trees) {

    tree.definitions$tree[i] <- i
    tree.definitions$cues[i] <- paste(level.stats.df$cue[level.stats.df$tree == i], collapse = ";")
    tree.definitions$nodes[i] <- length(level.stats.df$cue[level.stats.df$tree == i])
    tree.definitions$classes[i] <- paste(level.stats.df$class[level.stats.df$tree == i], collapse = ";")
    tree.definitions$exits[i] <- paste(level.stats.df$exit[level.stats.df$tree == i], collapse = ";")
    tree.definitions$thresholds[i] <- paste(level.stats.df$threshold[level.stats.df$tree == i], collapse = ";")
    tree.definitions$directions[i] <- paste(level.stats.df$direction[level.stats.df$tree == i], collapse = ";")

  }

  duplicate.trees <- duplicated(tree.definitions[c("cues", "exits", "thresholds", "directions")])
  tree.definitions <- tree.definitions[duplicate.trees == FALSE,]
  rownames(tree.definitions) <- 1:nrow(tree.definitions)
  tree.definitions$tree <- 1:nrow(tree.definitions)
  tree.definitions <- tree.definitions[,c(which(names(tree.definitions) == "tree"), which(names(tree.definitions) != "tree"))]

}


# Order tree definitions by wacc:

my.applytree <- apply.tree(data = data,
                           formula = formula,
                           tree.definitions = tree.definitions,
                           sens.w = sens.w)

tree.order <- rank(-1 * my.applytree$treestats$wacc, ties.method = "first")

tree.definitions$rank <- tree.order
tree.definitions <- tree.definitions[order(tree.definitions$rank),]
tree.definitions$tree <- 1:nrow(tree.definitions)
tree.definitions <- tree.definitions[,names(tree.definitions) != "rank"]

return(list(tree.definitions = tree.definitions,
            cue.accuracies = cue.accuracies))

}
