#' Grows fast-and-frugal trees using the fan algorithm
#'
#' @param x FFTrees. An FFTrees object
#' @param repeat.cues logical.
#' @importFrom stats anova predict glm as.formula var
#'
fftrees_grow_fan <- function(x,
                             repeat.cues = TRUE) {

if(!x$params$quiet) {message(paste0("Growing FFTs with ", x$params$algorithm))}

# Some global variables which could be changed later.

exit.method <- "fixed"
correction <- .25

# Extract key variables

criterion_name <- x$criterion_name
criterion_v <- x$data$train[[criterion_name]]
cues_n <- length(x$cue_names)
cases_n <- nrow(x$data$train)
cue_df <- x$data$train[,names(x$data$train) != criterion_name]

# ----------
# INITIAL TRAINING CUE ACCURACIES
# ----------

x <- fftrees_cuerank(x,
                     newdata = x$data$train,
                     data = "train")

# ----------
# GROW TREES
# ----------
{

# SETUP TREES
# [tree_dm, tree_stats_ls, level_stats_ls]
{
if(x$params$max.levels > 1) {

  expand.ls <- lapply(1:(x$params$max.levels - 1),
                      FUN = function(x) {return(c(0, 1))})

  expand.ls[[length(expand.ls) + 1]] <- .5
  names(expand.ls) <- c(paste("exit.", 1:(x$params$max.levels - 1), sep = ""),
                        paste("exit.", x$params$max.levels, sep = ""))

  tree_dm <- expand.grid(
    expand.ls,
    stringsAsFactors = FALSE)

}

if(x$params$max.levels == 1) {

  tree_dm <- data.frame("exit.1" = .5)

}

tree_dm$tree.num <- 1:nrow(tree_dm)
tree_n <- nrow(tree_dm)

# Set up tree_stats_ls
#  A list containing dataframes representing
#   one-row-per-case, one-column-per-tree statistics

tree_table_names <- c("decision", "levelout")

tree_stats_ls <- lapply(1:length(tree_table_names), FUN = function(x) {

  output <- as.data.frame(matrix(NA, nrow = cases_n, ncol = tree_n))

  names(output) <- paste("tree", 1:tree_n, sep = ".")

  output

})

names(tree_stats_ls) <- tree_table_names

level_stats_ls <- vector("list", length = tree_n)

}

  # Loop over trees
for(tree_i in 1:tree_n) {

  ## data
  data_current <- x$data$train
  cue_df_current <- x$cues$stats$train

  ## Determine exits for tree_i

  exits_i <- unlist(tree_dm[tree_i, grepl("exit.", names(tree_dm))])
  level_n <- length(exits_i)

  ## Set up placeholders
  cue_best_df.original <- x$cues$stats$train

  # Decisions, levelout, and cost vectors
  decision_v <- rep(NA, cases_n)
  levelout_v <- rep(NA, cases_n)
  cuecost_v <- rep(0, cases_n)
  outcomecost_v <- rep(NA, cases_n)
  totalcost_v <- rep(0, cases_n)
  hi_v <- rep(NA, cases_n)
  fa_v <- rep(NA, cases_n)
  mi_v <- rep(NA, cases_n)
  cr_v <- rep(NA, cases_n)

  ## level_stats_i shows cumulative classification decisions statistics at each level
  level_stats_i <- data.frame("level" = NA,
                           "cue" = NA,
                           "cost_cues" = NA,
                           "cost_cues.cum" = NA,
                           "cost_decisions"= NA,
                           "class" = NA,
                           "threshold" = NA,
                           "direction" = NA,
                           "exit" = NA)

  level_stat_names <- setdiff(names(fftrees_threshold_factor_grid()), c("threshold", "direction"))
  level_stats_i[level_stat_names] <- NA

  ## asif.stats shows cumulative classification statistics as if all exemplars were
  #   classified at the current level (i.e; if the tree stopped here)

  asif.stats <- data.frame("level" = 1:level_n,
                           "sens" = NA,
                           "spec" = NA,
                           "acc" = NA,
                           "bacc" = NA,
                           "wacc" = NA,
                           "dprime" = NA,
                           "cost" = NA,
                           "goal.change" = NA)

  # Starting values
  grow.tree <- TRUE
  level_current <- 0

    # ------------------
    # Grow Tree!
    # --------------------

while(grow.tree == TRUE) {

level_current <- level_current + 1
exit_current <- exits_i[level_current]
cases_remaining <- is.na(decision_v)

# Step 1) Determine cue for current level
{
# ifan x$params$algorithm
if(x$params$algorithm == "ifan") {

  # Get accuracies of un-used cues
  cue_best_df_current <- cue_best_df.original[(cue_best_df.original$cue %in% level_stats_i$cue) == FALSE,]

}

# dfan x$params$algorithm
if(x$params$algorithm == "dfan") {

  data_current <- x$data$train[cases_remaining, ]

  # If cues can NOT be repeated, then remove old cues as well
  if(repeat.cues == FALSE) {

    remaining.cues.index <- (names(cue_df) %in% level_stats_i$cue) == FALSE
    remaining.cues <- names(cue_df)[remaining.cues.index]
    data_current <- data_current[, c(criterion_name, remaining.cues)]

  }

  # If there is no variance in the criterion, then stop growth!
  if(all(duplicated(data_current)[-1L])) {grow.tree <- FALSE ; break}

  # Create new dynamic cue ran

  x <- fftrees_cuerank(x,
                                newdata = data_current,
                                data = "dynamic")

  # Calculate cue accuracies with remaining exemplars
  cue_best_df_current <-  x$cues$stats$dynamic

}

# Get next cue based on maximizing goal
cue_best_i <- which(cue_best_df_current[[x$params$goal.chase]] == max(cue_best_df_current[[x$params$goal.chase]], na.rm = TRUE))

# If there is a tie, take the first
if(length(cue_best_i) > 1) {cue_best_i <- cue_best_i[1]}

cues_name_new <- cue_best_df_current$cue[cue_best_i]
cue_stats_new <- cue_best_df_current$cue[cue_best_i]
cue_cost_new <- x$params$cost.cues[[cues_name_new]]
cue_class_new <- cue_best_df_current$class[cue_best_i]
cue_threshold_new <- cue_best_df_current$threshold[cue_best_i]
cue_direction_new <- cue_best_df_current$direction[cue_best_i]

# Add cue costs to cuecost_v
cuecost_v[is.na(decision_v)] <- cuecost_v[is.na(decision_v)] + cue_cost_new

# ADD CUE INFO TO LEVEL.STATS

level_stats_i$level[level_current] <- level_current
level_stats_i$cue[level_current] <- cues_name_new
level_stats_i$cost_cues[level_current] <- cue_cost_new
level_stats_i$cost_cues.cum[level_current] <- sum(level_stats_i$cost_cues[1:level_current])
level_stats_i$class[level_current] <- cue_class_new
level_stats_i$threshold[level_current] <- cue_threshold_new
level_stats_i$direction[level_current] <- cue_direction_new
level_stats_i$exit[level_current] <- exit_current
}

# Step 2) Determine how classifications would look if
#  all remaining exemplars were classified
{

# Get decisions for current cue
cue.decisions <- apply.break(direction = cue_direction_new,
                             threshold.val = cue_threshold_new,
                             cue.v = x$data$train[[cues_name_new]],
                             cue.class = cue_class_new)

# How would classifications look if all remaining exemplars
#   were classified at the current level?

asif.decision_v <- decision_v
asif.levelout_v <- levelout_v
asif.cuecost_v <- cuecost_v

asif.decision_v[cases_remaining] <- cue.decisions[cases_remaining]
asif.levelout_v[cases_remaining] <- level_current
asif.cuecost_v[cases_remaining] <- cue_cost_new

# Calculate asif_cm

asif_results <- classtable(prediction_v = asif.decision_v,
                           criterion_v = criterion_v)


# Add key stats to asif.stats

asif.stats[level_current, c("sens", "spec", "acc", "bacc", "wacc", "dprime", "cost")] <-  c(asif_results$sens, asif_results$spec, asif_results$acc, asif_results$bacc, asif_results$wacc, asif_results$dprime, asif_results$cost)


  # If ASIF classification is perfect, then stop!

  if(x$params$goal.chase != "cost") {

  if(asif.stats[[x$params$goal.chase]][level_current] == 1) {grow.tree <- FALSE}

  } else {

    if(asif.stats[[x$params$goal.chase]][level_current] == 0) {grow.tree <- FALSE}

  }

# Calculate goal change
{
  if(level_current == 1) {

    asif.stats$goal.change[1] <- asif.stats[[x$params$goal]][1]

  }

  if(level_current > 1) {

    goal.change <- asif.stats[[x$params$goal.chase]][level_current] - asif.stats[[x$params$goal.chase]][level_current - 1]
    asif.stats$goal.change[level_current] <- goal.change

  }
}
}

# Step 3) Classify exemplars in current level
{

    if(exit_current == 1 | exit_current == .5) {

      decide.1.index <- cases_remaining & cue.decisions == TRUE

      decision_v[decide.1.index] <- TRUE
      levelout_v[decide.1.index] <- level_current

    }

    if(exit_current == 0 | exit_current == .5) {

      decide.0.index <- is.na(decision_v) & cue.decisions == FALSE

      decision_v[decide.0.index] <- FALSE
      levelout_v[decide.0.index] <- level_current
    }

    # Update cost vectors

    hi_v <- decision_v == TRUE & criterion_v == TRUE
    mi_v <- decision_v == FALSE & criterion_v == TRUE
    fa_v <- decision_v == TRUE & criterion_v == FALSE
    cr_v <- decision_v == FALSE & criterion_v == FALSE

    outcomecost_v[hi_v == TRUE] <- x$params$cost.outcomes$hi
    outcomecost_v[mi_v == TRUE] <- x$params$cost.outcomes$mi
    outcomecost_v[fa_v == TRUE] <- x$params$cost.outcomes$fa
    outcomecost_v[cr_v == TRUE] <- x$params$cost.outcomes$cr

  }

# Step 4) Update Results
{
  cases_remaining <- is.na(decision_v)

  # NEED TO FIX THIS BELOW TO INCORPORATE ALL COSTS

  # Get cumulative stats of examplars currently classified

  results_cum <- classtable(prediction_v = decision_v[cases_remaining == FALSE],
                            criterion_v = criterion_v[cases_remaining == FALSE],
                            sens.w = x$params$sens.w,
                            cost.v = cuecost_v[cases_remaining == FALSE],
                            cost.outcomes = x$params$cost.outcomes)


  # Update level stats
  level_stats_i[level_current,] <- NA
  level_stats_i$level[level_current] <- level_current
  level_stats_i$cue[level_current] <- cues_name_new
  level_stats_i$class[level_current] <- cue_class_new
  level_stats_i$threshold[level_current] <- cue_threshold_new
  level_stats_i$direction[level_current] <- cue_direction_new
  level_stats_i$exit[level_current] <- exit_current


  level_stats_i[level_current, c("hi", "fa", "mi", "cr", "sens", "spec", "bacc", "acc", "wacc", "cost_decisions", "cost")] <- results_cum[,c("hi", "fa", "mi", "cr", "sens", "spec", "bacc", "acc", "wacc", "cost_decisions", "cost")]

}

# Step 5) Continue growing tree?
{

  cases_remaining_n <- sum(cases_remaining)

  if(cases_remaining_n > 0 & level_current != cues_n & exit.method == "fixed") {

    if(level_current < level_n) {grow.tree <- TRUE}
    if(level_current == level_n) {grow.tree <- FALSE ; break}

  }
  if(cases_remaining_n == 0 | level_current == cues_n) {break}
  if(x$params$stopping.rule == "exemplars" & cases_remaining_n < x$params$stopping.par * nrow(cue_df)) {break}
  if(x$params$stopping.rule == "levels" & level_current == x$params$stopping.par) {break}

  if(x$params$algorithm == "dfan" & sd(criterion_v[cases_remaining]) == 0) {break}

  # Set up next level stats
  level_stats_i[level_current + 1,] <- NA

}

}  # STOP while(grow.tree) Loop

# Step 6) No more growth. Make sure last level is bidirectional
{

  last.level <- max(level_stats_i$level)
  last.cue <- level_stats_i$cue[last.level]
  cost.cue <- x$params$cost.cues[[last.cue]]

  last.exitdirection <- level_stats_i$exit[level_stats_i$level == last.level]

  if(last.exitdirection != .5) {

    decision_v[levelout_v == last.level] <- NA

    last_cue_stats <- cue_best_df_current[cue_best_df_current$cue == last.cue,]

    decision.index <- is.na(decision_v)

    # Step 2) Determine accuracy of negative and positive classification

    current.decisions <- apply.break(direction = last_cue_stats$direction,
                                     threshold.val = last_cue_stats$threshold,
                                     cue.v = x$data$train[[last.cue]],
                                     cue.class = last_cue_stats$class)

    decide.0.index <- decision.index == TRUE & current.decisions == FALSE
    decide.1.index <- decision.index == TRUE & current.decisions == TRUE

    decision_v[decide.0.index] <- FALSE
    decision_v[decide.1.index] <- TRUE

    levelout_v[decide.0.index] <- level_current
    levelout_v[decide.1.index] <- level_current

    # up

    last.classtable <- classtable(prediction_v = as.logical(decision_v),
                                  criterion_v = as.logical(criterion_v),
                                  sens.w = x$params$sens.w,
                                  cost.v = cuecost_v,
                                  cost.outcomes = x$params$cost.outcomes)

    level_stats_i$exit[last.level] <- .5


    level_stats_i[last.level,  c("hi", "fa", "mi", "cr", "sens", "spec", "bacc", "acc", "wacc", "cost_decisions")] <- last.classtable[,c("hi", "fa", "mi", "cr", "sens", "spec", "bacc", "acc", "wacc", "cost_decisions")]

  }

}

# ------------------
# Tree is finished!
# --------------------

# Set up final output

tree_stats_ls$decision[,tree_i] <- decision_v
tree_stats_ls$levelout[,tree_i] <- levelout_v

level_stats_i$tree <- tree_i

level_stats_ls[[tree_i]] <- level_stats_i

  }

}

# -------------------------
# SUMMARISE TREE DEFINITIONS AND STATISTICS
#   tree.definitions
# -------------------------
{
  # Summarise tree definitions

  tree.definitions <- as.data.frame(matrix(NA, nrow = tree_n, ncol = 7))
  names(tree.definitions) <- c("tree", "nodes", "classes", "cues", "directions", "thresholds", "exits")

  for(tree_i in 1:tree_n) {

    level_stats_i <- level_stats_ls[[tree_i]]

    tree.definitions$tree[tree_i] <- tree_i
    tree.definitions$cues[tree_i] <- paste(level_stats_i$cue, collapse = ";")
    tree.definitions$nodes[tree_i] <- length(level_stats_i$cue)
    tree.definitions$classes[tree_i] <- paste(substr(level_stats_i$class, 1, 1), collapse = ";")
    tree.definitions$exits[tree_i] <- paste(level_stats_i$exit, collapse = ";")
    tree.definitions$thresholds[tree_i] <- paste(level_stats_i$threshold, collapse = ";")
    tree.definitions$directions[tree_i] <- paste(level_stats_i$direction, collapse = ";")

  }

  duplicate.trees <- duplicated(tree.definitions[c("cues", "exits", "thresholds", "directions")])
  tree.definitions <- tree.definitions[duplicate.trees == FALSE,]
  rownames(tree.definitions) <- 1:nrow(tree.definitions)
  tree.definitions$tree <- 1:nrow(tree.definitions)
  tree.definitions <- tree.definitions[,c(which(names(tree.definitions) == "tree"), which(names(tree.definitions) != "tree"))]

}


# Add tree.definitions to x

x$trees$definitions <- tree.definitions
x$trees$n <- nrow(tree.definitions)

return(x)

}
