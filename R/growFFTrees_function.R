#' Grows fast-and-frugal trees using an algorithm specified by \code{algorithm}.
#'
#' @param formula formula. A formula
#' @param data dataframe. A dataset
#' @param max.levels integer. The maximum number of levels in the tree(s)
#' @param algorithm character. A string indicating how to rank cues during tree construction. "m" (for ifan) means that cues will only be ranked once with the entire training dataset. "c" (conditional) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars.
#' @param goal character. A string indicating the statistic to maximize: "acc" = overall accuracy, "bacc" = balanced accuracy, "wacc" = weighted accuracy
#' @param sens.w numeric. A number from 0 to 1 indicating how to weight sensitivity relative to specificity.
#' @param numthresh.method character. How should thresholds for numeric cues be determined? \code{"o"} will optimize thresholds, while \code{"m"} will always use the median.
#' @param stopping.rule character. A string indicating the method to stop growing trees. "levels" means the tree grows until a certain level. "exemplars" means the tree grows until a certain number of unclassified exemplars remain. "statdelta" means the tree grows until the change in the criterion statistic is less than a specified level.
#' @param stopping.par numeric. A number indicating the parameter for the stopping rule. For stopping.rule == "levels", this is the number of levels. For stopping rule == "exemplars", this is the smallest percentage of examplars allowed in the last level.
#' @param progress logical. Should tree growing progress be displayed?
#' @param rank.method depricated arguments
#' @param cue.accuracies depricated arguments
#' @param ... Currently ignored
#' @importFrom stats anova predict glm as.formula var
#' @return A list of length 4. tree.definitions contains definitions of the tree(s). tree.stats contains classification statistics for the tree(s). levelout shows which level in the tree(s) each exemplar is classified. Finally, decision shows the classification decision for each tree for each exemplar
#' @export
#' @examples
#'
#'  titanic.trees <- grow.FFTrees(formula = survived ~.,
#'                                data = titanic)
#'
#' # Tree definitions are stored in tree.definitions
#'
#' titanic.trees$tree.definitions
#'
#' # Tree classification statistics are in tree.stats
#'
#' titanic.trees$tree.stats
#'
#' # The level at which each exemplar is classified for each tree is in levelout
#'
#' titanic.trees$levelout
#'
#' # The decision for each exemplar for each tree is in decision
#'
#' titanic.trees$decision
#'
#'
#'
grow.FFTrees <- function(formula,
                         data,
                         max.levels = NULL,
                         algorithm = "ifan",
                         goal = "bacc",
                         sens.w = .5,
                         numthresh.method = "o",
                         stopping.rule = "exemplars",
                         stopping.par = .1,
                         progress = FALSE,
                         rank.method = NULL,
                         cue.accuracies = NULL,
                         ...
) {
#
  # formula = formula
  # data = data.train
  # algorithm = algorithm
  # goal = goal
  # stopping.rule = stopping.rule
  # stopping.par = stopping.par
  # max.levels = max.levels
  # sens.w = sens.w
  # progress = progress

# Check for depricated arguments
if(is.null(rank.method) == FALSE) {

  warning("The argument rank.method is depricated. Use algorithm instead.")

  algorithm <- rank.method

}

# Set up dataframes

# data.mf contains only criterion and predictors
data.mf <- model.frame(formula = formula,
                       data = data)

# cue.df contains cues
cue.df <- data.mf[,2:ncol(data.mf), drop = FALSE]

criterion.v <- data.mf[,1]
crit.name <- names(data.mf)[1]
n.cues <- ncol(cue.df)

# Determine tree.definitions and cue.accuracies


if(algorithm %in% c("max", "zigzag")) {


  heuristicResult <- heuristic.algorithm(formula = formula,
                                          data = data.mf,
                                          max.levels = max.levels,
                                          algorithm = algorithm)

  tree.definitions <- heuristicResult$tree.definitions
  cue.accuracies <- heuristicResult$cue.accuracies

}

if(algorithm %in% c("ifan", "dfan")) {

  if(is.null(max.levels)) {max.levels <- 4}

  fanResult <- fan.algorithm(formula = formula,
                             data = data.mf,
                             max.levels = max.levels,
                             algorithm = algorithm,
                             goal = "bacc",
                             sens.w = sens.w,
                             numthresh.method = numthresh.method,
                             stopping.rule = stopping.rule,
                             stopping.par = stopping.par,
                             progress = progress)

  tree.definitions <- fanResult$tree.definitions
  cue.accuracies <- fanResult$cue.accuracies

}




# Apply tree.definitions to data and calculate statistics

n.trees <- nrow(tree.definitions)

my.apply.tree <- apply.tree(data = data,
                            formula = formula,
                            tree.definitions = tree.definitions,
                            sens.w = sens.w)


stat.names <- names(classtable(c(1, 1, 0), c(1, 0, 0)))

output <- list(tree.definitions = tree.definitions,
               tree.stats = my.apply.tree$treestats[,c("tree", stat.names)],
               cue.accuracies = cue.accuracies,
               levelout = my.apply.tree$levelout,
               decision = my.apply.tree$decision)

return(output)

}

