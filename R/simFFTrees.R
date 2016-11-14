#' Generates and summarises many FFTrees objects from multiple training sets
#'
#' @param formula A formula specifying a binary criterion as a function of multiple variables
#' @param data A dataframe containing variables in formula
#' @param max.levels A number indicating the maximum number of levels considered for the tree.
#' @param sim integer. Number of simulations to perform.
#' @param train.p A number between 0 and 1 indicating what percentage of the data to use for training. This only applies when data.test is not specified by the user.
#' @param rank.method rank.method A string indicating how to rank cues during tree construction. "m" (for marginal) means that cues will only be ranked once with the entire training dataset. "c" (conditional) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars. This also means that the same cue can be used multiple times in the trees. However, the "c" method will take longer and may be prone to overfitting.
#' @param hr.weight A number between 0 and 1 indicating how much weight to give to maximizing hits versus minimizing false alarms.
#' @param verbose A logical value indicating whether or not to print progress reports.
#' @param cpus integer. Number of cpus to use (any value larger than 1 will initiate parallel calculations in snowfall)
#' @importFrom stats median formula
#' @importFrom graphics text points segments plot
#' @return A dataframe containing best thresholds and marginal classification statistics for each cue
#' @export
#' @examples
#'
#' train.5m <- simFFTrees(formula = diagnosis ~.,
#'                         data = breastcancer,
#'                         train.p = .5,
#'                         sim = 5,
#'                         rank.method = "m",
#'                         cpus = 1)
#'
#'
#'
simFFTrees <- function(formula = NULL,
                       data = NULL,
                       max.levels = 5,
                       sim = 10,
                       train.p = .5,
                       rank.method = "m",
                       hr.weight = .5,
                       verbose = FALSE,
                       cpus = 1
) {

sim.result.df <- data.frame(
  sim = 1:sim,
  cues = rep(NA, sim),
  thresholds = rep(NA, sim)
)

# getsim.fun does one training split and returns tree statistics
getsim.fun <- function(i) {

result.i <- FFTrees::FFTrees(formula = formula,
                    data = data,
                    data.test = NULL,
                    train.p = train.p,
                    max.levels = max.levels,
                    rank.method = rank.method,
                    hr.weight = hr.weight,
                    object = NULL,
                    do.cart = FALSE,
                    do.lr = FALSE)

treestats.i <- result.i$tree.stats

return(treestats.i)

}

if(cpus == 1) {

  result.ls <- lapply(1:nrow(sim.result.df), FUN = function(x) {

    if(verbose) {print(x)}
    return(getsim.fun(x))

    })

}

if(cpus > 1) {

  snowfall::sfInit(parallel = TRUE, cpus = cpus)
  snowfall::sfExport("sim.result.df")
  snowfall::sfLibrary(FFTrees)
  snowfall::sfExport("formula")
  snowfall::sfExport("data")
  snowfall::sfExport("max.levels")
  snowfall::sfExport("train.p")
  snowfall::sfExport("max.levels")
  snowfall::sfExport("rank.method")
  snowfall::sfExport("hr.weight")

  result.ls <- snowfall::sfClusterApplySR(1:nrow(sim.result.df), fun = getsim.fun, perUpdate = 1)
  snowfall::sfStop()

  }

# Append final results to sim.result.df

best.tree.v <- sapply(1:length(result.ls), FUN = function(x) {

  best.tree.i <- which(result.ls[[x]]$train$v == max(result.ls[[x]]$train$v))

  if(length(best.tree.i) > 1) {best.tree.i <- sample(best.tree.i, 1)}

  return(best.tree.i)

})

sim.result.df$cues <- sapply(1:length(result.ls),
                             FUN = function(x) {result.ls[[x]]$train$cues[best.tree.v[x]]})

sim.result.df$thresholds <- sapply(1:length(result.ls),
                             FUN = function(x) {result.ls[[x]]$train$thresholds[best.tree.v[x]]})

sim.result.df$train.v <- sapply(1:length(result.ls),
                                   FUN = function(x) {result.ls[[x]]$train$v[best.tree.v[x]]})

sim.result.df$train.hr <- sapply(1:length(result.ls),
                                FUN = function(x) {result.ls[[x]]$train$hr[best.tree.v[x]]})

sim.result.df$train.far <- sapply(1:length(result.ls),
                                 FUN = function(x) {result.ls[[x]]$train$far[best.tree.v[x]]})

sim.result.df$test.v <- sapply(1:length(result.ls),
                                FUN = function(x) {result.ls[[x]]$test$v[best.tree.v[x]]})

sim.result.df$test.hr <- sapply(1:length(result.ls),
                               FUN = function(x) {result.ls[[x]]$test$hr[best.tree.v[x]]})

sim.result.df$test.far <- sapply(1:length(result.ls),
                                FUN = function(x) {result.ls[[x]]$test$hr[best.tree.v[x]]})
output <- sim.result.df


return(output)

}
