#' Generates and summarises many FFTrees objects from multiple training sets
#'
#' @param formula formula. A formula specifying a binary criterion as a function of multiple variables
#' @param data dataframe. A dataframe containing variables in formula
#' @param max.levels integer. Maximum number of levels considered for the trees.
#' @param sim integer. Number of simulations to perform.
#' @param train.p numeric. What percentage of the data to use for training in simulations.
#' @param rank.method string. How to rank cues during tree construction. "m" (for marginal) means that cues will only be ranked once with the entire training dataset. "c" (conditional) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars. This also means that the same cue can be used multiple times in the trees. Note that the "c" method will take (much) longer and may be prone to overfitting.
#' @param hr.weight numeric. How much weight to give to maximizing hits versus minimizing false alarms (between 0 and 1)
#' @param verbose logical. Should progress reports be printed?
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
                       verbose = TRUE,
                       cpus = 1
) {

simulations <- data.frame(
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

  result.ls <- lapply(1:nrow(simulations), FUN = function(x) {

    if(verbose) {print(paste0(x, " of ", nrow(simulations)))}
    return(getsim.fun(x))

    })

}

if(cpus > 1) {

  suppressMessages(snowfall::sfInit(parallel = TRUE, cpus = cpus))
  snowfall::sfExport("simulations")
  snowfall::sfLibrary(FFTrees)
  snowfall::sfExport("formula")
  snowfall::sfExport("data")
  snowfall::sfExport("max.levels")
  snowfall::sfExport("train.p")
  snowfall::sfExport("max.levels")
  snowfall::sfExport("rank.method")
  snowfall::sfExport("hr.weight")

  result.ls <- snowfall::sfClusterApplySR(1:nrow(simulations), fun = getsim.fun, perUpdate = 1)
  suppressMessages(snowfall::sfStop())

  }

# Append final results to simulations

best.tree.v <- sapply(1:length(result.ls), FUN = function(x) {

  best.tree.i <- which(result.ls[[x]]$train$v == max(result.ls[[x]]$train$v))

  if(length(best.tree.i) > 1) {best.tree.i <- sample(best.tree.i, 1)}

  return(best.tree.i)

})

simulations$cues <- sapply(1:length(result.ls),
                             FUN = function(x) {result.ls[[x]]$train$cues[best.tree.v[x]]})

simulations$thresholds <- sapply(1:length(result.ls),
                             FUN = function(x) {result.ls[[x]]$train$thresholds[best.tree.v[x]]})

simulations$train.hr <- sapply(1:length(result.ls),
                                FUN = function(x) {result.ls[[x]]$train$hr[best.tree.v[x]]})

simulations$train.far <- sapply(1:length(result.ls),
                                 FUN = function(x) {result.ls[[x]]$train$far[best.tree.v[x]]})

simulations$train.v <- sapply(1:length(result.ls),
                              FUN = function(x) {result.ls[[x]]$train$v[best.tree.v[x]]})

simulations$train.dprime <- qnorm(simulations$train.hr) - qnorm(simulations$train.far)


simulations$test.hr <- sapply(1:length(result.ls),
                               FUN = function(x) {result.ls[[x]]$test$hr[best.tree.v[x]]})

simulations$test.far <- sapply(1:length(result.ls),
                                FUN = function(x) {result.ls[[x]]$test$far[best.tree.v[x]]})

simulations$test.v <- sapply(1:length(result.ls),
                             FUN = function(x) {result.ls[[x]]$test$v[best.tree.v[x]]})

simulations$test.dprime <- qnorm(simulations$test.hr) - qnorm(simulations$test.far)

# Get overall cue frequencies

frequencies <- table(unlist(strsplit(simulations$cues, ";")))

# Get connections from simulation
{
## get unique values

unique.cues <- unique(unlist(strsplit(simulations$cues, ";")))

connections <- expand.grid("cue1" = unique.cues,
                        "cue2" = unique.cues,
                        stringsAsFactors = FALSE)

for(i in 1:nrow(connections)) {

  N <- sum(sapply(1:length(simulations$cues), FUN = function(x) {

    connections[i, 1] %in% unlist(strsplit(simulations$cues[x], ";")) &
    connections[i, 2] %in% unlist(strsplit(simulations$cues[x], ";"))

  }))

  connections$N[i] <- N

}
}

return(list("simulations" = simulations,
            "frequencies" = frequencies,
            "connections" = connections
            ))

}
