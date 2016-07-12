# simfft
#' A wrapper for fft_function that calculates the predictive accuracy of trees for different training set sizes
#'
#' @param train.cue.df A model training dataset. An m x n dataframe containing n cue values for each of the m exemplars.
#' @param train.criterion.v The criterion for training. A logical vector of length m containing criterion values for exemplars in cue.df
#' @param train.p A vector of numbers between 0 and 1 indicating what percentage of the data to use for training.
#' @param sim.n Number of simulations per train.p
#' @param hr.weight A vector of numbers between 0 and 1 indicating how much weight to give to increasing hit rates versus avoiding false alarms. 1 means maximizing HR and ignoring FAR, while 0 does the opposite. The default of 0.5 gives equal weight to both. Different trees will be constructed for each weight in the vector.
#' @param rank.method A string vector indicating how to rank cues during tree construction. "m" (for marginal) means that cues will only be ranked once with the entire training dataset. "c" (conditional) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars.
#' @param numthresh.method A string indicating how to calculate cue splitting thresholds. "m" = median split, "o" = split that maximizes the tree criterion.
#' @param stopping.rule A string indicating the method to stop growing trees. "levels" means the tree grows until a certain level. "exemplars" means the tree grows until a certain number of unclassified exemplars remain. "statdelta" means the tree grows until the change in the tree.criterion statistic is less than a specified level.
#' @param stopping.par A number indicating the parameter for the stopping rule.
#' @param parallel.cpus An integer indicating how many cups to use for parallel calculations using the snowfall package. A value of 0 means no parallel. You may need to explicitly load the snowfall package with library("snowfall") for parallel calculations to work.
#' @param correction A positive number indicating how much to add to classification cells in the case that at least 1 cell is 0.
#' @param max.levels A number indicating the maximum number of tree levels to consder.
#' @param do.lr  A logical value indicating whether to conduct linear regression for model comparison purposes.
#' @param do.cart  A logical value indicating whether to conduct CART for model comparison purposes.
#' @param roc.p A vector of probabilities to plot in the ROC density curve
#' @param verbose A logical value indicating whether or not to print simulation updates
#' @return A list of length 3. The first element "decision.df" is a dataframe with the decisions (and level of decisions) for each exemplar. The second element, "final.df" is a dataframe showing final tree accuracy statistics. The third element "level.df" shows tree accuracy statistics at each level.
#' @export



simfft <- function(
                train.cue.df = NULL,
                train.criterion.v = NULL,
                train.p = c(.5),
                sim.n = 20,
                hr.weight = .5,
                rank.method = "m",
                numthresh.method = "o",
                stopping.rule = "exemplars",
                stopping.par = .1,
                correction = .25,
                max.levels = 4,
                do.lr = T,
                do.cart = T,
                roc.p = .8,
                verbose = T,
                parallel.cpus = 0
) {


# Set some global variables
tree.criterion <- "v"

# Set up design matrix

design.matrix <- expand.grid(train.p = train.p,
                             sim = 1:sim.n,
                             stringsAsFactors = F
                             )

design.matrix[c("hr.train", "far.train", "hr.test", "far.test", "level.class", "level.name", "level.exit", "level.threshold", "level.sigdirection")] <- NA

tree.results <- list()

# Loop over design.matrix and fit trees
#  Put results in a list called tree.results


start.time <- proc.time()[3]


if(parallel.cpus == 0) {

for(i in 1:nrow(design.matrix)) {

current.time <- proc.time()[3]
elapsed.time <- current.time - start.time
percent.complete <- (i - 1) / nrow(design.matrix)
expected.total.time <- elapsed.time / percent.complete
expected.remaining.time <- expected.total.time - elapsed.time

if(verbose) {

print(paste("Sim ", i, " out of ", nrow(design.matrix), ". ", round(elapsed.time/ 60, 0), " min(s) spent, ",round(expected.remaining.time / 60, 0), " min(s) remaining.", sep = ""))

}


train.p.i <- design.matrix$train.p[i]

result.i <- fft(train.cue.df = train.cue.df,
                train.criterion.v = train.criterion.v,
                hr.weight = hr.weight,
                numthresh.method = numthresh.method,
                train.p = train.p.i,
                rank.method = rank.method,
                stopping.rule = stopping.rule,
                stopping.par = stopping.par,
                correction = correction,
                max.levels = max.levels,
                do.lr = do.lr,
                do.cart = do.cart,
                verbose = verbose
                )

tree.results[[i]] <- result.i

}

}

# if(parallel.cpus > 0) {
#
#   cluster.fun <- function(x) {
#
#     fft(train.cue.df = train.cue.df,
#         train.criterion.v = train.criterion.v,
#         test.cue.df = NULL,
#         hr.weight = hr.weight,
#         train.p = design.matrix$train.p[i],
#         numthresh.method = numthresh.method,
#         rank.method = rank.method,
#         stopping.rule = stopping.rule,
#         stopping.par = stopping.par,
#         correction = correction,
#         max.levels = max.levels,
#         do.lr = do.lr,
#         do.cart = do.cart
#     )
#
#   }
#
# snowfall::sfInit(parallel = T, cpus = parallel.cpus)
#
# snowfall::sfLibrary(FFTrees)
# snowfall::sfExportAll()
#
# tree.results <- snowfall::sfLapply(1:nrow(design.matrix),
#                          fun = cluster.fun)
#
# snowfall::sfStop()
#
# }


# Determine train and test HR and FAR for best train tree in each sim

cols.to.add <- c("hr.train", "far.train", "hr.test", "far.test", "level.class", "level.name", "level.exit", "level.threshold", "level.sigdirection")
design.matrix[cols.to.add] <- NA

for(i in 1:nrow(design.matrix)) {

tree.stats.i <- tree.results[[i]]$trees

best.train.tree.i <- which(tree.stats.i$v.train == max(tree.stats.i$v.train))
best.train.tree.i <- sample(best.train.tree.i, 1)

design.matrix[i, cols.to.add] <- tree.stats.i[best.train.tree.i, cols.to.add]

design.matrix$lr.hr.train[i] <- tree.results[[i]]$lr$hr.train[tree.results[[i]]$lr$threshold == .5]
design.matrix$lr.far.train[i] <- tree.results[[i]]$lr$far.train[tree.results[[i]]$lr$threshold == .5]

design.matrix$lr.hr.test[i] <- tree.results[[i]]$lr$hr.test[tree.results[[i]]$lr$threshold == .5]
design.matrix$lr.far.test[i] <- tree.results[[i]]$lr$far.test[tree.results[[i]]$lr$threshold == .5]

design.matrix$cart.hr.train[i] <- tree.results[[i]]$cart$hr.train[tree.results[[i]]$cart$miss.cost == tree.results[[i]]$cart$fa.cost]
design.matrix$cart.far.train[i] <- tree.results[[i]]$cart$far.train[tree.results[[i]]$cart$miss.cost == tree.results[[i]]$cart$fa.cost]

design.matrix$cart.hr.test[i] <- tree.results[[i]]$cart$hr.test[tree.results[[i]]$cart$miss.cost == tree.results[[i]]$cart$fa.cost]
design.matrix$cart.far.test[i] <- tree.results[[i]]$cart$far.test[tree.results[[i]]$cart$miss.cost == tree.results[[i]]$cart$fa.cost]

}

names(design.matrix)[which(names(design.matrix) %in% cols.to.add)] <- paste("fft.", cols.to.add, sep = "")

output <- design.matrix

return(output)

}


