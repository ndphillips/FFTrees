#' Creates a forest of fast and frugal decision trees
#'
#' This function is currently in development. The idea is to generate a random forest of fast and frugal trees from many splits of the training dataset.
#'
#' @param formula formula. A formula specifying a binary criterion as a function of multiple variables
#' @param data dataframe. A dataframe containing variables in formula
#' @param data.test dataframe. An optional dataframe of test data
#' @param max.levels integer. Maximum number of levels considered for the trees.
#' @param ntree integer. Number of trees to create.
#' @param train.p numeric. What percentage of the data should be used to fit each tree? Smaller values will result in more diverse trees.
#' @param algorithm string. How to rank cues during tree construction. "m" (for marginal) means that cues will only be ranked once with the entire training dataset. "c" (conditional) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars. This also means that the same cue can be used multiple times in the trees. Note that the "c" method will take (much) longer and may be prone to overfitting.
#' @param goal character. A string indicating the statistic to maximize: "acc" = overall accuracy, "bacc" = balanced accuracy, "d" = d-prime
#' @param sens.weight numeric. How much weight to give to maximizing hits versus minimizing false alarms (between 0 and 1)
#' @param verbose logical. Should progress reports be printed?
#' @param cpus integer. Number of cpus to use. Any value larger than 1 will initiate parallel calculations in snowfall.
#' @param do.lr,do.cart,do.rf,do.svm logical. Should logistic regression, cart, regularized logistic regression, random forests and/or support vector machines be calculated for comparison?
#' @param rank.method,hr.weight depricated arguments
#' @importFrom stats  formula
#' @return An object of class \code{FFForest} with the following elements...
#' @export
#' @examples
#'
#'\dontrun{
#' cancer.fff <- FFForest(formula = diagnosis ~.,
#'                      data = breastcancer,
#'                      ntree = 10,
#'                      cpus = 1)
#'}
#'
#'
FFForest <- function(formula = NULL,
                     data = NULL,
                     data.test = NULL,
                     max.levels = 5,
                     ntree = 10,
                     train.p = .5,
                     algorithm = "m",
                     goal = "bacc",
                     sens.weight = .5,
                     verbose = TRUE,
                     cpus = 1,
                     do.lr = TRUE,
                     do.cart = TRUE,
                     do.rf = TRUE,
                     do.svm = TRUE,
                     rank.method = NULL,
                     hr.weight = NULL
) {
#
#
#   formula = diagnosis ~.
#   data = heartdisease
#   max.levels = 5
#   ntree = 100
#   train.p = .5
#   algorithm = "m"
#   goal = "bacc"
#   sens.weight = .5
#   verbose = TRUE
#   cpus = 4
#   do.lr = TRUE
#   do.cart = TRUE
#   do.rf = TRUE
#   do.svm = TRUE
#   rank.method = NULL
#   hr.weight = NULL
#


# Check for depricated arguments

if(is.null(rank.method) == FALSE) {

  warning("The argument rank.method is depricated. Use algorithm instead.")

  algorithm <- rank.method

}
if(is.null(hr.weight) == FALSE) {

    warning("The argument hr.weight is depricated. Use sens.weight instead.")

    sens.weight <- hr.weight

  }

data.mf <- model.frame(formula = formula,
                       data = data)


if(is.null(data.test) == FALSE) {

  data.mf.test <- model.frame(formula = formula, data = data.test)

}

criterion.v <- data.mf[,1]

simulations <- data.frame(
  sim = 1:ntree,
  cues = rep(NA, ntree),
  thresholds = rep(NA, ntree)
)

# getsim.fun does one training split and returns tree statistics
getsim.fun <- function(i) {

result.i <- FFTrees::FFTrees(formula = formula,
                              data = data,
                              data.test = NULL,
                              object = NULL,
                              train.p = train.p,
                              max.levels = max.levels,
                              algorithm = algorithm,
                              goal = goal,
                              sens.weight = sens.weight,
                              do.cart = do.cart,
                              do.lr = do.lr,
                              do.rf = do.rf,
                              do.svm = do.svm)

decisions.i <- predict(result.i, data)

tree.stats.i <- result.i$tree.stats
comp.stats.i <- c()

if(do.lr) {
lr.stats.i <- result.i$comp$lr$stats
names(lr.stats.i) <- paste0("lr.", names(lr.stats.i))
comp.stats.i <- c(comp.stats.i, lr.stats.i)
}



if(do.cart) {
  cart.stats.i <- result.i$comp$cart$stats
  names(cart.stats.i) <- paste0("cart.", names(cart.stats.i))
  comp.stats.i <- c(comp.stats.i, cart.stats.i)
  }

if(do.rf) {
  rf.stats.i <- result.i$comp$rf$stats
  names(rf.stats.i) <- paste0("rf.", names(rf.stats.i))
  comp.stats.i <- c(comp.stats.i, rf.stats.i)
}

if(do.svm) {
  svm.stats.i <- result.i$comp$svm$stats
  names(svm.stats.i) <- paste0("svm.", names(svm.stats.i))
  comp.stats.i <- c(comp.stats.i, svm.stats.i)
}

comp.stats.i <- unlist(comp.stats.i)

return(list("trees" = tree.stats.i,
            "decisions" = decisions.i,
            "competitors" = comp.stats.i
            ))

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
  snowfall::sfExport("goal")
  snowfall::sfExport("data")
  snowfall::sfExport("max.levels")
  snowfall::sfExport("train.p")
  snowfall::sfExport("do.lr")
  snowfall::sfExport("do.rf")
  snowfall::sfExport("do.cart")
  snowfall::sfExport("do.svm")
  snowfall::sfExport("max.levels")
  snowfall::sfExport("algorithm")
  snowfall::sfExport("sens.weight")

  result.ls <- snowfall::sfClusterApplySR(1:nrow(simulations), fun = getsim.fun, perUpdate = 1)
  suppressMessages(snowfall::sfStop())

  }

# Append final results to simulations

best.tree.v <- sapply(1:length(result.ls), FUN = function(i) {

  best.tree.i <- which(result.ls[[i]]$trees$train[[goal]] == max(result.ls[[i]]$trees$train[[goal]]))

  if(length(best.tree.i) > 1) {best.tree.i <- sample(best.tree.i, 1)}

  return(best.tree.i)

})

decisions <- matrix(unlist(lapply(1:length(result.ls), FUN = function(i) {

  return(result.ls[[i]]$decisions)

})), nrow = nrow(data), ncol = ntree)

# Tree definitions

for(stat.i in c("cues", "thresholds", "directions", "classes", "exits")) {

  simulations[[stat.i]] <- sapply(1:length(result.ls),
                                  FUN = function(x) {

                                    result.ls[[x]]$trees$train[[stat.i]][best.tree.v[x]]})

}


# Train stats

for(stat.i in c("n", "hi", "mi", "fa", "sens", "spec", "acc", "bacc", "dprime", "pci", "mcu")) {


  simulations[[paste0(stat.i, ".train")]] <- sapply(1:length(result.ls),
                                  FUN = function(x) {

                                    result.ls[[x]]$trees$train[[stat.i]][best.tree.v[x]]})

  simulations[[paste0(stat.i, ".test")]] <- sapply(1:length(result.ls),
                                                    FUN = function(x) {

                                                      result.ls[[x]]$trees$test[[stat.i]][best.tree.v[x]]})

}



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

# Get training performance
FFForest.Train.Decisions <- rowMeans(decisions) >= .5

train.stats <- classtable(FFForest.Train.Decisions, criterion.v)

# Get testing performance
if(is.null(data.test) == FALSE) {

FFForest.Test.Decisions <- sapply(1:nrow(simulations), FUN = function(x) {

  pred <- apply.tree(data = data.test,
                     formula = formula,
                     tree.definitions = simulations[x,])$decision


})

FFForest.Test.Decisions <- rowMeans(FFForest.Test.Decisions) >= .5

test.stats <- classtable(prediction.v = FFForest.Test.Decisions,
                         criterion.v = data.mf.test[,1])

}
if(is.null(data.test)) {test.stats <- NULL}

forest.stats <- list(train = train.stats, test = test.stats)


# Get single surrogate tree with highest agreement with FFForest
{
FFForest.Decisions <- rowMeans(decisions) >= .5

# Get agreement
criterion <- data.mf[,1]
FFForest.Decisions.neg <- FFForest.Decisions[criterion == 0]
FFForest.Decisions.pos <- FFForest.Decisions[criterion == 1]

agreement <- matrix(as.numeric(decisions) == rep(FFForest.Decisions, times = ncol(decisions)),
                    nrow = nrow(decisions), ncol = ncol(decisions))

agree.pos <- colMeans(agreement[criterion == 1,])
agree.neg <- colMeans(agreement[criterion == 0,])

agree.combined <- (agree.pos + agree.neg) / 2

surrogate.tree <- which(agree.combined == max(agree.combined))

if(length(surrogate.tree) > 1) {surrogate.tree <- sample(surrogate.tree, size = 1)}

surrogate.tree.definition <- simulations[surrogate.tree, c("cues", "thresholds", "directions", "classes", "exits")]
surrogate.tree.definition$tree <- 1

# Create new FFTrees object from surrogate tree

surrogate.FFTrees <- FFTrees(formula = formula,
                             data = data,
                             data.test = data.test,
                             tree.definitions = surrogate.tree.definition)
}

# Get competition results
competitors <- as.data.frame(t(sapply(1:length(result.ls), FUN = function(x) {result.ls[[x]]$competitors})))

if(do.lr) {

  lr.sim <- competitors[,grepl("lr", names(competitors))]
  names(lr.sim) <- gsub("lr.", replacement = "", x = names(lr.sim))

} else {lr.sim <- NULL}



if(do.cart) {

  cart.sim <- competitors[,grepl("cart", names(competitors))]
  names(cart.sim) <- gsub("cart.", replacement = "", x = names(cart.sim))

} else {cart.sim <- NULL}


if(do.rf) {

  rf.sim <- competitors[,grepl("rf", names(competitors))]
  names(rf.sim) <- gsub("rf.", replacement = "", x = names(rf.sim))

} else {rf.sim <- NULL}

if(do.svm) {

  svm.sim <- competitors[,grepl("svm", names(competitors))]
  names(svm.sim) <- gsub("svm.", replacement = "", x = names(svm.sim))

} else {svm.sim <- NULL}


# Summarise output

output <-list("formula" = formula,
              "tree.sim" = simulations,
              "decisions" = decisions,
              "frequencies" = frequencies,
              "connections" = connections,
              "surrogate" = surrogate.FFTrees,
              "forest.stats" = forest.stats,
              "lr.sim" = lr.sim,
              "cart.sim" = cart.sim,
              "rf.sim" = rf.sim,
              "svm.sim" = svm.sim,
              "params" = list("train.p" = train.p,
                              "ntree" = ntree,
                              "max.levels" = max.levels,
                              "algorithm" = algorithm,
                              "goal" = goal,
                              "sens.weight" = sens.weight))

class(output) <- "FFForest"

return(output)

}
