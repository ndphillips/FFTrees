#' Create Fast and Frugal Trees (FFTrees)
#'
#' @param formula A formula
#' @param data A model training dataset. An m x n dataframe containing n cue values for each of the m exemplars.
#' @param data.test (Optional) A model testing dataset (same format as data.train)
#' @param max.levels A number indicating the maximum number of levels considered for the tree.
#' @param train.p A number between 0 and 1 indicating what percentage of the data to use for training. This only applies when data.test is not specified by the user.
#' @param rank.method A string indicating how to rank cues during tree construction. "m" (for marginal) means that cues will only be ranked once with the entire training dataset. "c" (conditional) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars.
#' @param do.cart,do.lr logical values indicating whether or not to evaluate logistic regression and/or CART on the data for comparison.
#' @param verbose A logical value indicating whether or not to print progress reports. Can be helpful for diagnosis when the function is running slowly...
#' @importFrom stats anova predict glm as.formula formula
#' @return A list of length 3. The first element "tree.acc" is a dataframe containing the final statistics of all trees. The second element "cue.accuracies" shows the accuracies of all cues. The third element "tree.class.ls" is a list with n.trees elements, where each element shows the final decisions for each tree for each exemplar.
#' @export
#'

FFTrees <- function(
                formula = NULL,
                data = NULL,
                data.test = NULL,
                train.p = 1,
                rank.method = "m",
                verbose = F,
                max.levels = 4,
                do.cart = T,
                do.lr = T
) {
#
  # formula = diagnosis ~ thickness + chromatin
  # data = breastcancer
  # data.test <- NULL
  # rank.method = "m"
  # max.levels = 4
  # verbose = T
  # hr.weight = .5
  # do.cart = T
  # do.lr = T
  # train.p <- 1

# Set some global parameters

tree.criterion <- "v"
stopping.rule <- "exemplars"
stopping.par <- .1
correction <- .25
numthresh.method <- "o"
rounding <- 2
exit.method <- "fixed"


n.cues <- ncol(data)
max.levels <- min(max.levels, ncol(data) - 1)



# Check for missing or bad inputs

if(is.null(data) |
   "data.frame" %in% class(data) == F) {

  stop("Please specify a valid dataframe object in data")

}

# Convert factors to strings

for(i in 1:ncol(data)) {

  x.i <- data[,i]

  if("factor" %in% class(x.i)) {data[,i] <- paste(x.i)}

}

# Remove non-formula terms from training data
data <- model.frame(formula = formula,
                    data = data)

crit.o <- data[,1]
crit.name <- names(data)[1]
cue.names <- names(data)[2:ncol(data)]

## SETUP: DEFINE TRAINING AND TEST DATASETS
#  data.train, cue.train, crit.train, data.test, cue.test, crit.test

if(is.null(data.test) == F) {

  # TRAINING DATA

  data.train <- data
  cue.train <- data.train[,2:ncol(data.train)]
  crit.train <- data.train[,1]

  # TESTING DATA

  data.test <- model.frame(formula = formula,
                          data = data.test)
  cue.test <- data.test[,2:ncol(data.test)]
  crit.test <- data.test[,1]

}
if(is.null(data.test) & train.p < 1) {

# create 'training testing' sets of size train.p * nrow(data.train)

  continue <- T
  run.i <- 0

# Create train and test set

# Test set must not have new factor values
# training set must have at least one positive and one negative case

  while(continue) {

    run.i <- run.i + 1

    n.train <- floor(train.p * nrow(data))
    train.exemplars.i <- sample(1:nrow(data), size = n.train)
    test.exemplars.i <- setdiff(1:nrow(data), train.exemplars.i)
    crit.train.i <- data[train.exemplars.i, 1]
    crit.test.i <- data[test.exemplars.i, 1]

    data.train.i <- data[train.exemplars.i,]
    data.test.i <- data[test.exemplars.i,]

    orig.vals.ls <- lapply(1:ncol(data.train.i), FUN = function(x) {unique(data.train.i[,x])})

    can.predict.mtx <- matrix(1, nrow = nrow(data.test.i), ncol = ncol(data.test.i))

    for(i in 1:ncol(can.predict.mtx)) {

      test.vals.i <- data.test.i[,i]

      if(is.numeric(test.vals.i)) {
        can.predict.mtx[,i] <- 1} else {

          can.predict.mtx[,i] <- paste(test.vals.i) %in% paste(orig.vals.ls[[i]])


        }
    }

    model.can.predict <- rowMeans(can.predict.mtx) == 1


    if(mean(crit.train.i) > 0 & mean(crit.train.i) < 1 &
       mean(crit.test.i > 0) & mean(crit.test.i < 1) & all(model.can.predict)) {continue <- F}

    if(run.i == 20) {print("I'm having a hard time coming up with valid training and test datasets...You may need to stop the processor and create them manually")}

  }

data.train <- data[train.exemplars.i,]
cue.train <- data.train[,2:ncol(data.train)]
crit.train <- data.train[,1]

data.test <- data[test.exemplars.i,]
cue.test <- data.test[,2:ncol(data.test)]
crit.test <- data.test[,1]

}

if(is.null(data.test) & train.p == 1) {

  data.train <- data
  cue.train <- data[,2:ncol(data)]
  crit.train <- data[,1]

  data.test <- NULL
  cue.test <- NULL
  crit.test <- NULL

}

## VALIDITY CHECKS
{
# Non-binary DV
if(setequal(unique(data[,1]), c(0, 1)) == F) {

  stop("Warning! Your DV is not binary. Convert to binary (0, 1)")

}

}

# OLD

# LOOP OVER TRAINING TEST SETS TO DETERMINE COMMON CUES
# {
# test.result.ls <- list()
#
#
#   {print("Growing trees!")}
#
#   if(do.simulation == T) {
#
#   training.ex <- train.cases.df[,1] == 1
#   testing.ex <- test.cases.df[,1] == 1
#
#  test.result.ls[[1]] <-  grow.FFTrees(
#                             formula = formula,
#                             data.train = data[training.ex,],
#                             data.test = data[testing.ex,],
#                             hr.weight = hr.weight,
#                             rank.method = rank.method,
#                             numthresh.method = numthresh.method,
#                             stopping.rule = stopping.rule,
#                             stopping.par = stopping.par,
#                             max.levels = max.levels
#  )
#
#   }
#
# # Get most common cues used across training sets
#
# cue.freq <- unlist(lapply(1:length(test.result.ls), FUN = function(x) {
#
#   all <- test.result.ls[[x]]$trees$level.name
#
#   return(all[nchar(all) == max(nchar(all))][1])
#
#
#   }))
#
# cue.freq <- paste(cue.freq, collapse = ";")
# cue.freq <- unlist(strsplit(cue.freq, split = "[;]"))
# cue.freq.table <- sort(table(cue.freq), decreasing = T)
#
# common.cues <- names(cue.freq.table)
#
# }

# CREATE FINAL TREE

# Now create a new tree based on all data using common cues from simulation

# Only include 1:max.levels common cues
#
# if(length(common.cues) > max.levels) {common.cues <- common.cues[1:max.levels]}
#
#
# common.cue.formula <- paste(common.cues, collapse = " + ")
# common.cue.formula <- as.formula(paste(crit.name, " ~ ", common.cue.formula, collapse = ""))



# CALCULATE CUE ACCURACIES

cue.accuracies.train <- cuerank(cue.df = cue.train,
                                criterion.v = crit.train,
                                tree.criterion = tree.criterion,
                                numthresh.method = numthresh.method,
                                rounding = rounding,
                                verbose = verbose
)

stat.names <- c("hi", "mi", "fa", "cr", "hr", "far", "v", "dprime")

names(cue.accuracies.train)[names(cue.accuracies.train) %in% stat.names] <- paste(stat.names, ".train", sep = "")


if(is.null(data.test) == F) {

cue.accuracies.test <- cuerank(cue.df = cue.test,
                                criterion.v = crit.test,
                                tree.criterion = tree.criterion,
                                numthresh.method = numthresh.method,
                                rounding = rounding,
                                verbose = verbose
)

cue.accuracies.test <- cue.accuracies.test[,names(cue.accuracies.test) %in% c("cue.name", stat.names)]

names(cue.accuracies.test)[names(cue.accuracies.test) %in% stat.names] <- paste(stat.names, ".test", sep = "")

}

if(is.null(data.test)) {

cue.accuracies.test <- as.data.frame(matrix(NA, nrow = nrow(cue.accuracies.train), ncol = length(stat.names)))
names(cue.accuracies.test) <- paste(stat.names, ".test", sep = "")
cue.accuracies.test <- cbind(cue.accuracies.train[,1], cue.accuracies.test)
names(cue.accuracies.test)[1] <- "cue.name"

}

cue.accuracies <- merge(cue.accuracies.train, cue.accuracies.test)



# GROW THE TREES!
{
final.result <- grow.FFTrees(
                         formula = formula,
                         data.train = data.train,
                         data.test = data.test,
                         rank.method = rank.method,
                         numthresh.method = numthresh.method,
                         stopping.rule = stopping.rule,
                         stopping.par = stopping.par,
                         max.levels = max.levels
)


# Sort trees by far.train

n.trees <- nrow(final.result$trees)

final.tree.order <- order(final.result$trees$far.train)

final.result$trees <- final.result$trees[final.tree.order,]
final.result$decision.train <- final.result$decision.train[,final.tree.order]
final.result$levelout.train <- final.result$levelout.train[,final.tree.order]

final.result$trees$tree.num <- 1:n.trees
names(final.result$decision.train) <- paste("tree", 1:n.trees, sep = ".")
names(final.result$levelout.train) <- paste("tree", 1:n.trees, sep = ".")

if(is.null(final.result$decision.test) == F) {

  final.result$decision.test <- final.result$decision.test[,final.tree.order]
  final.result$levelout.test <- final.result$levelout.test[,final.tree.order]

  names(final.result$decision.test) <- paste("tree", 1:n.trees, sep = ".")
  names(final.result$levelout.test) <- paste("tree", 1:n.trees, sep = ".")

}


tree.stats <- final.result$trees
fft.auc <- final.result$auc


}

# LR
{
if(do.lr) {

lr.acc <- lr.pred(formula = formula,
               data.train = data.train,
               data.test = data.test
               )


lr.stats <- lr.acc$accuracy
lr.auc <- lr.acc$auc
lr.model <- lr.acc$model

}

if(do.lr == F) {

lr.acc <- NULL; lr.stats <- NULL ; lr.model <- NULL

lr.auc <- matrix(NA, nrow = 2, ncol =1)
rownames(lr.auc) <- c("train", "test")
colnames(lr.auc) <- "lr"

}

}

# CART
{

if(do.cart) {

cart.acc <- cart.pred(formula = formula,
                      data.train = data.train,
                      data.test = data.test
)

cart.stats <- cart.acc$accuracy
cart.auc <- cart.acc$auc
cart.model <- cart.acc$model

}

if(do.cart == F) {

  cart.acc <- NULL ; cart.stats <- NULL ; cart.model <- NULL

  cart.auc <- matrix(NA, nrow = 2, ncol =1)
  rownames(cart.auc) <- c("train", "test")
  colnames(cart.auc) <- "cart"

}
}


# Get AUC matrix

auc <- cbind(fft.auc, lr.auc, cart.auc)

output.fft <- list(
                  "formula" = formula,
                  "data.train" = data.train,
                  "data.test" = data.test,
                  "cue.accuracies" = cue.accuracies,
                  "tree.stats" = tree.stats,
                  "lr.stats" = lr.stats,
                  "cart.stats" = cart.stats,
                  "auc" = auc,
                  "lr.model" = lr.model,
                  "cart.model" = cart.model,
                  "decision.train" = final.result$decision.train,
                  "decision.test" = final.result$decision.test,
                  "levelout.train" = final.result$levelout.train,
                  "levelout.test" = final.result$levelout.test
)

class(output.fft) <- "FFTrees"

  return(output.fft)

}


