#' Create Fast and Frugal Trees (FFTrees).
#'
#' @param formula A formula
#' @param data A model training dataset. An m x n dataframe containing n cue values for each of the m exemplars.
#' @param data.test (Optional) A model testing dataset (same format as data.train)
#' @param max.levels A number indicating the maximum number of levels considered for the tree.
#' @param train.p A number between 0 and 1 indicating what percentage of the data to use for training. This only applies when data.test and crit.test are not specified by the user.
#' @param n.sim An integer
#' @param hr.weight A number between 0 and 1 indicating how much weight to give to increasing hit rates versus avoiding false alarms. 1 means maximizing HR and ignoring FAR, while 0 does the opposite. The default of 0.5 gives equal weight to both. Different trees will be constructed for each weight in the vector.
#' @param rank.method A string indicating how to rank cues during tree construction. "m" (for marginal) means that cues will only be ranked once with the entire training dataset. "c" (conditional) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars.
#' @param verbose A logical value indicating whether or not to print progress reports. Can be helpful for diagnosis when the function is running slow...
#' @importFrom stats anova predict glm as.formula formula
#' @return A list of length 3. The first element "tree.acc" is a dataframe containing the final statistics of all trees. The second element "cue.accuracies" shows the accuracies of all cues. The third element "tree.class.ls" is a list with n.trees elements, where each element shows the final decisions for each tree for each exemplar.
#' @export
#'

fft <- function(
                formula = NULL,
                data = NULL,
                data.test = NULL,
                train.p = 1,
                n.sim = 1,
                rank.method = "m",
                verbose = F,
                max.levels = 4,
                hr.weight = .5
) {
#
  # formula = diagnosis ~.
  # data = heartdisease
  # data.test = NULL
  # train.p <- 1
  # n.sim <- 1
  # rank.method <- "m"
  # max.levels <- 4
  # verbose <- F

  tree.criterion <- "v"
  stopping.rule <- "exemplars"
  stopping.par <- .1
  correction <- .25
  do.lr <- T
  do.cart <- T
  rounding <- 0
  numthresh.method <- "o"
  rounding <- 2



# Set some global parameters

exit.method <- "fixed"
n.cues <- ncol(data)
max.levels <- min(max.levels, ncol(data) - 1)



# Check for missing or bad inputs

if(is.null(data) |
   "data.frame" %in% class(data) == F) {

  stop("Please specify a valid dataframe object in data")

}

## SETUP
{


# Save original data.train and data.test
data.train.o <- data
data.train <- model.frame(formula = formula,
                          data = data)
cue.train <- data.train[,2:ncol(data.train)]
crit.train <- data.train[,1]
crit.name <- names(data.train)[1]
cue.names <- names(cue.train)

if(is.null(data.test) == F) {

  data.test.o <- data.test
  data.test <- model.frame(formula = formula,
                          data = data.test)
  cue.test <- data.test[,2:ncol(data.test)]
  crit.test <- data.test[,1]

}


# Check for non-binary dv

if(setequal(unique(crit.train), c(0, 1)) == F) {

  stop("Warning! Your DV is not binary. Convert to binary (0, 1)")

}

}

## Create trees
{

if(train.p < 1) {

# create 'training testing' sets of size train.p * nrow(data.train)

n.original.exemplars <- nrow(data.train)

# test.cases.df, train.cases.df
train.cases.df <- matrix(NA, nrow = n.original.exemplars, ncol = n.sim)
test.cases.df <- matrix(NA, nrow = n.original.exemplars, ncol = n.sim)

for(test.data.i in 1:n.sim) {

continue <- T

# Create train and test set (and make sure training criterion has at least one positive value)
while(continue) {

  n.train <- floor(train.p * n.original.exemplars)
  train.exemplars.i <- sample(1:n.original.exemplars, size = n.train)
  test.exemplars.i <- setdiff(1:n.original.exemplars, train.exemplars.i)
  crit.train.i <- crit.all[train.exemplars.i]
  crit.test.i <- crit.all[test.exemplars.i]

  if(mean(crit.train.i) > 0 & mean(crit.train.i) < 1 & mean(crit.test.i > 0) & mean(crit.test.i < 1)) {continue <- F}

}

test.exemplars.i.num <- rep(0, n.original.exemplars)
test.exemplars.i.num[train.exemplars.i] <- 1

test.cases.df[,test.data.i] <- test.exemplars.i.num
train.cases.df[,test.data.i] <- test.exemplars.i.num == 0


}


}


if(train.p == 1) {

  n.original.exemplars <- nrow(data.train)
  cue.all <- cue.train
  crit.all <- crit.train
  data.all <- data.train
  train.cases.df <- matrix(1, nrow = nrow(data.all), ncol = 1)
  test.cases.df <- matrix(1, nrow = nrow(data.all), ncol = 1)
  can.test.df <- matrix(1, nrow = nrow(data.all), ncol = 1)
  n.sim <- 1

}

}

# LOOP OVER TRAINING TEST SETS TO DETERMINE BEST CUES
{
test.result.ls <- list()

for(i in 1:n.sim) {

  if(verbose) {print(paste("Simulation", i, "of", n.sim))}

  training.ex <- train.cases.df[,i] == 1
  testing.ex <- test.cases.df[,i] == 1

 test.result.ls[[i]] <-  grow.ffts(
                                  formula = formula,
                                  data.train = data[training.ex,],
                                  data.test = data[testing.ex,],
                                  hr.weight = hr.weight,
                                  rank.method = rank.method,
                                  numthresh.method = numthresh.method,
                                  stopping.rule = stopping.rule,
                                  stopping.par = stopping.par,
                                  max.levels = max.levels
 )

}

# Get most common cues used across training sets

cue.freq <- unlist(lapply(1:length(test.result.ls), FUN = function(x) {test.result.ls[[x]]$trees$level.name}))
cue.freq <- paste(cue.freq, collapse = ";")
cue.freq <- unlist(strsplit(cue.freq, split = "[;]"))
cue.freq.table <- sort(table(cue.freq), decreasing = T)

common.cues <- names(cue.freq.table)
}

# CREATE FINAL TREE
{
# Now create a new tree based on all data using common cues from simulation

# Only include 1:max.levels common cues

if(length(common.cues) > max.levels) {common.cues <- common.cues[1:max.levels]}


common.cue.formula <- paste(common.cues, collapse = " + ")
common.cue.formula <- as.formula(paste(crit.name, " ~ ", common.cue.formula, collapse = ""))


data.train.final <- model.frame(formula = common.cue.formula,
                          data = data.train.o)

cue.train.final <- data.train.final[,2:ncol(data.train.final)]
crit.train.final <- data.train.final[,1]


# Determine final testing data

if(is.null(data.test)) {

  cue.test <- NULL
  crit.test <- NULL

}

if(is.null(data.test) == F) {

  cue.test <- data.test.o[,common.cues]
  crit.test <- crit.test

}


final.result <- grow.ffts(
                         formula = formula,
                         data.train = data.train.final,
                         data.test = data.test,
                         hr.weight = hr.weight,
                         rank.method = rank.method,
                         numthresh.method = numthresh.method,
                         stopping.rule = stopping.rule,
                         stopping.par = stopping.par,
                         max.levels = max.levels
)
}


# Calculate original cue accuracies

cue.accuracies.train <- cuerank(cue.df = cue.train,
                                   criterion.v = crit.train,
                                   hr.weight = hr.weight,
                                   tree.criterion = tree.criterion,
                                   numthresh.method = numthresh.method,
                                   rounding = rounding,
                                   verbose = verbose
)


# Calculate LR predictions

lr.acc <- lr.pred(formula = formula,
                  data.train = data,
                  data.test = data.test
                  )

# Calculate CART predictions
cart.acc <- cart.pred(formula = formula,
                      data.train = data,
                      data.test = data.test)


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

output.fft <- list(
                  "formula" = formula,
                  "data.train" = data,
                  "data.test" = data.test,
                  "cue.accuracies" = cue.accuracies.train,
                  "trees" = final.result$trees,
                  "trees.auc" = final.result$auc,
                  "cart" = cart.acc,
                  "lr" = lr.acc,
                  "decision.train" = final.result$decision.train,
                  "decision.test" = final.result$decision.test,
                  "levelout.train" = final.result$levelout.train,
                  "levelout.test" = final.result$levelout.test
)


class(output.fft) <- "fft"

  return(output.fft)

}


