#' Create Fast and Frugal Trees (FFTrees).
#'
#' @param train.cue.df A model training dataset. An m x n dataframe containing n cue values for each of the m exemplars.
#' @param train.criterion.v The criterion for training. A logical vector of length m containing criterion values for exemplars in cue.df
#' @param test.cue.df (Optional) A model testing dataset (same format as train.cue.df)
#' @param test.criterion.v (Optional) A model testing criterion
#' @param max.levels A number indicating the maximum number of levels considered for the tree.
#' @param train.p A number between 0 and 1 indicating what percentage of the data to use for training. This only applies when test.cue.df and test.criterion.v are not specified by the user.
#' @param hr.weight A number between 0 and 1 indicating how much weight to give to increasing hit rates versus avoiding false alarms. 1 means maximizing HR and ignoring FAR, while 0 does the opposite. The default of 0.5 gives equal weight to both. Different trees will be constructed for each weight in the vector.
#' @param numthresh.method A string indicating how to calculate cue splitting thresholds. "m" = median split, "o" = split that maximizes the tree criterion.
#' @param rank.method A string indicating how to rank cues during tree construction. "m" (for marginal) means that cues will only be ranked once with the entire training dataset. "c" (conditional) means that cues will be ranked after each level in the tree with the remaining unclassified training exemplars.
#' @param stopping.rule A string indicating the method to stop growing trees. "levels" means the tree grows until a certain level. "exemplars" means the tree grows until a certain number of unclassified exemplars remain. "statdelta" means the tree grows until the change in the tree.criterion statistic is less than a specified level.
#' @param stopping.par A number indicating the parameter for the stopping rule. For stopping.rule == "levels", this is the number of levels. For stopping rule == "exemplars", this is the smallest percentage of examplars allowed in the last level.
#' @param rounding An integer indicating how many digits to round numeric variables to (0 means round to the integer).
#' @param correction A positive number indicating how much to add to classification cells in the case that at least 1 cell is 0. Default is 0.25.
#' @param do.lr, A logical value indicating whether to conduct linear regression for model comparison purposes.
#' @param do.cart A logical value indicating whether to conduct CART for model comparison purposes.
#' @param verbose A logical value indicating whether or not to print progress reports. Can be helpful for diagnosis when the function is running slow...
#' @importFrom stats anova predict glm as.formula
#' @return A list of length 3. The first element "tree.acc" is a dataframe containing the final statistics of all trees. The second element "cue.accuracies" shows the accuracies of all cues. The third element "tree.class.ls" is a list with n.trees elements, where each element shows the final decisions for each tree for each exemplar.
#' @export
#'



fft <- function(train.cue.df = NULL,
                train.criterion.v = NULL,
                test.cue.df = NULL,
                test.criterion.v = NULL,
                hr.weight = .5,
                numthresh.method = "o",
                train.p = .5,
                rank.method = "m",
                stopping.rule = "exemplars",
                stopping.par = .1,
                correction = .25,
                max.levels = 4,
                do.lr = T,
                do.cart = T,
                rounding = 0,
                verbose = F
) {

# Set some global parameters

exit.method <- "fixed"
n.cues <- ncol(train.cue.df)
max.levels <- min(max.levels, ncol(train.cue.df))
tree.criterion <- "v"


# Check for missing or bad inputs

if(is.null(train.cue.df) |
   "data.frame" %in% class(train.cue.df) == F) {

  stop("Please specify a valid dataframe object in train.cue.df")

}

if(is.null(train.cue.df) == F &
   (nrow(train.cue.df) != length(train.criterion.v))) {

  stop("The number of rows in train.cue.df do not match the length of train.criterion.v")

}

if(setequal(unique(train.criterion.v), c(0, 1)) == F) {

  stop("train.criterion.v must be a binary vector containing only 0 and 1 (and at least 1 of each)")

}


# ----------------------------
# SETUP
# Create training and test set
# -----------------------------
{

if(is.null(test.cue.df) == F) {

  full.cue.df <- rbind(train.cue.df, test.cue.df)
  full.criterion.v <- c(train.criterion.v, test.criterion.v)

}

# Only fitting a single full dataset

if(is.null(test.cue.df) & train.p == 1) {

  fitorpred <- "f"

}

if(!is.null(test.cue.df) | train.p < 1) {

  fitorpred <- "p"

  }

create.test.set <- F

if(fitorpred == "p" & is.null(test.cue.df)) {create.test.set <- T}

if(fitorpred == "f") {

  n.train.exemplars <- nrow(train.cue.df)
  train.exemplars <- 1:n.train.exemplars

}

if(fitorpred == "p") {

    if(create.test.set) {

    n.original.exemplars <- nrow(train.cue.df)
    original.train.cue.df <- train.cue.df
    original.train.criterion.v <- train.criterion.v


    continue <- T

  # Create train and test set (and make sure training criterion has at least one positive value)

    while(continue) {

    n.train.exemplars <- floor(train.p * n.original.exemplars)
    train.exemplars <- sample(1:n.original.exemplars, size = n.train.exemplars)
    train.cue.df <- original.train.cue.df[train.exemplars,]
    train.criterion.v <- original.train.criterion.v[train.exemplars]

    n.test.exemplars <- n.original.exemplars - n.train.exemplars
    test.exemplars <- setdiff(1:n.original.exemplars, train.exemplars)
    test.cue.df <- original.train.cue.df[test.exemplars,]
    test.criterion.v <- original.train.criterion.v[test.exemplars]

    if(mean(train.criterion.v) > 0 & mean(train.criterion.v < 1)) {continue <- F}

    }


    }

    if(!create.test.set) {

     n.train.exemplars <- nrow(train.cue.df)
     train.exemplars <- 1:nrow(train.cue.df)

     n.test.exemplars <- nrow(test.cue.df)

    }


}

# -------------
# Determine which test exemplars can be predicted
# That is, which exemplars have factor values that exist in the training set.
# -------------

if(fitorpred == "p") {

  {

    # Get all factor values in training set

    train.cue.values.ls <- vector("list", length = ncol(train.cue.df))

    for(col.i in 1:ncol(train.cue.df)) {

      train.cue.vals <- unlist(unique(train.cue.df[,col.i]))

      train.cue.values.ls[[col.i]] <- train.cue.vals


    }

    # See which test exemplars can be predicted

    model.can.predict <- matrix(NA, nrow = nrow(test.cue.df), ncol = ncol(test.cue.df))

    for(col.i in 1:ncol(test.cue.df)) {

      if(sum(class(test.cue.df[,col.i]) %in% c("numeric", "logical", "integer")) > 0) {model.can.predict[,col.i] <- TRUE}
      if(sum(class(test.cue.df[,col.i]) %in% c("factor", "character")) > 0) {

        test.cue.vals <- paste(unlist(test.cue.df[,col.i]))
        train.cue.vals <- paste(train.cue.values.ls[[col.i]])

        model.can.predict[,col.i] <- test.cue.vals %in% train.cue.vals}

    }

    model.can.predict <- rowMeans(model.can.predict)
    model.can.predict <- model.can.predict == 1

    test.cue.df <- test.cue.df[model.can.predict,]
    test.criterion.v <- test.criterion.v[model.can.predict]

  }
}

}

# ----------
# INITIAL TRAINING CUE ACCURACIES
# ----------
{

if(verbose) {print("Calculating initial cue accuracies...")}

cue.accuracies.train <- cuerank(cue.df = train.cue.df,
                               criterion.v = train.criterion.v,
                               hr.weight = hr.weight,
                               tree.criterion = tree.criterion,
                               numthresh.method = numthresh.method,
                               correction = correction,
                               rounding = rounding,
                               verbose = verbose
                               )
}

# ----------
# GENERATE TREES
# ----------
{

if(verbose) {print("Growing trees..")}


if("fixed" %in% exit.method) {

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

}

n.trees <- nrow(tree.dm)

# Set up decision.df and levelout.df

train.decision.df <- as.data.frame(matrix(NA,
                                          nrow = length(train.criterion.v),
                                          ncol = n.trees))

train.levelout.df <- as.data.frame(matrix(NA,
                                          nrow = length(train.criterion.v),
                                          ncol = n.trees))

names(train.decision.df) <- paste("tree", 1:n.trees, sep = ".")
names(train.levelout.df) <- paste("tree", 1:n.trees, sep = ".")


for(tree.i in 1:n.trees) {

# ----------
# Step 3: Grow tree
# ----------

## Determine exits
if(exit.method == "fixed") {

level.exits.v.i <- unlist(tree.dm[tree.i, grepl("exit.", names(tree.dm))])
n.levels <- length(level.exits.v.i)

}

## Set up placeholders

cue.df.i <- train.cue.df
criterion.v.i <- train.criterion.v
cue.accuracies.original <- cue.accuracies.train
decision.v <- rep(NA, length(train.criterion.v))
levelout.v <- rep(NA, length(train.criterion.v))


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
  remaining.cues.index <- (names(cue.df.i) %in% level.stats$level.name) == F
  remaining.cues <- names(cue.df.i)[remaining.cues.index]

  if(length(remaining.cues) == 1) {

    cue.df.i.temp <- as.data.frame(cue.df.i[remaining.exemplars, remaining.cues])
    names(cue.df.i.temp) <- remaining.cues

  }

  if(length(remaining.cues) > 1) {

    cue.df.i.temp <- cue.df.i[remaining.exemplars, remaining.cues]

  }

  cue.accuracies.current <-  cuerank(cue.df = cue.df.i.temp,
                                     criterion.v = criterion.v.i[remaining.exemplars],
                                     hr.weight = hr.weight,
                                     tree.criterion = tree.criterion,
                                     numthresh.method = numthresh.method,
                                     correction = correction,
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
                                 cue.v = cue.df.i[, new.cue],
                                 cue.class = new.cue.stats$cue.class
)

current.level.classtable <- classtable(prediction.v = current.decisions[is.na(decision.v)],
                           criterion.v = criterion.v.i[is.na(decision.v)],
                           correction = correction,
                           hr.weight = hr.weight
)

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
                       criterion.v = criterion.v.i[levelout.v <= current.level & is.na(levelout.v) == F],
                       correction = correction,
                       hr.weight = hr.weight)

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
if(stopping.rule == "exemplars" & n.remaining < stopping.par * nrow(cue.df.i)) {grow.tree <- F}
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
                                 cue.v = cue.df.i[, last.cue],
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
                                    criterion.v = train.criterion.v,
                                    correction = correction,
                                    hr.weight = hr.weight

)


level.stats$level.exitdirection[last.level] <- .5
level.stats[last.level, names(current.level.classtable)] <- last.level.classtable

}

}

# ------------------
# Tree is finished!
# --------------------

# Set up final output

train.decision.df[,tree.i] <- decision.v
train.levelout.df[,tree.i] <- levelout.v

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

trees$n.train <- nrow(cue.df.i)
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

  tree.i.train.stats <- classtable(prediction.v = train.decision.df[,tree.i],
                                   criterion.v = train.criterion.v,
                                   correction = correction,
                                   hr.weight = hr.weight
                                   )


  if(tree.i == 1) {tree.train.stats <- tree.i.train.stats}
  if(tree.i != 1) {tree.train.stats <- rbind(tree.train.stats,
                                            tree.i.train.stats)}

}

tree.train.stats$tree <- 1:n.trees
names(tree.train.stats)[1:8] <- paste(names(tree.train.stats)[1:8], ".train", sep = "")
trees <- cbind(trees, tree.train.stats[, 1:8])

# Add testing stats for all trees

test.decision.df <- NULL
test.levelout.df <- NULL

if(fitorpred == "p") {

  n.test <- length(test.criterion.v)

  test.levelout.df <- as.data.frame(matrix(NA, nrow = n.test, ncol = n.trees))
  test.decision.df <- as.data.frame(matrix(NA, nrow = n.test, ncol = n.trees))

  names(test.levelout.df) <- paste("tree.", 1:n.trees, sep = "")
  names(test.decision.df) <- paste("tree.", 1:n.trees, sep = "")

  trees[paste(c("n", "hi", "mi", "fa", "cr", "hr", "far", "v", "dprime"), ".test", sep = "")] <- NA

  for(tree.i in 1:n.trees) {

    tree.i.pred <- applyfft(level.name.v = trees$level.name[tree.i],
                            level.class.v = trees$level.class[tree.i],
                            level.exit.v = trees$level.exit[tree.i],
                            level.threshold.v = trees$level.threshold[tree.i],
                            level.sigdirection.v = trees$level.sigdirection[tree.i],
                            cue.df = test.cue.df,
                            criterion.v = test.criterion.v,
                            hr.weight = hr.weight,
                            correction = correction,
                            which.trees  = tree.i
    )

    test.levelout.df[,tree.i] <- tree.i.pred$levelout.df
    test.decision.df[,tree.i] <- tree.i.pred$decision.df

    trees[tree.i, paste(c("n", "hi", "mi", "fa", "cr", "hr", "far", "v", "dprime"), ".test", sep = "")] <- c(n.test, tree.i.pred$tree.stats[ paste(c("hi", "mi", "fa", "cr", "hr", "far", "v", "dprime"), ".test", sep = "")])
  }


}
}

# -------------------------
# LOGISTIC REGRESSION
#   lr.acc
# -------------------------

  if(do.lr) {

    if(verbose) {print("Doing Logistic Regression...")}


    lr.result <- NA


    lr.train.df <- cbind(train.cue.df, train.criterion.v)
    names(lr.train.df)[ncol(lr.train.df)] <- "crit"

    # Remove cues with only one value
    train.df.ex <- sapply(1:ncol(lr.train.df), FUN = function(x) {length(unique(lr.train.df[,x]))})
    lr.train.df <- lr.train.df[,train.df.ex > 1]

  if(fitorpred == "p") {

    lr.test.df <- cbind(test.cue.df, test.criterion.v)
    names(lr.test.df)[ncol(lr.test.df)] <- "crit"

  }

    # Run logistic regression on training set (can be time consuming....)

    lr.train.mod <- suppressWarnings(glm(crit ~.,
                  family = "binomial",
                  data = lr.train.df
    ))


    lr.train.predictions <- suppressWarnings(predict(lr.train.mod,
                                 newdata = lr.train.df))

    lr.train.predictions <- 1 / (1 + exp(-lr.train.predictions))
    lr.train.predictions.bin <- rep(0, length(lr.train.predictions))
    lr.train.predictions.bin[lr.train.predictions >= .5] <- 1


    lr.train.stats <- classtable(prediction.v = lr.train.predictions.bin,
                           criterion.v = train.criterion.v,
                         correction = correction,
                         hr.weight = hr.weight
                         )

  if(fitorpred == "p") {

    lr.test.predictions <- suppressWarnings(predict(lr.train.mod,
                                 newdata = lr.test.df))

    lr.test.predictions <- 1 / (1 + exp(-lr.test.predictions))
    lr.test.predictions.bin <- rep(0, length(lr.test.predictions))
    lr.test.predictions.bin[lr.test.predictions >= .5] <- 1

    lr.test.stats <- classtable(prediction.v = lr.test.predictions.bin,
                          criterion.v = test.criterion.v,
                         correction = correction, hr.weight = hr.weight
    )

    lr.test.stats$n.exemplars <- length(model.can.predict)
    lr.test.stats$n.exemplars.can.pred <- sum(model.can.predict)
    lr.test.stats$p.exemplars.can.pred <- sum(model.can.predict) / length(model.can.predict)
}


  # Get summary vectors of FAR and HR for all thresholds


  lr.threshold.v <- seq(.9, .1, -.1)


      lr.train.far.v <- rep(NA, length(lr.threshold.v))
      lr.train.hr.v <- rep(NA, length(lr.threshold.v))


      if(fitorpred == "p") {

      lr.test.far.v <- rep(NA, length(lr.threshold.v))
      lr.test.hr.v <- rep(NA, length(lr.threshold.v))

    }




      lr.test.hr.v <- NA
      lr.test.far.v <- NA

    for(threshold.i in lr.threshold.v) {

      lr.train.stats.i <- classtable(prediction.v = lr.train.predictions >= threshold.i,
                                    criterion.v = train.criterion.v)

      lr.train.hr.v[lr.threshold.v == threshold.i] <- lr.train.stats.i$hr
      lr.train.far.v[lr.threshold.v == threshold.i] <- lr.train.stats.i$far



      if(fitorpred == "p") {


        lr.test.stats.i <- classtable(prediction.v = lr.test.predictions >= threshold.i,
                                      criterion.v = test.criterion.v)

        lr.test.hr.v[lr.threshold.v == threshold.i] <- lr.test.stats.i$hr
        lr.test.far.v[lr.threshold.v == threshold.i] <- lr.test.stats.i$far

      }


    }


      lr.acc <- data.frame("threshold" = lr.threshold.v,
                           "hr.train" = lr.train.hr.v,
                           "far.train" = lr.train.far.v,
                           "hr.test" = lr.test.hr.v,
                           "far.test" = lr.test.far.v
                           )


  }

# -------------------------
# CART
#   cart.acc
# -------------------------

  if(do.cart) {

    if(verbose) {print("Doing CART...")}

    train.cue.df.cart <- cbind(train.cue.df, train.criterion.v)
    names(train.cue.df.cart)[ncol(train.cue.df.cart)] <- "criterion"

    cart.form.a <- paste(names(train.cue.df.cart)[length(names(train.cue.df.cart))], "~")
    cart.form.b <- paste(names(train.cue.df.cart)[1:(ncol(train.cue.df.cart) - 1)], collapse = " + ")

    cart.form <- as.formula(paste(cart.form.a, cart.form.b, collapse = " "))


    # Calculate loss df (for ROC curve)

    loss.df <- expand.grid(miss.cost = seq(1, 5, length.out = 5),
                           fa.cost = seq(1, 5, length.out = 5)
    )

    # Remove some cases where costs are identical
    loss.df <- loss.df[loss.df$miss.cost != loss.df$fa.cost | loss.df$miss.cost == 1,]


    cart.train.acc.ls <- vector("list", length = nrow(loss.df))
    cart.cues.vec <- c()

  if(fitorpred == "f") {

   cart.test.acc.df <- NULL

  }


  if(fitorpred == "p") {

    test.cue.df.cart <- cbind(test.cue.df, test.criterion.v)
    names(test.cue.df.cart)[ncol(test.cue.df.cart)] <- "criterion"
    cart.test.acc.ls <- vector("list", length = nrow(loss.df))

    }



  for(row.i in 1:nrow(loss.df)) {

    miss.cost.i <- loss.df$miss.cost[row.i]
    fa.cost.i <- loss.df$fa.cost[row.i]

    # Train cart model

    cart.train.mod.i <- rpart::rpart(cart.form,
                        data = train.cue.df.cart,
                        method = "class",
                        parms = list(loss = matrix(c(0, miss.cost.i, fa.cost.i, 0), byrow = T, nrow = 2))
                        )

    # Determine the cues used by cart model

    cart.cues.used <- as.character(cart.train.mod.i$frame$var)
    cart.cues.used <- paste(cart.cues.used[cart.cues.used != "<leaf>"], collapse = ";")
    cart.cues.vec <- c(cart.cues.vec, cart.cues.used)


    # Get training decisions

    cart.train.pred.i <- predict(cart.train.mod.i,
                           train.cue.df.cart,
                           type = "class")

    # Recode to logical

      if("TRUE" %in% paste(cart.train.pred.i)) {cart.train.pred.i <- as.logical(paste(cart.train.pred.i))}
      if("1" %in% paste(cart.train.pred.i)) {cart.train.pred.i <- as.logical(as.numeric(paste(cart.train.pred.i)))}




  if(fitorpred == "p") {

    # Get test decisions

    cart.test.pred.i <- predict(cart.train.mod.i,
                            test.cue.df.cart,
                            type = "class")

    # Recode to logical

    if("TRUE" %in% paste(cart.test.pred.i)) {cart.test.pred.i <- as.logical(paste(cart.test.pred.i))}
    if("1" %in% paste(cart.test.pred.i)) {cart.test.pred.i <- as.logical(as.numeric(paste(cart.test.pred.i)))}


  }




     # Calculate training accuracy stats

    cart.train.acc.i <- classtable(prediction.v = cart.train.pred.i,
                                   criterion.v = train.criterion.v,
                                   correction = correction,
                                   hr.weight = hr.weight
                                    )

    cart.train.acc.ls[[row.i]] <- cart.train.acc.i


    if(fitorpred == "p") {

    cart.test.acc.i <- classtable(prediction.v = cart.test.pred.i,
                                  criterion.v = test.criterion.v,
                                  correction = correction,
                                  hr.weight = hr.weight)

    cart.test.acc.ls[[row.i]] <- cart.test.acc.i

    }

  }

    # Combine training statistics into a df

  cart.train.acc.df <- do.call(rbind, cart.train.acc.ls)
  cart.train.acc.df <- cbind(cart.train.acc.df, loss.df)


  if(fitorpred == "p") {

    # Combine test statistics into a df

    cart.test.acc.df <- do.call(rbind, cart.test.acc.ls)
    cart.test.acc.df <- cbind(cart.test.acc.df, loss.df)

  }


  ## Combine cart.train and cart.test

  cart.train.acc.df <- cart.train.acc.df[c("miss.cost", "fa.cost", "hr", "far", "v", "dprime")]
  names(cart.train.acc.df) <- c("miss.cost", "fa.cost", "hr.train", "far.train", "v.train", "dprime.train")

  if(fitorpred == "p") {

  cart.test.acc.df <- cart.test.acc.df[c("hr", "far", "v", "dprime")]
  names(cart.test.acc.df) <- c("hr.test", "far.test", "v.test", "dprime.test")

  }

  # Combine CART training stats, test stats, and cues

  if(fitorpred == "f") {

    cart.acc <- cbind(cart.train.acc.df,
                      cart.cues.vec)

  }

  if(fitorpred == "p") {

  cart.acc <- cbind(cart.train.acc.df,
                    cart.test.acc.df,
                    cart.cues.vec)

  }


  # Remove cases with the same result

  dup.rows <- duplicated.data.frame(cart.acc[,-1])

  cart.acc <- cart.acc[dup.rows == F,]

  }

# ----------
# Compile all results in to an fft object
# ----------

best.train.tree <- (1:nrow(trees))[which(trees$v.train == max(trees$v.train))[1]]

if(fitorpred == "p") {best.test.tree <- (1:nrow(trees))[which(trees$v.test == max(trees$v.test))[1]]}
if(fitorpred == "f") {best.test.tree <- NULL}


output.fft <- list(
                  "trees" = trees,
                  "cue.accuracies" = cue.accuracies.original,
                  "cart" = cart.acc,
                  "lr" = lr.acc,
                  "train.cue" = train.cue.df,
                  "train.crit" = train.criterion.v,
                  "test.cue" = test.cue.df,
                  "test.crit" = test.criterion.v,
                  "train.decision.df" = train.decision.df,
                  "test.decision.df" = test.decision.df,
                  "train.levelout.df" = train.levelout.df,
                  "test.levelout.df" = test.levelout.df,
                  "best.train.tree" = best.train.tree,
                  "best.test.tree" = best.test.tree
)


class(output.fft) <- "fft"

  return(output.fft)

}


