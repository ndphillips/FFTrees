#' Heuristic algorithms for building FFTs originally developed by Martignon, Katsikopoulos & Woike (2008)
#'
#' @param formula formula. A formula specifying a logical criterion as a function of 1 or more predictors.
#' @param data dataframe. A training dataset.
#' @param max.levels integer. The maximum number of levels considered for the trees.
#' @param algorithm string. Either 'max' or 'zigzag'
#'
#' @return A definition of an FFT
#' @export
#'
heuristic.algorithm <- function(formula,
                          data,
                          max.levels = NULL,
                          algorithm = "max") {

  # formula = formula
  # data = data.mf
  # max.levels = max.levels
  # algorithm = algorithm

# Convert factors to character

for(i in 1:ncol(data)) {

  if("factor" %in% class(data[,i])) {data[,i] <- paste(data[,i])}

}

# Split factors into binary cues

# Start with criterion
data.mf <- model.frame(formula, data)

criterion.name <- names(data.mf)[1]
data.mm <- data.frame(data.mf[criterion.name])

for(col.i in 2:ncol(data.mf)) {

  col.name <- names(data.mf)[col.i]

  # If column class is numeric, then add to data.mm
  if(class(data.mf[,col.i]) %in% c("numeric", "integer", "logical")) {

    data.mm <- data.frame(data.mm, data.mf[,col.i])
    names(data.mm)[ncol(data.mm)] <- col.name
  }

  # If column class is factor or character, then create dummy coded versions
  if(class(data.mf[,col.i]) %in% c("character", "factor")) {

    factor.values <- unique(data.mf[,col.i])
    factor.values.n <- length(factor.values)

    factor.df <- as.data.frame(matrix(NA, nrow = nrow(data.mf), ncol = factor.values.n))
    names(factor.df) <- paste0(col.name, factor.values)

    factor.df[,1:factor.values.n] <- sapply(1:factor.values.n, FUN = function(x) {

      data.mf[,col.i] == factor.values[x]}

      )

    data.mm <- data.frame(data.mm, factor.df, check.names = FALSE)
  }

}

# Add criterion to data.mm
data.mm[criterion.name] <- data.mf[,1]

# Remove intercept from datamm
data.mm <- data.mm[,names(data.mm) != "(Intercept)"]

# If any classes in data.mm are 'matrix' convert to integer
#  For some reason this can happen...

for(i in 1:ncol(data.mm)) {

  if(class(data.mm[,i]) == "matrix") {data.mm[,i] <- as.logical(data.mm[,i])}

  }

# Calculate cue accuracies with median num threshold
cue.acc <- cuerank(formula,
                   data = data.mm,
                   goal = "bacc",
                   considerFALSE = FALSE,
                   numthresh.method = "m",
                   factor.directions = c("="),
                   numeric.directions = c(">=", "<", ">=", ">"))


# Convert missing ppv and npv to 0

cue.acc$npv[is.finite(cue.acc$npv) == FALSE] <- 0
cue.acc$ppv[is.finite(cue.acc$ppv) == FALSE] <- 0

# Determine number of levels

if(is.null(max.levels)) {max.levels <- nrow(cue.acc)}

# Add max.val, the maximum of npv and ppv

cue.acc$max.val[cue.acc$ppv >= cue.acc$npv] <- cue.acc$ppv[cue.acc$ppv >= cue.acc$npv]
cue.acc$max.val[cue.acc$npv >= cue.acc$ppv] <- cue.acc$npv[cue.acc$npv >= cue.acc$ppv]

cue.acc$cue.dummy <- cue.acc$cue
cue.acc$threshold.dummy <- cue.acc$threshold
cue.acc$direction.dummy <- cue.acc$direction

# Convert dummy variable names back to original names

factors.log <- sapply(1:ncol(data.mf), FUN = function(x) {class(data.mf[,x]) %in% c("character", "factor")})

cues.factors <- names(data.mf)[factors.log]

dummycode.lu <- lapply(cues.factors, FUN = function(cue.i) {

  vals <- unlist(unique(data.mf[cue.i]))
  vals.n <- length(vals)

  output <- data.frame(cue.o = rep(cue.i, vals.n),
                       cue.d = paste0(cue.i, vals),
                       vals.d = vals, stringsAsFactors = FALSE)

  return(output)

  })
dummycode.lu <- do.call(rbind, dummycode.lu)

cues.dummy.log <- cue.acc$cue.dummy %in% dummycode.lu$cue.d

cue.acc$cue <- cue.acc$cue.dummy
cue.acc$cue[cues.dummy.log] <- dummycode.lu$cue.o[match(cue.acc$cue.dummy[cues.dummy.log], dummycode.lu$cue.d)]

cue.acc$threshold <- cue.acc$threshold.dummy
cue.acc$threshold[cues.dummy.log] <- dummycode.lu$vals.d[match(cue.acc$cue.dummy[cues.dummy.log], dummycode.lu$cue.d)]

cue.acc$class[cues.dummy.log] <- "character"

# Change directions for dummy variables to == and !=

cue.acc$direction <- cue.acc$direction.dummy
cue.acc$direction[cues.dummy.log & cue.acc$threshold.dummy == 1 & cue.acc$direction.dummy == "<"] <- "!="
cue.acc$direction[cues.dummy.log & cue.acc$threshold.dummy == 1 & cue.acc$direction.dummy == "=="] <- "!="
cue.acc$direction[cues.dummy.log & cue.acc$threshold.dummy == 0 & cue.acc$direction.dummy == ">"] <- "="
cue.acc$direction[cues.dummy.log & cue.acc$threshold.dummy == 0 & cue.acc$direction.dummy == "!="] <- "=="

if(algorithm == "max") {

# Order cues by max.val

cue.acc <- cue.acc[order(cue.acc$max.val,
                         decreasing = TRUE),]

# Determine exits based on max.val

cue.acc$exits <- 0
cue.acc$exits[cue.acc$max.val == cue.acc$ppv] <- 1

# If two consecutive nodes have the same factor cue and direction, put them together

cue.acc.f <- cue.acc[1,]
for(i in 2:nrow(cue.acc)) {

  previous.cue <- cue.acc$cue[i - 1]
  previous.class <- cue.acc$class[i - 1]
  previous.threshold <- cue.acc$threshold[i - 1]
  previous.exits <- cue.acc$exits[i - 1]

  current.cue <- cue.acc$cue[i]
  current.class <- cue.acc$class[i]
  current.threshold <- cue.acc$threshold[i]
  current.exits <- cue.acc$exits[i]

  # If new cue is same as old
  if(current.cue == previous.cue &
     current.class == "character" &
     current.threshold != previous.threshold &
     current.exits == previous.exits) {

    new.threshold <- paste(c(previous.threshold, current.threshold), collapse = ",")

    cue.acc.f$threshold[nrow(cue.acc.f)] <- new.threshold

  } else {

    cue.acc.f <- rbind(cue.acc.f, cue.acc[i,])

  }

}

cue.acc <- cue.acc.f

cue.acc.o <- cue.acc
cue.acc.o <- cue.acc.o[,names(cue.acc.o) %in% c("cue", "class", "threshold", "direction", names(classtable(c(1, 1, 0), c(1, 0, 1))))]

# Now get final tree definition
fft.cue.acc <- cue.acc[1:min(nrow(cue.acc),max.levels),]
fft.cue.acc$exits[min(max.levels, nrow(fft.cue.acc))] <- .5


tree.definitions <- data.frame(tree = 1,
                               nodes = nrow(fft.cue.acc),
                               classes = paste(substr(fft.cue.acc$class, start = 1, stop = 1), collapse = ";"),
                               cues = paste(fft.cue.acc$cue, collapse = ";"),
                               directions = paste(fft.cue.acc$direction, collapse = ";"),
                               thresholds = paste(fft.cue.acc$threshold, collapse = ";"),
                               exits =  paste(fft.cue.acc$exits, collapse = ";"),
                               stringsAsFactors = FALSE)

# remove unnecessary columns from cue.acc
cue.acc <- cue.acc[,(names(cue.acc) %in% c("max.val", "cue.dummy", "threshold.dummy", "direction.dummy", "cue.thresh", "node", "exits")) == FALSE]


}

if(algorithm == "zigzag") {

  # Order cues by max.val

  cue.acc <- cue.acc[order(cue.acc$max.val,
                           decreasing = TRUE),]

  cue.acc$cue.thresh <- paste(cue.acc$cue, cue.acc$threshold, sep = ".")

  cue.acc$node <- 0
  cue.acc$exits <- NA

  for(node.i in 1:nrow(cue.acc)) {

    if(node.i == 1) {

      current.cue <- cue.acc$cue.thresh[cue.acc$max.val == max(cue.acc$max.val)]

      if(length(current.cue) > 1) {current.cue <- sample(current.cue, size = 1)}

        # If highest predictive value is ppv, set first exit to TRUE, otherwise, set to FALSE

        if(cue.acc$ppv[cue.acc$cue.thresh == current.cue] >= cue.acc$npv[cue.acc$cue.thresh == current.cue]) {

          current.exit <- 1

        } else {current.exit <- 0}

    }

    if(node.i > 1) {

    # What was the last exit?

      last.exit <- cue.acc$exits[cue.acc$node == (node.i - 1)]

      if(last.exit == 0) {

        current.exit <- 1

        # Which cue has the highest ppv that hasn't been selected yet?

        cues.eligible <- is.na(cue.acc$exits)
        current.cue <- cue.acc$cue.thresh[cues.eligible & cue.acc$ppv == max(cue.acc$ppv[cues.eligible])]
        if(length(current.cue) > 1) {current.cue <- sample(current.cue, 1)}


      }

      if(last.exit == 1) {

        current.exit <- 0

        # Which cue has the highest npv that hasn't been selected yet?

        cues.eligible <- is.na(cue.acc$exits)
        current.cue <- cue.acc$cue.thresh[cues.eligible & cue.acc$npv == max(cue.acc$npv[cues.eligible])]
        if(length(current.cue) > 1) {current.cue <- sample(current.cue, 1)

      }

    }
  }

    cue.acc$node[cue.acc$cue.thresh == current.cue] <- node.i
    cue.acc$exits[cue.acc$cue.thresh == current.cue] <- current.exit

  }

  cue.acc$exits[cue.acc$node == max.levels] <- .5

  cue.acc.o <- cue.acc
  cue.acc.o <- cue.acc.o[,names(cue.acc.o) %in% c("cue", "class", "threshold", "direction", names(classtable(c(1, 1, 0), c(1, 0, 1))))]

  # Now get final tree definition
  cue.acc <- cue.acc[cue.acc$node != 0,]
  cue.acc <- cue.acc[order(cue.acc$node),]

  fft.cue.acc <- cue.acc[1:min(nrow(cue.acc),max.levels),]
  fft.cue.acc$exits[fft.cue.acc$node == max.levels] <- .5


  tree.definitions <- data.frame(tree = 1,
                                 nodes = nrow(fft.cue.acc),
                                 classes = paste(substr(fft.cue.acc$class, start = 1, stop = 1), collapse = ";"),
                                 cues = paste(fft.cue.acc$cue, collapse = ";"),
                                 directions = paste(fft.cue.acc$direction, collapse = ";"),
                                 thresholds = paste(fft.cue.acc$threshold, collapse = ";"),
                                 exits =  paste(fft.cue.acc$exits, collapse = ";"),
                                 stringsAsFactors = FALSE)

}

return(list(tree.definitions = tree.definitions,
            cue.accuracies = cue.acc.o))

}
