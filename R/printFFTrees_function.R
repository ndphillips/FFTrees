#' Prints summary information from an FFTrees object
#'
#' @description Printing function for an FFTrees object
#' @param x A FFTrees object created from FFTrees()
#' @param ... additional arguments passed to print.
#' @export

print.FFTrees <- function(
  x = NULL,
  ...
) {

# -------------------------
# TESTING VALUES
# --------------------------

cues.used <- paste(unique(unlist(strsplit(x$tree.stats$level.name, ";"))), collapse = ",")
n.cues.used <- length(unique(unlist(strsplit(x$tree.stats$level.name, ";"))))

best.train.tree <- x$tree.stats$tree.num[x$tree.stats$v.train == max(x$tree.stats$v.train)]
best.train.hr <- round(x$tree.stats$hr.train[best.train.tree], 2)
best.train.far <- round(x$tree.stats$far.train[best.train.tree], 2)
best.test.hr <- round(x$tree.stats$hr.test[best.train.tree], 2)
best.test.far <- round(x$tree.stats$far.test[best.train.tree], 2)

summary.text <- paste("An FFTrees object containing ", nrow(x$tree.stats), " trees using ", n.cues.used,
                      " cues {", cues.used, "} out of an original ", ncol(x$data.train) - 1, sep = "")

data.text <- paste("Data were trained on ", nrow(x$data.train), " exemplars", sep = "")

if(is.null(x$data.test)) {data.text <- paste(data.text, ". There were no test data", sep = "")}
if(is.null(x$data.test) == F) {data.text <- paste(data.text, ", and tested on " , nrow(x$data.test), " new exemplars", sep = "")}

auc.text <- paste("Trees AUC: (Train = ", round(x$auc[1,1], 2), ", Test = ", round(x$auc[2,1], 2), ")", sep = "")
accuracy.text <- paste("My favorite tree is #", best.train.tree, " [Training: HR = ", best.train.hr, ", FAR = ", best.train.far, "]", sep = "")
accuracy.text <- paste(accuracy.text, ", [Testing: HR = ", best.test.hr, ", FAR = ", best.test.far, "]", sep = "")

print(summary.text)
print(data.text)
print(auc.text)
print(accuracy.text)

}
