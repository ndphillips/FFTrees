#' Prints summary information from an FFForest x
#'
#' @description Printing function for an FFTrees x
#' @param x FFForest. An FFForest x created from FFForest()
#' @param ... additional arguments passed to print.
#' @export

print.FFForest <- function(
  x = NULL,
  ...
) {

#x <- FFForest(diagnosis ~., data = heartdisease, train.p = .5, cpus = 4, ntree = 100)

ntree <- x$params$ntree
train.p <- x$params$train.p
importance <- sort(x$frequencies, decreasing = TRUE) / sum(x$frequencies)
importance.df <- data.frame(importance)
names(importance.df) <- c("cue", "importance")
importance.df[,2] <- round(importance.df[,2], 2)

summary.text.1 <- paste("Forest of ", ntree, " FFTs from ", train.p * 100, "% random training splits.", sep = "")
summary.text.2 <- paste0("Mean performance across all ", ntree, " trees:")

summary.df <- data.frame("train" = c(
                                      round(mean(x$tree.sim$acc.train), 2),
                                      round(mean(x$tree.sim$bacc.train), 2),
                                      round(mean(x$tree.sim$sens.train), 2),
                                      round(mean(x$tree.sim$spec.train), 2),
                                      round(mean(x$tree.sim$mcu.train), 2),
                                      round(mean(x$tree.sim$pci.train), 2)
                                     ),
                         "test" = c(
                           round(mean(x$tree.sim$acc.test), 2),
                           round(mean(x$tree.sim$bacc.test), 2),
                           round(mean(x$tree.sim$sens.test), 2),
                           round(mean(x$tree.sim$spec.test), 2),
                           round(mean(x$tree.sim$mcu.test), 2),
                           round(mean(x$tree.sim$pci.test), 2)
                         )
                         )

row.names(summary.df) <- c("acc", "bacc", "sens", "spec", "mcu", "pci")
importance.text <- paste("Cue Importance:")


print(summary.text.1)
print(summary.text.2)
print(summary.df)
print(importance.text)
print(importance.df)

}
