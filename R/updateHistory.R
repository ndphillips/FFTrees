#' Update the history of decisions from trees in an FFTrees object
#'
#' @param object FFTrees. An FFTrees object
#' @param newdata dataframe. A dataframe of new data that can be used with \code{predict.FFTrees()}
#' @param tree.definitions dataframe. Tree definitions (i.e., from an existing FFTrees object)
#' @param formula formula. Formula used when building FFTrees.
#'
#' @export
#'
updateHistory <- function(object = NULL,
                          newdata = NULL,
                          tree.definitions = NULL,
                          formula = NULL) {

# Check inputs
{
  if(is.null(object) & is.null(tree.definitions)) {

    stop("Either object or tree.definitions must be specified")

  }


  if(is.null(object) == FALSE & class(object) != "FFTrees") {

    stop("The specified object does not have the class 'FFTrees'")
  }
}

# Get formula
{
if(is.null(object) == FALSE) {

  formula <- object$formula

} else {

  if(is.null(formula)) {stop("Either an object or formula must be specified")

  }
  }
}

# Get tree decisions for newdata
new.applytree <- apply.tree(data = newdata,
                            tree.definitions = tree.definitions,
                            formula = formula)


# Create history.new
{

# Determine number of trees
n.trees <- nrow(tree.definitions)

# Create history list
history.new <- vector("list", length = n.trees)

# Loop over trees

for(tree.i in 1:n.trees) {

  n.nodes <- tree.definitions$nodes[tree.i]

  # Get exits for current tree
  exits.c <- unlist(strsplit(tree.definitions$exits[tree.i], ";"))

  # Get classification statistics for each level

  levelstats.c <- new.applytree$levelstats[new.applytree$levelstats$tree == tree.i,]

  hi.c <- levelstats.c$hi - c(0, levelstats.c$hi[1:n.nodes - 1])
  mi.c <- levelstats.c$mi - c(0, levelstats.c$mi[1:n.nodes - 1])
  fa.c <- levelstats.c$fa - c(0, levelstats.c$fa[1:n.nodes - 1])
  cr.c <- levelstats.c$cr - c(0, levelstats.c$cr[1:n.nodes - 1])

  n.c <- hi.c + mi.c + fa.c + cr.c

  correct.c <- (hi.c + cr.c) / n.c
  error.c <-  1 - correct.c

  history.new[[tree.i]] <- data.frame(

    tree = rep(tree.i, n.nodes),
    dataset = rep(1, n.nodes),
    level = 1:n.nodes,
    exit = exits.c,
    n = n.c,
    hi = hi.c,
    mi = mi.c,
    fa = fa.c,
    cr = cr.c,
    corr = correct.c,
    err = error.c

  )


}

# Bring together in one dataframe
history.new <- do.call(rbind, history.new)

}

# Create or update (full) history
{
# If object does not exist, then create history, otherwise update it

  if(is.null(object)) {

    history <- history.new

  } else {

    history <- rbind(history, history.new)}

}


return(list("history" = history))

}
