#' Summarize an \code{FFTrees} object
#'
#' \code{summary.FFTrees} summarizes key contents of an \code{FFTrees} object.
#'
#' \code{summary.FFTrees} returns an invisible list containing two elements:
#' \enumerate{
#'   \item \code{definitions} and corresponding performance measures of \code{tree}s;
#'   \item \code{stats} on decision frequencies, derived probabilities, and costs (separated by \code{train} and \code{test}).
#' }
#'
#' A header prints descriptive information of the \code{FFTrees} object (to the console):
#' Its \code{main} title, number of trees (\code{object$trees$n}), and the name of the criterion variable (\code{object$criterion_name}).
#'
#' Per default, information on all available trees is shown and returned.
#' Specifying \code{tree} filters the output list elements for the corresponding tree(s).
#' When only a single \code{tree} is specified, the printed header includes a verbal description of
#' the corresponding tree.
#'
#' While \code{summary.FFTrees} provides key details about the specified \code{tree}(s),
#' the individual decisions (stored in \code{object$trees$decisions}) are not shown or returned.
#'
#' @return An invisible list with elements containing the \code{definitions} and performance \code{stats}
#' of the FFT(s) specified by \code{tree}(s).
#'
#' @param object An \code{FFTrees} object.
#' @param tree The tree to summarize (as an integer, but may be a vector).
#' If \code{tree = NULL} (as per default) or exceeding the possible range \code{1:object$trees$n},
#' information on all trees in \code{object} is returned.
#' @param ... Additional arguments (currently ignored).
#'
#' @seealso
#' \code{\link{print.FFTrees}} for printing FFTs;
#' \code{\link{plot.FFTrees}} for plotting FFTs;
#' \code{\link{inwords}} for obtaining a verbal description of FFTs;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @importFrom knitr kable
#'
#' @export
#'

summary.FFTrees <- function(object,
                            tree = NULL,
                            ...) {

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(class(object) == "FFTrees",
                        info = "Argument object is no FFTrees object")

  if (is.null(tree) == FALSE){

    testthat::expect_true(is.numeric(tree),
                          info = "Argument tree is not numeric")

    tol <- .Machine$double.eps^0.5
    testthat::expect_true(all(abs(tree - round(tree)) < tol),
                          info = "Argument tree is not an integer")

  }

  # Parameters:
  o_main  <- object$params$main
  o_crit  <- object$criterion_name
  n_trees <- object$trees$n
  if (n_trees > 1) { tree_s <- "trees" } else { tree_s <- "tree" }
  tree_options <- 1:n_trees

  digits <- 2  # N of digits to print in stats table
  out <- vector(mode = "list", length = 0)  # initialize as empty list


  # Print general info on FFTrees object (similar to print.FFTrees): ------

  if (is.null(o_main) == FALSE) {

    cat(o_main, "\n\n", sep = "")  # main object title
  }

  cat(crayon::blue("FFTrees ")) # , rep("-", times = 50 - nchar("FFTrees")), "\n", sep = "")
  cat("\n")

  cat("- Trees: ", n_trees, " fast-and-frugal ", tree_s, " predicting ", crayon::underline(o_crit), "\n", sep = "")


  # Parameter summary: ------

  # Algorithm, goals, etc.:
  params_txt <- paste0("algorithm = '", object$params$algorithm,
                       "', goal = '", object$params$goal,
                       "', goal.chase = '", object$params$goal.chase,
                       "',")
  params_num <- paste0("sens.w = ", object$params$sens.w,
                       ", max.levels = ", object$params$max.levels)

  cat("- Parameters: ", params_txt, "\n", "              ", params_num, "\n", sep = "")


  # Print a FFT description (iff only 1 tree): ------

  if ((is.null(tree) == FALSE) && (length(tree) == 1) && (tree %in% tree_options)){  # only 1 tree:

    cat("\n")
    cat(crayon::blue("FFT #", tree, ": Definition", "\n", sep = ""), sep = "")

    for (i in 1:length(object$trees$inwords[[tree]])) { # for each sentence:

      cat(paste0("[", i, "] ", object$trees$inwords[[tree]][i], "\n"))

    } # for i.

  } # if only 1 tree.


  # Get all tree definitions and stats: ----

  out$definitions <- object$trees$definitions
  out$stats       <- object$trees$stats

  # print(out$stats)  # 4debugging: Note that out$stats$train is tibble, but out$stats$test is data frame!


  # Filter/reduce out (iff tree is specified): ----

  if (is.null(tree) == FALSE){

    if (any(tree %in% tree_options == FALSE)){

      tree_diff <- paste0(setdiff(tree, tree_options), collapse = ", ")

      warning(paste0("You asked for tree(s) ", tree_diff, ", but object only contains ", n_trees, " trees. Here are all..."))

    } else { # filter desired tree info:

      out$definitions <- out$definitions[tree, ]

      out$stats$train <- out$stats$train[tree, ]

      if (is.null(out$stats$test) == FALSE){
        out$stats$test <- out$stats$test[tree, ]
      }

    } # if (tree > n_trees).
  } # if tree not NULL.


  # Print tables (on console): ----

  cap_def <- crayon::blue(paste("Tree", crayon::underline("definitions")))
  print(knitr::kable(out$definitions, caption = cap_def))

  cap_train <- crayon::blue(paste("Tree statistics on", crayon::underline("training"), "data"))
  print(knitr::kable(out$stats$train, caption = cap_train, digits = digits))

  if (is.null(out$stats$test) == FALSE){
    cap_test <- crayon::blue(paste("Tree statistics on", crayon::underline("test"), "data"))
    print(knitr::kable(out$stats$test, caption = cap_test, digits = digits))
  }


  # Output: ----

  return(invisible(out))

} # summary.FFTrees().


# eof.
