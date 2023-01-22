#' Create FFT definitions
#'
#' @description \code{fftrees_define} defines fast-and-frugal trees (FFTs)
#' either from the definitions provided or by applying algorithms (when no definitions are provided),
#' and returns a modified \code{FFTrees} object that contains those definitions.
#'
#' In most use cases, \code{fftrees_define} passes a new \code{FFTrees} object \code{x} either
#' to \code{\link{fftrees_grow_fan}} (to create new FFTs by applying algorithms to data) or
#' to \code{\link{fftrees_wordstofftrees}} (if \code{my.tree} is specified).
#'
#' If an existing \code{FFTrees} object \code{object} or \code{tree.definitions} are provided as inputs,
#' no new FFTs are created.
#' When both arguments are provided, \code{tree.definitions} take priority over the FFTs in an existing \code{object}.
#' Specifically,
#'
#' \itemize{
#'
#'   \item{If \code{tree.definitions} are provided, these are assigned to the FFTs of \code{x}.}
#'
#'   \item{If no \code{tree.definitions} are provided, but an existing \code{FFTrees} object \code{object} is provided,
#'   the trees from \code{object} are assigned to the FFTs of \code{x}.}
#'
#' }
#'
#' @param x The current \code{FFTrees} object (to be changed and returned).
#' @param object An existing \code{FFTrees} object (with tree definitions).
#' @param tree.definitions A \code{data.frame}. An optional hard-coded definition of FFTs (in the same format as in an \code{FFTrees} object).
#' If specified, no new FFTs are created, but the tree definitions in \code{object} or \code{x} are replaced by the tree definitions provided
#' and the current object is re-evaluated.
#'
#' @return An \code{FFTrees} object with tree definitions.
#'
#' @keywords internal
#'
#' @seealso
#' \code{\link{fftrees_create}} for creating \code{FFTrees} objects;
#' \code{\link{fftrees_grow_fan}} for creating FFTs by applying algorithms to data;
#' \code{\link{fftrees_wordstofftrees}} for creating FFTs from verbal descriptions;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @import testthat
#'
#' @export

fftrees_define <- function(x,
                           object = NULL,
                           tree.definitions = NULL
) {

  # Provide user feedback: ----

  if (!x$params$quiet) {
    msg <- paste0("Aiming to define FFTs:\n")
    cat(u_f_ini(msg))
  }


  # Verify inputs: ------

  testthat::expect_s3_class(x, class = "FFTrees")


  # Main: Distinguish between 4 use cases ------

  if (!is.null(tree.definitions)) { # 1. Use FFTs from tree.definitions: ----

    n_trees <- as.integer(nrow(tree.definitions))

    # HACK: Ensure that the tree IDs in tree.definitions are sorted (1:n_trees):
    if (any(tree.definitions$tree != 1:n_trees)){

      # Sort tree ID variable to 1:n_trees:
      tree.definitions$tree <- 1:n_trees

      # Provide user feedback: ----

      if (!x$params$quiet) {
        msg <- paste0("Sorted tree IDs in tree.definitions into 1:n_trees (tree = 1:", n_trees, ").\n")
        cat(u_f_hig(msg))
      }

      # print(tree.definitions)  # 4debugging

    }

    # Change object x by using tree.definitions:
    x$trees$definitions <- tree.definitions
    x$trees$n <- n_trees

    if (!x$params$quiet) {
      msg <- paste0("Using ", x$trees$n, " FFTs from 'tree.definitions' as current trees.\n")
      cat(u_f_hig(msg))
    }


  } else if (!is.null(object)) { # 2. Use FFTs from object: ----

    # Verify object$trees$definitions:
    testthat::expect_true(!is.null(object$trees$definitions))

    # Change x by using the tree definitions of object:
    x$trees$definitions <- object$trees$definitions
    x$trees$n <- as.integer(nrow(object$trees$definitions))

    if (!x$params$quiet) {
      msg <- paste0("Using ", x$trees$n, " FFTs from 'object' as current trees.\n")
      cat(u_f_hig(msg))
    }


  } else if (!is.null(x$params$my.tree)) { # 3. Create 1 new FFT from verbal description: ----

    x <- fftrees_wordstofftrees(x, my.tree = x$params$my.tree)


  } else if (x$params$algorithm %in% c("ifan", "dfan")) { # 4. Create new FFTs by applying algorithm to data: ----

    x <- fftrees_grow_fan(x)


  } else {

    stop("I don't know how to define your trees...")

  }

  # Provide user feedback: ----

  if (!x$params$quiet) {

    n_trees <- x$trees$n

    if (n_trees == 1){
      msg <- paste0("Successfully defined ", n_trees, " FFT.\n")
    } else if (n_trees > 1){
      msg <- paste0("Successfully defined ", n_trees, " FFTs.\n")
    } else {
      msg <- "No FFTs were defined."
    }

    cat(u_f_fin(msg))
  }


  # Output: ----

  return(x)

} # fftrees_define().

# eof.
