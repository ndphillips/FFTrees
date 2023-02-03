#' Summarize an \code{FFTrees} object
#'
#' \code{summary.FFTrees} summarizes key contents of an \code{FFTrees} object.
#'
#' Given an \code{FFTrees} object \code{x},
#' \code{summary.FFTrees} selects key parameters from \code{x$params}
#' and provides the definitions and performance statistics for \code{tree} from \code{x$trees}.
#' Inspect and query \code{x} for additional details.
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
#' @importFrom scales percent
#' @importFrom cli style_underline
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


  # User feedback: General info on FFTrees object (similar to print.FFTrees): ------

  if (is.null(o_main) == FALSE) {

    cat(o_main, "\n\n", sep = "")  # main object title
  }

  cat(in_blue("FFTrees ")) # , rep("-", times = 50 - nchar("FFTrees")), "\n", sep = "")
  cat("\n")

  cat("- Trees: ", n_trees, " fast-and-frugal ", tree_s, " predicting ", cli::style_underline(o_crit), sep = "")
  cat("\n")


  # Parameter summary: ------

  # General information: Current algorithm, goals, numerical parameters, etc.:

  params_txt <- paste0("algorithm = '", object$params$algorithm, "'")
  params_num <- paste0("max.levels = ", object$params$max.levels)

  params_goal <- paste0("goal = '", object$params$goal,
                        "', goal.chase = '", object$params$goal.chase,
                        "', goal.threshold = '", object$params$goal.threshold, "'")


  # Specific information (based on goals):
  cur_goals <- c(object$params$goal, object$params$goal.chase, object$params$goal.threshold)

  if ("wacc" %in% cur_goals){ # add sens.w value to params_num:

    params_num <- paste0(params_num,
                         ", sens.w = ", object$params$sens.w)

  }

  # General user feedback (in all settings): ----
  cat("- Parameters: ",
      params_txt, ", ", # "              ",
      params_num, ",\n",   "              ",
      params_goal, ".",
      sep = "")
  cat("\n")


  # Specific user feedback (conditional on current settings): ----

  # Cost information:
  if ("cost" %in% cur_goals){ # report cost information:

    cost_out_v <- unlist(object$params$cost.outcomes)

    params_cost_out <- paste0("(",
                              paste(names(cost_out_v), collapse = ", "), ") = (",
                              paste(cost_out_v, collapse = ", "), ")")

    # User feedback:
    cat("- Cost of outcomes: ", params_cost_out, "\n", sep = "")


    if (!is.null(object$params$cost.cues)){ # cue costs were set:

      cost_cue_v <- unlist(object$params$cost.cues)

      # params_cost_cue <- paste0("(",
      #                           paste(names(cost_cue_v), collapse = ", "), ") = (",
      #                           paste(cost_cue_v, collapse = ", "), ")")

      # User feedback:
      # cat("- Cost of cues: ", params_cost_cue, "\n", sep = "")
      cat("- Cost of cues: ", "\n", sep = "")
      print(cost_cue_v)

    }

  } # if "cost" goals.


  # Print an FFT description (iff only 1 tree): ------

  if ((is.null(tree) == FALSE) && (length(tree) == 1) && (tree %in% tree_options)){  # only 1 tree:

    cat("\n")
    cat(in_blue("FFT #", tree, ": Definition", "\n", sep = ""), sep = "")

    # FFT in words:

    if (is.null(object$trees$stats$test) == FALSE){
      tree_in_words <- inwords(object, data = "test", tree = tree)  # re-generate tree descriptions for "test" data
    } else {
      tree_in_words <- object$trees$inwords[[tree]]  # look up tree description
    }

    for (i in 1:length(tree_in_words)) { # for each sentence:

      cat(paste0("[", i, "] ", tree_in_words[i], "\n"))

    }

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


  # Print tables (to Console): ----

  # A. Tree definitions:
  cap_def <- in_blue(paste("Tree", cli::style_underline("definitions.")))
  print(knitr::kable(out$definitions, caption = cap_def))


  # B. Tree stats on training data:

  # Compute criterion baseline/base rate:
  criterion_name <- object$criterion_name
  crit_br <- mean(object$data$train[[criterion_name]])
  crit_val <- scales::percent(crit_br)
  crit_lbl <- object$params$decision.labels[2]

  cap_train <- in_blue(paste0("Tree statistics on ", cli::style_underline("training"), " data [p(", crit_lbl, ") = ", crit_val, "]."))
  print(knitr::kable(out$stats$train, caption = cap_train, digits = digits))

  # C. Tree stats of test data:
  if (is.null(out$stats$test) == FALSE){

    crit_br2 <- mean(object$data$test[[criterion_name]])
    crit_val <- scales::percent(crit_br2)
    # crit_lbl <- object$params$decision.labels[2]

    cap_test <- in_blue(paste0("Tree statistics on ", cli::style_underline("test"), " data [p(", crit_lbl, ") = ", crit_val, "]."))
    print(knitr::kable(out$stats$test, caption = cap_test, digits = digits))
  }


  # Output: ----

  return(invisible(out))

} # summary.FFTrees().


# eof.
