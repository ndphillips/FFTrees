#' Rank FFTs by current goal
#'
#' @description \code{fftrees_ranktrees} ranks trees in an \code{FFTrees} object \code{x}
#' based on the current goal (either \code{"cost"} or as specified in \code{x$params$goal}).
#'
#' \code{fftrees_ranktrees} is called by the main \code{\link{FFTrees}} function
#' when creating FFTs from and applying them to data.
#'
#' @param x An \code{FFTrees} object.
#' @param data character. Default is \code{data = "train"}.
#'
#' @seealso
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.

fftrees_ranktrees <- function(x,
                              data = "train") {

  # Initialize: ----

  tree_stats <- x$trees$stats[[data]]


  # 1. Sort trees by goal: ----

  if (x$params$goal == "cost") {  # rank by cost:
    tree_rank <- rank(tree_stats$cost, ties.method = "first")
  } else { # rank by current goal:
    tree_rank <- rank(-tree_stats[[x$params$goal]], ties.method = "first")
  }


  # 2. Get tree rankings by goal (as df): ----

  tree_rank_df <- data.frame(
    tree = 1:nrow(tree_stats),
    tree_new = tree_rank
  ) %>%
    dplyr::arrange(tree_new)


  # 3. Update elements of FFTrees x: ----

  # Tree definitions:
  x$trees$definitions <- x$trees$definitions %>%
    dplyr::left_join(tree_rank_df, by = "tree") %>%
    dplyr::select(-tree) %>%
    dplyr::rename(tree = tree_new) %>%
    dplyr::select(tree, dplyr::everything()) %>%
    dplyr::arrange(tree) %>%
    tibble::as_tibble()

  # For training data: ----
  if (data == "train"){

  # Training stats:
  x$trees$stats$train <- x$trees$stats$train %>%
    dplyr::left_join(tree_rank_df, by = "tree") %>%
    dplyr::select(-tree) %>%
    dplyr::rename(tree = tree_new) %>%
    dplyr::select(tree, dplyr::everything()) %>%
    dplyr::arrange(tree) %>%
    tibble::as_tibble()

  # Training level_stats:
  x$trees$level_stats$train <- x$trees$level_stats$train %>%
    dplyr::left_join(tree_rank_df, by = "tree") %>%
    dplyr::select(-tree) %>%
    dplyr::rename(tree = tree_new) %>%
    dplyr::select(tree, dplyr::everything()) %>%
    dplyr::arrange(tree, level) %>%
    tibble::as_tibble()

  # Training decisions:
  x$trees$decisions$train <- x$trees$decisions$train[tree_rank_df$tree]
  names(x$trees$decisions$train) <- paste0("tree_", 1:nrow(tree_rank_df))

  # Best training tree:
  x$trees$best$train <- select_best_tree(x, data = "train", goal = x$params$goal)

  } # if (data == "train").


  # Note: The analog (data == "test") case is currently NOT ranked.


  # Output: ----

  return(x)

} # fftrees_ranktrees().

# eof.
