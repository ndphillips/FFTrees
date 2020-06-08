#' Rank trees by goal
#'
#' @param x FFTrees.
#' @param data character.
#'
#'
fftrees_ranktrees <- function(x,
                              data = "train") {

  tree_stats <- x$trees$stats[[data]]

# Sort trees by goal

if(x$params$goal == "cost") {

  tree_rank <- rank(tree_stats$cost, ties.method = "first")

} else {

  tree_rank <- rank(-tree_stats[[x$params$goal]], ties.method = "first")

}

# Get tree rankings by goal

tree_rank_df <- data.frame(tree = 1:nrow(tree_stats),
                           tree_new = tree_rank) %>%
  dplyr::arrange(tree_new)

# Update

x$trees$definitions <- x$trees$definitions %>%
  dplyr::left_join(tree_rank_df, by = "tree") %>%
  dplyr::select(-tree) %>%
  dplyr::rename(tree = tree_new) %>%
  dplyr::select(tree, dplyr::everything()) %>%
  dplyr::arrange(tree) %>%
  tibble::as_tibble()

x$trees$stats$train <- x$trees$stats$train %>%
  dplyr::left_join(tree_rank_df, by = "tree") %>%
  dplyr::select(-tree) %>%
  dplyr::rename(tree = tree_new) %>%
  dplyr::select(tree, dplyr::everything()) %>%
  dplyr::arrange(tree) %>%
  tibble::as_tibble()

x$trees$level_stats$train <- x$trees$level_stats$train %>%
  dplyr::left_join(tree_rank_df, by = "tree") %>%
  dplyr::select(-tree) %>%
  dplyr::rename(tree = tree_new) %>%
  dplyr::select(tree, dplyr::everything()) %>%
  dplyr::arrange(tree, level) %>%
  tibble::as_tibble()


x$trees$decisions$train <- x$trees$decisions$train[tree_rank_df$tree]
names(x$trees$decisions$train) <- paste0("tree_", 1:nrow(tree_rank_df))

return(x)

}


