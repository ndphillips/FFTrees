#' Rank trees by goal
#'
#' @param x FFTrees.
#'
#'
fftrees_ranktrees <- function(x, data = "train") {



  tree_stats <- x$trees$results[[data]]$stats

# Sort trees by goal

if(x$params$goal == "cost") {

  tree_rank <- rank(tree_stats$cost, ties.method = "first")

} else {

  tree_rank <- rank(-tree_stats[[x$params$goal]], ties = "first")

}

# Get tree rankings by goal

tree_rank_df <- data.frame(tree = 1:nrow(tree_stats),
                           tree_new = tree_rank)

# Update

tree.definitions_ranked <- x$trees$definitions %>%
  dplyr::left_join(tree_rank_df, by = "tree") %>%
  dplyr::select(-tree) %>%
  dplyr::rename(tree = tree_new) %>%
  dplyr::select(tree, dplyr::everything()) %>%
  dplyr::arrange(tree) %>%
  tibble::as_tibble()

tree_stats_ranked <- x$trees$results$train$stats %>%
  dplyr::left_join(tree_rank_df, by = "tree") %>%
  dplyr::select(-tree) %>%
  dplyr::rename(tree = tree_new) %>%
  dplyr::select(tree, dplyr::everything()) %>%
  dplyr::arrange(tree) %>%
  tibble::as_tibble()

level_stats_ranked <- x$trees$results$train$level_stats %>%
  dplyr::left_join(tree_rank_df, by = "tree") %>%
  dplyr::select(-tree) %>%
  dplyr::rename(tree = tree_new) %>%
  dplyr::select(tree, dplyr::everything()) %>%
  dplyr::arrange(tree, level) %>%
  tibble::as_tibble()

decisions_ranked <- x$trees$results$train$decisions %>%
  dplyr::select(tree_rank_df$tree_new) %>%
  dplyr::rename_at(dplyr::vars(1:x$trees$n), function(i) {paste0("tree.", 1:x$trees$n)})

levelout_ranked <- x$trees$results$train$levelout %>%
  dplyr::select(tree_rank_df$tree_new) %>%
  dplyr::rename_at(dplyr::vars(1:x$trees$n), function(i) {paste0("tree.", 1:x$trees$n)})

cost_decisions_ranked <- x$trees$results$train$cost_decisions %>%
  dplyr::select(tree_rank_df$tree_new) %>%
  dplyr::rename_at(dplyr::vars(1:x$trees$n), function(i) {paste0("tree.", 1:x$trees$n)})

cost_cues_ranked <- x$trees$results$train$cost_cues %>%
  dplyr::select(tree_rank_df$tree_new) %>%
  dplyr::rename_at(dplyr::vars(1:x$trees$n), function(i) {paste0("tree.", 1:x$trees$n)})

cost_ranked <- x$trees$results$train$cost %>%
  dplyr::select(tree_rank_df$tree_new) %>%
  dplyr::rename_at(dplyr::vars(1:x$trees$n), function(i) {paste0("tree.", 1:x$trees$n)})

# Add results to x -------------------------

x$trees$definitions <- tree.definitions_ranked
x$trees$results[[data]]$stats <- tree_stats_ranked
x$trees$results[[data]]$level_stats <- level_stats_ranked
x$trees$results[[data]]$decisions <- decisions_ranked
x$trees$results[[data]]$levelout <- levelout_ranked
x$trees$results[[data]]$cost_decisions <- cost_decisions_ranked
x$trees$results[[data]]$cost_cues <- cost_cues_ranked
x$trees$results[[data]]$cost <- cost_ranked

# Calculate best tree and save in x$trees$best ==============================

if(x$params$goal == "cost") {

  best_tree <- x$trees$results[[data]]$stats$tree[x$trees$results[[data]]$stats[[x$params$goal]] == min(x$trees$results[[data]]$stats[[x$params$goal]])]

} else {

  best_tree <- x$trees$results[[data]]$stats$tree[x$trees$results[[data]]$stats[[x$params$goal]] == max(x$trees$results[[data]]$stats[[x$params$goal]])]
}

if(1 %in% best_tree == FALSE) {

  stop("Something is weird, the best tree is not labelled as number 1 (and it should be)")
}

# If multiple, take the simpler tree (with fewer nodes)

if(length(best_tree) > 1) {

  best_tree <- best_tree[1]

  # best_tree <- x$trees$definitions %>%
  #   dplyr::filter(tree %in% best_tree) %>%
  #   dplyr::arrange(nodes) %>%
  #   dplyr::slice(1) %>%
  #   dplyr::pull(tree)

}

x$trees$best[[data]] <- best_tree

return(x)

}


