context("Get, edit, and use tree.definitions")

# Create FFTs from tree.definitions and edited tree.definitions:

test_that("Can create, extract, edit, add, and evaluate FFTs from tree.definitions", {

  # 1. Create FFTrees object x for iris data: ------
  x <- FFTrees(formula = virginica ~ .,
               data = iris.v,
               main = "Iris viginica",
               decision.labels = c("Not-Vir", "Vir"),
               quiet = TRUE)



  # 2. Get tree definitions: ------

  # Get tree definitions of x (as non-tidy df):
  tree_dfs <- get_fft_df(x)

  # tree_dfs  # 6 original tree definitions



  # 3. Extract individual tree definitions: ------

  # Get specific trees (each tree as 1 tidy df):
  fft_1 <- read_fft_df(ffts_df = tree_dfs, tree = 1)
  fft_3 <- read_fft_df(ffts_df = tree_dfs, tree = 3)



  # 4. Edit individual tree definitions: ------

  # Reorder nodes:
  my_fft_1 <- reorder_nodes(fft = fft_1, order = c(2, 1), quiet = TRUE)     # reverse cues
  my_fft_2 <- reorder_nodes(fft = fft_3, order = c(2, 1, 3), quiet = TRUE)  # no new exit node
  my_fft_3 <- reorder_nodes(fft = fft_3, order = c(1, 3, 2), quiet = TRUE)  # new exit node

  # Flip exits:
  my_fft_4 <- flip_exits(my_fft_1, nodes = 1, quiet = TRUE)           # flip exits of node 1
  my_fft_5 <- flip_exits(my_fft_2, nodes = c(1, 2, 3), quiet = TRUE)  # flip only exits of node 1 and 2

  # Drop nodes:
  my_fft_1 <- drop_nodes(my_fft_1, nodes = 2, quiet = TRUE)  # drop exit node
  my_fft_2 <- drop_nodes(my_fft_2, nodes = 2, quiet = TRUE)  # drop non-exit node

  # Edit nodes:
  my_fft_3 <- edit_nodes(my_fft_3,                           # edit 2 nodes:
                         nodes = c(1, 2),
                         direction = c("<", "<="),
                         threshold = c(5, 6),
                         exit = c(1, 0),
                         quiet = TRUE)

  # Add nodes:
  my_fft_4 <- add_nodes(my_fft_4, nodes = 2, class = "n", cue = "sep.len", direction = "<=", threshold = "5", exit = 0, quiet = TRUE)  # new 2nd node
  my_fft_5 <- add_nodes(my_fft_5, nodes = 4, class = "n", cue = "sep.len", direction = ">", threshold = "5", exit = .5, quiet = TRUE)  # new final node



  # 5. Convert and add/collect/gather tree definitions: ------

  # Write FFT definition (into non-tidy df):
  my_tree_dfs <- write_fft_df(my_fft_1, tree = 1)

  # Add other trees (using pipes):
  my_tree_dfs <- my_fft_2 |> write_fft_df(tree = 2) |> add_fft_df(my_tree_dfs)
  my_tree_dfs <- my_fft_3 |> write_fft_df(tree = 3) |> add_fft_df(my_tree_dfs)
  my_tree_dfs <- my_fft_4 |> write_fft_df(tree = 4) |> add_fft_df(my_tree_dfs)
  my_tree_dfs <- my_fft_5 |> write_fft_df(tree = 5) |> add_fft_df(my_tree_dfs)

  # my_tree_dfs  # => 5 new tree definitions

  # add set of new trees to old ones:
  all_fft_dfs <- add_fft_df(my_tree_dfs, tree_dfs)
  # all_fft_dfs  # contains 6 old and 5 new tree definitions (re-numbering new ones)



  # 6. Apply tree.definitions to data: ------

  # a: Evaluate new tree.definitions for an existing FFTrees object x:
  y <- FFTrees(object = x,                      # existing FFTrees object x
               tree.definitions = all_fft_dfs,  # set of all FFT definitions
               main = "Iris y",                 # new label
               quiet = TRUE
  )


  # b: Create a new FFTrees object z (using formula and original data):
  z <- FFTrees(formula = virginica ~ .,
               data = iris.v,                   # using original data
               tree.definitions = all_fft_dfs,  # set of all FFT definitions
               main = "Iris z",                 # new label
               quiet = TRUE
  )


  # 7. Compare results: ------
  # summary(y)
  # summary(z)



  # 8. Tests: ------

  testthat::expect_is(y, "FFTrees")
  testthat::expect_is(z, "FFTrees")


})


# eof.
