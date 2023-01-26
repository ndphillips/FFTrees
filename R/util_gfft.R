# util_gfft.R:
# FFT manipulation functions.
# ---------------------------

# A grammar of FFTs
#
# Functions for converting/translating and editing/manipulating FFTs:
#
# A. Tree translation functions for more modular elements.
# B. Tree manipulation functions for editing individual FFTs.


# (0) Define global constants: ------


# - Node separation marker (symbol): ----

fft_node_sep <- ";"  # (global constant)


# - Direction markers (symbols/words): ----

direction_same <- c("equal", "identical", "same")
direction_diff <- c("unequal", "different", "differs")

direction_more <- c("more", "larger", "bigger", "greater", "above", "beyond", "exceed")
direction_less <- c("less", "lower", "smaller", "fewer", "below")

directions_df <- data.frame(
  direction   = c("=",  ">", ">=", "<",  "<=", "!=",
                  direction_same,
                  direction_diff,
                  direction_more,
                  direction_less),
  direction_f = c("=",  ">",  ">=", "<",  "<=", "!=",
                  rep("=",  length(direction_same)),    # same
                  rep("!=", length(direction_diff)),    # diff
                  rep(">",  length(direction_more)),    # more
                  rep("<",  length(direction_less))),   # less
  negation    = c("!=", "<=", "<",  ">=", ">",  "=",
                  rep("!=",  length(direction_same)),   # NOT same
                  rep("=",   length(direction_diff)),   # NOT diff
                  rep("<=",  length(direction_more)),   # NOT more
                  rep(">=",  length(direction_less))),  # NOT less
  #
  stringsAsFactors = FALSE)  # (global constant)


# - Negation markers: ----

negations_v <- c("not", "is not")  # (global constant)



# (A) Tree conversion/translation functions: ------

# Goals: Two translation functions:
# - read: From multi-FFT df (with 1 row per tree) to 1 FFT df (with 1 row per node),
# - write: back from to 1 FFT df (with 1 row per node) to multi-tree df (with 1 row per tree).


# 1. read_fft_df: ------

# Goal: Extract 1 FFT (as df) from multi-line FFT definitions (as df).
#
# Inputs:
# ffts: A set of FFT definitions (as df, usually from an FFTrees object,
#       with suitable variable names to pass verify_fft_definitions()).
# tree: A tree ID (corresponding to tree in ffts).
#
# Output: A definition of 1 FFT with 1 row per node (as df).

# Currently used to extract individual trees in
# - fftrees_apply()
# - fftrees_ffttowords()

read_fft_df <- function(ffts, tree = 1){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_definitions(ffts)) # verify structure and content

  testthat::expect_true(is.numeric(tree))
  testthat::expect_true(length(tree) == 1)

  if (!(tree %in% ffts$tree)){
    stop(paste0("No FFT #", tree, " found in ffts"))
  }

  # print(ffts)  # 4debugging

  # Get 1 line by tree ID (ffts may be unsorted):
  cur_fft <- ffts[(ffts$tree == tree), ]
  # print(cur_fft)  # 4debugging

  # Key values:
  # fft_node_sep <- ";"  # (local constant)
  n_nodes <- cur_fft$nodes


  # Main: ----

  # Extract elements of definition (as vectors):
  classes    <- trimws(unlist(strsplit(cur_fft$classes,    split = fft_node_sep, fixed = TRUE)))
  cues       <- trimws(unlist(strsplit(cur_fft$cues,       split = fft_node_sep, fixed = TRUE)))
  directions <- trimws(unlist(strsplit(cur_fft$directions, split = fft_node_sep, fixed = TRUE)))
  thresholds <- trimws(unlist(strsplit(cur_fft$thresholds, split = fft_node_sep, fixed = TRUE)))
  exits      <- trimws(unlist(strsplit(cur_fft$exits,      split = fft_node_sep, fixed = TRUE)))

  # Verify that the vector lengths (of tree definition parts) correspond to n_nodes:
  v_lengths <- sapply(list(classes, cues, directions, thresholds, exits), FUN = length)

  if (!all(v_lengths == n_nodes)) { # note error:

    # Determine vectors with lengths differing from n_nodes:
    req_tvec_names <- c("classes", "cues", "directions", "thresholds", "exits")  # [mostly plural]
    ixs_with_diffs <- v_lengths != n_nodes  # name indices
    tvec_diffs_col <- paste(req_tvec_names[ixs_with_diffs], collapse = ", ")
    vlen_diffs_col <- paste(v_lengths[ixs_with_diffs], collapse = ", ")

    msg <- paste0("The lengths of some FFT definition parts differ from n_nodes = ", n_nodes, ":\n  names of v = (", tvec_diffs_col, "), v_lengths = (", vlen_diffs_col, ").")
    stop(msg)

  }


  # Create 1 FFT (as df):
  fft <- data.frame(class = classes,
                    cue = cues,
                    direction = directions,
                    threshold = thresholds,
                    exit = exits,
                    stringsAsFactors = FALSE)

  # Output: ----

  return(fft) # (df with 1 row per node)

} # read_fft_df().

# # Check:
# hd <- FFTrees(formula = diagnosis ~ .,
#                 data = heart.train,
#                 data.test = heart.test)
# x <- hd  # copy object (with 7 FFTs)
#
# # FFT definitions:
# ffts_df <- get_fft_definitions(x)  # using the helper function
# ffts_df

# read_fft_df(ffts_df, 2)
# read_fft_df(ffts_df, 2:3)  # yields error
# read_fft_df(ffts_df, 8)    # yields error



# 2. write_fft_df: ------


# Goal: Turn 1 FFT (as df) into a line of multi-line FFT definitions (as df).
# Inputs:
# - fft: A definition of 1 FFT (as df, with 1 row per node,
#        and suitable variable names to pass verify_fft_components()).
# - tree: tree ID (as integer).
# Output: FFT definition in 1 line (as df).

# Code is currently used at the end of
# - fftrees_grow_fan()         +++ here now +++
# - fftrees_wordstofftrees()   +++ here now +++

write_fft_df <- function(fft, tree = -99L){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_components(fft)) # verify structure and content

  testthat::expect_true(is.numeric(tree))
  testthat::expect_true(length(tree) == 1)


  # Key values:
  # fft_node_sep <- ";"  # (local constant)
  nodes_n <- nrow(fft)


  # Main: ----

  fft_in_1_line <- data.frame(
    # Add. variables:
    tree       = as.integer(tree),
    nodes      = as.integer(nodes_n),
    # Key variables of fft (all plural):
    classes    = paste(substr(fft$class, 1, 1), collapse = fft_node_sep),
    cues       = paste(fft$cue,                 collapse = fft_node_sep),
    directions = paste(fft$direction,           collapse = fft_node_sep),
    thresholds = paste(fft$threshold,           collapse = fft_node_sep),
    exits      = paste(fft$exit,                collapse = fft_node_sep),
    #
    stringsAsFactors = FALSE
  )

  # Output: ----

  # Convert df to tibble:
  # fft_in_1_line <- tibble::as_tibble(fft_in_1_line)

  return(fft_in_1_line)

} # write_fft_df().

# # Check:
# fft_df <- read_fft_df(ffts_df, 3)  # from above
# fft_df
#
# write_fft_df(fft_df, tree = 123)



# # Check:
# # Verify that read_fft_df() and write_fft_df() are complementary functions:
#
# # Show that:
# # 1. converting a line of ffts by read_fft_df() into df of 1 FFT and
# # 2. re-converting df back into 1 row per tree by write_fft_df()
# # yields original line:
#
# for (id in 1:nrow(ffts_df)){
#
#   check <- all.equal(ffts_df[id, ], write_fft_df(read_fft_df(ffts_df, id), id))
#
#   print(paste0("\u2014 tree id = ", id, ": ", check))
#
# } # loop, qed.




# ToDo: ------

# - etc.

# eof.
