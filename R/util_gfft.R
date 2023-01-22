# util_gfft.R:
# FFT manipulation functions.
# ---------------------------

# A grammar of FFTs:
# Functions for editing and manipulating FFTs


# (A) Tree conversion functions: ------

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
  testthat::expect_true(verify_fft_definitions(ffts))

  testthat::expect_true(is.numeric(tree))
  testthat::expect_true(length(tree) == 1)

  if (!(tree %in% ffts$tree)){
    stop(paste0("No FFT #", tree, " found in ffts"))
  }

  # Main: ----

  # Get 1 line by tree ID (ffts may be unsorted):
  cur_fft <- ffts[(ffts$tree == tree), ]

  # Get elements:
  n_nodes <- cur_fft$nodes

  # Extract elements of definition (as vectors):
  class_v     <- trimws(unlist(strsplit(cur_fft$classes, ";")))
  cue_v       <- trimws(unlist(strsplit(cur_fft$cues, ";")))
  direction_v <- trimws(unlist(strsplit(cur_fft$directions, ";")))
  threshold_v <- trimws(unlist(strsplit(cur_fft$thresholds, ";")))
  exit_v      <- trimws(unlist(strsplit(cur_fft$exits, ";")))

  # Verify that all element lengths correspond to n_nodes:
  v_lengths <- sapply(list(class_v, cue_v, direction_v, threshold_v, exit_v), FUN = length)
  if (!all(n_nodes == v_lengths)) { stop("Some FFT element length(s) differ from N of nodes") }

  # Create 1 FFT (as df):
  fft <- data.frame(classes = class_v,
                    cues = cue_v,
                    directions = direction_v,
                    thresholds = threshold_v,
                    exits = exit_v,
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
#
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

# Similar code is currently used
# - at the end of fftrees_grow_fan()
# - at the end of fftrees_wordstofftrees()

write_fft_df <- function(fft, tree = 101){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_components(fft))

  testthat::expect_true(is.numeric(tree))
  testthat::expect_true(length(tree) == 1)


  # Main: ----

  nodes_n <- nrow(fft)

  fft_in_1_line <- data.frame(
    tree       = as.integer(tree),
    nodes      = nodes_n,
    classes    = paste(fft$classes, collapse = ";"),
    cues       = paste(fft$cues, collapse = ";"),
    directions = paste(fft$directions, collapse = ";"),
    thresholds = paste(fft$thresholds, collapse = ";"),
    exits      = paste(fft$exits, collapse = ";"),
    stringsAsFactors = FALSE
  )

  # Output: ----

  # Convert df to tibble:
  fft_in_1_line <- tibble::as_tibble(fft_in_1_line)

  return(fft_in_1_line)

} # write_fft_df().

# # Check:
# fft_df <- read_fft_df(ffts_df, 3)
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
