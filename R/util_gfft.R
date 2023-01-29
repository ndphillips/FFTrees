# util_gfft.R:
# FFT manipulation functions.
# ---------------------------

# A grammar of FFTs
#
# Functions for converting/translating and editing/manipulating and varying FFTs:
#
# A. Tree translation functions (for converting and collecting FFT definitions/trees)
# B. Tree editing functions (for manipulating individual FFTs)
# C. Macros / combinations (e.g., for creating specific variants of a given FFT)


# (A) Tree conversion/translation functions: --------

# General objective: Convert/translate FFT descriptions (definitions as df, with 1 row per tree)
# into a more modular format of individual FFTs (as df with 1 row per node), and back.
# Reason: The latter can be manipulated more easily in Tree manipulation functions (B).

# Details:
#
# 2 FFT translation functions:
# - read: From multi-FFT df (with 1 row per tree) to 1 FFT df (with 1 row per node),
# - write: back from to 1 FFT df (with 1 row per node) to multi-tree df (with 1 row per tree).
#
# 1 FFT collection function:
# - add_fft_df: Adds definitions (as df) of individual FFTs (as df) to (a set of existing) definitions.

# read_fft_df: ------

# Goal: Extract 1 FFT (as df) from multi-line FFT definitions (as df).
#
# Inputs:
# ffts_df: A set of FFT definitions (as df, usually from an FFTrees object,
#       with suitable variable names to pass verify_fft_definition()).
# tree: A tree ID (corresponding to tree in ffts_df).
#
# Output: A definition of 1 FFT with 1 row per node (as df).

# Currently used to extract individual trees in
# - fftrees_apply()
# - fftrees_ffttowords()


read_fft_df <- function(ffts_df, tree = 1){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_definition(ffts_df)) # verify structure and content

  testthat::expect_true(is.numeric(tree))
  testthat::expect_true(length(tree) == 1)

  if (!(tree %in% ffts_df$tree)){
    stop(paste0("No FFT #", tree, " found in ffts_df"))
  }

  # print(ffts_df)  # 4debugging

  # Get 1 line by tree ID (ffts_df may be unsorted):
  cur_fft <- ffts_df[(ffts_df$tree == tree), ]
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



# write_fft_df: ------


# Goal: Turn 1 FFT (as df) into a line of multi-line FFT definitions (as df).
# Inputs:
# - fft: A definition of 1 FFT (as df, with 1 row per node,
#        and suitable variable names to pass verify_fft_as_df()).
# - tree: tree ID (as integer).
# Output: FFT definition in 1 line (as df).

# Code is currently used at the end of
# - fftrees_grow_fan()         +++ here now +++
# - fftrees_wordstofftrees()   +++ here now +++

write_fft_df <- function(fft, tree = -99L){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft)) # verify structure and content

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
# # 1. converting a line of ffts_df by read_fft_df() into df of 1 FFT and
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



# add_fft_df: ------

# Goal: Add an FFT definition (Case 1) or 1 FFT as df (Case 2) to an existing set of FFT definitions.
# Output: Verified tree definitions of x$trees$definitions (as 1 df); else NA.

add_fft_df <- function(fft, ffts_df = NULL){

  if (verify_fft_definition(fft)){   # Case 1: fft is a (set of) FFT-definitions (in 1 row per tree, as df) ----

    if (is.null(ffts_df)){ # no addend:

      return(fft)  # Output (as is)

    } else { # add to existing FFT definitions of ffts_df:

      if (verify_fft_definition(ffts_df)){

        out_ffts_df <- rbind(ffts_df, fft)  # add to existing definitions

        out_ffts_df$tree <- 1:nrow(out_ffts_df)  # re-set tree numbers

        return(out_ffts_df)  # Output (default)

      }

    }

  } else if (verify_fft_as_df(fft)){ # Case 2: fft is 1 FFT (as df, 1 row per node) ----

    cur_fft <- write_fft_df(fft = fft, tree = 1)

    message("Wrote 1 FFT (from df) to a 1-line definition.")

    add_fft_df(fft = cur_fft, ffts_df = ffts_df)  # re-call (with modified 1st argument)

  } else {

    stop("fft must be a valid FFT definition (as df) or a valid FFT (as df)")

  }

} # add_fft_df().

# # Check:
# (ffts_df <- get_fft_definitions(x))
# (fft <- read_fft_df(ffts_df, tree = 2))
#
# # Baselines:
# add_fft_df(ffts_df)  # Case 0a: Add a set of definitions to NULL.
# add_fft_df(fft)   # Case 0c: Add 1 FFT (as df) to NULL.
#
# # Intended use:
# add_fft_df(ffts_df[2, ], ffts_df)  # Case 1: Add 1 definition to a set of definitions.
# add_fft_df(fft, ffts_df)     # Case 2: Add 1 FFT (as df) to a set of definitions.





# (B) Editing tree descriptions: --------


# Goals: Functions for converting, manipulating, and processing
#        (e.g., storing) FFT descriptions.

# Note: These functions assume 1 FFT (in multi-line format, as df) as their 1-st input argument (for piping)
#       and return a modified version of 1 FFT (in same format) as output.


# reorder_nodes: ------


# Goal: Re-order the nodes of an existing FFT.
# Inputs:
#  fft: 1 FFT (in multi-line format, as df)
#  order: desired order of cues (as a numeric vector of 1:n_cues)
# Output:
# fft_mod: modified FFT (in multi-line format, as df)


reorder_nodes <- function(fft, order = NA){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft)) # verify structure and content
  n_cues <- nrow(fft)

  if (all(is.na(order))){
    order <- 1:n_cues  # default: no change
  }

  testthat::expect_true(is.numeric(order))
  testthat::expect_true(n_cues == length(order),
                        info = paste0("FFT has ", n_cues, " cues, but order has length ", length(order)))
  testthat::expect_true(all.equal(sort(order), 1:n_cues),
                        info = paste0("order must be a permutation of the cues in fft"))


  if (all(order == 1:n_cues)) { # catch case:

    message("reorder_nodes: fft remains unchanged")  # 4debugging

    return(fft)

  }


  # Main: ----

  # Re-order rows:
  fft_mod <- fft[order, ]

  # IF the exit cue has been changed:
  exit_cue_pos <- which(order == n_cues)

  if (exit_cue_pos != n_cues){

    message(paste0("reorder_nodes: Previous exit cue moved to cue position ", exit_cue_pos))

    # Option 1:
    # Current direction and threshold settings always predict Signal (1):

    # a. previous exit cue: Decide/predict 1 (signal)
    fft_mod$exit[exit_cue_pos] <- 1

    # b. final cue: Make new exit (0.5)
    fft_mod$exit[n_cues] <- 0.5

    # Option 2:
    # Goal: Preserve the overall tree structure:
    #       Swap exit directions of previous and new exit cues!
    #       (Requires flipping directions when predicting NON-signal direction 0)

    # Option 3:
    # Goal: Provide BOTH noise (0: left) and signal (1: right) alternatives.

  } # if (exit_cue_pos).


  # Repair row names:
  row.names(fft_mod) <- 1:nrow(fft_mod)


  # Output: ----

  return(fft_mod)

} # reorder_nodes().


# # Check:
# ffts_df <- get_fft_definitions(x)  # x$trees$definitions / definitions (as df)
# fft_df  <- read_fft_df_v0(ffts_df, tree = 5)
# fft_df
#
# plot(x, tree = 5)
# x$trees$definitions[5, ]
# x$trees$inwords[5]
#
# reorder_nodes(fft_df)  # unchanged
# reorder_nodes(fft_df, order = c(1, 2, 3))  # unchanged
# reorder_nodes(fft_df, order = c(3, 2))     # ERROR: wrong length of order
# reorder_nodes(fft_df, order = c(2, 1, 3))  # exit cue unchanged
# reorder_nodes(fft_df, order = c(1, 3, 2))  # exit cue changed



# flip_exit: ------


# Goal: Flip the exits (i.e., cue direction and exit type) of some FFT's (non-final) nodes.
# Inputs:
#   fft = 1 FFT (as df)
#   nodes = vector of nodes to swap (as integer in 1:nrow(fft))
# Output: Modified version of fft (as df, with flipped cue directions/exits)


flip_exit <- function(fft, nodes = NA){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft))

  if (all(is.na(nodes))) { # catch case:

    message("flip_exit: fft remains unchanged")  # 4debugging

    return(fft)

  }

  nodes <- as.integer(nodes)
  testthat::expect_true(is.integer(nodes), info = "nodes must be an integer vector")

  n_cues <- nrow(fft)

  if (any(nodes %in% 1:n_cues == FALSE)){
    stop("Some nodes do not occur in fft.")
  }

  if (n_cues %in% nodes){
    msg <- paste0("The final cue ", n_cues, " cannot be flipped and will be ignored...")
    message(msg)
    nodes <- nodes[nodes != n_cues]
  }


  # Main: ----

  # For current nodes:

  # ERROR: Cue direction is ALWAYS for criterion = TRUE/signal/1 (on right) => Must not be flipped when exit changes!
  # # 1. flip cue direction:
  # fft$direction[nodes] <- directions_df$negation[match(fft$direction[nodes], table = directions_df$direction)]

  # 2. swap exit:
  fft$exit[nodes] <- ifelse(fft$exit[nodes] == 1, 0, 1)


  # Output: ----

  return(fft)

} # flip_exit().

# # Check:
# ffts_df <- get_fft_definitions(x)  # x$trees$definitions / definitions (as df)
# (fft <- read_fft_df(ffts_df, tree = 2))  # 1 FFT (as df, from above)
#
# flip_exit(fft)
# flip_exit(fft, nodes = c(1))
# flip_exit(fft, nodes = c(3))
# flip_exit(fft, nodes = c(3, 1))
#
# # Note:
# flip_exit(fft, nodes = 4)
# flip_exit(fft, nodes = 1:4)





# (C) Macros / Combinations of tree editing functions: --------

# Conundrums:
# - What makes 2 FFTs "similar" (in which respect) or belong to the same "family"?
# - What is the "identity" of an FFT?
#
# A clear demarcation: Using different cues corresponds to different trees.
# However, if we define a "family of FFTs" as using a fixed set of cues,
# we still allow for a considerable variation/range of options.
#
# Possible differences/similarities within a "family of FFTs" (from narrow to wide):
# 1. A specific set of cues, their order, and exit structure:  1 FFT from FFTrees object.
# 2. A specific set of cues, their order, but variable exit structures:  The FFTs in 1 FFTrees object.
# 3. A specific set of cues, but variable cue orders and exit structures.


# all_node_orders: ------


# Goal: Apply reorder_nodes(fft) to all possible permutations of cues.
# Input:
#   fft: 1 FFT (as df, 1 row per cue)
# Output:
#   Definitions of FFT in all possible cue orders (predicting 1/Signal/TRUE for all changed cues, as reorder_nodes())


all_node_orders <- function(fft){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft))

  # Initialize:
  out <- NULL
  n_cues <- nrow(fft)

  # Main: ----

  all_orders <- all_permutations(1:n_cues)
  # print(all_orders)  # 4debugging

  for (i in 1:nrow(all_orders)){

    cur_fft <- reorder_nodes(fft = fft, order = all_orders[i, ])

    cur_fft_def <- write_fft_df(fft = cur_fft, tree = as.integer(i))

    out <- add_fft_df(fft = cur_fft_def, ffts_df = out)

  }

  # Output: ----

  return(out)

} # all_node_orders().


# Check:
# ffts_df <- get_fft_definitions(x)  # x$trees$definitions / definitions (as df)
# fft  <- read_fft_df(ffts_df, tree = 1)  # 1 FFT (as df, from above)

# (dfs_1 <- all_node_orders(fft = read_fft_df(ffts_df, tree = 1)))
# (dfs_2 <- all_node_orders(fft = read_fft_df(ffts_df, tree = 2)))



# all_exit_structures: ------


# Goal: Get all 2^(n-1) possible exit structures for an FFT with n cues.
# Method: Use flip_exit() on nodes = `all_combinations()` for all length values of 1:(n_cues - 1).
# Input: fft: 1 FFT (as df, 1 row per cue)


all_exit_structures <- function(fft){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft))

  # Initialize:
  out <- NULL
  cnt <- 1

  n_cues <- nrow(fft)

  # Main: ----

  cur_fft_def <- write_fft_df(fft = fft, tree = as.integer(cnt))

  out <- add_fft_df(fft = cur_fft_def, ffts_df = out)

  if (n_cues > 1){

    n_non_exit_cues <- (n_cues - 1)

    for (i in 1:n_non_exit_cues){

      comb_i <- all_combinations(1:n_non_exit_cues, length = i)
      # print(comb_i)  # 4debugging

      for (j in 1:nrow(comb_i)){

        cur_fft <- flip_exit(fft = fft, nodes = comb_i[j, ])

        cnt <- cnt + 1

        cur_fft_def <- write_fft_df(fft = cur_fft, tree = as.integer(cnt))

        out <- add_fft_df(fft = cur_fft_def, ffts_df = out)

      } # for j.

    } # for i.

  } # if (n_cues > 1).


  # Output: ----

  return(out)

} # all_exit_structures().


# # Check:
# ffts_df <- get_fft_definitions(x)  # x$trees$definitions / definitions (as df)
# fft  <- read_fft_df(ffts_df, tree = 1)  # 1 FFT (as df, from above)
#
# (dfs_3 <- all_exit_structures(fft = fft))
# (dfs_4 <- all_exit_structures(fft = read_fft_df(ffts_df, tree = 2)))



# ToDo: ------

# - Make some functions (e.g., tree editing functions) work alternative inputs of either
#   (1) FFT definitions (df, 1 row per tree) OR
#   (2) single FFTs (as df, 1 row per node).
# - Return the result in the same format as the input.
# - When entering a set of FFT definitions, return modified set?

# eof.
