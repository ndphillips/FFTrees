# util_gfft.R:
# FFT manipulation functions.
# ---------------------------

# A grammar of FFTs
#
# Functions for converting/translating and editing/manipulating/trimming and varying FFTs:
#
# A. Tree translation functions (for converting and collecting FFT definitions/trees)
# B. Tree editing/trimming functions (for changing and manipulating individual FFTs)
# C. Macros / combinations (e.g., for creating sets of variants of a given FFT)



# (A) Tree conversion/translation functions: --------

# Goal: Convert/translate FFT descriptions (definitions as df, with 1 row per tree)
# into a more modular format of individual FFTs (as df with 1 row per node), and back.
# Reason: The latter can be manipulated more easily by tree editing/manipulation/trimming functions (B).

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
#
# Note: Currently used to extract individual trees in
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
# ffts_df <- get_fft_df(x)  # using the helper function
# ffts_df
#
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
#
# Note: Code is currently used at the end of
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
#
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
# (ffts_df <- get_fft_df(x))
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

# Goal: Functions for editing, manipulating, and trimming individual FFTs (in df format).

# Note: These functions assume 1 FFT (in multi-line format, as df) as their 1-st input argument (for piping)
#       and return a modified version of 1 FFT (in the same format) as output.




# add_nodes: ------

# Goal: Add some node(s) (or cues) to a given FFT.
#
# Inputs:
#   fft = 1 FFT (as df)
#   nodes = vector of added node positions (as integer from 1 to nrow(fft) + length(nodes))
#   class = class of cue to add
#   cue = cue to add
#   direction = direction to change
#   threshold = threshold values to change
#   exit = exit to change
#   my.node = a vector of verbal node descriptions (dominates all other arguments).
#
# Output: Modified version of fft (as df, but with modified nodes).
#
# Note: As edit_nodes() is ignorant of data, the values of class, cue, and threshold
#       are currently NOT validated for a specific set of data.

add_nodes <- function(fft,
                      nodes = NA,  # as vector (1 integer or multiple nodes)
                      class = NA, cue = NA, direction = NA, threshold = NA, exit = NA,  # variables of fft nodes (as df rows)
                      my.node = NA){

  # Prepare: ------

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft))

  if (!all(is.na(direction))){
    testthat::expect_true(verify_dir_sym(direction))
  }

  if (all(is.na(nodes))) { # catch case 0:

    message("add_nodes: FFT remains unchanged")  # 4debugging

    return(fft)

  }

  nodes <- as.integer(nodes)
  testthat::expect_true(is.integer(nodes), info = "nodes must be an integer vector")

  # Do all args have same length:
  key_args <- list(nodes, class, cue, direction, threshold, exit, my.node)
  non_NA_args  <- (is.na(key_args) == FALSE)
  len_set_args <- sapply(X = key_args, FUN = length)[non_NA_args]

  if (length(unique(len_set_args)) > 1){
    stop("edit_nodes: All modifier args (e.g., nodes, ..., exit) must have the same length.")
  }

  n_cues  <- nrow(fft)         # N of existing fft nodes
  n_nodes <- length(nodes)     # N of new nodes
  n_total <- n_cues + n_nodes  # N of nodes defined for new fft

  # Special case 1:
  if (max(nodes) > n_total){

    msg <- paste0("add_nodes: Maximum node value (", max(nodes), ") > total of defined nodes (", n_total, ")")
    message(msg)

    # Adjusting values of nodes beyond n_total:
    ix_decr <- (nodes > n_total)
    nodes[ix_decr] <- (n_total - sum(ix_decr) + 1):n_total

    message("Adjusted nodes to ", paste(nodes, collapse = ", "))

  }

  # Special case 2:
  if (any(duplicated(nodes))){

    # Find the last ix of each unique node:
    uni_nodes <- unique(nodes)
    last_node_ix <- rep(NA, length(uni_nodes))

    for (i in seq_along(uni_nodes)){ # loop though unique nodes:

      cur_node <- uni_nodes[i]

      last_node_ix[i] <- max(which(nodes == cur_node))  # last ix

    }

    # Remove duplicated nodes:
    nodes <- nodes[last_node_ix]

    message(paste0("add_nodes: Removing duplicate nodes leaves nodes = ", paste0(nodes, collapse = ", ")))

    # Adjust other inputs accordingly:
    class <- class[last_node_ix]
    cue <- cue[last_node_ix]
    direction <- direction[last_node_ix]
    threshold <- threshold[last_node_ix]
    exit <- exit[last_node_ix]

    n_nodes <- length(nodes)     # N of new nodes
    n_total <- n_cues + n_nodes  # N of nodes defined for new fft

  }


  # Main: ------

  new_row <- rep(NA, n_total)
  fft_new <- data.frame(class = new_row,
                        cue = new_row,
                        direction = new_row,
                        threshold = new_row,
                        exit = new_row)

  cue_i  <- 1
  ix_old <- cue_i:n_cues

  for (i in seq_along(nodes)){ # loop through nodes:

    # print(paste0("i = ", i)) # 4debugging

    node_i <- nodes[i]
    # print(paste0("node_i = ", node_i)) # 4debugging

    if (node_i <= max(ix_old)){

      ix_inc <- (node_i <= ix_old)  # indices to increment

      ix_old[ix_inc] <- ix_old[ix_inc] + 1  # increment indices
      # print(ix_old) # 4debugging

      # cue_i <- cue_i + 1
      # print(paste0("cue_i = ", cue_i)) # 4debugging

    }

  } # for (i in nodes).

  # print(ix_old) # 4debugging

  # Fill fft_new:
  fft_new[ix_old, ] <- fft[1:n_cues, ]  # original nodes/cues/rows

  # New nodes/rows:
  fft_new[nodes, ]  <- data.frame(class = class,
                                  cue = cue,
                                  direction = direction,
                                  threshold = threshold,
                                  exit = exit)

  # +++ here now +++

  # ToDo: Add verification of inputs and output.


  # Output: ------

  # # Repair row names:
  # row.names(fft_new) <- 1:nrow(fft_new)

  # ToDo: Verify fft_new?

  return(fft_new)

} # add_nodes().

# # Check:
# (ffts_df <- get_fft_df(x))  # x$trees$definitions / definitions (as df)
# (fft <- read_fft_df(ffts_df, tree = 1))  # 1 FFT (as df, from above)
#
# add_nodes(fft)
# add_nodes(fft, nodes = 1,
#           class = "class_1", cue = "cue_1", direction = "dir_1", threshold = "thr_1", exit = "ext_1")
# add_nodes(fft, nodes = 2,
#           class = "class_2", cue = "cue_2", direction = "dir_2", threshold = "thr_2", exit = "ext_2")
# add_nodes(fft, nodes = 4,
#           class = "class_4", cue = "cue_4", direction = "dir_4", threshold = "thr_4", exit = "ext_4")
#
# my_nodes <- c(1, 2, 4, 6, 8:10, 50, 100)
#
# add_nodes(fft, nodes = my_nodes,
#           class = paste0("class_", my_nodes),
#           cue = paste0("cue_", my_nodes),
#           direction = paste0("dir_", my_nodes),
#           threshold = paste0("thr_", my_nodes),
#           exit = paste0("ext_", my_nodes)
#           )
#
# my_nodes <- c(2, 4, 2, 4)
# my_value <- 1:4
#
# add_nodes(fft, nodes = my_nodes,
#           class = paste0("class_", my_value),
#           cue = paste0("cue_", my_value),
#           direction = paste0("dir_", my_value),
#           threshold = paste0("thr_", my_value),
#           exit = paste0("ext_", my_value)
# )




# drop_nodes: ------

# Goal: Delete/drop some nodes (or cues) of a given FFT.
#
# Inputs:
#   fft = 1 FFT (as df)
#   nodes = vector of nodes to drop (as integer in 1:nrow(fft))
#
# Output: Modified version of fft (as df, with fewer nodes).

drop_nodes <- function(fft, nodes = NA){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft))

  if (all(is.na(nodes))) { # catch case 0:

    message("drop_nodes: FFT remains unchanged")  # 4debugging

    return(fft)

  }

  nodes <- as.integer(nodes)
  testthat::expect_true(is.integer(nodes), info = "nodes must be an integer vector")

  n_cues <- nrow(fft)

  # Special case 1:
  if (any(nodes %in% 1:n_cues == FALSE)){ # notify that nodes are missing:

    missing_nodes <- setdiff(nodes, 1:n_cues)

    msg <- paste0("drop_nodes: Some nodes do not occur in FFT and will be ignored: ",
                  paste(missing_nodes, collapse = ", "))

    message(msg)

  }

  # Main: ----

  # Determine new nodes:
  new_nodes <- setdiff(1:n_cues, nodes)

  # Special case 2:
  if ( length(new_nodes) == 0 ){  # nothing left:

    msg <- paste0("drop_nodes: Nothing left of fft")
    stop(msg)

  }

  # Filter rows of fft:
  fft_mod <- fft[new_nodes, ]

  # Special case 3:
  if (n_cues %in% nodes){ # Previous exit cue was dropped/new exit node:

    new_exit_node <- max(new_nodes)

    fft_mod$exit[new_exit_node] <- exit_types[3]  # Set new exit node

    msg <- paste0("drop_nodes: New final node (", new_exit_node, ")")
    message(msg)

  }

  # Output: ----

  # Repair row names:
  row.names(fft_mod) <- 1:nrow(fft_mod)

  # ToDo: Verify fft_mod?

  return(fft_mod)

} # drop_nodes().

# # Check:
# (ffts_df <- get_fft_df(x))  # x$trees$definitions / definitions (as df)
# (fft <- read_fft_df(ffts_df, tree = 2))  # 1 FFT (as df, from above)
#
# drop_nodes(fft)
# drop_nodes(fft, nodes = c(1))
# drop_nodes(fft, nodes = c(3))
# drop_nodes(fft, nodes = c(3, 1))
#
# # Dropping final node or dropping everything:
# drop_nodes(fft, nodes = 4)    # works
# drop_nodes(fft, nodes = 2:6)  # works (1 node left)
# drop_nodes(fft, nodes = 1:6)  # note + error: nothing left
# drop_nodes(fft, nodes = 1:4)  # error: nothing left


# edit_nodes: ------

# Goal: Change (some parameters of) existing nodes.
#
# Inputs:
#   fft = 1 FFT (as df)
#   nodes = vector of nodes to change (as integer in 1:nrow(fft))
#   class = class of cue to add
#   cue = cue to change
#   direction = direction to change
#   threshold = threshold values to change
#   exit = exit value to change
#   my.node = a vector of verbal node descriptions (dominates all other arguments).
#
# Output: Modified version of fft (as df, but with modified nodes).
#
# Note: As edit_nodes() is ignorant of data, the values of class, cue, and threshold
#       are currently NOT validated for a specific set of data.

edit_nodes <- function(fft,
                       nodes = NA,  # as vector (1 or multiple nodes)
                       class = NA, cue = NA, direction = NA, threshold = NA, exit = NA,  # variables of fft nodes (as df rows)
                       my.node = NA){

  # Prepare: ------

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft))

  if (all(is.na(nodes))) { # catch case 0:

    message("edit_nodes: FFT remains unchanged")  # 4debugging

    return(fft)

  }

  nodes <- as.integer(nodes)
  testthat::expect_true(is.integer(nodes), info = "nodes must be an integer vector")

  # Do all args have same length:
  key_args <- list(nodes, class, cue, direction, threshold, exit, my.node)
  non_NA_args  <- (is.na(key_args) == FALSE)
  len_set_args <- sapply(X = key_args, FUN = length)[non_NA_args]

  if (length(unique(len_set_args)) > 1){
    stop("edit_nodes: All modifier args (e.g., nodes, ..., exit) must have the same length.")
  }

  n_cues <- nrow(fft)

  # Special case 1:
  if (any(nodes %in% 1:n_cues == FALSE)){ # notify that nodes are missing:

    missing_nodes <- setdiff(nodes, 1:n_cues)

    msg <- paste0("edit_nodes: Some nodes do not occur in FFT and will be ignored: ",
                  paste(missing_nodes, collapse = ", "))

    message(msg)

    # Remove missing nodes:
    nodes <- setdiff(nodes, missing_nodes)

  }


  # Main: ------

  fft_mod <- fft  # copy

  for (i in seq_along(nodes)){ # loop through nodes:

    node_i <- nodes[i]

    # class: ----

    if (!any(is.na(class))){

      cur_class <- tolower(class[i])

      if (cur_class %in% cue_classes){ # verify: valid cue class

        fft_mod$class[node_i] <- cur_class  # set value

      } else {

        msg_class <- paste0("edit_nodes: The class of node ", node_i, " must be in ", paste0(cue_classes, collapse = ", "))
        stop(msg_class)

      }

    } # class.


    # cue: ----

    if (!any(is.na(cue))){

      fft_mod$cue[node_i] <- cue[i]  # set value (unchanged/by brute force)

    }


    # direction: ----

    if (!any(is.na(direction))){

      cur_dir <- direction[i]

      if (verify_dir_sym(cur_dir)){ # verify direction symbol:

        fft_mod$direction[node_i] <- cur_dir  # set value

      } else {

        stop(paste0("edit_nodes: Unknown direction symbol: ", cur_dir))

      }

    } # direction.


    # threshold: ----

    if (!any(is.na(threshold))){

      fft_mod$threshold[node_i] <- threshold[i]  # set value (unchanged/by brute force)

    } # threshold.


    # exit: ----

    if (!any(is.na(exit))){

      cur_exit <- get_exit_type(exit[i])

      if (node_i < n_cues){ # non-final nodes:

        if (cur_exit %in% exit_types[1:2]){  # verify: valid non-final exit

          fft_mod$exit[node_i] <- cur_exit  # set value

        } else {

          msg_exit <- paste0("edit_nodes: The exit of non-final node ", node_i, " must be in ", paste0(exit_types[1:2], collapse = ", "))
          stop(msg_exit)

        }

      } else if (node_i == n_cues){ # final node:

        if (cur_exit == exit_types[3]){  # verify: valid final exit

          fft_mod$exit[node_i] <- cur_exit  # set value

        } else {

          msg_final <- paste0("edit_nodes: The exit of final node ", node_i, " must be ", exit_types[3])
          stop(msg_final)

        }

      } else { # Error:

        stop("edit_nodes: Current node_i exceeds n_cues of fft")

      }

    } # exit.

  } # for (i in nodes).

  # +++ here now +++

  # Output: ------

  # # Repair row names:
  # row.names(fft_mod) <- 1:nrow(fft_mod)

  # ToDo: Verify fft_mod?

  return(fft_mod)

} # edit_nodes().

# # Check:
# (ffts_df <- get_fft_df(x))  # x$trees$definitions / definitions (as df)
# (fft <- read_fft_df(ffts_df, tree = 1))  # 1 FFT (as df, from above)
#
# edit_nodes(fft)
# edit_nodes(fft, nodes = c(1, 1), cue = "ABC", exit = c(1, 0))  # error: args differ in length
#
# edit_nodes(fft, nodes = 1:3, class = c("c", "n", "c"))  # works, for valid class values
# edit_nodes(fft, nodes = 1:3, class = c("c", "n", "n"))  # works, but NO validation with data
#
# edit_nodes(fft, nodes = 1:3, cue = c("A", "B", "C"))  # works, but NO validation with data
# edit_nodes(fft, nodes = c(1, 1, 1), cue = c("A", "B", "C"))  # repeated change of 1 node: works
#
# edit_nodes(fft, nodes = c(1, 2, 3), direction = c("!=", "<=", ">="))  # works, for valid direction symbols
# edit_nodes(fft, nodes = c(1, 1, 1), direction = c("=", "<<", ">>"))  # repeated change of 1 node: works
# edit_nodes(fft, nodes = c(1, 2, 3), direction = c("!=", "<<", ">>"))  # NOTE: INVALID direction symbol are IGNORED.
#
# edit_nodes(fft, nodes = 1:3, cue = c("A", "B", "C"), threshold = c("X", "Y", "xyz"))  # works, but NO validation with data
#
# edit_nodes(fft, nodes = c(1, 1, 1), exit = c(1, 0, 1))  # repeated change of 1 node works
# edit_nodes(fft, nodes = c(1, 2, 2), exit = c(0, 0, 1))
# edit_nodes(fft, nodes = c(1, 2, 3), exit = c("FALse", " Signal ", 3/6))



# flip_exits: ------

# Goal: Flip the exits (i.e., cue direction and exit type) of some FFT's (non-final) nodes.
#
# Inputs:
#   fft = 1 FFT (as df)
#   nodes = vector of nodes to swap (as integer in 1:nrow(fft))
#
# Output: Modified version of fft (as df, with flipped cue directions/exits)

flip_exits <- function(fft, nodes = NA){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft))

  if (all(is.na(nodes))) { # catch case 0:

    message("flip_exits: FFT remains unchanged")  # 4debugging

    return(fft)

  }

  nodes <- as.integer(nodes)
  testthat::expect_true(is.integer(nodes), info = "nodes must be an integer vector")

  n_cues <- nrow(fft)

  # Special case 1:
  if (any(nodes %in% 1:n_cues == FALSE)){ # some nodes are missing:

    missing_nodes <- setdiff(nodes, 1:n_cues)

    # A. Stop with error:
    # err_msg <- paste0("flip_exits: Some nodes do not occur in FFT: ",
    #                   paste(missing_nodes, collapse = ", "))
    #
    # stop(err_msg)  # Error

    # B. Continue, but ignore the missing nodes:
    msg <- paste0("flip_exits: Some nodes do not occur in FFT and will be ignored: ",
                  paste(missing_nodes, collapse = ", "))

    message(msg)

    nodes <- setdiff(nodes, missing_nodes)

  }

  # Special case 2:
  if (n_cues %in% nodes){ # aiming to flip the final/exit node:

    msg <- paste0("flip_exits: Exits of a final node (", n_cues, ") cannot be flipped")

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

} # flip_exits().

# # Check:
# (ffts_df <- get_fft_df(x))  # x$trees$definitions / definitions (as df)
# (fft <- read_fft_df(ffts_df, tree = 2))  # 1 FFT (as df, from above)
#
# flip_exits(fft)
# flip_exits(fft, nodes = c(1))
# flip_exits(fft, nodes = c(3))
# flip_exits(fft, nodes = c(3, 1))
# flip_exits(fft, 3:1)
#
# # Flipping missing nodes and exit node:
# flip_exits(fft, nodes = 6:9)  # message & no change
# flip_exits(fft, nodes = 4)    # message & no change
# flip_exits(fft, nodes = 1:4)  # message & others change
# flip_exits(fft, nodes = 1:10) # 2 messages & others change
#
# # Flipping all and back:
# all.equal(fft, flip_exits(flip_exits(fft, nodes = 3:1), nodes = 1:3))


# reorder_nodes: ------

# Goal: Re-order the nodes of an existing FFT.
#
# Inputs:
#  fft: 1 FFT (in multi-line format, as df)
#  order: desired order of cues (as a numeric vector of 1:n_cues)
#
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

    message("reorder_nodes: FFT remains unchanged")  # 4debugging

    return(fft)

  }


  # Main: ----

  # Re-order rows:
  fft_mod <- fft[order, ]

  # IF the exit cue has been changed:
  exit_cue_pos <- which(order == n_cues)

  if (exit_cue_pos != n_cues){

    message(paste0("reorder_nodes: Previous exit node moved to node position ", exit_cue_pos))

    # ?: Which exit direction should be used for previous exit cue?

    # Option 1:
    # Current direction and threshold settings always predict Signal (1):

    # a. previous exit cue: Decide/predict 1 (signal)
    fft_mod$exit[exit_cue_pos] <- exit_types[2]  # (as by cue threshold definition)

    # b. final cue: Make final exit bi-directional (0.5)
    fft_mod$exit[n_cues] <- exit_types[3]

    # Option 2:
    # Goal: Preserve the overall tree structure:
    # How:  Align exit directions of previous exit node to previous non-exit node!
    #       (Requires setting exit to 0 when previous non-exit node was predicting NON-signal direction 0)

    # Option 3:
    # Goal: Provide BOTH the noise (0: left) and the signal (1: right) alternative for a previous exit node.

  } # if (exit_cue_pos).


  # Output: ----

  # Repair row names:
  row.names(fft_mod) <- 1:nrow(fft_mod)

  return(fft_mod)

} # reorder_nodes().


# # Check:
# ffts_df <- get_fft_df(x)  # x$trees$definitions / definitions (as df)
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
#
# Input:
#   fft: 1 FFT (as df, 1 row per cue)
#
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
# ffts_df <- get_fft_df(x)  # x$trees$definitions / definitions (as df)
# fft  <- read_fft_df(ffts_df, tree = 1)  # 1 FFT (as df, from above)

# (dfs_1 <- all_node_orders(fft = read_fft_df(ffts_df, tree = 1)))
# (dfs_2 <- all_node_orders(fft = read_fft_df(ffts_df, tree = 2)))



# all_exit_structures: ------

# Goal: Get all 2^(n-1) possible exit structures for an FFT with n cues.
#
# Method: Use flip_exits() on nodes = `all_combinations()` for all length values of 1:(n_cues - 1).
#
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

        cur_fft <- flip_exits(fft = fft, nodes = comb_i[j, ])

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
# ffts_df <- get_fft_df(x)  # x$trees$definitions / definitions (as df)
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
