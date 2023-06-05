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


# (A) Tree selection/conversion/collection functions: --------

# Goal: Convert/translate FFT descriptions (definitions as df, with 1 row per tree)
# into a more modular format of individual FFTs (as df with 1 row per node), and back.
# Reason: The latter can be manipulated more easily by tree editing/manipulation/trimming functions (B).

# Details:
#
# 2 FFT translation functions:
# - read_fft_df: From multi-FFT df (with 1 row per tree) to 1 FFT df (in "tidy" format: with 1 row per node),
# - write_fft_df: Back from to 1 FFT df (with 1 row per node) to multi-tree df (with 1 row per tree).
#
# 1 FFT collection function:
# - add_fft_df: Adds definitions (as df) of individual FFTs (as df) to (a set of existing) definitions.

# # Create example data/object x:
# hd <- FFTrees(formula = diagnosis ~ .,
#               data = heart.train,
#               data.test = heart.test)
# x <- hd  # copy object (with 7 FFTs)


# - read_fft_df: ------


# Goal: Extract 1 FFT (as df) from multi-line FFT definitions (as df).
#
# This creates a "tidy" representation of 1 FFT.
#
# Inputs:
# : A set of FFT definitions (as df, usually from an FFTrees object,
#       with suitable variable names to pass verify_ffts_df()).
# tree: A tree ID (corresponding to tree in ffts_df).
#
# Output: A definition of 1 FFT with 1 row per node (in "tidy" format, as df).
#
# Note: Currently used to extract individual trees in
# - fftrees_apply()
# - fftrees_ffttowords()


#' Read an FFT definition from tree definitions
#'
#' @description \code{read_fft_df} reads and returns
#' the definition of a single FFT (as a tidy data frame)
#' from the multi-line FFT definitions of an \code{FFTrees} object.
#'
#' \code{read_fft_df} allows reading individual tree definitions
#' to manipulate them with other tree trimming functions.
#'
#' \code{\link{write_fft_df}} provides the inverse functionality.
#'
#' @param ffts_df A set of FFT definitions (as a data frame,
#' usually from an \code{FFTrees} object,
#' with suitable variable names to pass \code{verify_ffts_df}.
#'
#' @param tree The ID of the to-be-selected FFT (as an integer),
#' corresponding to a tree in \code{ffts_df}.
#' Default: \code{tree = 1}.
#'
#' @return One FFT definition
#' (as a data frame in tidy format, with one row per node).
#'
#' @family tree definition and manipulation functions
#'
#' @seealso
#' \code{\link{get_fft_df}} for getting the FFT definitions of an \code{FFTrees} object;
#' \code{\link{write_fft_df}} for writing one FFT to tree definitions;
#' \code{\link{add_fft_df}} for adding FFTs to tree definitions;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export

read_fft_df <- function(ffts_df, tree = 1){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_ffts_df(ffts_df)) # verify structure and content

  testthat::expect_true(is.numeric(tree))
  testthat::expect_true(length(tree) == 1)

  if (!(tree %in% ffts_df$tree)){
    # Error message:
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

    # Error message:
    stop(paste0("The lengths of some FFT definition parts differ from n_nodes = ", n_nodes,
                ":\n  names of v = (", tvec_diffs_col, "), v_lengths = (", vlen_diffs_col, ")."))

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
# # FFT definitions:
# (ffts <- get_fft_df(x))  # using the helper function
#
# read_fft_df(ffts, 1)    # works
# read_fft_df(ffts, 7)    # works
# read_fft_df(ffts, 2:3)  # yields an error
# read_fft_df(ffts, 8)    # yields an error



# - write_fft_df: ------


# Goal: Turn 1 FFT (in "tidy" format, as df) into a line of multi-line FFT definitions (as df).
# Inputs:
# - fft: A definition of 1 FFT (as "tidy" df, with 1 row per node,
#        and suitable variable names to pass verify_fft_as_df()).
# - tree: tree ID (as integer).
# Output: FFT definition in 1 line (as non-tidy df).
#
# Note: Code is currently used at the end of
# - fftrees_grow_fan()
# - fftrees_wordstofftrees()


#' Write an FFT definition to tree definitions
#'
#' @description \code{write_fft_df} writes
#' the definition of a single FFT (as a tidy data frame)
#' into the one-line FFT definition used by an \code{FFTrees} object.
#'
#' \code{write_fft_df} allows turning individual tree definitions
#' into the one-line FFT definition format
#' used by an \code{FFTrees} object.
#'
#' \code{\link{read_fft_df}} provides the inverse functionality.
#'
#' @param fft One FFT definition
#' (as a data frame in tidy format, with one row per node).
#'
#' @param tree The ID of the to-be-written FFT (as an integer).
#' Default: \code{tree = -99L}.
#'
#' @return An FFT definition in the one line
#' FFT definition format used by an \code{FFTrees} object
#' (as a data frame).
#'
#' @family tree definition and manipulation functions
#'
#' @seealso
#' \code{\link{get_fft_df}} for getting the FFT definitions of an \code{FFTrees} object;
#' \code{\link{read_fft_df}} for reading one FFT definition from tree definitions;
#' \code{\link{add_fft_df}} for adding FFTs to tree definitions;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export

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
# (ffts <- get_fft_df(x))  # using the helper function
# (fft <- read_fft_df(ffts, 2))  # from above
#
# write_fft_df(fft, tree = 123)



# # Verify that read_fft_df() and write_fft_df() are complementary functions:

# Show that:
# 1. converting a line of ffts_df by read_fft_df() into df of 1 FFT and
# 2. re-converting df back into 1 row per tree by write_fft_df()
# yields original line:

# for (id in 1:nrow(ffts)){
#
#   def_org <- as.data.frame(ffts[id, ])
#   def_new <- write_fft_df(read_fft_df(ffts, id), id)
#   # print(def_org)  # 4debugging
#   # print(def_new)  # 4debugging
#
#   check <- all.equal(def_org, def_new)
#
#   print(paste0("\u2014 tree id = ", id, ": ", check))
#
# } # loop, qed.



# - add_fft_df: ------


# Goal: Add an FFT definition (Case 1) or 1 FFT as df (Case 2) to an existing set of FFT definitions.
#
# Output: Verified tree definitions of x$trees$definitions (as 1 df); else NA.


#' Add an FFT definition to tree definitions
#'
#' @description \code{add_fft_df} adds the definition(s) of
#' one or more FFT(s) (in the multi-line format of an \code{FFTrees} object)
#' or a single FFT (as a tidy data frame)
#' to the multi-line FFT definitions of an \code{FFTrees} object.
#'
#' \code{add_fft_df} allows for collecting and combining
#' (sets of) tree definitions after
#' manipulating them with other tree trimming functions.
#'
#' @param fft A (set of) FFT definition(s)
#' (in the multi-line format of an \code{FFTrees} object)
#' or one FFT definition
#' (as a data frame in tidy format, with one row per node).
#'
#' @param ffts_df A set of FFT definitions (as a data frame,
#' usually from an \code{FFTrees} object,
#' with suitable variable names to pass \code{verify_ffts_df}.
#' Default: \code{ffts_df = NULL}.
#'
#' @param quiet Hide feedback messages (as logical)?
#' Default: \code{quiet = FALSE}.
#'
#' @return A (set of) FFT definition(s) in the one line
#' FFT definition format used by an \code{FFTrees} object
#' (as a data frame).
#'
#' @family tree definition and manipulation functions
#'
#' @seealso
#' \code{\link{get_fft_df}} for getting the FFT definitions of an \code{FFTrees} object;
#' \code{\link{read_fft_df}} for reading one FFT definition from tree definitions;
#' \code{\link{write_fft_df}} for writing one FFT to tree definitions;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export

add_fft_df <- function(fft, ffts_df = NULL, quiet = FALSE){

  if (verify_ffts_df(fft)){   # Case 1: fft is a (set of) FFT-definitions (in 1 row per tree, as df) ----

    if (is.null(ffts_df)){ # no addend:

      return(fft)  # Output (as is)

    } else { # add fft to a set of existing FFT definitions ffts_df:

      if (verify_ffts_df(ffts_df)){

        out_ffts_df <- rbind(ffts_df, fft)  # add below existing definitions

        out_ffts_df$tree <- 1:nrow(out_ffts_df)  # re-set tree numbers

        return(out_ffts_df)  # Output (default)

      } # if ffts_df.

    } # if else fft.


  } else if (verify_fft_as_df(fft)){ # Case 2: fft is 1 FFT (as tidy df, 1 row per node) ----

    cur_fft <- write_fft_df(fft = fft, tree = 1)

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){
      cat(u_f_msg(paste0("add_fft_df: Wrote fft to a 1-line definition\n")))
    }


    add_fft_df(fft = cur_fft, ffts_df = ffts_df)  # re-call (with modified 1st argument)

  } else {

    # Error message:
    stop("fft must be a valid FFT definition (as df) or a valid FFT (as df)")

  }

} # add_fft_df().


# # Check:
# (ffts <- get_fft_df(x))
# (fft  <- read_fft_df(ffts, tree = 2))
#
# # Baselines:
# add_fft_df(ffts)  # Case 0a: Add a set of definitions to NULL.
# add_fft_df(fft)   # Case 0c: Add 1 FFT (as df) to NULL.
#
# # Intended use:
# add_fft_df(ffts[2, ], ffts)  # Case 1: Add 1 definition to a set of definitions.
# add_fft_df(fft, ffts)        # Case 2: Add 1 FFT (as df) to a set of definitions.




# (B) Tree editing/trimming functions: --------

# Goal: Functions for editing, manipulating, and trimming individual FFTs (in "tidy" df format).

# Note: These functions assume 1 FFT (in multi-line format, as df) as their 1-st input argument (for piping)
#       and return a modified version of 1 FFT (in the same format) as output.


# - add_nodes: ------


# Goal: Add some node(s) (or cues) to a given FFT definition.
#
# Inputs:
#   fft = 1 FFT (as "tidy" df)
#   nodes = vector of added node positions (as integer from 1 to nrow(fft) + length(nodes))
#   #
#   class = class of cue(s) to add
#   cue = cue(s) to add
#   direction = direction(s) to change
#   threshold = threshold value(s) to change
#   exit = exit(s) to change
#   #
#   my.node = a vector of verbal node descriptions (dominates other arguments).
#
# Output: A modified version of fft (as df, but with modified nodes).
#
# Notes:
# 1. As add_nodes() is ignorant of data, the values of class, cue, and threshold
#    are currently NOT validated for a specific set of data.
# 2. The values of nodes refer to their position in NEW fft (after inserting new nodes).
# 3. Duplicate values of nodes are ignored and only the last (rightmost) entry is used.
# 4. If nodes contain a new exit, it needs to have a valid final type (i.e., exit_types[3]).
#    The exit of the former exit is re-set to signal (i.e., exit_types[2]).


#' Add nodes to an FFT definition
#'
#' @description \code{add_nodes} allows adding
#' one or more \code{nodes} to an existing FFT definition
#' (in the tidy data frame format).
#'
#' \code{add_nodes} allows to directly set and change the value(s) of
#' \code{class}, \code{cue}, \code{direction}, \code{threshold}, and \code{exit},
#' in an FFT definition for the specified \code{nodes}.
#'
#' There is only rudimentary verification for plausible entries.
#' Importantly, however, as \code{add_nodes} is ignorant of \code{data},
#' the values of its variables are not validated for a specific set of data.
#'
#' Values in \code{nodes} refer to their new position in the final FFT.
#' Duplicate values of \code{nodes} are ignored (and only the last
#' entry is used).
#'
#' When a new exit node is added, the exit type of a former final node
#' is set to the signal value (i.e., \code{exit_types[2]}).
#'
#' @param fft One FFT definition
#' (as a data frame in tidy format, with one row per node).
#'
#' @param nodes The FFT nodes to be added (as an integer vector).
#' Values refer to their new position in the final FFT
#' (i.e., after adding all \code{nodes} to \code{fft}).
#' Default: \code{nodes = NA}.
#'
#' @param class The class values of \code{nodes} (as character).
#' @param cue The cue names of \code{nodes} (as character).
#' @param direction The direction values of \code{nodes} (as character).
#' @param threshold The threshold values of \code{nodes} (as character).
#' @param exit The exit values of \code{nodes} (as values from \code{exit_types}).
#'
#' @param quiet Hide feedback messages (as logical)?
#' Default: \code{quiet = FALSE}.
#'
#' @return One FFT definition
#' (as a data frame in tidy format, with one row per node).
#'
#' @family tree definition and manipulation functions
#'
#' @seealso
#' \code{\link{drop_nodes}} for deleting nodes from an FFT definition;
#' \code{\link{edit_nodes}} for editing nodes in an FFT definition;
#' \code{\link{flip_exits}} for reversing exits in an FFT definition;
#' \code{\link{reorder_nodes}} for reordering nodes of an FFT definition;
#' \code{\link{select_nodes}} for selecting nodes in an FFT definition;
#' \code{\link{get_fft_df}} for getting the FFT definitions of an \code{FFTrees} object;
#' \code{\link{read_fft_df}} for reading one FFT definition from tree definitions;
#' \code{\link{add_fft_df}} for adding FFTs to tree definitions;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export


add_nodes <- function(fft,
                      nodes = NA,  # as vector (1 integer or multiple nodes)
                      #
                      class = NA,  # variables of fft nodes (in tidy df, nodes as rows)
                      cue = NA,
                      direction = NA,
                      threshold = NA,
                      exit = NA,
                      #
                      # my.node = NA,  # ToDo
                      #
                      quiet = FALSE){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft))

  if (!all(is.na(direction))){
    testthat::expect_true(verify_dir_sym(direction))
  }

  if (all(is.na(nodes))) { # catch case 0:

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){
      cat(u_f_msg(paste0("add_nodes: fft remains unchanged\n")))
    }

    return(fft)

  }

  nodes <- as.integer(nodes)
  testthat::expect_true(is.integer(nodes), info = "nodes must be an integer vector")

  # Do all args have same length:
  key_args <- list(nodes, class, cue, direction, threshold, exit)  # my.node
  non_NA_args  <- (is.na(key_args) == FALSE)
  len_set_args <- sapply(X = key_args, FUN = length)[non_NA_args]

  if (length(unique(len_set_args)) > 1){

    # Error message:
    stop("edit_nodes: All modifier args (e.g., nodes, ..., exit) must have the same length.")
  }

  n_cues  <- nrow(fft)         # N of existing fft nodes
  n_nodes <- length(nodes)     # N of new nodes
  n_total <- n_cues + n_nodes  # N of nodes defined for new fft


  # Special case 1:
  if (any(duplicated(nodes))){ # remove duplicated nodes:

    # Find the last ix of each unique node:
    uni_nodes <- unique(nodes)
    last_node_ix <- rep(NA, length(uni_nodes))  # initialize

    for (i in seq_along(uni_nodes)){ # loop though unique nodes:

      cur_node <- uni_nodes[i]

      last_node_ix[i] <- max(which(nodes == cur_node))  # last ix

    } # for unique nodes.

    # Remove duplicated nodes:
    nodes <- nodes[last_node_ix]

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){
      msg_1 <- paste0("add_nodes: Removing duplicated nodes. Remaining nodes: ", paste0(nodes, collapse = ", "))
      cat(u_f_msg(paste0(msg_1, "\n")))
    }

    # Adjust other inputs accordingly:
    class <- class[last_node_ix]
    cue <- cue[last_node_ix]
    direction <- direction[last_node_ix]
    threshold <- threshold[last_node_ix]
    exit <- exit[last_node_ix]

    n_nodes <- length(nodes)     # N of new nodes
    n_total <- n_cues + n_nodes  # N of nodes defined for new fft

  } # if sc 1.


  # Special case 2:
  if (max(nodes) > n_total){ # values of nodes are too high:

    # msg_1 <- paste0("add_nodes: Maximum node value (", max(nodes), ") > total of defined nodes (", n_total, ")")
    # message(msg_1)

    # Adjusting values of nodes beyond n_total:
    ix_decr <- (nodes > n_total)
    nodes[ix_decr] <- (n_total - sum(ix_decr) + 1):n_total

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){
      msg_2 <- paste0("add_nodes: Adjusted values of nodes to ", paste(nodes, collapse = ", "))
      cat(u_f_msg(paste0(msg_2, "\n")))
    }

  } # if sc 2.


  # Initialize new df:
  new_vec <- rep(NA, n_total)
  fft_mod <- data.frame(class = new_vec,
                        cue = new_vec,
                        direction = new_vec,
                        threshold = new_vec,
                        exit = new_vec)


  # Main: ----

  # 1. Adjust old indices (to evade new nodes):
  ix_old <- 1:n_cues

  for (i in seq_along(nodes)){ # loop through nodes:

    node_i <- nodes[i]

    if (node_i <= max(ix_old)){

      ix_inc <- (node_i <= ix_old)  # indices to increment

      ix_old[ix_inc] <- ix_old[ix_inc] + 1  # increment indices
      # print(ix_old) # 4debugging

    }

  } # for (i in nodes).

  # print(ix_old) # 4debugging

  # 2. Fill fft_mod:
  fft_mod[ix_old, ] <- fft[1:n_cues, ]  # a. original nodes/rows

  fft_mod[nodes, ]  <- data.frame(class = class,  # b. new nodes/rows
                                  cue = cue,
                                  direction = direction,
                                  threshold = threshold,
                                  exit = exit)

  # Special case 3: Remove duplicate exit nodes
  if (max(nodes) > max(ix_old)){ # A new exit node:

    ix_old_exit <- max(ix_old)

    fft_mod$exit[ix_old_exit] <- exit_types[2]  # set to 1/TRUE/Signal

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){

      old_exit_cue <- fft_mod$cue[ix_old_exit]

      msg_3 <- paste0("add_nodes: Set exit type of former final node ", n_cues, ": ", old_exit_cue, " (now node ", ix_old_exit, ") to ", exit_types[2])
      cat(u_f_msg(paste0(msg_3, "\n")))
    }


  } # if sc 3.


  # Output: ----

  # # Repair row names:
  # row.names(fft_mod) <- 1:nrow(fft_mod)

  # Verify output:
  testthat::expect_true(verify_fft_as_df(fft_mod))

  return(fft_mod)

} # add_nodes().


# # Check:
# (ffts_df <- get_fft_df(x))  # x$trees$definitions / definitions (as df)
# (fft <- read_fft_df(ffts_df, tree = 1))  # 1 FFT (as df, from above)
#
# add_nodes(fft)
# add_nodes(fft, nodes = 1,
#           class = "class_1", cue = "cue_1", direction = ">=", threshold = "thr_1", exit = 1)
# add_nodes(fft, nodes = 2,
#           class = "class_2", cue = "cue_2", direction = "<=", threshold = "thr_2", exit = 1)
#
# # A new exit node:
# add_nodes(fft, nodes = 4,
#           class = "class_4", cue = "cue_4", direction = "!=", threshold = "thr_4", exit = 0.5)
#
# # More adjustments:
# my_nodes <- c(1, 2, 4, 6, 8:10, 50, 100)
#
# add_nodes(fft, nodes = my_nodes,
#           class = paste0("class_", my_nodes),
#           cue = paste0("cue_", my_nodes),
#           direction = sample(c("!=", ">=", "<="), length(my_nodes), replace = TRUE),
#           threshold = paste0("thr_", my_nodes),
#           exit = c(sample(exit_types[1:2], size = length(my_nodes) - 1, replace = TRUE), 0.5)
#           )
#
# my_nodes <- c(2, 10, 2, 10)
# my_value <- 11:14
#
# add_nodes(fft, nodes = my_nodes,
#           class = paste0("class_", my_value),
#           cue = paste0("cue_", my_value),
#           direction = sample(c("!=", ">=", "<="), length(my_nodes), replace = TRUE),
#           threshold = paste0("thr_", my_value),
#           exit = c(sample(exit_types[1:2], size = length(my_nodes) - 1, replace = TRUE), 0.5)
# )



# - drop_nodes: ------


# Goal: Delete/drop some nodes (or cues) of a given FFT.
#
# Inputs:
#   fft = 1 FFT (as "tidy" df)
#   nodes = vector of nodes to drop (as integer in 1:nrow(fft))
#
# Output: Modified version of fft (as df, with fewer nodes).


#' Drop a node from an FFT definition
#'
#' @description \code{drop_nodes} deletes
#' one or more \code{nodes} from an existing FFT definition
#' (by removing the corresponding rows from the FFT definition
#' in the tidy data frame format).
#'
#' When dropping the final node,
#' the last remaining node becomes the new final node
#' (i.e., gains a second exit).
#'
#' Duplicates in \code{nodes} are dropped only once
#' (rather than incrementally) and \code{nodes} not in
#' the range \code{1:nrow(fft)} are ignored.
#' Dropping all nodes yields an error.
#'
#' \code{drop_nodes} is the inverse function of \code{\link{select_nodes}}.
#' Inserting new nodes is possible by \code{\link{add_nodes}}.
#'
#' @param fft One FFT definition
#' (as a data frame in tidy format, with one row per node).
#'
#' @param nodes The FFT nodes to drop (as an integer vector).
#' Default: \code{nodes = NA}.
#'
#' @param quiet Hide feedback messages (as logical)?
#' Default: \code{quiet = FALSE}.
#'
#' @return One FFT definition
#' (as a data frame in tidy format, with one row per node).
#'
#' @family tree definition and manipulation functions
#'
#' @seealso
#' \code{\link{add_nodes}} for adding nodes to an FFT definition;
#' \code{\link{edit_nodes}} for editing nodes in an FFT definition;
#' \code{\link{select_nodes}} for selecting nodes in an FFT definition;
#' \code{\link{get_fft_df}} for getting the FFT definitions of an \code{FFTrees} object;
#' \code{\link{read_fft_df}} for reading one FFT definition from tree definitions;
#' \code{\link{add_fft_df}} for adding FFTs to tree definitions;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export


drop_nodes <- function(fft, nodes = NA, quiet = FALSE){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft))

  if (all(is.na(nodes))) { # catch case 0:

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){
      cat(u_f_msg(paste0("drop_nodes: fft remains unchanged\n")))
    }

    return(fft)

  }

  nodes <- as.integer(nodes)
  testthat::expect_true(is.integer(nodes), info = "nodes must be an integer vector")

  n_cues <- nrow(fft)

  # Special case 1:
  if (any(nodes %in% 1:n_cues == FALSE)){ # notify that nodes are missing:

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){

      missing_nodes <- setdiff(nodes, 1:n_cues)

      msg_1 <- paste0("drop_nodes: Some nodes do not occur in FFT and will be ignored: ",
                      paste(missing_nodes, collapse = ", "))
      cat(u_f_msg(paste0(msg_1, "\n")))
    }

  } # if sc 1.

  # Special case 2:
  if (any(duplicated(nodes))){

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){

      duplicated_nodes <- unique(nodes[duplicated(nodes)])

      msg_2 <- paste0("drop_nodes: Duplicated nodes are dropped only once: ",
                      paste(duplicated_nodes, collapse = ", "))
      cat(u_f_msg(paste0(msg_2, "\n")))
    }

  } # if sc 2.


  # Main: ----

  # Determine new nodes:
  new_nodes <- setdiff(1:n_cues, nodes)
  # print(new_nodes)  # 4debugging

  # Special case 3:
  if (length(new_nodes) == 0){ # nothing left:

    # Error message:
    msg_3 <- paste0("drop_nodes: Nothing left of fft")
    stop(msg_3)

  } # if sc 3.

  # Filter rows of fft:
  fft_mod <- fft[new_nodes, ]  # main step

  # Special case 4:
  if (n_cues %in% new_nodes == FALSE){ # Previous exit cue was dropped/new exit node:

    new_exit_node <- nrow(fft_mod) # = length(new_nodes)

    fft_mod$exit[new_exit_node] <- exit_types[3]  # set new exit node

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){

      new_exit_cue <- fft_mod$cue[new_exit_node]
      old_node_pos <- which(fft$cue == new_exit_cue)

      msg_4 <- paste0("drop_nodes: New final node ", new_exit_node, ": ", new_exit_cue, " (was node ", old_node_pos, " of fft)")
      cat(u_f_msg(paste0(msg_4, "\n")))
    }

  } # if sc 4.


  # Output: ----

  # Repair row names:
  row.names(fft_mod) <- 1:nrow(fft_mod)

  # Verify output:
  testthat::expect_true(verify_fft_as_df(fft_mod))

  return(fft_mod)

} # drop_nodes().


# # Check:
# (ffts <- get_fft_df(x))  # x$trees$definitions / definitions (as df)
# (fft <- read_fft_df(ffts, tree = 2))  # 1 FFT (as df, from above)
#
# drop_nodes(fft)
# drop_nodes(fft, nodes = c(1))
# drop_nodes(fft, nodes = c(3))
# drop_nodes(fft, nodes = c(3, 1))
# drop_nodes(fft, nodes = c(3, 1, 2))  # works: NO new final node
#
# drop_nodes(fft, nodes = c(1, 1, 2, 2))  # duplicated nodes are only dropped once.
#
# # Dropping final node / new final node:
# drop_nodes(fft, nodes = 2)    # works: NO new final node
# drop_nodes(fft, nodes = 1:3)
# drop_nodes(fft, nodes = 4)    # works: new final node
# drop_nodes(fft, nodes = 3:4)  # works: new final node
# drop_nodes(fft, nodes = 2:6)  # works (1 node left: new exit)
#
# # Dropping everything:
# drop_nodes(fft, nodes = 1:6)  # note + error: nothing left
# drop_nodes(fft, nodes = 1:4)  # error: nothing left



# - select_nodes: ------


# Goal: Select some nodes (or cues) of a given FFT.
#       Note: select_nodes() is the complement of drop_nodes().
#
# Inputs:
#   fft = 1 FFT (as "tidy" df)
#   nodes = vector of nodes to select (as integer in 1:nrow(fft))
#
# Output: Modified version of fft (as df, with fewer nodes).


#' Select nodes from an FFT definition
#'
#' @description \code{select_nodes} selects
#' one or more \code{nodes} from an existing FFT definition
#' (by filtering the corresponding row(s) from the FFT definition
#' in the tidy data frame format).
#'
#' When not selecting the final node,
#' the last selected node becomes the new final node
#' (i.e., gains a second exit).
#'
#' Duplicates in \code{nodes} are selected only once
#' (rather than incrementally) and \code{nodes} not in
#' the range \code{1:nrow(fft)} are ignored.
#'
#' \code{select_nodes} is the inverse function
#' of \code{\link{drop_nodes}}.
#'
#' @param fft One FFT definition
#' (as a data frame in tidy format, with one row per node).
#'
#' @param nodes The FFT nodes to select (as an integer vector).
#' Default: \code{nodes = NA}.
#'
#' @param quiet Hide feedback messages (as logical)?
#' Default: \code{quiet = FALSE}.
#'
#' @return One FFT definition
#' (as a data frame in tidy format, with one row per node).
#'
#' @family tree definition and manipulation functions
#'
#' @seealso
#' \code{\link{add_nodes}} for adding nodes to an FFT definition;
#' \code{\link{drop_nodes}} for deleting nodes from an FFT definition;
#' \code{\link{edit_nodes}} for editing nodes in an FFT definition;
#' \code{\link{flip_exits}} for reversing exits in an FFT definition;
#' \code{\link{reorder_nodes}} for reordering nodes of an FFT definition;
#' \code{\link{get_fft_df}} for getting the FFT definitions of an \code{FFTrees} object;
#' \code{\link{read_fft_df}} for reading one FFT definition from tree definitions;
#' \code{\link{add_fft_df}} for adding FFTs to tree definitions;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export


select_nodes <- function(fft, nodes = NA, quiet = FALSE){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft))

  if (all(is.na(nodes))) { # catch case 0:

    # Error message:
    stop("select_nodes: Nothing left of fft")

    # return(NA)

  } # if case 0.


  nodes <- as.integer(nodes)
  testthat::expect_true(is.integer(nodes), info = "nodes must be an integer vector")

  n_cues <- nrow(fft)


  # Special case 1:
  if (any(nodes %in% 1:n_cues == FALSE)){ # notify that nodes are missing:

    missing_nodes <- setdiff(nodes, 1:n_cues)

    nodes <- setdiff(nodes, missing_nodes)  # remove missing nodes

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){

      msg <- paste0("select_nodes: Some nodes do not occur in FFT and will be ignored: ",
                    paste(missing_nodes, collapse = ", "))
      cat(u_f_msg(paste0(msg, "\n")))
    }

  } # if sc 1.


  if (all(is.na(nodes))) { # catch case 0b (after removing missing_nodes):

    # Error message:
    stop("select_nodes: Nothing left of fft")

    # return(NA)

  } # if case 0b.


  # Special case 2:
  if (any(duplicated(nodes))){

    nodes <- unique(nodes)  # remove duplicated nodes

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){

      duplicated_nodes <- unique(nodes[duplicated(nodes)])

      msg <- paste0("select_nodes: Duplicated nodes are selected only once: ",
                    paste(duplicated_nodes, collapse = ", "))
      cat(u_f_msg(paste0(msg, "\n")))
    }

  } # if sc 2.


  # Special case 3 (AFTER removing any missing or duplicated nodes):
  if (length(nodes) == n_cues) {

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){
      cat(u_f_msg(paste0("select_nodes: fft remains unchanged\n")))
    }

    return(fft)

  } # if sc 3.


  # Main: ----

  # Determine nodes to drop (complement):
  drop_nodes <- setdiff(1:n_cues, nodes)

  # Filter rows of fft:
  fft_mod <- fft[nodes, ]  # main step

  # Special case 4:
  if (n_cues %in% drop_nodes){ # Previous exit cue was dropped/new exit node:

    new_exit_node <- length(nodes)  # NOT max(nodes)
    old_exit_node <- max(nodes)

    fft_mod$exit[new_exit_node] <- exit_types[3]  # set new exit node

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){

      msg <- paste0("select_nodes: New final node: ", new_exit_node, " (was node ", old_exit_node, " of fft)")
      cat(u_f_msg(paste0(msg, "\n")))

    }

  } # if sc 4.


  # Output: ----

  # Repair row names:
  row.names(fft_mod) <- 1:nrow(fft_mod)

  # Verify output:
  testthat::expect_true(verify_fft_as_df(fft_mod))

  return(fft_mod)

} # select_nodes().


# # Check:
# (ffts <- get_fft_df(x))  # x$trees$definitions / definitions (as df)
# (fft <- read_fft_df(ffts, tree = 2))  # 1 FFT (as df, from above)
#
# select_nodes(fft)  # Nothing selected => nothing left (as ERROR)
# select_nodes(fft, nodes = 4:1)  # selecting all: fft unchanged.
#
# select_nodes(fft, nodes = c(1))
# select_nodes(fft, nodes = c(3))
#
# select_nodes(fft, nodes = c(3, 1))
#
# select_nodes(fft, nodes = c(1, 1, 1))  # duplicated nodes are only dropped once.
#
# # Selecting final node / new final node:
# select_nodes(fft, nodes = 4)        # works: OLD final node
# select_nodes(fft, nodes = c(1, 3))  # works: NEW final node
# select_nodes(fft, nodes = 2:6)  # works (1 node left: OLD exit)
# select_nodes(fft, nodes = c(1, 3, 6:9))  # works (2 nodes left: NEW exit)
#
# # Selecting everything/nothing:
# select_nodes(fft, nodes = 4:1)  # note + error: nothing left
# select_nodes(fft, nodes = NA)  # error: nothing left



# - edit_nodes: ------


# Goal: Change (some parameters of) existing nodes.
#
# Inputs:
#   fft = 1 FFT (as "tidy" df)
#   nodes = vector of node(s) to change (as integer in 1:nrow(fft))
#   #
#   class = class of cue(s) to add
#   cue = cue(s) to change
#   direction = direction(s) to change
#   threshold = threshold value(s) to change
#   exit = exit value(s) to change
#   my.node = a vector of verbal node descriptions (dominates other arguments).
#
# Output: A modified version of fft (as df, but with modified nodes).
#
# Note: As edit_nodes() currently is ignorant of data, the values of class, cue, and threshold
#       are NOT validated for a specific set of data.


#' Edit nodes in an FFT definition
#'
#' @description \code{edit_nodes} allows manipulating
#' one or more \code{nodes} from an existing FFT definition
#' (in the tidy data frame format).
#'
#' \code{edit_nodes} allows to directly set and change the value(s) of
#' \code{class}, \code{cue}, \code{direction}, \code{threshold}, and \code{exit},
#' in an FFT definition for the specified \code{nodes}.
#'
#' There is only rudimentary verification for plausible entries.
#' Importantly, however, as \code{edit_nodes} is ignorant of \code{data},
#' the values of its variables are not validated for a specific set of data.
#'
#' Repeated changes of a node are possible
#' (by repeating the corresponding integer value in \code{nodes}).
#'
#' @param fft One FFT definition
#' (as a data frame in tidy format, with one row per node).
#'
#' @param nodes The FFT nodes to be edited (as an integer vector).
#' Default: \code{nodes = NA}.
#'
#' @param class The class values of \code{nodes} (as character).
#' @param cue The cue names of \code{nodes} (as character).
#' @param direction The direction values of \code{nodes} (as character).
#' @param threshold The threshold values of \code{nodes} (as character).
#' @param exit The exit values of \code{nodes} (as values from \code{exit_types}).
#'
#' @param quiet Hide feedback messages (as logical)?
#' Default: \code{quiet = FALSE}.
#'
#' @return One FFT definition
#' (as a data frame in tidy format, with one row per node).
#'
#' @family tree definition and manipulation functions
#'
#' @seealso
#' \code{\link{add_nodes}} for adding nodes to an FFT definition;
#' \code{\link{drop_nodes}} for deleting nodes from an FFT definition;
#' \code{\link{flip_exits}} for reversing exits in an FFT definition;
#' \code{\link{reorder_nodes}} for reordering nodes of an FFT definition;
#' \code{\link{select_nodes}} for selecting nodes in an FFT definition;
#' \code{\link{get_fft_df}} for getting the FFT definitions of an \code{FFTrees} object;
#' \code{\link{read_fft_df}} for reading one FFT definition from tree definitions;
#' \code{\link{add_fft_df}} for adding FFTs to tree definitions;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export


edit_nodes <- function(fft,
                       nodes = NA,  # as vector (1 or multiple nodes)
                       #
                       class = NA,  # variables of fft nodes (in tidy df, nodes as rows)
                       cue = NA,
                       direction = NA,
                       threshold = NA,
                       exit = NA,
                       #
                       # my.node = NA,  # ToDo
                       #
                       quiet = FALSE){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft))

  if (all(is.na(nodes))) { # catch case 0:

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){
      cat(u_f_msg(paste0("edit_nodes: fft remains unchanged\n")))
    }

    return(fft)

  }

  nodes <- as.integer(nodes)
  testthat::expect_true(is.integer(nodes), info = "nodes must be an integer vector")

  # Do all args have same length:
  key_args <- list(nodes, class, cue, direction, threshold, exit) # my.node
  non_NA_args  <- (is.na(key_args) == FALSE)
  len_set_args <- sapply(X = key_args, FUN = length)[non_NA_args]

  if (length(unique(len_set_args)) > 1){
    stop("edit_nodes: All modifier args (e.g., nodes, ..., exit) must have the same length.")
  }

  n_cues <- nrow(fft)

  # Special case 1:
  if (any(nodes %in% 1:n_cues == FALSE)){ # notify that nodes are missing:

    missing_nodes <- setdiff(nodes, 1:n_cues)

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){

      msg <- paste0("edit_nodes: Some nodes do not occur in FFT and will be ignored: ",
                    paste(missing_nodes, collapse = ", "))
      cat(u_f_msg(paste0(msg, "\n")))

    }

    # Remove missing nodes:
    nodes <- setdiff(nodes, missing_nodes)

  } # if sc 1.


  # Main: ----

  fft_mod <- fft  # copy

  for (i in seq_along(nodes)){ # loop through nodes:

    node_i <- nodes[i]

    # class: ----

    if (!any(is.na(class))){

      cur_class <- tolower(class[i])

      if (cur_class %in% cue_classes){ # verify: valid cue class

        fft_mod$class[node_i] <- cur_class  # set value

      } else {

        # Error message:
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

        # Error message:
        stop(paste0("edit_nodes: Unknown direction symbol: ", cur_dir))

      }

    } # direction.


    # threshold: ----

    if (!any(is.na(threshold))){

      fft_mod$threshold[node_i] <- threshold[i]  # set value (unchanged/by brute force)

    } # threshold.


    # exit: ----

    if (!any(is.na(exit))){

      cur_exit <- get_exit_type(exit[i], verify = FALSE)

      if (node_i < n_cues){ # non-final nodes:

        if (cur_exit %in% exit_types[1:2]){  # verify: valid non-final exit

          fft_mod$exit[node_i] <- cur_exit  # set value

        } else {

          # Error message:
          msg_exit <- paste0("edit_nodes: The exit of non-final node ", node_i, " must be in ", paste0(exit_types[1:2], collapse = ", "))
          stop(msg_exit)

        }

      } else if (node_i == n_cues){ # final node:

        if (cur_exit == exit_types[3]){  # verify: valid final exit

          fft_mod$exit[node_i] <- cur_exit  # set value

        } else {

          # Error message:
          msg_final <- paste0("edit_nodes: The exit of final node ", node_i, " must be ", exit_types[3])
          stop(msg_final)

        }

      } else { # Error:

        # Error message:
        stop("edit_nodes: Current node_i exceeds n_cues of fft")

      }

    } # exit.

  } # for (i in nodes).


  # Output: ------

  # # Repair row names:
  # row.names(fft_mod) <- 1:nrow(fft_mod)

  # Verify output:
  testthat::expect_true(verify_fft_as_df(fft_mod))

  return(fft_mod)

} # edit_nodes().


# # Check:
# (ffts <- get_fft_df(x))  # x$trees$definitions / definitions (as df)
# (fft <- read_fft_df(ffts, tree = 1))  # 1 FFT (as df, from above)
#
# edit_nodes(fft)
# # edit_nodes(fft, nodes = c(1, 1), cue = "ABC", exit = c(1, 0))  # error: args differ in length
#
# edit_nodes(fft, nodes = 1:3, class = c("c", "n", "c"))  # works, for valid class values
# edit_nodes(fft, nodes = 1:3, class = c("N", "C", "N"))  # works, but NO validation with data
#
# edit_nodes(fft, nodes = 1:3, cue = c("A", "B", "C"))  # works, but NO validation with data
# edit_nodes(fft, nodes = c(1, 1, 1), cue = c("A", "B", "C"))  # repeated change of 1 node: works
#
# edit_nodes(fft, nodes = c(1, 2, 3), direction = c("!=", "<=", ">="))  # works, for valid direction symbols
# edit_nodes(fft, nodes = c(1, 1, 1), direction = c("=", "<=", ">="))  # repeated change of 1 node: works (using LAST)
# # edit_nodes(fft, nodes = c(1, 2, 3), direction = c("!=", "<<", ">"))  # NOTE: INVALID direction symbol FAILS.
#
# edit_nodes(fft, nodes = 1:3, cue = c("A", "B", "C"), threshold = c("X", "Y", "xyz"))  # works, but NO validation with data
#
# edit_nodes(fft, nodes = c(1, 1, 1), exit = c(0, 0, 1))  # repeated change of 1 node works (using LAST)
# edit_nodes(fft, nodes = c(1, 2, 2), exit = c(0, 0, 1))  # works (using LAST)
# edit_nodes(fft, nodes = c(1, 2, 3), exit = c("FALse", " Signal ", 3/6)) # interpreting exit_type
# # edit_nodes(fft, nodes = c(1, 2, 3), exit = c(0, 0, 1))  # Error: Final exit must be 0.5



# - flip_exits: ------


# Goal: Flip the exits (i.e., cue direction and exit type) of some FFT's (non-final) nodes.
#
# Inputs:
#   fft = 1 FFT (as "tidy" df)
#   nodes = vector of nodes to swap (as integer in 1:nrow(fft))
#
# Output: A modified version of fft (as df, with flipped cue directions/exits)


#' Flip exits in an FFT definition
#'
#' @description \code{flip_exits} reverses the exits of
#' one or more \code{nodes} from an existing FFT definition
#' (in the tidy data frame format).
#'
#' \code{flip_exits} alters the value(s) of the non-final
#' exits specified in \code{nodes} (from 0 to 1, or from 1 to 0).
#' By contrast, exits of final \code{nodes} remain unchanged.
#'
#' Duplicates in \code{nodes} are flipped only once
#' (rather than repeatedly) and \code{nodes} not in
#' the range \code{1:nrow(fft)} are ignored.
#'
#' \code{flip_exits} is a more specialized function
#' than \code{\link{edit_nodes}}.
#'
#' @param fft One FFT definition
#' (as a data frame in tidy format, with one row per node).
#'
#' @param nodes The FFT nodes whose exits are to be flipped (as an integer vector).
#' Default: \code{nodes = NA}.
#'
#' @param quiet Hide feedback messages (as logical)?
#' Default: \code{quiet = FALSE}.
#'
#' @return One FFT definition
#' (as a data frame in tidy format, with one row per node).
#'
#' @family tree definition and manipulation functions
#'
#' @seealso
#' \code{\link{add_nodes}} for adding nodes to an FFT definition;
#' \code{\link{edit_nodes}} for editing nodes in an FFT definition;
#' \code{\link{drop_nodes}} for deleting nodes from an FFT definition;
#' \code{\link{reorder_nodes}} for reordering nodes of an FFT definition;
#' \code{\link{select_nodes}} for selecting nodes in an FFT definition;
#' \code{\link{get_fft_df}} for getting the FFT definitions of an \code{FFTrees} object;
#' \code{\link{read_fft_df}} for reading one FFT definition from tree definitions;
#' \code{\link{add_fft_df}} for adding FFTs to tree definitions;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export


flip_exits <- function(fft, nodes = NA, quiet = FALSE){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft))

  if (all(is.na(nodes))) { # catch case 0:

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){
      cat(u_f_msg(paste0("flip_exits: fft remains unchanged\n")))
    }

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

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){

      msg <- paste0("flip_exits: Some nodes do not occur in FFT and will be ignored: ",
                    paste(missing_nodes, collapse = ", "))
      cat(u_f_msg(paste0(msg, "\n")))

    }

    nodes <- setdiff(nodes, missing_nodes)  # removes duplicate nodes

  } # if sc 1.


  # Special case 2:
  if (n_cues %in% nodes){ # aiming to flip the final/exit node:

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){

      msg <- paste0("flip_exits: Cannot flip exits of the final node: ", n_cues)
      cat(u_f_msg(paste0(msg, "\n")))

    }

    nodes <- nodes[nodes != n_cues]

  } # if sc 2.


  # Main: ----

  fft_mod <- fft  # copy

  # For current nodes:

  # Note ERROR: Cue direction is ALWAYS for criterion = TRUE/signal/1 (on right) => Must not be flipped when exit changes!
  # # 1. flip cue direction:
  # fft_mod$direction[nodes] <- directions_df$negation[match(fft_mod$direction[nodes], table = directions_df$direction)]

  # 2. swap exit:
  fft_mod$exit[nodes] <- ifelse(fft_mod$exit[nodes] == 1, 0, 1)


  # Output: ----

  # Verify output:
  testthat::expect_true(verify_fft_as_df(fft_mod))

  return(fft_mod)

} # flip_exits().


# # Check:
# (ffts <- get_fft_df(x))  # x$trees$definitions / definitions (as df)
# (fft <- read_fft_df(ffts, tree = 2))  # 1 FFT (as df, from above)
#
# flip_exits(fft)
# flip_exits(fft, nodes = c(1))
# flip_exits(fft, nodes = c(3))
# flip_exits(fft, nodes = c(3, 1))
# flip_exits(fft, 3:1)
#
# # Flipping duplicates:
# flip_exits(fft, nodes = c(1, 1, 1))  # ignore duplicates (flip once)
#
# # Flipping missing nodes and exit node:
# flip_exits(fft, nodes = 6:9)  # message & no change
# flip_exits(fft, nodes = 4)    # message & no change
# flip_exits(fft, nodes = 1:4)  # message & non-final nodes change
# flip_exits(fft, nodes = 1:10) # 2 messages & others change
#
# # Flipping all and back:
# all.equal(fft, flip_exits(flip_exits(fft, nodes = 5:1, quiet = FALSE), nodes = 1:5, quiet = FALSE))



# - reorder_nodes: ------


# Goal: Re-order the nodes of an existing FFT.
#
# Inputs:
#  fft: 1 FFT (in multi-line format, as "tidy" df)
#  order: desired order of cues (as a numeric vector of 1:n_cues)
#
# Output:
# fft_mod: modified FFT (in multi-line format, as df)


#' Reorder nodes in an FFT definition
#'
#' @description \code{reorder_nodes} allows reordering
#' the \code{nodes} in an existing FFT definition
#' (in the tidy data frame format).
#'
#' \code{reorder_nodes} allows to directly set and change the node
#' order in an FFT definition by specifying \code{nodes}.
#'
#' When a former non-final node becomes a final node,
#' the exit type of the former final node
#' is set to the signal value (i.e., \code{exit_types[2]}).
#'
#' @param fft One FFT definition
#' (as a data frame in tidy format, with one row per node).
#'
#' @param order The desired node order (as an integer vector).
#' The values of \code{order} must be a permutation of \code{1:nrow(fft)}.
#' Default: \code{order = NA}.
#'
#'
#' @param quiet Hide feedback messages (as logical)?
#' Default: \code{quiet = FALSE}.
#'
#' @return One FFT definition
#' (as a data frame in tidy format, with one row per node).
#'
#' @family tree definition and manipulation functions
#'
#' @seealso
#' \code{\link{add_nodes}} for adding nodes to an FFT definition;
#' \code{\link{edit_nodes}} for editing nodes in an FFT definition;
#' \code{\link{drop_nodes}} for deleting nodes from an FFT definition;
#' \code{\link{flip_exits}} for reversing exits in an FFT definition;
#' \code{\link{select_nodes}} for selecting nodes in an FFT definition;
#' \code{\link{get_fft_df}} for getting the FFT definitions of an \code{FFTrees} object;
#' \code{\link{read_fft_df}} for reading one FFT definition from tree definitions;
#' \code{\link{add_fft_df}} for adding FFTs to tree definitions;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export


reorder_nodes <- function(fft, order = NA, quiet = FALSE){

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

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){
      cat(u_f_msg(paste0("reorder_nodes: fft remains unchanged\n")))
    }

    return(fft)

  } # if no change.


  # Main: ----

  # Re-order rows:
  fft_mod <- fft[order, ]

  # IF the exit cue has been changed:
  exit_cue_pos <- which(order == n_cues)

  # Special case 1:
  if (exit_cue_pos != n_cues){

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){
      cat(u_f_msg(paste0("reorder_nodes: Former exit node now is node ", exit_cue_pos, "\n")))
    }

    # ?: Which exit direction should be used for previous exit cue?

    # Option 1:
    # Current direction and threshold settings always predict Signal (1):

    # a. previous exit cue: Decide/predict 1 (signal/TRUE/right):
    fft_mod$exit[exit_cue_pos] <- exit_types[2]  # (as by cue threshold definition)

    # b. final cue: Make the final exit bi-directional (0.5):
    fft_mod$exit[n_cues] <- exit_types[3]

    # Option 2:
    # Goal: Preserve the overall tree structure:
    # How:  Align exit directions of previous exit node to previous non-exit node!
    #       (Requires setting exit to 0 when previous non-exit node was predicting NON-signal direction 0)

    # Option 3:
    # Goal: Provide BOTH the noise (0: left) and the signal (1: right) alternative for a previous exit node.

  } # if sc 1.


  # Output: ----

  # Repair row names:
  row.names(fft_mod) <- 1:nrow(fft_mod)

  # Verify output:
  testthat::expect_true(verify_fft_as_df(fft_mod))

  return(fft_mod)

} # reorder_nodes().


# # Check:
# (ffts <- get_fft_df(x))  # x$trees$definitions / definitions (as df)
# (fft  <- read_fft_df(ffts, tree = 5))
#
# plot(x, tree = 5)
# ffts[5, ]
# x$trees$inwords[5]
#
# reorder_nodes(fft)  # unchanged
# reorder_nodes(fft, order = c(1, 2, 3))  # unchanged
# reorder_nodes(fft, order = c(3, 2))     # ERROR: wrong length of order
# reorder_nodes(fft, order = c(2, 1, 3))  # exit cue unchanged
# reorder_nodes(fft, order = c(1, 3, 2))  # exit cue changed
# reorder_nodes(fft, order = c(3, 1, 2))  # exit cue changed



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
# +. A specific set of cues, but variable cue orders AND exit structures.
#
# 3. get all node subsets (with all possible subsets of nodes)
#    = all_combinations() for each possible length (excluding extremes of 0 and n_nodes, i.e., 1:(n_nodes - 1)).

# ToDo:
# Combine 3 macro-functions:
#    1. all_node_subsets(), and
#    2. all_node_orders(),  and
#    3. all_exit_structures()
#    to get ALL possible variants of a given FFT.



# - all_node_orders: ------


# Goal: Apply reorder_nodes(fft) to get all possible permutations of cues for a fft.
#
# Input:
#   fft: 1 FFT (as tidy df, 1 row per cue)
#
# Output:
#   A set of FFT definitions in all possible cue orders (predicting 1/Signal/TRUE for all changed cues, as reorder_nodes())


all_node_orders <- function(fft, quiet = FALSE){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft))

  # Initialize:
  out <- NULL
  n_cues <- nrow(fft)


  # Special case 1:
  if (n_cues == 1){

    # Provide user feedback:
    if (any(sapply(quiet, isFALSE))){
      cat(u_f_msg(paste0("all_orders: fft contains only 1 node\n")))
    }

    # Write fft definition:
    cur_fft_df <- write_fft_df(fft = fft, tree = 1)

    return(cur_fft_df)

  } # if sc 1.


  # Main: ----

  all_orders <- all_permutations(1:n_cues)
  # print(all_orders)  # 4debugging

  for (i in 1:nrow(all_orders)){

    cur_fft <- reorder_nodes(fft = fft, order = all_orders[i, ], quiet = quiet)

    cur_fft_df <- write_fft_df(fft = cur_fft, tree = as.integer(i))

    out <- add_fft_df(fft = cur_fft_df, ffts_df = out)

  } # for i.


  # Provide user feedback:
  if (any(sapply(quiet, isFALSE))){

    # cat(u_f_msg(paste0("\u2014 Generated ", nrow(out), " node orders.\n")))

    cli::cli_alert_success("Generated {nrow(out)} node order{?s}.")

  }


  # Output: ----

  return(out)  # ffts_df (definitions of ffts)

} # all_node_orders().


# # Check:
# (ffts <- get_fft_df(x))  # x$trees$definitions / definitions (as df)
# (fft  <- read_fft_df(ffts, tree = 1))  # 1 FFT (as df, from above)
#
# (dfs_1 <- all_node_orders(fft = read_fft_df(ffts, tree = 1)))
# (dfs_2 <- all_node_orders(fft = read_fft_df(ffts, tree = 2)))



# - all_exit_structures: ------


# Goal: Get all 2^(n-1) possible exit structures for an FFT with n cues.
#
# Method: Use flip_exits() on nodes = `all_combinations()` for all length values of 1:(n_cues - 1).
#
# Input: fft: 1 FFT (as tidy df, 1 row per cue).
# Output: A set of FFT definitions (ffts_df).


all_exit_structures <- function(fft, quiet = FALSE){

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

    for (i in 1:n_non_exit_cues){ # loop 1: non-exit cues

      comb_i <- all_combinations(1:n_non_exit_cues, length = i)
      # print(comb_i)  # 4debugging

      for (j in 1:nrow(comb_i)){ # loop 2: combinations

        cur_fft <- flip_exits(fft = fft, nodes = comb_i[j, ], quiet = quiet)

        cnt <- cnt + 1  # increment

        cur_fft_df <- write_fft_df(fft = cur_fft, tree = as.integer(cnt))

        out <- add_fft_df(fft = cur_fft_df, ffts_df = out)

      } # for j.

    } # for i.

  } # if (n_cues > 1).


  # Provide user feedback:
  if (any(sapply(quiet, isFALSE))){

    # cat(u_f_msg(paste0("\u2014 Generated ", nrow(out), " exit structures.\n")))

    cli::cli_alert_success("Generated {nrow(out)} exit structure{?s}.")

  }


  # Output: ----

  return(out)  # ffts_df (definitions of ffts)

} # all_exit_structures().

# # Check:
# (ffts <- get_fft_df(x))  # x$trees$definitions / definitions (as df)
# (fft  <- read_fft_df(ffts, tree = 1))  # 1 FFT (as df, from above)
#
# (dfs_3 <- all_exit_structures(fft = fft))
# (dfs_4 <- all_exit_structures(fft = read_fft_df(ffts, tree = 2)))



# - all_node_subsets: ------

# Goal: Get all subtrees of an FFT.
#
# Input: fft: 1 FFT (as tidy df, 1 row per cue).
# Output: A set of FFT definitions (ffts_df).


all_node_subsets <- function(fft, quiet = FALSE){

  # Prepare: ----

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft))

  # Initialize:
  out <- NULL
  cnt <- 1

  n_cues <- nrow(fft)


  # Main: ----

  # Get subsets of nodes (as list):
  node_subsets_l <- all_subsets(x = 1:n_cues, include_x = TRUE)  # include the full set of all nodes

  # Loop through list elements:
  for (i in 1:length(node_subsets_l)){

    cur_nodes <- node_subsets_l[[i]]
    # print(cur_nodes)  # 4debugging

    fft_sub <- select_nodes(fft = fft, nodes = cur_nodes, quiet = quiet)
    # print(fft_sub)  # 4debugging

    fft_df <- write_fft_df(fft = fft_sub, tree = cnt)
    # print(fft_df)  # 4debugging

    cnt <- cnt + 1  # increment

    out <- add_fft_df(fft = fft_df, ffts_df = out)

  } # for i.


  # Provide user feedback:
  if (any(sapply(quiet, isFALSE))){

    # cat(u_f_msg(paste0("\u2014 Generated ", nrow(out), " node subsets.\n")))

    cli::cli_alert_success("Generated {nrow(out)} node subset{?s}.")

  }


  # Output: ----

  return(out)  # ffts_df (definitions of ffts)

} # all_node_subsets().

# # Check:
# (ffts <- get_fft_df(x))  # x$trees$definitions / definitions (as df)
# (fft  <- read_fft_df(ffts, tree = 1))  # 1 FFT (as df, from above)
#
# (ast_3 <- all_node_subsets(fft = fft, quiet = FALSE))
# (ast_4 <- all_node_subsets(fft = read_fft_df(ffts, tree = 2), quiet = FALSE))



# - all_fft_variants: ------

# Goal: Get all node subsets, node orders, and exit structures of a given fft.
#       Combine 3 macro-functions:
#       1. all_node_subsets(), and
#       2. all_node_orders(),  and
#       3. all_exit_structures()
#       to get ALL possible variants of a given FFT.
#
# Input: fft: 1 FFT (as tidy df, 1 row per cue).
# Output: A set of FFT definitions (ffts_df).


all_fft_variants <- function(fft, quiet = FALSE){

  # Prepare: ------

  # Verify inputs:
  testthat::expect_true(verify_fft_as_df(fft))

  # Initialize:
  out <- NULL
  cnt <- 1

  n_cues <- nrow(fft)


  # Main: ------

  # 1. Get all_node_subsets(): ----

  set_1 <- all_node_subsets(fft = fft, quiet = quiet)
  # print(set_1)  # 4debugging


  # 2. Get all node orders (for each fft definition): ----

  set_2 <- NULL

  for (i in 1:nrow(set_1)){ # for each fft definition in set_1:

    cur_fft <- read_fft_df(ffts_df = set_1, tree = i)
    # print(cur_fft)  # 4debugging

    # Get all_node_orders() for cur_fft:
    cur_node_orders <- all_node_orders(fft = cur_fft, quiet = quiet)

    set_2 <- add_fft_df(fft = cur_node_orders, ffts_df = set_2)

  } # for i.


  # 3. Get all exit structures (for each fft definition): ----

  set_3 <- NULL

  for (i in 1:nrow(set_2)){ # for each fft definition in set_2:

    cur_fft <- read_fft_df(ffts_df = set_2, tree = i)
    # print(cur_fft)  # 4debugging

    # Get all_() for cur_fft:
    cur_exit_structures <- all_exit_structures(fft = cur_fft, quiet = quiet)

    set_3 <- add_fft_df(fft = cur_exit_structures, ffts_df = set_3)

  } # for i.


  # Provide user feedback:
  if (any(sapply(quiet, isFALSE))){

    # cat(u_f_msg(paste0("\u2014 Generated ", nrow(set_3), " variants.\n")))

    cli::cli_alert_success("Generated {nrow(set_3)} variant{?s}.")

  }


  # Output: ------

  out <- set_3  # copy

  return(out)  # ffts_df (definitions of ffts)

} # all_fft_variants().


# # Check:
# (ffts <- get_fft_df(x))  # x$trees$definitions / definitions (as df)
# (fft  <- read_fft_df(ffts, tree = 1))  # 1 FFT (as df, from above)
#
# (all_3 <- all_fft_variants(fft = read_fft_df(ffts, tree = 1), quiet = FALSE))
# nrow(all_3)
# verify_ffts_df(all_3)
#
# (all_4 <- all_fft_variants(fft = read_fft_df(ffts, tree = 2), quiet = TRUE))
# nrow(all_4)
# verify_ffts_df(all_4)



# ToDo: ------

# - Make some functions (e.g., tree editing functions) work for alternative inputs of either
#   (1) FFT definitions (df, 1 row per tree) OR
#   (2) single FFTs (as df, 1 row per node).
#
# - Return the result in the same format as the input.
# - When applying fn to a set of FFT definitions, return the modified set?

# eof.
