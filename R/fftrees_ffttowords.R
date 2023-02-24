#' Describe a fast-and-frugal tree (FFT) in words
#'
#' @description \code{fftrees_ffttowords} provides a verbal description
#' of tree definition (as defined in an \code{FFTrees} object).
#' Thus, \code{fftrees_ffttowords} translates an abstract FFT definition
#' into natural language output.
#'
#' \code{fftrees_ffttowords} is the complement function to
#' \code{\link{fftrees_wordstofftrees}}, which parses a verbal description
#' of an FFT into the abstract tree definition of an \code{FFTrees} object.
#'
#' The final sentence (or tree node) of the FFT's description
#' always predicts positive criterion values (i.e., \code{TRUE} instances) first,
#' before predicting negative criterion values (i.e., \code{FALSE} instances).
#' Note that this may require a reversal of exit directions,
#' if the final cue predicted \code{FALSE} instances.
#'
#' Note that the cue directions and thresholds computed by \strong{FFTrees}
#' always predict positive criterion values (i.e., \code{TRUE} or signal,
#' rather than \code{FALSE} or noise).
#' Using these thresholds for negative exits (i.e., for predicting instances of
#' \code{FALSE} or noise) usually requires a reversal (e.g., negating cue direction).
#'
#' @param x An \code{FFTrees} object created with \code{\link{FFTrees}}.
#' @param mydata The type of data to which a tree is being applied (as character string "train" or "test").
#' Default: \code{mydata = "train"}.
#' @param digits How many digits to round numeric values (as integer)?
#'
#' @return A modified \code{FFTrees} object \code{x} with
#' \code{x$trees$inwords} containing a list of string vectors.
#'
#' @examples
#'
#' heart.fft <- FFTrees(diagnosis ~ .,
#'   data = heartdisease,
#'   decision.labels = c("Healthy", "Disease")
#' )
#'
#' inwords(heart.fft)
#'
#' @seealso
#' \code{\link{fftrees_wordstofftrees}} for converting a verbal description
#' of an FFT into an \code{FFTrees} object;
#' \code{\link{fftrees_create}} for creating \code{FFTrees} objects;
#' \code{\link{fftrees_grow_fan}} for creating FFTs by applying algorithms to data;
#' \code{\link{print.FFTrees}} for printing FFTs;
#' \code{\link{plot.FFTrees}} for plotting FFTs;
#' \code{\link{summary.FFTrees}} for summarizing FFTs;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export

fftrees_ffttowords <- function(x = NULL,
                               mydata = "train",
                               digits = 2) {

  # Simplify: ----

  # Extract key parts from FFTrees object x:
  n_trees <- x$trees$n

  # tree_df <- x$trees$definition  # df (from object x)
  tree_df <- get_fft_df(x = x)  # df (using helper fn)
  # print(tree_df)  # 4debugging

  # Provide user feedback: ----

  if (!x$params$quiet$ini) {

    # msg <- paste0("Aiming to express FFTs in words:\n")
    # cat(u_f_ini(msg))

    cli::cli_alert("Express {x$trees$n} FFT{?s} in words:", class = "alert-start")

  }


  # Prepare: ----

  x$trees$inwords <- vector("list", length = n_trees)

  get_exit_word <- tolower(get_exit_word(mydata))  # either 'train':'decide' or 'test':'predict'



  # LOOPs: ------


  # Loop 1 (over trees): ----

  for (tree_i in 1:n_trees) {

    # print(paste0("tree ", tree_i, ":"))  # 4debugging

    # NEW code start: ----

    # Get ID of tree_df$tree for tree_i value (to consider all trees in turn):
    tree_i_id <- tree_df$tree[tree_i]
    # print(paste0("\u2014 Current tree_i = ", tree_i, " corresponds to tree_i_id = ", tree_i_id)) # 4debugging

    # Read FFT definitions (with 1 row per tree) into 1 FFT as df (with 1 row per cue/node):
    cur_fft_df <- read_fft_df(ffts_df = tree_df, tree = tree_i_id)
    # print(cur_fft_df)  # 4debugging

    # Get variables of cur_fft_df (as vectors):
    class_v     <- cur_fft_df$class
    cue_v       <- cur_fft_df$cue
    direction_v <- cur_fft_df$direction
    threshold_v <- cur_fft_df$threshold
    exit_v      <- cur_fft_df$exit

    # NEW code end. ----

    # +++ here now +++

    # # OLD code start: ----
    #
    # # Extract definition of current tree:
    # class_o     <- trimws(unlist(strsplit(tree_df$classes[tree_i], ";")))
    # cue_o       <- trimws(unlist(strsplit(tree_df$cues[tree_i], ";")))
    # direction_o <- trimws(unlist(strsplit(tree_df$directions[tree_i], ";")))
    # threshold_o <- trimws(unlist(strsplit(tree_df$thresholds[tree_i], ";")))
    # exit_o      <- trimws(unlist(strsplit(tree_df$exits[tree_i], ";")))
    #
    # # Check: Verify equality of OLD and NEW code results:
    # if (!all.equal(class_o, class_v)) { stop("OLD vs. NEW: class diff") }
    # if (!all.equal(cue_o, cue_v)) { stop("OLD vs. NEW: cue diff") }
    # if (!all.equal(direction_o, direction_v)) { stop("OLD vs. NEW: direction diff") }
    # if (!all.equal(threshold_o, threshold_v)) { stop("OLD vs. NEW: threshold diff") }
    # if (!all.equal(exit_o, exit_v)) { stop("OLD vs. NEW: exit diff") }
    #
    # # OLD code end. ----


    decision.labels <- x$params$decision.labels  # (Note: keep "." naming, as in list x)

    n_nodes <- length(cue_v)

    sentences_v <- c()


    # Loop 2 (over nodes/levels): ----

    for (i in 1:n_nodes) {

      exit_i <- paste(exit_v[i])

      # 1. Non-final nodes: ----

      non_final_exit_types <- as.character(exit_types[1:2])

      if (exit_i %in% non_final_exit_types) {

        # a. Node with positive exit ("1" / TRUE / signal / right):
        if (exit_i == non_final_exit_types[2]) {

          if (class_v[i] %in% c("c", "l")) {

            sentence_i <- paste0(
              "If ", cue_v[i], " ", direction_v[i], " {", threshold_v[i],
              "}, ", get_exit_word, " ", decision.labels[2], ".")

          }

          if (class_v[i] %in% c("n", "i")) {
            threshold_i <- threshold_v[i]

            threshold_i <- round(as.numeric(threshold_v[i]), 2)

            sentence_i <- paste0(
              "If ", cue_v[i], " ", direction_v[i], " ", threshold_i,
              ", ", get_exit_word, " ", decision.labels[2], ".")
          }

        } # a. Node with positive exit.


        # b. Node with negative exit ("0" / FALSE / noise / left):
        if (exit_i == non_final_exit_types[1]) {

          # Negate the exit / cue threshold direction:
          direction_i <- switch(direction_v[i],
                                "=" = "!=",
                                "!=" = "=",
                                ">" = "<=",
                                "<" = ">=",
                                ">=" = "<",
                                "<=" = ">"
          )

          if (class_v[i] %in% c("c", "l")) {

            sentence_i <- paste0(
              "If ", cue_v[i], " ", direction_i, " {", threshold_v[i], "}, ",
              get_exit_word, " ", decision.labels[1], ".")

          }

          if (class_v[i] %in% c("n", "i")) {

            threshold_i <- threshold_v[i]
            threshold_i <- round(as.numeric(threshold_v[i]), 2)

            sentence_i <- paste0(
              "If ", cue_v[i], " ", direction_i, " ", threshold_i,
              ", ", get_exit_word, " ", decision.labels[1], ".")

          }

        } # b. Node with negative exit.

        sentences_v <- c(sentences_v, sentence_i)

      } # non-final nodes.


      # 2. Final nodes: ----

      # Final exit type (0.5 / both / final):
      if (exit_i == as.character(exit_types[3]))  {

        direction_pos_i <- direction_v[i]

        # # Negate direction:
        # direction_neg_i <- switch(direction_v[i],
        #                           "=" = "!=",
        #                           "!=" = "=",
        #                           ">" = "<=",
        #                           "<" = ">=",
        #                           ">=" = "<",
        #                           "<=" = ">"
        # )

        # REMOVED, as negation is only indicated if left exit == decision.labels[2]!

        if (class_v[i] %in% c("c", "l")) {

          # # Negative directions (FALSE cases first):
          # sentence_i_1 <- paste0(
          #   "If ", cue_v[i], " ", direction_neg_i, " {", threshold_v[i], "}, decide ",
          #   decision.labels[1], "") # FALSE cases
          #
          # sentence_i_2 <- paste0(
          #   ", otherwise, decide ",
          #   decision.labels[2], ".") # TRUE cases

          # Positive directions (TRUE cases first):
          sentence_i_1 <- paste0(
            "If ", cue_v[i], " ", direction_pos_i, " {", threshold_v[i], "}, ",
            get_exit_word, " ", decision.labels[2], "") # 1 / TRUE / signal / right cases

          sentence_i_2 <- paste0(
            ", otherwise, ", get_exit_word, " ",
            decision.labels[1], ".") # 0 / FALSE / noise / left cases

        }

        if (class_v[i] %in% c("n", "i")) {

          threshold_i <- threshold_v[i]
          threshold_i <- round(as.numeric(threshold_v[i]), digits)

          # # Negative directions (FALSE cases first):
          # sentence_i_1 <- paste0(
          #   "If ", cue_v[i], " ", direction_neg_i, " ", threshold_v[i],
          #   ", decide ", decision.labels[1], "") # FALSE cases
          #
          # sentence_i_2 <- paste0(
          #   ", otherwise, decide ",
          #   decision.labels[2], ".") # TRUE cases

          # Positive directions (TRUE cases first):
          sentence_i_1 <- paste0(
            "If ", cue_v[i], " ", direction_pos_i, " ", threshold_v[i],
            ", ", get_exit_word, " ", decision.labels[2], "") # 1 / TRUE / signal / right cases

          sentence_i_2 <- paste0(
            ", otherwise, ", get_exit_word, " ",
            decision.labels[1], ".") # 0 / FALSE / noise / left cases

        }

        sentence_i <- paste0(sentence_i_1, sentence_i_2, collapse = "")

        sentences_v <- c(sentences_v, sentence_i)

      } # 2. final nodes.

    } # inner Loop 2: for (i nodes).


    # # Combine sentences:
    # sentences.comb <- paste(sentences_v, collapse = ". ")

    # Add to FFTrees object x:
    x$trees$inwords[[tree_i]] <- sentences_v

  } # outer Loop 1: for (tree_i).


  # Provide user feedback: ----

  if (!x$params$quiet$fin) {

    # msg <- paste0("Successfully expressed FFTs in words.\n")
    # cat(u_f_fin(msg))

    cli::cli_alert_success("Expressed {x$trees$n} FFT{?s} in words.")

  }


  # Output: ----

  return(x)

} # fftrees_ffttowords().

# eof.
