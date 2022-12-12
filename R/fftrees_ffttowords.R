#' Describe a fast-and-frugal tree (FFT) in words
#'
#' @description \code{fftrees_ffttowords} provides a verbal description
#' of an FFT (in an \code{FFTrees} object).
#'
#' \code{fftrees_ffttowords} is the complement to
#' \code{\link{fftrees_wordstofftrees}}, which parses a verbal description
#' of an FFT into an \code{FFTrees} object.
#'
#' The final sentence (or tree node) of the FFT's description
#' always predicts positive criterion values (i.e., TRUE instances) first,
#' before predicting negative criterion values (i.e., FALSE instances).
#' Note that this may require a reversal of cue directions (if the
#' original tree description predicted FALSE instances
#' before predicting TRUE instances).
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
  tree_df <- x$trees$definitions  # (definitions are df)

  # Prepare: ----

  x$trees$inwords <- vector("list", length = n_trees)

  exit_word <- tolower(exit_word(mydata))  # either 'train':'decide' or 'test':'predict'



  # LOOPs: ------


  # Loop 1 (over trees): ----

  for (tree in 1:n_trees) {

    # Extract definition of current tree:
    classes_v    <- trimws(unlist(strsplit(tree_df$classes[tree], ";")))
    cues_v       <- trimws(unlist(strsplit(tree_df$cues[tree], ";")))
    directions_v <- trimws(unlist(strsplit(tree_df$directions[tree], ";")))
    thresholds_v <- trimws(unlist(strsplit(tree_df$thresholds[tree], ";")))
    exits_v      <- trimws(unlist(strsplit(tree_df$exits[tree], ";")))

    decision.labels <- x$params$decision.labels  # (Note: keep "." naming, as in list x)

    n_nodes <- length(cues_v)

    sentences_v <- c()


    # Loop 2 (over nodes/levels): ----

    for (i in 1:n_nodes) {

      exits_i <- paste(exits_v[i])

      # 1. Non-final nodes: ----

      if (exits_i %in% c("0", "1")) {

        # a. Node with positive exit:
        if (exits_i == "1") {

          if (classes_v[i] %in% c("c", "l")) {

            sentence_i <- paste0(
              "If ", cues_v[i], " ", directions_v[i], " {", thresholds_v[i],
              "}, ", exit_word, " ", decision.labels[2], ".")

          }

          if (classes_v[i] %in% c("n", "i")) {
            threshold_i <- thresholds_v[i]

            threshold_i <- round(as.numeric(thresholds_v[i]), 2)

            sentence_i <- paste0(
              "If ", cues_v[i], " ", directions_v[i], " ", threshold_i,
              ", ", exit_word, " ", decision.labels[2], ".")
          }

        } # a. Node with positive exit.


        # b. Node with negative exit:
        if (exits_i == "0") {

          # Negate the direction:
          direction_i <- switch(directions_v[i],
                                "=" = "!=",
                                "!=" = "=",
                                ">" = "<=",
                                "<" = ">=",
                                ">=" = "<",
                                "<=" = ">"
          )

          if (classes_v[i] %in% c("c", "l")) {

            sentence_i <- paste0(
              "If ", cues_v[i], " ", direction_i, " {", thresholds_v[i], "}, ",
              exit_word, " ", decision.labels[1], ".")

          }

          if (classes_v[i] %in% c("n", "i")) {

            threshold_i <- thresholds_v[i]
            threshold_i <- round(as.numeric(thresholds_v[i]), 2)

            sentence_i <- paste0(
              "If ", cues_v[i], " ", direction_i, " ", threshold_i,
              ", ", exit_word, " ", decision.labels[1], ".")

          }

        } # b. Node with negative exit.

        sentences_v <- c(sentences_v, sentence_i)

      } # non-final nodes.


      # 2. Final nodes: ----

      if (exits_i %in% c(".5", "0.5", "0.50", "0.500")) {

        direction_pos_i <- directions_v[i]

        # # Negate direction:
        # direction_neg_i <- switch(directions_v[i],
        #                           "=" = "!=",
        #                           "!=" = "=",
        #                           ">" = "<=",
        #                           "<" = ">=",
        #                           ">=" = "<",
        #                           "<=" = ">"
        # )

        # REMOVED, as negation is only indicated if left exit == decision.labels[2]!
        # +++ here now +++

        if (classes_v[i] %in% c("c", "l")) {

          # # Negative directions (FALSE cases first):
          # sentence_i_1 <- paste0(
          #   "If ", cues_v[i], " ", direction_neg_i, " {", thresholds_v[i], "}, decide ",
          #   decision.labels[1], "") # FALSE cases
          #
          # sentence_i_2 <- paste0(
          #   ", otherwise, decide ",
          #   decision.labels[2], ".") # TRUE cases

          # Positive directions (TRUE cases first):
          sentence_i_1 <- paste0(
            "If ", cues_v[i], " ", direction_pos_i, " {", thresholds_v[i], "}, ",
            exit_word, " ", decision.labels[2], "") # TRUE cases/right

          sentence_i_2 <- paste0(
            ", otherwise, ", exit_word, " ",
            decision.labels[1], ".") # FALSE cases/left

        }

        if (classes_v[i] %in% c("n", "i")) {

          threshold_i <- thresholds_v[i]
          threshold_i <- round(as.numeric(thresholds_v[i]), digits)

          # # Negative directions (FALSE cases first):
          # sentence_i_1 <- paste0(
          #   "If ", cues_v[i], " ", direction_neg_i, " ", thresholds_v[i],
          #   ", decide ", decision.labels[1], "") # FALSE cases
          #
          # sentence_i_2 <- paste0(
          #   ", otherwise, decide ",
          #   decision.labels[2], ".") # TRUE cases

          # Positive directions (TRUE cases first):
          sentence_i_1 <- paste0(
            "If ", cues_v[i], " ", direction_pos_i, " ", thresholds_v[i],
            ", ", exit_word, " ", decision.labels[2], "") # TRUE cases/right

          sentence_i_2 <- paste0(
            ", otherwise, ", exit_word, " ",
            decision.labels[1], ".") # FALSE cases/left

        }

        sentence_i <- paste0(sentence_i_1, sentence_i_2, collapse = "")

        sentences_v <- c(sentences_v, sentence_i)

      } # 2. final nodes.

    } # Loop 2: for (i nodes).


    # # Combine sentences:
    # sentences.comb <- paste(sentences_v, collapse = ". ")

    # Add to FFTrees object x:
    x$trees$inwords[[tree]] <- sentences_v

  } # Loop 2: for (tree).


  # Output: ----

  return(x)

} # fftrees_ffttowords().

# eof.
