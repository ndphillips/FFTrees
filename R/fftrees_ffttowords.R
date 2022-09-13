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

  # Prepare: ----

  x$trees$inwords <- vector("list", length = x$trees$n)

  exit_word <- tolower(exit_word(mydata))  # either 'train':'decide' or 'test':'predict'


  # Loop 1 through trees: ----

  for (tree in 1:x$trees$n) {

    classes.v    <- unlist(strsplit(x$trees$definitions$classes[tree], ";"))
    cues.v       <- unlist(strsplit(x$trees$definitions$cues[tree], ";"))
    directions.v <- unlist(strsplit(x$trees$definitions$directions[tree], ";"))
    thresholds.v <- unlist(strsplit(x$trees$definitions$thresholds[tree], ";"))
    exits.v      <- unlist(strsplit(x$trees$definitions$exits[tree], ";"))
    decision.labels <- x$params$decision.labels

    nodes.n <- length(cues.v)

    sentences.v <- c()


    # Loop 2 through nodes: ----

    for (i in 1:nodes.n) {

      exits.i <- paste(exits.v[i])


      # 1. Non-final nodes: ----

      if (exits.i %in% c("0", "1")) {

        # a. Node with positive exit:
        if (exits.i == "1") {

          if (classes.v[i] %in% c("c", "l")) {

            sentence.i <- paste0(
              "If ", cues.v[i], " ", directions.v[i], " {", thresholds.v[i],
              "}, ", exit_word, " ", decision.labels[2], ".")

          }

          if (classes.v[i] %in% c("n", "i")) {
            threshold.i <- thresholds.v[i]

            threshold.i <- round(as.numeric(thresholds.v[i]), 2)

            sentence.i <- paste0(
              "If ", cues.v[i], " ", directions.v[i], " ", threshold.i,
              ", ", exit_word, " ", decision.labels[2], ".")
          }

        } # a. Node with positive exit.


        # b. Node with negative exit:
        if (exits.i == "0") {

          # Negate the direction:
          direction.i <- switch(directions.v[i],
                                "=" = "!=",
                                "!=" = "=",
                                ">" = "<=",
                                "<" = ">=",
                                ">=" = "<",
                                "<=" = ">"
          )

          if (classes.v[i] %in% c("c", "l")) {

            sentence.i <- paste0(
              "If ", cues.v[i], " ", direction.i, " {", thresholds.v[i], "}, ",
              exit_word, " ", decision.labels[1], ".")

          }

          if (classes.v[i] %in% c("n", "i")) {

            threshold.i <- thresholds.v[i]
            threshold.i <- round(as.numeric(thresholds.v[i]), 2)

            sentence.i <- paste0(
              "If ", cues.v[i], " ", direction.i, " ", threshold.i,
              ", ", exit_word, " ", decision.labels[1], ".")

          }

        } # b. Node with negative exit.

        sentences.v <- c(sentences.v, sentence.i)

      } # non-final nodes.


      # 2. Final nodes: ----

      if (exits.i %in% c(".5", "0.5")) {

        direction.pos.i <- directions.v[i]

        # # Negate direction:
        # direction.neg.i <- switch(directions.v[i],
        #                           "=" = "!=",
        #                           "!=" = "=",
        #                           ">" = "<=",
        #                           "<" = ">=",
        #                           ">=" = "<",
        #                           "<=" = ">"
        # )

        # REMOVED, as negation is only indicated if left exit == decision.labels[2]!
        # +++ here now +++

        if (classes.v[i] %in% c("c", "l")) {

          # # Negative directions (FALSE cases first):
          # sentence.i.1 <- paste0(
          #   "If ", cues.v[i], " ", direction.neg.i, " {", thresholds.v[i], "}, decide ",
          #   decision.labels[1], "") # FALSE cases
          #
          # sentence.i.2 <- paste0(
          #   ", otherwise, decide ",
          #   decision.labels[2], ".") # TRUE cases

          # Positive directions (TRUE cases first):
          sentence.i.1 <- paste0(
            "If ", cues.v[i], " ", direction.pos.i, " {", thresholds.v[i], "}, ",
            exit_word, " ", decision.labels[2], "") # TRUE cases/right

          sentence.i.2 <- paste0(
            ", otherwise, ", exit_word, " ",
            decision.labels[1], ".") # FALSE cases/left

        }

        if (classes.v[i] %in% c("n", "i")) {

          threshold.i <- thresholds.v[i]
          threshold.i <- round(as.numeric(thresholds.v[i]), digits)

          # # Negative directions (FALSE cases first):
          # sentence.i.1 <- paste0(
          #   "If ", cues.v[i], " ", direction.neg.i, " ", thresholds.v[i],
          #   ", decide ", decision.labels[1], "") # FALSE cases
          #
          # sentence.i.2 <- paste0(
          #   ", otherwise, decide ",
          #   decision.labels[2], ".") # TRUE cases

          # Positive directions (TRUE cases first):
          sentence.i.1 <- paste0(
            "If ", cues.v[i], " ", direction.pos.i, " ", thresholds.v[i],
            ", ", exit_word, " ", decision.labels[2], "") # TRUE cases/right

          sentence.i.2 <- paste0(
            ", otherwise, ", exit_word, " ",
            decision.labels[1], ".") # FALSE cases/left

        }

        sentence.i <- paste0(sentence.i.1, sentence.i.2, collapse = "")

        sentences.v <- c(sentences.v, sentence.i)

      } # 2. final nodes.

    } # Loop 2: for (i nodes).


    # # Combine sentences:
    # sentences.comb <- paste(sentences.v, collapse = ". ")

    # Add to FFTrees object x:
    x$trees$inwords[[tree]] <- sentences.v

  } # Loop 2: for (tree).


  # Output: ----

  return(x)

} # fftrees_ffttowords().

# eof.
