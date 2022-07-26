#' Describes a fast-and-frugal tree (FFT) in words.
#'
#' @param x An \code{FFTrees} object created with \code{\link{FFTrees}}.
#' @param digits How many digits to round numeric values (as integer)?
#'
#' @return A list of string vectors.
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
#' @export

fftrees_ffttowords <- function(x = NULL,
                               digits = 2) {

  # Prepare: ----
  x$trees$inwords <- vector("list", length = x$trees$n)

  # Loop through trees: ----
  for (tree in 1:x$trees$n) {

    classes.v <- unlist(strsplit(x$trees$definitions$classes[tree], ";"))
    cues.v <- unlist(strsplit(x$trees$definitions$cues[tree], ";"))
    directions.v <- unlist(strsplit(x$trees$definitions$directions[tree], ";"))
    thresholds.v <- unlist(strsplit(x$trees$definitions$thresholds[tree], ";"))
    exits.v <- unlist(strsplit(x$trees$definitions$exits[tree], ";"))
    decision.labels <- x$params$decision.labels

    nodes.n <- length(cues.v)

    sentences.v <- c()

    for (i in 1:nodes.n) {

      exits.i <- paste(exits.v[i])

      if (exits.i %in% c("0", "1")) {

        # 1. Node with positive exit:
        if (exits.i == "1") {

          if (classes.v[i] %in% c("c", "l")) {

            sentence.i <- paste0(
              "If ", cues.v[i], " ", directions.v[i], " {", thresholds.v[i],
              "}, decide ", decision.labels[2], ".")

          }

          if (classes.v[i] %in% c("n", "i")) {
            threshold.i <- thresholds.v[i]

            threshold.i <- round(as.numeric(thresholds.v[i]), 2)

            sentence.i <- paste0(
              "If ", cues.v[i], " ", directions.v[i], " ", threshold.i,
              ", decide ", decision.labels[2], ".")

          }
        }

        # 2. Node with negative exit:
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
              "If ", cues.v[i], " ", direction.i, " {", thresholds.v[i], "}, decide ",
              decision.labels[1], ".")

          }

          if (classes.v[i] %in% c("n", "i")) {

            threshold.i <- thresholds.v[i]
            threshold.i <- round(as.numeric(thresholds.v[i]), 2)

            sentence.i <- paste0(
              "If ", cues.v[i], " ", direction.i, " ", threshold.i,
              ", decide ", decision.labels[1], ".")

          }
        }

        sentences.v <- c(sentences.v, sentence.i)

      }

      # 3. Final node: ----
      if (exits.i %in% c(".5", "0.5")) {

        # Negate the direction:
        direction.neg.i <- switch(directions.v[i],
                                  "=" = "!=",
                                  "!=" = "=",
                                  ">" = "<=",
                                  "<" = ">=",
                                  ">=" = "<",
                                  "<=" = ">"
        )

        direction.pos.i <- directions.v[i]

        if (classes.v[i] %in% c("c", "l")) {

          # negate direction:
          sentence.i.1 <- paste0(
            "If ", cues.v[i], " ", direction.neg.i, " {", thresholds.v[i], "}, decide ",
            decision.labels[1]
          )

          # # NOT negate direction:
          # sentence.i.1 <- paste0(
          #   "If ", cues.v[i], " ", direction.pos.i, " {", thresholds.v[i], "}, decide ",
          #   decision.labels[1]
          # )


          sentence.i.2 <- paste0(
            ", otherwise, decide ",
            decision.labels[2], ".")

          # sentence.i.2 <- paste0(", otherwise, if ", cues.v[i], " ", direction.pos.i, " {", thresholds.v[i], "}, predict ",
          #                       decision.labels[2])

        }

        if (classes.v[i] %in% c("n", "i")) {

          threshold.i <- thresholds.v[i]
          threshold.i <- round(as.numeric(thresholds.v[i]), digits)

          sentence.i.1 <- paste0(
            "If ", cues.v[i], " ", direction.neg.i, " ", thresholds.v[i],
            ", decide ", decision.labels[1], "")

          sentence.i.2 <- paste0(
            ", otherwise, decide ",
            decision.labels[2], "."
          )

          # sentence.i.2 <- paste0(", otherwise, if ", cues.v[i], " ", direction.pos.i, " ", thresholds.v[i], ", predict ",
          #                        decision.labels[2])
        }

        sentence.i <- paste0(sentence.i.1, sentence.i.2, collapse = "")

        sentences.v <- c(sentences.v, sentence.i)

      }
    }

    # # Combine sentences:
    # sentences.comb <- paste(sentences.v, collapse = ". ")

    # Add to FFTrees x:
    x$trees$inwords[[tree]] <- sentences.v

  } # for (tree).

  # Output: ----

  return(x)

} # fftrees_ffttowords().

# eof.
