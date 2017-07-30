#' Describes an FFT in words
#'
#' @param x FFTrees. An FFTrees object created with FFTrees()
#' @param tree integer. An integer specifying which tree in the object to verbalise. The default is \code{x$tree.max}
#' @param classes.v integer.
#' @param cues.v integer.
#' @param directions.v string.
#' @param thresholds.v string.
#' @param exits.v numeric.
#' @param decision.labels string. A string vector (of length 2) indicating labels for negative (0 or FALSE) and positive (1 or TRUE) cases in the crterion
#' @param digits integer. How many digits to round numeric values
#'
#' @return A list of string vectors
#' @export
#'
#' @examples
#'
#' heart.fft <- FFTrees(diagnosis ~.,
#'                      data = heartdisease,
#'                      decision.labels = c("Healthy", "Disease"))
#'
#' inwords(heart.fft)
#'
inwords <- function(x = NULL,
                   tree = NULL,
                   classes.v = NULL,
                   cues.v = NULL,
                   directions.v = NULL,
                   thresholds.v = NULL,
                   exits.v = NULL,
                   decision.labels = NULL,
                   digits = 2) {

#
# x <- y
# tree = NULL
# classes.v = NULL
# cues.v = NULL
# directions.v = NULL
# thresholds.v = NULL
# exits.v = NULL
# decision.labels = NULL
# digits = 2

  if(is.null(x) == FALSE) {

  if(is.null(decision.labels)) {decision.labels <- x$params$decision.labels}

  if(is.null(tree)) {

    tree <- x$tree.max

  }

  tree.def <- x$tree.definitions[tree,]

  classes.v <- unlist(strsplit(tree.def$classes, ";"))
  cues.v <- unlist(strsplit(tree.def$cues, ";"))
  directions.v <- unlist(strsplit(tree.def$directions, ";"))
  thresholds.v <- unlist(strsplit(tree.def$thresholds, ";"))
  exits.v <- unlist(strsplit(tree.def$exits, ";"))

  }

  if(is.null(x)) {

    if(is.null(decision.labels)) {decision.labels <- c("False", "True")}

  }

  nodes.n <- length(cues.v)

  sentences.v <- c()

  for(i in 1:nodes.n) {

    exits.i <- paste(exits.v[i])

if(exits.i %in% c("0", "1")) {

    if(exits.i == "1") {

      if(classes.v[i] == "c") {

        sentence.i <- paste0("If ", cues.v[i], " ", directions.v[i], " {", thresholds.v[i],
                             "}, predict ",decision.labels[2], "")

      }

      if(classes.v[i] %in% c("n", "i")) {

        threshold.i <- thresholds.v[i]
        threshold.i <- round(as.numeric(thresholds.v[i]), 2)

        sentence.i <- paste0("If ", cues.v[i], " ", directions.v[i], " ",threshold.i,
                             ", predict ",decision.labels[2])
      }


    }

    if(exits.i == "0") {

      # Negate the direction
      direction.i <- switch(directions.v[i],
                            "=" = "!=",
                            "!=" = "=",
                            ">" = "<=",
                            "<" = ">=")

      if(classes.v[i] == "c") {

        sentence.i <- paste0("If ", cues.v[i], " ", direction.i, " {", thresholds.v[i], "}, predict ",
                             decision.labels[1], "")

      }

      if(classes.v[i] %in% c("n", "i", "l")) {

        threshold.i <- thresholds.v[i]

        if(classes.v[i] == "l") {

          threshold.i <- paste(thresholds.v[i])

        } else {

        threshold.i <- round(as.numeric(thresholds.v[i]), 2)

        }

        sentence.i <- paste0("If ", cues.v[i], " ", direction.i, " ", threshold.i,
                             ", predict ", decision.labels[1], "")
      }


    }

  sentences.v <- c(sentences.v, sentence.i)

}

    if(exits.i %in% c(".5", "0.5")) {

      # Negate the direction
      direction.neg.i <- switch(directions.v[i],
                            "=" = "!=",
                            "!=" = "=",
                            ">" = "<=",
                            "<" = ">=")

      direction.pos.i <- directions.v[i]

      if(classes.v[i] == "c") {

        sentence.i.1 <- paste0("If ", cues.v[i], " ", direction.neg.i, " {", thresholds.v[i], "}, predict ",
                             decision.labels[1])


        sentence.i.2 <- paste0(", otherwise, predict ",
                               decision.labels[2])

        # sentence.i.2 <- paste0(", otherwise, if ", cues.v[i], " ", direction.pos.i, " {", thresholds.v[i], "}, predict ",
        #                       decision.labels[2])

      }

      if(classes.v[i] %in% c("n", "i", "l")) {

        threshold.i <- thresholds.v[i]

        if(classes.v[i] %in% c("n", "i")) {

        threshold.i <- round(as.numeric(thresholds.v[i]), digits)

        }


        sentence.i.1 <- paste0("If ", cues.v[i], " ", direction.neg.i, " ", thresholds.v[i], ", predict ",
                               decision.labels[1])


        sentence.i.2 <- paste0(", otherwise, predict ",
                               decision.labels[2])


        # sentence.i.2 <- paste0(", otherwise, if ", cues.v[i], " ", direction.pos.i, " ", thresholds.v[i], ", predict ",
        #                        decision.labels[2])

      }

      sentence.i <- paste0(sentence.i.1, sentence.i.2, collapse = "")


      sentences.v <- c(sentences.v, sentence.i)


    }



  }

  sentences.comb <- paste(sentences.v, collapse = ". ")

  output <- list("v1" = sentences.v, "v2" = sentences.comb)

  return(output)

}
