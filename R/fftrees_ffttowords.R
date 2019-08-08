#' Describes an FFT in words
#'
#' @param x FFTrees. An FFTrees object created with FFTrees()
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
fftrees_ffttowords <- function(x = NULL,
                               digits = 2) {



  x$trees$inwords <- vector("list", length = x$trees$n)

  for(tree in 1:x$trees$n) {

  classes.v <- unlist(strsplit(x$trees$definitions$classes[tree], ";"))
  cues.v <- unlist(strsplit(x$trees$definitions$cues[tree], ";"))
  directions.v <- unlist(strsplit(x$trees$definitions$directions[tree], ";"))
  thresholds.v <- unlist(strsplit(x$trees$definitions$thresholds[tree], ";"))
  exits.v <- unlist(strsplit(x$trees$definitions$exits[tree], ";"))
  decision.labels <- x$params$decision.labels


  nodes.n <- length(cues.v)

  sentences.v <- c()

  for(i in 1:nodes.n) {

    exits.i <- paste(exits.v[i])

if(exits.i %in% c("0", "1")) {

    if(exits.i == "1") {

      if(classes.v[i] == "c") {

        sentence.i <- paste0("If ", cues.v[i], " ", directions.v[i], " {", thresholds.v[i],
                             "}, decide ",decision.labels[2], "")

      }

      if(classes.v[i] %in% c("n", "i")) {

        threshold.i <- thresholds.v[i]
        threshold.i <- round(as.numeric(thresholds.v[i]), 2)

        sentence.i <- paste0("If ", cues.v[i], " ", directions.v[i], " ",threshold.i,
                             ", decide ",decision.labels[2])
      }


    }

    if(exits.i == "0") {

      # Negate the direction
      direction.i <- switch(directions.v[i],
                            "=" = "!=",
                            "!=" = "=",
                            ">" = "<=",
                            "<" = ">=",
                            ">=" = "<",
                            "<=" = ">")

      if(classes.v[i] == "c") {

        sentence.i <- paste0("If ", cues.v[i], " ", direction.i, " {", thresholds.v[i], "}, decide ",
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
                             ", decide ", decision.labels[1], "")
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
                            "<" = ">=",
                            ">=" = "<",
                            "<=" = ">")

      direction.pos.i <- directions.v[i]

      if(classes.v[i] == "c") {

        sentence.i.1 <- paste0("If ", cues.v[i], " ", direction.neg.i, " {", thresholds.v[i], "}, decide ",
                             decision.labels[1])


        sentence.i.2 <- paste0(", otherwise, decide ",
                               decision.labels[2])

        # sentence.i.2 <- paste0(", otherwise, if ", cues.v[i], " ", direction.pos.i, " {", thresholds.v[i], "}, predict ",
        #                       decision.labels[2])

      }

      if(classes.v[i] %in% c("n", "i", "l")) {

        threshold.i <- thresholds.v[i]

        if(classes.v[i] %in% c("n", "i")) {

        threshold.i <- round(as.numeric(thresholds.v[i]), digits)

        }


        sentence.i.1 <- paste0("If ", cues.v[i], " ", direction.neg.i, " ", thresholds.v[i], ", decide ",
                               decision.labels[1])


        sentence.i.2 <- paste0(", otherwise, decide ",
                               decision.labels[2])


        # sentence.i.2 <- paste0(", otherwise, if ", cues.v[i], " ", direction.pos.i, " ", thresholds.v[i], ", predict ",
        #                        decision.labels[2])

      }

      sentence.i <- paste0(sentence.i.1, sentence.i.2, collapse = "")


      sentences.v <- c(sentences.v, sentence.i)


    }

  }

  sentences.comb <- paste(sentences.v, collapse = ". ")

  x$trees$inwords[[tree]] <- sentences.v

  }

  return(x)

}
