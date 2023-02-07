#' Convert a verbal description of an FFT into an \code{FFTrees} object
#'
#' @description \code{fftrees_wordstofftrees} converts a verbal description
#' of an FFT (provided as a string of text) into
#' a tree definition (of an \code{FFTrees} object).
#' Thus, \code{fftrees_wordstofftrees} provides a simple
#' natural language parser for FFTs.
#'
#' \code{fftrees_wordstofftrees} is the complement function to
#' \code{\link{fftrees_ffttowords}}, which converts an abstract tree definition
#' (of an \code{FFTrees} object) into a verbal description
#' (i.e., provides natural language output).
#'
#' To increase robustness, the parsing of \code{fftrees_wordstofftrees}
#' allows for lower- or uppercase spellings (but not typographical variants)
#' and ignores the else-part of the final sentence (i.e., the part
#' beginning with "otherwise").
#'
#' @param x An \code{FFTrees} object.
#' @param my.tree A character string. A verbal description (as a string of text) defining an FFT.
#'
#' @return An \code{FFTrees} object with a new tree definition as described by \code{my.tree}.
#'
#' @seealso
#' \code{\link{fftrees_ffttowords}} for converting FFTs into verbal descriptions;
#' \code{\link{print.FFTrees}} for printing FFTs;
#' \code{\link{plot.FFTrees}} for plotting FFTs;
#' \code{\link{summary.FFTrees}} for summarizing FFTs;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @importFrom stringr str_extract str_detect
#'
#' @export

fftrees_wordstofftrees <- function(x,
                                   my.tree) {

  # Provide user feedback: ----

  if (!x$params$quiet) {
    cat(u_f_ini("Aiming to create an FFT from 'my.tree' description:\n"))
  }

  # Parameters / options: ------

  # # Direction markers (symbols/words):
  # directions_df <- data.frame(
  #   direction   = c( "=",  ">", ">=", "<",  "<=", "!=", "equal", "equals", "equal to", "greater", "less"),
  #   negation    = c("!=", "<=", "<",  ">=", ">",   "=",  "!=",    "!=",     "!=",       "<=",      ">=" ),
  #   direction_f = c( "=",  ">", ">=", "<",  "<=", "!=",   "=",     "=",      "=",       ">",       "<"  ),
  #   #
  #   stringsAsFactors = FALSE
  # ) # (local constant)

  # exits_df <- data.frame(     # is NOT used anywhere?
  #   exit.char = x$params$decision.labels,
  #   exit = c("0", "1"),       # 0:left/noise vs. 1:right/signal
  #   stringsAsFactors = FALSE
  # )


  # Clean up and check my.tree: ------

  # Collapse into one sentence:
  if (length(my.tree) > 1) {
    my.tree <- paste(my.tree, collapse = ". ")
  }

  # Remove line breaks (\n):
  my.tree <- gsub(pattern = "\n", replacement = "", x = my.tree)

  # Use lowercase spelling (for robustness against typos):
  my.tree         <- tolower(my.tree)
  cue_names_l     <- tolower(x$cue_names)
  decision_labels <- tolower(x$params$decision.labels)


  # Verify the presence of both decision labels/exit types (at least once):

  lbl_0 <- decision_labels[1]  # exit type 0: False/noise/0/left
  if (all(grepl(lbl_0, x = my.tree) == FALSE)) {
    warning(paste0("The decision label '", decision_labels[1], "' does not occur in my.tree."))
  }

  lbl_1 <- decision_labels[2]  # exit type 1: True/signal/1/right
  if (all(grepl(lbl_1, x = my.tree) == FALSE)) {
    warning(paste0("The decision label '", decision_labels[2], "' does not occur in my.tree."))
  }

  # Note: As the final else/'otherwise' part is ignored, rake trees CAN mention only 1 exit type.
  #       Thus, enforcing that both exit types are mentioned (at least once) is too restrictive.
  # Done: Turn stops into warnings, but provide feedback which exit type is not being mentioned.


  # Split my.tree into def parts (dropping "otherwise" clause): ------

  def <- unlist(strsplit(my.tree, split = "if ", fixed = TRUE))  # Note: Also removes trailing " " after "if"!
  def <- paste0(" ", def)    # add leading " " again (to include in detecting cue name below)
  def <- def[2:length(def)]  # remove initial empty string
  # print(def)  # 4debugging

  def_fin_2 <- unlist(strsplit(def[length(def)], split = "otherwise", fixed = TRUE))
  # ToDo: Could be generalized to include "else", "in other cases", etc.
  # print(def_fin_2)  # 4debugging

  # Drop the final sub-sentence (beginning with "otherwise"):
  def <- c(def[-length(def)], def_fin_2[1])
  # print(def)  # 4debugging

  nodes_n <- length(def)


  # Extract FFT elements from def: ------

  # 1. cues_v: ----
  {
    cues_v <- names(unlist(lapply(def[1:nodes_n], FUN = function(node_sentence) {

      # Can I find the name of a cue in this sentence?
      cue_exists <- any(sapply(cue_names_l, FUN = function(cue_i_name) {
        any(stringr::str_detect(node_sentence, paste0(" ", cue_i_name, " ")))
      }))

      if (!cue_exists) {
        stop(paste("I could not find any valid cue names in the sentence: '", node_sentence, "'. Please rewrite", sep = ""))
      }

      if (cue_exists) {
        output <- which(sapply(cue_names_l, FUN = function(cue_i_name) {
          stringr::str_detect(node_sentence, paste0(" ", cue_i_name, " "))
        }))
      }

      return(output)

    })))

    # Convert cue names back to original (non lower) values:
    cues_v <- x$cue_names[sapply(cues_v, FUN = function(x) {
      which(cue_names_l == x)
    })]
  }


  # 2. classes_v: ----
  {
    classes_v <- rep(NA, nodes_n)

    contains_brack <- stringr::str_detect(def[1:nodes_n], "\\[") | stringr::str_detect(def[1:nodes_n], "\\{")
    classes_v[contains_brack] <- "c"
    classes_v[contains_brack == FALSE] <- "n"

  }

  # 3. exits_v: ----
  {
    exits_v <- unlist(lapply(def[1:nodes_n], FUN = function(node_sentence) {

      y <- unlist(strsplit(node_sentence, " "))

      false_ix <- grep(tolower(decision_labels[1]), x = y)  # indices of FALSE/noise/0//left
      true_ix  <- grep(tolower(decision_labels[2]), x = y)  # indices of TRUE/signal/1/right

      if (any(grepl(decision_labels[2], x)) & any(grepl(decision_labels[1], y))) {

        if (min(true_ix) < min(false_ix)) {
          return(1)
        }

        if (min(true_ix) > min(false_ix)) {
          return(0)
        }

      }

      if (any(grepl(decision_labels[2], y)) & !any(grepl(decision_labels[1], y))) {
        return(1)
      }

      if (!any(grepl("v", y)) & any(grepl(decision_labels[1], y))) {
        return(0)
      }

    }))

    # print(exits_v)  # 4debugging
  }

  # 4. thresholds_v: ----
  {
    thresholds_v <- sapply(1:nodes_n, FUN = function(i) {

      # Get definition:
      x <- def[i]

      # Remove the name of the cue:
      x <- gsub(pattern = tolower(cues_v[i]), replacement = "", x = x)

      # Is there a number?
      num_log <- grepl("[0-9]", x = x)

      # Is there a brace?
      bracket_log <- grepl("\\{", x = x)

      # If there is a number and no brace, get the number:
      if (!bracket_log & num_log) {
        threshold_i <- stringr::str_extract(x, "[-+]?\\d+\\.*\\d*")
      }

      # If there is a brace get what's inside the braces (and remove any spaces):
      if (bracket_log) {
        threshold_i <- stringr::str_replace_all(unlist(strsplit(x, "\\{|\\}"))[2], pattern = " ", "")
      }

      return(threshold_i)

    })
  }


  # 5. directions_v: ----
  {

    # A. Detect directions and negations: ----

    # Look for directions in sentences:

    directions_v <- names(unlist(lapply(def[1:nodes_n], FUN = function(node_sentence) {
      output <- which(sapply(directions_df$direction, FUN = function(direction_i) {
        stringr::str_detect(node_sentence, direction_i)
      }))

      output <- output[length(output)]

      return(output)

    })))

    directions_ix <- sapply(directions_v, function(direction_i) {
      which(direction_i == directions_df$direction)
    })

    # Look for negations in sentences:

    # Define negation markers:
    # negations_v <- c("not")  # (local constant)

    # Which sentences have negations?
    negations_log <- unlist(lapply(def[1:nodes_n], FUN = function(node_sentence) {
      output <- any(sapply(negations_v, FUN = function(negation_i) {
        stringr::str_detect(node_sentence, negation_i)
      }))

      return(output)

    }))


    # B. Adjust / flip directions: ----

    # Convert negated directions / negations:
    directions_v[negations_log] <- directions_df$negation[directions_ix[negations_log]]

    # Convert to direction_f (formal symbol/forward direction/to signal):
    directions_v <- directions_df$direction_f[match(directions_v, table = directions_df$direction)]

    # If any directions are 0, flip their direction:
    flip_direction_ix <- (exits_v == 0)

    directions_v[flip_direction_ix] <- directions_df$negation[match(directions_v[flip_direction_ix], table = directions_df$direction)]

  }


  # Set final exit to .5: ----

  exits_v[nodes_n] <- ".5"


  # Save result in x$trees$definitions (1 line, as df): ----

  # NEW code start: ----

  cur_fft <- data.frame(class = classes_v,
                        cue = cues_v,
                        direction = directions_v,
                        threshold = thresholds_v,
                        exit = exits_v,
                        #
                        stringsAsFactors = FALSE
  )

  my_tree_def <- write_fft_df(fft = cur_fft, tree = 1L)
  # print(my_tree_def)  # 4debugging

  # NEW code end. ----

  # +++ here now +++

  # OLD code start: ----

  # # fft_node_sep <- ";"  # (local constant)
  #
  # my_tree_def_o <- data.frame(
  #   # Add. variables:
  #   tree       = 1L,
  #   nodes      = nodes_n,
  #   # Key variables of fft (all plural):
  #   classes    = paste(classes_v,    collapse = fft_node_sep),
  #   cues       = paste(cues_v,       collapse = fft_node_sep),
  #   directions = paste(directions_v, collapse = fft_node_sep),
  #   thresholds = paste(thresholds_v, collapse = fft_node_sep),
  #   exits      = paste(exits_v,      collapse = fft_node_sep),
  #   #
  #   stringsAsFactors = FALSE
  # )
  # # print(my_tree_def_o)  # 4debugging

  # OLD code end. ----


  # # Check: Verify equality of OLD and NEW code results:
  # if (!all.equal(my_tree_def, my_tree_def_o)) { stop("OLD vs. NEW: my_tree_def diff") }


  # Modify object x:
  x$trees$definitions <- my_tree_def
  x$trees$n <- 1L


  # Provide user feedback: ----

  if (!x$params$quiet) {
    cat(u_f_fin("Successfully created an FFT from 'my.tree' description.\n"))
  }

  # Output: ------

  return(x)

} # fftrees_wordstofftrees().


# ToDo: ------

# - Abstraction: Store anonymous functions as utility functions
#   (to enable re-use from elsewhere).

# eof.
