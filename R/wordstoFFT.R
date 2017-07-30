
#' Converts text describing an FFT into an FFT definition.
#'
#' @param input string. A string describing an FFT in words (see examples)
#' @param cue.names string. A vector of cue names
#' @param decision.labels string. A vector of decision labels
#'
#' @export
#'
#' @importFrom stringr str_extract str_detect
#' @examples
#'
#' my.tree.def <- wordstoFFT(input = "If age > 55, predict True.
#'                                    If cp = {a,b,np}, predict False, otherwise, predict True",
#'                           cue.names = names(heartdisease))
#'
#'
wordstoFFT <- function(input,
                       cue.names,
                       decision.labels = NULL) {

#

   # input = "If thal = {rd,fd}, predict True. If cp != {a}, predict False. If ca <= 0, predict False, otherwise, predict True"
   # cue.names <- names(heartdisease)
   # decision.labels = c("Not M", "Myopathic")


if(is.null(decision.labels)) {decision.labels <- c("False", "True")}
if(grepl(decision.labels[1], x = input) == FALSE) {

  if(grepl("true", x = tolower(input))) {

    decision.labels <- c("False", "True")

  } else {stop("Something is wrong with decision.labels as they are not in the input.")}

}

#
#   input = "if thal = {rd}, healthy. If cp = {a}, disease, otherwise, healthy"
#   cue.names = names(heartdisease)
#   decision.labels = c("healthy", "disease")


directions.df <- data.frame(directions = c("=",  ">",  ">=", "<",  "<=", "!=", "equal", "equals", "equal to", "greater", "less"),
                            negations =  c("!=", "<=", "<",  ">=", ">",  "=",   "!=",    "!=",     "!=",       "<=",      ">="),
                            directions.f = c("=", ">", ">=", "<", "<=", "!=", "=", "=", "=", ">", "<"),
                            stringsAsFactors = FALSE)

exits.df <- data.frame(exit.char = decision.labels,
                       exit = c("0", "1"),
                       stringsAsFactors = FALSE)


# Split

cue.names.l <- tolower(cue.names)
input <- tolower(input)
decision.labels <- tolower(decision.labels)

def <- unlist(strsplit(input, split = "if", fixed = TRUE))
def <- def[2:length(def)]
nodes.n <- length(def)



# cues.v
{
cues.v <- names(unlist(lapply(def[1:nodes.n], FUN = function(node.sentence) {

  # Can I find the name of a cue in this sentence?
  cue.exists <- any(sapply(cue.names.l, FUN = function(cue.i) {any(stringr::str_detect(node.sentence, paste0(" ", cue.i, " ")))}))

  if(!cue.exists) {stop(paste("I could not find any valid cue names in the sentence: '", node.sentence, "'. Please rewrite", sep = ""))}

  if(cue.exists) {

  output <- which(sapply(cue.names.l, FUN = function(cue.i) {stringr::str_detect(node.sentence, paste0(" ", cue.i, " "))}))


  }



  return(output)

  })))

# Convert cue names back to original (non lower) values
cues.v <- cue.names[sapply(cues.v, FUN = function(x) {which(cue.names.l == x)})]

}

# classes.v
{
classes.v <- rep(NA, nodes.n)

contains.brack <- stringr::str_detect(def[1:nodes.n], "\\[") | stringr::str_detect(def[1:nodes.n], "\\{")
classes.v[contains.brack] <- "c"
classes.v[contains.brack == FALSE] <- "n"
}

# exits.v
{
exits.v <- unlist(lapply(def[1:nodes.n], FUN = function(node.sentence) {


  # Indices of TRUE

  x <- unlist(strsplit(node.sentence, " "))
  true.indices <- grep(decision.labels[2], x = x)
  false.indices <- grep(decision.labels[1], x = x)

  if(any(grepl(decision.labels[2], x)) & any(grepl(decision.labels[1], x))) {

    if(min(true.indices) < min(false.indices)) {return(1)}
    if(min(true.indices) > min(false.indices)) {return(0)}

  }

  if(any(grepl(decision.labels[2], x)) & !any(grepl(decision.labels[1], x))) {

    return(1)
  }

  if(!any(grepl("v", x)) & any(grepl(decision.labels[1], x))) {

    return(0)
  }


}))
}

# thresholds.v
{
thresholds.v <- sapply(def[1:nodes.n], FUN = function(x) {

  # Is there a number?
  num.log <- grepl("[0-9]", x = x)

  # Is there a brace?
  bracket.log <- grepl("\\{", x = x)

  # If there is a number and no brace, get the number

  if(num.log & !bracket.log) {

    threshold.i <- stringr::str_extract(x, "[-+]?\\d+\\.*\\d*")

  }

  # If no number and bracket, get what's inside the braces (and remove any spaces)

  if(!num.log & bracket.log) {

    threshold.i <- stringr::str_replace_all(unlist(strsplit(x, "\\{|\\}"))[2], pattern = " ", "")

  }

  return(threshold.i)


})
}

# directions.v
{
  # Look for directions in sentences

  directions.v <- names(unlist(lapply(def[1:nodes.n], FUN = function(node.sentence) {

    output <- which(sapply(directions.df$directions, FUN = function(direction.i) {

      stringr::str_detect(node.sentence, direction.i)}))


    output <- output[length(output)]


    return(output)

  })))

  directions.index <- sapply(directions.v, function(direction.i) {which(direction.i == directions.df$directions)})

  # Look for negations in sentences
  negations <- c("not")

  # Which sentences have negations?
  negations.log <- unlist(lapply(def[1:nodes.n], FUN = function(node.sentence) {

    output <- any(sapply(negations, FUN = function(negation.i) {

      stringr::str_detect(node.sentence, negation.i)}))

    return(output)

  }))

  # Convert negation directions
  directions.v[negations.log] <- directions.df$negations[directions.index[negations.log]]

  # now convert to directions.f
  directions.v <- directions.df$directions.f[match(directions.v, table = directions.df$directions)]

  # If any directions are 0, then flip the direction

  flip.direction.log <- exits.v == 0

  directions.v[flip.direction.log] <- directions.df$negations[match(directions.v[flip.direction.log], table = directions.df$directions)]


}


# Set final exit to .5
exits.v[nodes.n] <- ".5"


output <- data.frame(tree = 1,
                     nodes = nodes.n,
                     "classes" = paste(classes.v, collapse = ";"),
                     "cues" = paste(cues.v, collapse = ";"),
                     "directions" = paste(directions.v, collapse = ";"),
                     "thresholds" = paste(thresholds.v, collapse = ";"),
                     "exits" = paste(exits.v, collapse = ";"), stringsAsFactors = FALSE)



return(output)

}
