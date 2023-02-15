# util_const.R:
# Global constants for FFTrees.
# -----------------------------

# Define global constants
# [marked by "(global constant)"]


# - algorithm_options: ----

# Available algorithms (for FFT creation):

algorithm_options <- c("ifan", "dfan")  # (global constant)


# - goal_options: ----

# A set of default goals (for FFT selection):

goal_options <- c("acc", "bacc", "wacc",  "dprime",  "cost")  # (global constant)


# - cost_outcomes_default: ----

# Outcome cost = error cost / 1 - accuracy / "graded accuracy":
# Note: Values of 0 1 1 0 correspond to (1 - accuracy, r = -1).

cost_outcomes_default <- list(hi = 0, fa = 1, mi = 1, cr = 0)  # (global constant)


# - cost_cues_default: ----

# Cue cost = graded mcu / "graded frugality":
# Note: A value of 1 for all cues in data corresponds to mcu.

cost_cues_default <- 0  # (global constant)


# - fft_node_sep: ----

# A node separation marker in tree definitions (symbol):

fft_node_sep <- ";"  # (global constant)


# - cue_classes: ----

cue_classes <- c("c",  # categorical: character, factor, logical
                 "n")  # numeric: integer, double


# - directions_df: ----

# Direction markers for exit directions in verbal tree descriptions (symbols/words):

direction_same <- c("equal", "identical", "same")
direction_diff <- c("unequal", "different", "differs")

direction_more <- c("more", "larger", "bigger", "greater", "above", "beyond", "exceed")
direction_less <- c("less", "lower", "smaller", "fewer", "below")

directions_df <- data.frame(                          # (global constant)
  direction   = c("=",  ">", ">=", "<",  "<=", "!=",  # 6 directions (as symbols)
                  direction_same,
                  direction_diff,
                  direction_more,
                  direction_less),
  direction_f = c("=",  ">",  ">=", "<",  "<=", "!=",
                  rep("=",  length(direction_same)),    # same
                  rep("!=", length(direction_diff)),    # diff
                  rep(">",  length(direction_more)),    # more
                  rep("<",  length(direction_less))),   # less
  negation    = c("!=", "<=", "<",  ">=", ">",  "=",
                  rep("!=",  length(direction_same)),   # NOT same
                  rep("=",   length(direction_diff)),   # NOT diff
                  rep("<=",  length(direction_more)),   # NOT more
                  rep(">=",  length(direction_less))),  # NOT less
  #
  stringsAsFactors = FALSE)  # (global constant)


# - negations_v: ----

# Negation markers in verbal tree node descriptions (words):

negations_v <- c("not", "is not")  # (global constant)



# - exit_types: ----

# Exit types:

exit_types <- c(0, 1, 0.5)  # (global constant)

# Note:
# 1. exit_types[1]: 0, FALSE, left, noise
# 2. exit_types[2]: 1, TRUE, right, signal
# 3. exit_types[3]: 0.5, 2/4, both, final



# get_exit_type: ----

get_exit_type <- function(x){

  # Prepare: ----
  extype <- rep(NA, length(x))  # initialize

  x <- trimws(tolower(as.character(x))) # 4robustness


  # Main: ----

  # Case 1:
  extype[x == "0"]     <- exit_types[1]
  extype[x == "false"] <- exit_types[1]
  extype[x == "noise"] <- exit_types[1]
  extype[x == "left"]  <- exit_types[1]

  # Case 2:
  extype[x == "1"]      <- exit_types[2]
  extype[x == "true"]   <- exit_types[2]
  extype[x == "signal"] <- exit_types[2]
  extype[x == "right"]  <- exit_types[2]

  # Case 3:
  extype[x == "0.5"]   <- exit_types[3]
  extype[x == "both"]  <- exit_types[3]
  extype[x == "final"] <- exit_types[3]

  # Output: ----

  return(extype)

} # get_exit_type().

# # Check:
# get_exit_type(c(0, FALSE, " Left ", " NOISE "))
# get_exit_type(c(1, TRUE, " RigHT ", " SIGnal "))
# get_exit_type(c(0.5, 2/4, " boTH", " FINal "))


# ToDo: ------

# - etc.

# eof.
