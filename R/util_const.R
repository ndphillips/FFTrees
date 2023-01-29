# util_const.R:
# Global constants for FFTrees.
# -----------------------------

# Define global constants:


# - valid_algorithm: ----

# Available algorithms (for FFT creation):

valid_algorithm <- c("ifan", "dfan")  # (global) constant


# - default_goal: ----

# A set of default goals (for FFT selection):

default_goal <- c("acc", "bacc", "wacc",  "dprime",  "cost")  # (global) constant


# - default_cost_outcomes: ----

# Outcome cost = error cost / 1 - accuracy / "graded accuracy":

default_cost_outcomes <- list(hi = 0, fa = 1, mi = 1, cr = 0)  # (global) constant


# - default_cost_cues: ----

# Cue cost = graded mcu / "graded frugality":

default_cost_cues <- 1  # for all cues in data  # (global) constant


# - fft_node_sep: ----

# A node separation marker in tree definitions (symbol):

fft_node_sep <- ";"  # (global constant)


# - directions_df: ----

# Direction markers for exit directions in verbal tree descriptions (symbols/words):

direction_same <- c("equal", "identical", "same")
direction_diff <- c("unequal", "different", "differs")

direction_more <- c("more", "larger", "bigger", "greater", "above", "beyond", "exceed")
direction_less <- c("less", "lower", "smaller", "fewer", "below")

directions_df <- data.frame(
  direction   = c("=",  ">", ">=", "<",  "<=", "!=",
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



# ToDo: ------

# - etc.

# eof.
