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
# Note: A value of 1 (for all cues in data) corresponds to mcu.

cost_cues_default <- 0  # (global constant)



# - cue_classes: ----

# FFTrees only distinguishes/uses 2 types of cues / predictors:
# - character (converted from factor / logical)
# - numerical (integer / double )
# See clean_data() and handle_NA_data() in util_data.R for conversions.

cue_classes <- c( #  (global constant)
  "c",  # categorical: character, factor, logical
  "n")  # numeric: integer, double



# - fft_node_sep: ----

# A node separation marker in tree definitions (symbol):

fft_node_sep <- ";"  # (global constant)



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

# Exit types (as numeric):
# 1. exit_types[1]: 0   representing FALSE, left, noise N
# 2. exit_types[2]: 1   representing TRUE, right, signal S
# 3. exit_types[3]: 0.5 representing 2/4, both, final

exit_types <- c(0, 1, 0.5)  # (global constant)



# - stopping_rules: ----

# Stopping rules for (as character string):
# - "exemplars"  #
# - "levels"     # ToDo: implement by allowing stopping.par > 1
# - "statdelta"  # ToDo: stop 1 level earlier (when criterion satisfied)

stopping_rules <- c("exemplars", "levels", "statdelta")  # (global constant)



# Handling NA values: ------


# - allow_NA_pred: ----

# Allow NA cases in predictors (as logical)?

allow_NA_pred <- TRUE  # (global constant)



# - allow_NA_crit: ----

# Allow NA cases in criterion (as logical)?

allow_NA_crit <- FALSE  # (global constant)



# - replace_NA_num_pred: ----

# Replace NA values in numeric predictors (by mean of predictor)?
# - TRUE replaces NA in numeric predictors (currently by their mean);
# - FALSE (by default) keeps NA values (but tries handling them later)

replace_NA_num_pred <- FALSE  # (global constant)



# - fin_NA_options: ----

# Options/policy for dealing with NA values in final nodes / leaf cues:

fin_NA_options <- c("noise", "signal", "baseline", "majority")  # (global constant)



# User feedback: ------

# - debug: ----

# Provide additional details (as debugging feedback):

debug <- FALSE  # (global constant)



# The following are now obsolete, as handled by the FFTrees() argument:
# quiet = list(ini = TRUE, fin = FALSE, mis = FALSE, set = TRUE)
#
# # - quiet.ini: ----
#
# # Boolean: Should user feedback for initialization (of a command or process) be shown (i.e., NOT hidden/quiet)?
#
# quiet.ini <- TRUE # FALSE # default: TRUE
#
#
# # - quiet.fin: ----
#
# # Boolean: Should user feedback for finalization/success (of a command or process) be shown (i.e., NOT hidden/quiet)?
#
# quiet.fin <- FALSE # TRUE # default: FALSE
#
#
# # - quiet.set: ----
#
# # Boolean: Should user feedback for parameter settings be shown (i.e., NOT hidden/quiet)?
#
# quiet.set <- FALSE # TRUE # FALSE # default: FALSE
#
#
# # - feed_types: ----
#
# feed_types <- 0L:3L  # options: 4 levels
#
#
# # - ufeed: ----
#
# # #' @param ufeed User feedback level (as an integer from \code{0} to \code{3},
# # #' with higher values implying more detailed user feedback).
# # #' Setting \code{ufeed = 0} provides no user feedback.
# # #' Default: \code{ufeed = 2} (show parameter settings and success alerts).
#
#
# ufeed <- 3L  # user feedback level (from feed_types 0:3)
#
#
# if (ufeed %in% feed_types == FALSE){
#
#   msg <- paste0("The value of 'ufeed' must be in: ", paste(feed_types, collapse = ", "))
#
#   stop(msg)
#
# }
#
#
# # Define custom ufeed categories (from 0 to 3):
#
# if (ufeed == 0){ # most basic/elementary/sparse:
#
#   # quiet <- TRUE  # hide progress
#
#   quiet.ini <- TRUE  # hide initials
#   quiet.set <- TRUE  # hide settings
#   quiet.fin <- TRUE  # hide success
#
# } else if (ufeed == 1){ # show success/warnings:
#
#   # quiet <- FALSE  # show progress
#
#   quiet.ini <- TRUE   # hide initials
#   quiet.set <- TRUE   # hide settings
#   quiet.fin <- FALSE  # show success
#
# } else if (ufeed == 2){ # add settings:
#
#   # quiet <- FALSE  # show progress
#
#   quiet.ini <- TRUE   # hide initials
#   quiet.set <- FALSE  # show settings
#   quiet.fin <- FALSE  # show success
#
# } else if (ufeed == 3){ # most detailed/explicit:
#
#   # quiet <- FALSE  # show progress
#
#   quiet.ini <- FALSE  # show initials
#   quiet.set <- FALSE  # show settings
#   quiet.fin <- FALSE  # show success
#
# }



# ToDo: ------

# - etc.

# eof.
