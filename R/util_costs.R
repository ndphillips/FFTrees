# util_costs.R:
# Cost-related auxiliary/utility functions.
# ------------------------------------------

# Cost-related helper functions:


# cost_cues_append: ------

# Goal: Get cost.cues for ALL cues in data.
# ToDo: Distinguish function input/argument from output.

cost_cues_append <- function(formula,
                             data,
                             cost.cues = NULL) {

  # Prepare: ------

  criterion_name <- paste(formula)[2]

  data_mf <- model.frame(
    formula = formula,
    data = data
  )

  cue_df <- data_mf[, 2:ncol(data_mf), drop = FALSE]
  cue_name_v <- names(cue_df)


  # Main: ------

  if (is.null(cost.cues)) { # Case 1: No cost.cues provided: Use cost_cues_default ----

    cost.cues <- lapply(1:ncol(cue_df), FUN = function(x) {
      cost_cues_default
    })
    names(cost.cues) <- names(cue_df)


  } else { # if (is.null(cost.cues) == FALSE) { # Case 2: cost.cues provided: ----

    # Make sure all named cues in cost.cues are in data:
    {
      cue_not_in_data <- sapply(names(cost.cues), FUN = function(x) {
        x %in% cue_name_v == FALSE
      })

      if (any(cue_not_in_data)) {

        missing_cues <- paste(cost.cues[cue_not_in_data, 1], collapse = ",")

        warning(paste0("The cue(s) {", missing_cues, "} specified in cost.cues are not present in the data."))
      }
    }

    # Add any missing cue costs as cost_cues_default:
    {
      cost_cues_org <- cost.cues

      cost.cues <- lapply(1:ncol(cue_df), FUN = function(x) {
        cost_cues_default
      })
      names(cost.cues) <- names(cue_df)

      for (i in 1:length(cost.cues)) {

        cue_name_i <- names(cost.cues)[i]

        if (names(cost.cues)[i] %in% names(cost_cues_org)) {
          cost.cues[[i]] <- cost_cues_org[[cue_name_i]]
        }
      }
    }
  } # if (is.null(cost.cues) == FALSE).


  # Output: ------

  return(cost.cues)

} # cost_cues_append().




# ToDo: ------

# - Collect misc. cost calculation functions in this file (rather than in util_stats()).

# eof.
