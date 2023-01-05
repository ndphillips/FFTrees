#' Calculate thresholds that optimize some statistic (goal) for cues in data
#'
#' \code{fftrees_cuerank} takes an \code{FFTrees} object \code{x} and
#' optimizes its \code{goal.threshold} (from \code{x$params}) for all cues in
#' a dataset \code{newdata} (of some \code{data} type).
#'
#' \code{fftrees_cuerank} creates a data frame \code{cuerank_df}
#' that is added to \code{x$cues$stats}.
#'
#' Note that the cue directions and thresholds computed by \strong{FFTrees}
#' always predict positive criterion values (i.e., \code{TRUE} or signal,
#' rather than \code{FALSE} or noise).
#' Using these thresholds for negative exits (i.e., for predicting instances of
#' \code{FALSE} or noise) usually requires a reversal (e.g., negating cue direction).
#'
#' \code{fftrees_cuerank} is called (twice) by the \code{fftrees_grow_fan} algorithm
#' to grow fast-and-frugal trees (FFTs).
#'
#' @param x An \code{FFTrees} object.
#' @param newdata The dataset to with cues to be ranked (as data frame).
#' @param data The type of data with cues to be ranked (as character: \code{'train'}, \code{'test'}, or \code{'dynamic'}).
#' Default: \code{data = 'train'}.
#' @param rounding Number of digits used to round (as integer).
#' Default: \code{rounding = NULL}.
#'
#' @return A modified \code{FFTrees} object (with cue rank information
#' for the current \code{data} type in \code{x$cues$stats}).
#'
#' @importFrom stats median var
#' @importFrom progress progress_bar
#' @importFrom dplyr pull
#'
#' @export

fftrees_cuerank <- function(x = NULL,
                            newdata = NULL,
                            data = "train",
                            rounding = NULL) {

  # Prepare: ------

  # Verify: ----

  testthat::expect_true(!is.null(newdata))
  testthat::expect_true(!is.null(x))
  testthat::expect_true(data %in% c("train", "test", "dynamic"))


  # Define criterion (vector) and cues (dataframe): ----

  cue_df <- newdata[names(newdata) != x$criterion_name]
  criterion_v <- newdata %>% dplyr::pull(x$criterion_name)
  cases_n <- length(criterion_v)
  cue_n <- ncol(cue_df)

  # Validation: Make sure there is variance in the criterion!
  testthat::expect_true(length(unique(criterion_v)) > 1)

  # Provide user feedback:
  if (!x$params$quiet) {
    msg <- paste0("Aiming to rank ", cue_n, " cues:\n")
    cat(u_f_ini(msg))
  }

  # Define progress bar:

  if (!x$params$quiet) {

    pb <- progress::progress_bar$new(format = u_f_msg("[:bar] :percent"),
                                     width = 70,
                                     total = cue_n, clear = FALSE, show_after = .200)

    # cli::cli_progress_bar("Ranking cues", total = cue_n)

  }


  # Main: Loop over cues: ------

  for (cue_i in 1:cue_n) {

    # Progress bar update:
    if (!x$params$quiet) {

      pb$tick()
      Sys.sleep(1 / cue_n)

      # Sys.sleep(10/100)
      # cli::cli_progress_update()

    }

    # Get main information about current cue:

    cue_i_name <- names(cue_df)[cue_i]
    cue_i_class <- class(cue_df %>% dplyr::pull(cue_i))
    cue_i_v <- unlist(cue_df[, cue_i])
    cue_i_cost <- x$params$cost.cues[[cue_i_name]]


    if (all(is.na(cue_i_v)) == FALSE) { # (A) Some non-missing values:

      # Step 1: Determine possible cue levels [cue_i_levels] ----
      {

        # A. Numeric and integer cues: ----

        if (substr(cue_i_class, 1, 1) %in% c("n", "i")) {

          # 1. "optimize" method: ----

          if (x$params$numthresh.method == "o") {

            # Get all possible (sorted) cue values:
            cue_i_levels <- sort(unique(unlist(cue_i_v)))

            # If too long, reduce to numthresh.n:
            if (length(cue_i_levels) > x$params$numthresh.n) {
              indicies <- round(seq(1, length(cue_i_levels), length.out = x$params$numthresh.n), 0)
              cue_i_levels <- cue_i_levels[indicies]
            }

          }

          # 2. "median" method: ----

          if (x$params$numthresh.method == "m") {

            if (isTRUE(all.equal(length(unique(unlist(cue_i_v))), 2))) {
              cue_i_levels <- unique(unlist(cue_i_v))
            } else {
              cue_i_levels <- median(unlist(cue_i_v))
            }

          }

          # Round cue levels:
          if (!is.null(rounding)) {

            cue_i_levels <- round(cue_i_levels,
                                  digits = rounding
            )

          }

          # Remove potential duplicates:
          cue_i_levels <- cue_i_levels[duplicated(cue_i_levels) == FALSE]

        }


        # B. Factors, character, and logical cues ----

        if (substr(cue_i_class, 1, 1) %in% c("f", "c", "l")) {

          # Use all unique cue values:
          cue_i_levels <- sort(unique(unlist(cue_i_v)))

          # If there are > 50% unique cue.levels, provide a warning:
          if (length(cue_i_levels) > .50 * nrow(newdata)) {

            warning(paste0("The cue ", names(cue_df)[cue_i], " is nominal and contains mostly unique values. This could lead to dramatic overfitting.\nConsider excluding this cue or reducing the number of unique values."))

          }
        }

        # Check for cue levels containing protected characters (;):
        if (any(sapply(cue_i_levels, FUN = function(x) {

          grepl(";", x = x)

        }))) {

          stop(paste0("The cue ", names(cue_df)[cue_i], " contains the character ';' which is not allowed.\nPlease replace this value in the data and try again."))
        }

      } # Step 1.


      # Step 2: Determine possible cue directions [directions]: ----
      {
        if (substr(cue_i_class, 1, 1) %in% c("n", "i")) {
          directions <- c(">", "<=")
        }

        if (substr(cue_i_class, 1, 1) %in% c("c", "f", "l")) {
          directions <- c("=", "!=")
        }

      } # Step 2.


      # Step 3: Determine best direction and threshold for cue [cue_i_best]: ----
      {
        cost.outcomes  <- x$param$cost.outcomes
        goal.threshold <- x$params$goal.threshold
        sens.w <- x$params$sens.w

        # a. Numeric, integer cues: ----
        if (substr(cue_i_class, 1, 1) %in% c("n", "i")) {

          cue_i_stats <- fftrees_threshold_numeric_grid(
            thresholds = cue_i_levels,
            cue_v = cue_i_v,
            criterion_v = criterion_v,
            sens.w = sens.w,
            directions = directions,
            cost.each = cue_i_cost,
            cost.outcomes = cost.outcomes,
            goal.threshold = goal.threshold
          )

        } # a. numeric/integer cues.

        # b. Factors, character, and logical cues: ----
        if (substr(cue_i_class, 1, 1) %in% c("f", "c", "l")) {

          cue_i_stats <- fftrees_threshold_factor_grid(
            thresholds = cue_i_levels,
            cue_v = cue_i_v,
            criterion_v = criterion_v,
            directions = directions,
            sens.w = sens.w,
            cost.each = cue_i_cost,
            cost.outcomes = cost.outcomes,
            goal.threshold = goal.threshold
          )

        } # b. factor/character/logical cues.


        # Save results: ----

        x$cues$thresholds[[data]][[cue_i_name]] <- cue_i_stats

        # Get thresholds that maximizes goal.threshold:
        best.result.index <- which(cue_i_stats[x$params$goal.threshold] == max(cue_i_stats[x$params$goal.threshold], na.rm = TRUE))

        # If there are two best indices, take the first:
        #   ToDo: Not sure if this is the best way to do it...

        if (length(best.result.index) > 1) {
          best.result.index <- best.result.index[1]
        }

        if (is.na(best.result.index)) {
          best.result.index <- 1
        }

        cue_i_best <- cue_i_stats[best.result.index, ]

      } # Step 3.


    } else { # (B) All cue values are NA:

      # Step 3: Set best cue direction and threshold to NULL [cue_i_best]: ----
      {

        cue_i_best <- fftrees_threshold_factor_grid(
          thresholds = NULL,
          cue_v = NULL,
          criterion_v = NULL,
          sens.w = sens.w,
          cost.outcomes = cost.outcomes,
          goal.threshold = goal.threshold
        )

      } # Step 3.

    } # if (all(is.na(cue_i_v)).

    # Add name and class:
    cue_i_best$cue   <- cue_i_name
    cue_i_best$class <- cue_i_class

    # Sort into cuerank_df:
    if (cue_i == 1) {
      cuerank_df <- cue_i_best
    }

    if (cue_i > 1) {
      cuerank_df <- rbind(cuerank_df, cue_i_best)
    }

  } # for (cue_i).

  # cli::cli_progress_done()


  # Set rownames: ----

  rownames(cuerank_df) <- 1:nrow(cuerank_df)

  # Add cue costs: ----

  cuerank_df$cost_cue <- unlist(x$params$cost.cues[match(cuerank_df$cue, names(x$params$cost.cues))])


  # Store in x$cues$stats (as df): ----

  cuerank_df <- cuerank_df[, c("cue", "class", setdiff(names(cuerank_df), c("cue", "class")))]  # re-order

  x$cues$stats[[data]] <- cuerank_df


  # Provide user feedback:
  if (!x$params$quiet) {
    msg <- paste0("Successfully ranked ", cue_n, " cues.\n")
    cat(u_f_fin(msg))
  }


  # Output: ------

  return(x)

} # fftrees_cuerank().

# eof.
