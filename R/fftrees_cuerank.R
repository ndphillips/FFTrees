#' Calculate thresholds that optimize some statistic (goal) for cues in data
#'
#' \code{fftrees_cuerank} takes an \code{FFTrees} object \code{x} and
#' optimizes its \code{goal.threshold} (from \code{x$params}) for all cues in
#' \code{newdata} (of type \code{data}).
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
#' @param newdata A dataset with cues to be ranked (as data frame).
#' @param data The type of data with cues to be ranked (as character: \code{'train'}, \code{'test'}, or \code{'dynamic'}).
#' Default: \code{data = 'train'}.
#' @param rounding integer. An integer value indicating the decimal digit
#' to which non-integer numeric cue thresholds are to be rounded.
#' Default: \code{rounding = NULL} (i.e., no rounding).
#'
#' @return A modified \code{FFTrees} object (with cue rank information
#' for the current \code{data} type in \code{x$cues$stats}).
#'
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom dplyr pull
#' @importFrom stats median var
#'
#' @export

fftrees_cuerank <- function(x = NULL,
                            newdata = NULL,
                            data = "train",  # type of data
                            rounding = NULL) {

  # Prepare: ------

  # Verify inputs: ----

  # x:
  testthat::expect_true(!is.null(x))
  testthat::expect_s3_class(x, class = "FFTrees")

  # newdata:
  testthat::expect_true(!is.null(newdata))
  testthat::expect_true(is.data.frame(newdata), info = "Provided 'newdata' are not a data.frame")

  # data:
  testthat::expect_true(data %in% c("train", "test", "dynamic"))  # type of data


  # Define criterion (vector) and cues (data.frame): ----

  cue_df <- newdata[names(newdata) != x$criterion_name]
  criterion_v <- newdata[[x$criterion_name]]
  cases_n <- length(criterion_v)
  cue_n <- ncol(cue_df)

  # Verify: Make sure there is variance in the criterion!
  testthat::expect_true(length(unique(criterion_v)) > 1)

  # Store safe copy:
  criterion_v_org <- criterion_v

  # Determine current goal.threshold:
  goal.threshold <- x$params$goal.threshold  # (assign ONCE here and then use below)

  # Provide user feedback:
  if (!x$params$quiet$ini) {

    # msg <- paste0("Aiming to rank ", cue_n, " cues (optimizing '", goal.threshold, "'):\n")
    # cat(u_f_ini(msg))

    cli::cli_alert("Rank {cue_n} cue{?s} (optimizing '{goal.threshold}'):",
                   class = "alert-start")

  }


  # Define progress bar: ----

  if (any(sapply(x$params$quiet, isFALSE))) {

    # # Using progress pkg:
    # pb <- progress::progress_bar$new(format = u_f_msg("[:bar] :percent"),
    #                                  width = 60,
    #                                  total = cue_n, clear = FALSE, show_after = .200)

    # Using cli pkg:
    options(cli.progress_show_after = 1/5,  # delay of bar? (default = 2)
            cli.progress_clear = FALSE,     # clear after finishing? (default = TRUE)
            cli.spinner = "line")           # spinner for cli_progress_bar() of type = "tasks"?

    n_1 <- paste0("  Ranking ", cue_n, " cues: ")
    cli::cli_progress_bar(name = n_1, total = cue_n)  # linear bar

    # n_2 <- paste0("Ranking cues.")
    # cli::cli_progress_bar(name = n_2, total = cue_n, type = "tasks") # tasks

  }



  # Main: Loop over cues: ------

  for (cue_i in 1:cue_n) {

    # Progress bar update:
    if (any(sapply(x$params$quiet, isFALSE))) {

      # pb$tick()
      # Sys.sleep(1 / cue_n)

      cli::cli_progress_update()
      Sys.sleep(1 / cue_n)

    }

    # Re-store from safe copy (to allow dropping NA cases for every cue_i):
    criterion_v  <- criterion_v_org

    # Get key information of the current cue:
    cue_i_name  <- names(cue_df)[cue_i]
    cue_i_class <- class(as.vector(cue_df %>% dplyr::pull(cue_i))) # dplyr/tidyverse
    # cue_i_class <- class(as.vector(cue_df[[cue_i_name]]))  # base R
    cue_i_v     <- unlist(cue_df[ , cue_i])
    cue_i_cost  <- x$params$cost.cues[[cue_i_name]]

    # Problem: cue_i_class can be c("matrix", "array")
    # print(cue_i_class)   # 4debugging: 1. before
    #
    # if ("matrix" %in% cue_i_class){
    #
    #   cue_i_class <- cue_class_of_matrix(cue_i_v, cue_i_class)
    #   print(cue_i_class)   # 4debugging: 2. after
    #
    # }
    #
    # FIXED: Added as.vector() when determining cue_i_class() above.

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


      # +++ here now +++


      # Handle NA values: ------

      if ( allow_NA_pred | allow_NA_crit ){

        # Detect NA values: ----

        ix_NA_cue  <- is.na(cue_i_v)      # 1. NA in cue_i_v
        ix_NA_crit <- is.na(criterion_v)  # 2. NA in criterion_v


        # Report NA values (prior to removing them): ----

        if (!x$params$quiet$mis) { # Provide user feedback:

          # 1. Report NA in cue_i_v:
          if (allow_NA_pred & any(ix_NA_cue)){

            sum_NA_cue <- sum(ix_NA_cue)

            # Which corresponding values in criterion_v will be removed?
            rem_criterion_v <- criterion_v[ix_NA_cue]
            rem_criterion_s <- paste0(rem_criterion_v, collapse = ", ")

            cli::cli_alert_warning("Dropping {sum_NA_cue} NA value{?s} from {cue_i_name} and {x$criterion_name} = c({rem_criterion_s}).")

          }

          # 2. Report NA in criterion_v:
          if (allow_NA_crit & any(ix_NA_crit)){

            # d_type <- typeof(criterion_v)  # logical
            sum_NA_crit <- sum(ix_NA_crit)

            # Which values in cue_i_v will be removed?
            rem_cue_i_v <- cue_i_v[ix_NA_crit]
            rem_cue_i_s <- paste0(rem_cue_i_v, collapse = ", ")

            cli::cli_alert_warning("Dropping {sum_NA_crit} NA value{?s} from {x$criterion_name} and {cue_i_name} = c({rem_cue_i_s}).")

          }

        } # if (!x$params$quiet$mis).


        # Main: Filter vectors ----

        # # A: Remove NA and infinite values (from both):
        # both_finite <- is.finite(cue_i_v) & is.finite(criterion_v)
        #
        # cue_i_v      <- cue_i_v[both_finite]
        # criterion_v  <- criterion_v[both_finite]


        # B. Remove only NA cases (from both):
        both_not_NA  <- !ix_NA_cue & !ix_NA_crit

        cue_i_v      <- cue_i_v[both_not_NA]
        criterion_v  <- criterion_v[both_not_NA]


      } # Handle NA: if ( allow_NA_pred | allow_NA_crit ).



      # Step 3: Determine best direction and threshold for cue [cue_i_best]: ----
      {

        # Parameters:
        cost.outcomes  <- x$param$cost.outcomes
        # goal.threshold <- x$params$goal.threshold  # (assigned above)
        sens.w <- x$params$sens.w

        # A. Compute cue statistics (based on cue type): ----

        # a. Numeric/integer cue: ----
        if (substr(cue_i_class, 1, 1) %in% c("n", "i")) {

          # Compute stats for numeric cue:
          cue_i_stats <- fftrees_threshold_numeric_grid(
            thresholds = cue_i_levels,
            cue_v = cue_i_v,
            criterion_v = criterion_v,
            directions = directions,
            #
            goal.threshold = goal.threshold,
            #
            sens.w = sens.w,
            #
            my.goal = x$params$my.goal,
            my.goal.fun = x$params$my.goal.fun,
            #
            cost.each = cue_i_cost,
            cost.outcomes = cost.outcomes
          )

        } # if a. numeric/integer cue.

        # b. Factor, character, or logical cue: ----

        if (substr(cue_i_class, 1, 1) %in% c("f", "c", "l")) {

          # Compute stats for factor cue:
          cue_i_stats <- fftrees_threshold_factor_grid(
            thresholds = cue_i_levels,
            cue_v = cue_i_v,
            criterion_v = criterion_v,
            directions = directions,
            #
            goal.threshold = goal.threshold,
            #
            sens.w = sens.w,
            #
            my.goal = x$params$my.goal,
            my.goal.fun = x$params$my.goal.fun,
            #
            cost.each = cue_i_cost,
            cost.outcomes = cost.outcomes
          )

        } # if b. factor/character/logical cue.

        # # Get feedback (4debugging):
        # print(paste0(cue_i, ": cue_i_stats of cue_i_name = ", cue_i_name, ":"))
        # print(cue_i_stats)

        # Re-set rownames:
        # rownames(cue_i_stats) <- 1:nrow(cue_i_stats)
        # Note: NOT needed and potentially confusing (when comparing results).

        # Store results (i.e., ALL cue thresholds for cue_i): ----
        x$cues$thresholds[[data]][[cue_i_name]] <- cue_i_stats


        # B. Identify the best threshold (based on goal.threshold): ----

        # Get thresholds that maximize current goal.threshold:
        best_result_index <- which(cue_i_stats[goal.threshold] == max(cue_i_stats[goal.threshold], na.rm = TRUE))
        # Note that cost_dec and cost are NEGATIVE in cue_i_stats (so that goal.threshold == "cost" is MINimized)!

        # Handle 2 special cases:
        if (length(best_result_index) > 1) { # 1. multiple best indices:
          best_result_index <- best_result_index[1]  # take the 1st   ToDo: Is this the best way? Randomize?
        }

        if (is.na(best_result_index)) { # 2. NO best index:
          best_result_index <- 1  # use 1st   ToDo: Is this the best way? Randomize?
          warning(paste0("For cue ", cue_i_name, ", best_result_index was NA. Used 1st threshold..."))
        }

        cue_i_best <- cue_i_stats[best_result_index, ]  # get corresponding row of cue_i_stats

      } # Step 3.


    } else { # (B) All cue values are NA:

      # Step 3: Set cue direction and threshold of BEST cue to NULL [cue_i_best]: ----
      {

        # Compute stats for best factor cue:
        cue_i_best <- fftrees_threshold_factor_grid(
          thresholds = NULL,
          cue_v = NULL,
          criterion_v = NULL,
          # NOTE: NO directions = directions.
          #
          goal.threshold = goal.threshold,
          #
          sens.w = sens.w,
          #
          my.goal = x$params$my.goal,
          my.goal.fun = x$params$my.goal.fun,
          #
          # Note: NO cost.each.
          cost.outcomes = cost.outcomes
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



  # Set rownames: ----

  rownames(cuerank_df) <- 1:nrow(cuerank_df)


  # Add cost.cues (which are a constant for each cue, i.e., do NOT vary by decision outcomes): ----

  cuerank_df$cost_cue <- unlist(x$params$cost.cues[match(cuerank_df$cue, names(x$params$cost.cues))])


  # Re-order columns: ----

  cuerank_df <- cuerank_df[, c("cue", "class", setdiff(names(cuerank_df), c("cue", "class")))]


  # Store in x$cues$stats (as df): ----

  x$cues$stats[[data]] <- cuerank_df


  # Progress bar:

  cli::cli_progress_done()


  # Provide user feedback:
  if (!x$params$quiet$fin) {

    # msg <- paste0("Successfully ranked ", cue_n, " cues.\n")
    # cat(u_f_fin(msg))

    cli::cli_alert_success("Ranked {cue_n} cue{?s} (optimizing '{goal.threshold}').")

  }


  # Output: ------

  return(x)

} # fftrees_cuerank().

# eof.
