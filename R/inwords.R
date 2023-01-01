#' Provide a verbal description of an FFT
#'
#' @description \code{inwords} generates and provides a verbal description
#' of a fast-and-frugal tree (FFT) from an \code{FFTrees} object.
#'
#' When \code{data} remains unspecified, \code{inwords} will only look up \code{x$trees$inwords}.
#' When \code{data} is set to either "train" or "test", \code{inwords} first employs
#' \code{\link{fftrees_ffttowords}} to re-generate the verbal descriptions of FFTs in \code{x}.
#'
#' @param x An \code{FFTrees} object.
#' @param data The type of data to which a tree is being applied (as character string "train" or "test").
#' Default: \code{data = NULL} will only look up \code{x$trees$inwords}.
#' @param tree The tree to display (as an integer).
#'
#' @return A verbal description of an FFT (as a character string).
#'
#' @seealso
#' \code{\link{fftrees_ffttowords}} for converting FFTs into verbal descriptions;
#' \code{\link{print.FFTrees}} for printing FFTs;
#' \code{\link{plot.FFTrees}} for plotting FFTs;
#' \code{\link{summary.FFTrees}} for summarizing FFTs;
#' \code{\link{FFTrees}} for creating FFTs from and applying them to data.
#'
#' @export

inwords <- function(x, data = NULL, tree = 1) {

  # Verify inputs: ----

  testthat::expect_s3_class(x, class = "FFTrees")

  testthat::expect_true(dplyr::near(tree %% 1, 0))  # tree is an integer

  if (is.null(data) == FALSE) {

    data <- tolower(data)  # 4robustness

    testthat::expect_true(data %in% c("train", "test"))
    # if (!data %in% c("test", "train")){
    #  stop("The data to print must be 'test' or 'train'.")
    # }

  }


  tree_in_words <- NA  # initialize


  # Main: ----

  if (is.null(data) == FALSE){ # re-generate tree descriptions in x:

    x <- fftrees_ffttowords(x = x, mydata = data, digits = 2)

  }

  tree_in_words <- x$trees$inwords[[tree]]  # look up in x


  # Output: ----

  return(tree_in_words)

} # inwords().


# eof.
