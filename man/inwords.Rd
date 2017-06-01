% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inwords.R
\name{inwords}
\alias{inwords}
\title{Describes an FFT in words}
\usage{
inwords(x = NULL, tree = NULL, classes.v = NULL, cues.v = NULL,
  directions.v = NULL, thresholds.v = NULL, exits.v = NULL,
  decision.labels = NULL, digits = 2)
}
\arguments{
\item{x}{FFTrees. An FFTrees object created with FFTrees()}

\item{tree}{integer. An integer specifying which tree in the object to verbalise. The default is \code{x$tree.max}}

\item{classes.v}{integer.}

\item{cues.v}{integer.}

\item{directions.v}{string.}

\item{thresholds.v}{string.}

\item{exits.v}{numeric.}

\item{decision.labels}{string. A string vector (of length 2) indicating labels for negative (0 or FALSE) and positive (1 or TRUE) cases in the crterion}

\item{digits}{integer. How many digits to round numeric values}
}
\value{
A list of string vectors
}
\description{
Describes an FFT in words
}
\examples{

heart.fft <- FFTrees(diagnosis ~.,
                     data = heartdisease,
                     decision.labels = c("Healthy", "Disease"))

inwords(heart.fft)

}