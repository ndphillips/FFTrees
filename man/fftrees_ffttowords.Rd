% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fftrees_ffttowords.R
\name{fftrees_ffttowords}
\alias{fftrees_ffttowords}
\title{Describes an FFT in words}
\usage{
fftrees_ffttowords(x = NULL, digits = 2)
}
\arguments{
\item{x}{FFTrees. An FFTrees object created with FFTrees()}

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
