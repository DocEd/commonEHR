% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{parse_range}
\alias{parse_range}
\title{Parse range limits}
\usage{
parse_range(ranges = NULL)
}
\arguments{
\item{ranges}{a character vector of length 1.}
}
\value{

}
\description{
Parse range limits
}
\examples{
# Inclusive interval
parse_range("[0, 100]")
# Non-inclusive interval
parse_range("(0, 100)")
# Open positive interval
parse_range("[0, Inf)")
}
