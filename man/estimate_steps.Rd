% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{estimate_steps_forest}
\alias{estimate_steps_forest}
\alias{estimate_steps_verisense}
\title{Estimate Steps with Unified Syntax}
\usage{
estimate_steps_forest(data, ...)

estimate_steps_verisense(data, ..., method = c("original", "revised"))
}
\arguments{
\item{data}{A \code{data.frame} with a column for time in \code{POSIXct} (usually
\code{HEADER_TIMESTAMP}), and \code{X}, \code{Y}, \code{Z}}

\item{...}{additional arguments to pass to specific method}

\item{method}{Either using the original parameters \code{\link[=verisense_count_steps]{verisense_count_steps()}}
or the revised parameters \code{\link[=verisense_count_steps_revised]{verisense_count_steps_revised()}}}
}
\value{
A \code{data.frame} of seconds and steps
}
\description{
Estimate Steps with Unified Syntax
}
\examples{
csv_file = system.file("test_data_bout.csv", package = "walking")
if (requireNamespace("readr", quietly = TRUE) && walking::have_forest()) {
  x = readr::read_csv(csv_file)
  colnames(x)[colnames(x) == "UTC time"] = "time"
  out = estimate_steps_forest(x, sample_rate = 10L)
  out = estimate_steps_verisense(x, sample_rate = 10L,
                                 method = "original")
  out = estimate_steps_verisense(x, sample_rate = 10L,
                                 method = "revised")
}
}
