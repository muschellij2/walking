% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocess_bout.R
\name{standardize_data}
\alias{standardize_data}
\alias{standardise_data}
\title{Standardize the Accelerometry Data}
\usage{
standardize_data(data, subset = TRUE)

standardise_data(data, subset = TRUE)
}
\arguments{
\item{data}{A \code{data.frame} with a column for time in \code{POSIXct} (usually
\code{HEADER_TIMESTAMP}), and \code{X}, \code{Y}, \code{Z}}

\item{subset}{should only the \code{HEADER_TIME_STAMP} (if available)
and \code{XYZ} be subset?}
}
\value{
A \code{data.frame} with \code{X/Y/Z} and a time in
\code{HEADER_TIME_STAMP} (if available).
}
\description{
Standardize the Accelerometry Data
}
