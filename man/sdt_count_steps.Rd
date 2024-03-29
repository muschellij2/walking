% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdt.R
\name{sdt_count_steps}
\alias{sdt_count_steps}
\title{Estimate Steps via SDT}
\usage{
sdt_count_steps(
  data,
  sample_rate,
  order = 4L,
  high = 0.25,
  low = 2.5,
  location = c("wrist", "waist"),
  verbose = TRUE
)
}
\arguments{
\item{data}{A \code{data.frame} with a column for time in \code{POSIXct} (usually
\code{HEADER_TIMESTAMP}), and \code{X}, \code{Y}, \code{Z}}

\item{sample_rate}{sampling frequency (in Hz)}

\item{order}{order of bandpass Butterworth filter order}

\item{high}{highpass threshold for filter}

\item{low}{lowpass threshold for filter}

\item{location}{location of the device, which indicates a different
threshold for peaks}

\item{verbose}{print diagnostic messages}
}
\value{
A \code{data.frame} of \code{time}and \code{steps}
}
\description{
Estimate Steps via SDT
}
\examples{
csv_file = system.file("test_data_bout.csv", package = "walking")
if (requireNamespace("readr", quietly = TRUE)) {
  x = readr::read_csv(csv_file)
  colnames(x)[colnames(x) == "UTC time"] = "time"
  res = sdt_count_steps(data = x, sample_rate = 100L)
}
}
