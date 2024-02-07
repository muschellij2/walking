#' Estimate Steps via SDT
#'
#' @param data A `data.frame` with a column for time in `POSIXct` (usually
#' `HEADER_TIMESTAMP`), and `X`, `Y`, `Z`
#' @param sample_rate sampling frequency (in Hz)
#' @param order order of bandpass Butterworth filter order
#' @param high highpass threshold for filter
#' @param low lowpass threshold for filter
#' @param location location of the device, which indicates a different
#' threshold for peaks
#' @param verbose print diagnostic messages
#'
#' @return A `data.frame` of `time`and `steps`
#' @export
#'
#' @examples
#' csv_file = system.file("test_data_bout.csv", package = "walking")
#' if (requireNamespace("readr", quietly = TRUE)) {
#'   x = readr::read_csv(csv_file)
#'   colnames(x)[colnames(x) == "UTC time"] = "time"
#'   res = sdt_count_steps(data = x, sample_rate = 100L)
#' }
sdt_count_steps <- function(
    data,
    sample_rate,
    order = 4L,
    high = 0.25,
    low = 2.5,
    location = c("wrist", "waist"),
    verbose = TRUE
) {

  peak = HEADER_TIMESTAMP = vm = X = Y = Z = demean_vm = filt_vm = NULL
  rm(list = c("vm", "X", "Y", "Z", "demean_vm", "filt_vm", "peak",
              "HEADER_TIMESTAMP"))
  location = match.arg(location, choices = c("wrist", "waist"))
  threshold = ifelse(location == "wrist", 0.0267, 0.0359)

  data = standardize_data(data, subset = TRUE)
  assertthat::assert_that(
    assertthat::is.count(sample_rate)
  )
  # vm threshold based on location
  # create coefficients for a 4th order bandpass Butterworth filter
  b <- signal::butter(
    n = order,
    W = c(high, low) / (sample_rate / 2),
    type = "pass",
    plane = "z"
  )

  # demean and filter data with dual pass filter to avoid signal shift
  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      vm = sqrt(X^2 + Y^2 + Z^2),
      demean_vm = vm - mean(vm),
      filt_vm = signal::filtfilt(b, demean_vm))

  # find indices in which the value immediately before and immediately
  # after the value is smaller and vm is above threshold
  data <- data %>%
    dplyr::mutate(peak =
                    filt_vm > dplyr::lag(filt_vm) &
                    filt_vm > dplyr::lead(filt_vm) &
                    filt_vm > threshold
    )

  if (verbose) {
    # return steps by second
    message("sdt completed")
  }
  data %>%
    dplyr::group_by(time = lubridate::floor_date(HEADER_TIMESTAMP)) %>%
    dplyr::summarize(steps = sum(peak, na.rm = TRUE))

}
