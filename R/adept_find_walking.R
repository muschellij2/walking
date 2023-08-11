#' Finds walking and calculate steps from raw acceleration data.
#'
#' Method finds periods of repetitive and continuous oscillations with
#' predominant frequency occurring within know step frequency range.
#' Frequency components are extracted with Continuous Wavelet Transform.
#'
#' @param data A `data.frame` with a column for time in `POSIXct` (usually
#' `HEADER_TIMESTAMP`), and `X`, `Y`, `Z`
#' @param sample_rate sampling frequency (in Hz) **for the data**.
#' @param ... Additional arguments to pass to [adept::segmentWalking()]
#' @param verbose print diagnostic messages
#'
#' @return A vector of number of steps per second

#' @export
#'
#' @examples
#' csv_file = system.file("test_data_bout.csv", package = "walking")
#' if (requireNamespace("readr", quietly = TRUE) && have_forest()) {
#'   x = readr::read_csv(csv_file)
#'   colnames(x)[colnames(x) == "UTC time"] = "time"
#'   res = adept_find_walking(data = x, sample_rate = 15L)
#' }
adept_find_walking = function(
    data,
    sample_rate,
    template = NULL,
    ...,
    verbose = TRUE
) {

  assertthat::assert_that(
    assertthat::is.count(sample_rate)
  )
  sample_rate = as.integer(sample_rate)

  data = standardize_data(data, subset = TRUE)
  xyz = xyz_data(data)

  if (is.null(template)) {
    warning("No template given, assuming Left wrist templates!")
    all_wrist_templates = adeptdata::stride_template$left_wrist
    template = do.call(rbind, all_wrist_templates)
    template = apply(template, 1, identity, simplify = FALSE)
  }

  #' # Running with the matrix
  result <- adept::segmentWalking(
    xyz = xyz,
    xyz.fs = sample_rate,
    template = template,
    ...)

  return(result)
}

