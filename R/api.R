#' Estimate Steps with Unified Syntax
#'
#' @param data A `data.frame` with a column for time in `POSIXct` (usually
#' `HEADER_TIMESTAMP`), and `X`, `Y`, `Z`
#' @param ... additional arguments to pass to specific method
#'
#' @return A `data.frame` of seconds and steps
#' @export
#'
#' @rdname estimate_steps
#' @examples
#' csv_file = system.file("test_data_bout.csv", package = "walking")
#' if (requireNamespace("readr", quietly = TRUE) && walking::have_forest()) {
#'   x = readr::read_csv(csv_file)
#'   colnames(x)[colnames(x) == "UTC time"] = "time"
#'   out = estimate_steps_forest(x, sample_rate_analysis = 10L)
#'   out = estimate_steps_verisense(x, sample_rate = 10L,
#'                                  method = "original")
#'   out = estimate_steps_verisense(x, sample_rate = 10L,
#'                                  method = "revised")
#' }
estimate_steps_forest = function(data, ...) {
  find_walking(data, ...)
}

#' @rdname estimate_steps
#' @param method Either using the original parameters [verisense_count_steps()]
#' or the revised parameters [verisense_count_steps_revised()]
#' @param resample_to_15hz resample the data to 15Hz as `verisense` indicated
#' that was the sample rate this was designed under.
#' @export
estimate_steps_verisense = function(
    data,
    resample_to_15hz = FALSE,
    ...,
    method = c("original", "revised")) {
  if (is.vector(data) && is.numeric(data)) {
    stop("estimate_steps_verisense needs a data set/data.frame")
  }

  assertthat::assert_that(
    assertthat::is.flag(resample_to_15hz)
  )
  data = standardize_data(data, subset = TRUE)
  args = list(...)
  if ("sample_rate" %in% names(args) &&
      resample_to_15hz) {
    warning("sample_rate will be ignored because resample_to_15hz is TRUE")
  }
  if (resample_to_15hz) {
    data = resample_accel_data(
      data = data,
      sample_rate = 15L
    )
    args$sample_rate = 15L
  }
  seconds = unique(lubridate::floor_date(data$HEADER_TIMESTAMP))
  method = match.arg(method)
  func = switch(
    method,
    original = verisense_count_steps,
    revised = verisense_count_steps_revised
  )
  args$data = data
  res = do.call(func, args = args)
  n_seconds = length(seconds)
  n_steps = length(res)
  if (n_seconds - n_steps > 1) {
    stop("Different number of rows for steps: ", n_steps,
            " than time: ", n_seconds, ", time may be wrong!")
  }
  seconds = seconds[seq_along(res)]
  data.frame(
    time = seconds,
    steps = res
  )
}

#' @rdname estimate_steps
#' @export
estimate_steps_sdt = function(data, ...) {
  sdt_count_steps(data, ...)
}
