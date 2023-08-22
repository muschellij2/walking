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
#'   out = estimate_steps_forest(x, sample_rate = 10L)
#'   out = estimate_steps_verisense(x, sample_rate = 10L,
#'   method = "original")
#'   out = estimate_steps_verisense(x, sample_rate = 10L,
#'   method = "revised")
#' }
estimate_steps_forest = function(data, ...) {
  find_walking(data, ...)
}

#' @rdname estimate_steps
#' @param method Either using the original parameters [verisense_count_steps()]
#' or the revised parameters [verisense_count_steps_revised()]
#' @export
estimate_steps_verisense = function(data, ..., method = c("original", "revised")) {
  if (is.vector(data) && is.numeric(data)) {
    stop("estimate_steps_verisense needs a data set")
  }

  data = standardize_data(data, subset = TRUE)
  seconds = unique(lubridate::floor_date(data$HEADER_TIME_STAMP))
  method = match.arg(method)
  func = switch(
    method,
    original = verisense_count_steps,
    revised = verisense_count_steps_revised
  )
  res = func(data = data, ...)
  data.frame(
    time = seconds,
    steps = res
  )
}
