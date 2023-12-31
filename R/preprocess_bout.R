



process_vm_bout = function(vm_bout, tz, sample_rate = 10L) {
  names(vm_bout) = c("time", "vm")
  # vm_data = reticulate::py_to_r(vm_bout)
  vm_data = vm_bout

  vm_bout$time = round(vm_bout$time, 3)
  vm_bout$time = as.POSIXct(
    vm_bout$time,
    tz = tz,
    origin = lubridate::origin)
  vm_data$time = floor(vm_data$time)

  vm_data$time = seq(vm_data$time[1],
                     vm_data$time[length(vm_data$time)] + 1,
                     by = 1/sample_rate)
  vm_data$time = vm_data$time[1:length(vm_data$vm)]

  vm_data$time = as.POSIXct(
    vm_data$time,
    tz = tz,
    origin = lubridate::origin)
  vm_data = as.data.frame(vm_data)
  list(vm_bout = vm_bout,
       vm_data = vm_data)
}

pythonize_data = function(data) {
  np = reticulate::import("numpy")

  data = standardize_data(data, subset = TRUE)
  stopifnot(
    all(
      c("HEADER_TIME_STAMP", "X", "Y", "Z") %in% colnames(data)
    )
  )

  orig_tz = lubridate::tz(data$HEADER_TIME_STAMP)
  data$HEADER_TIME_STAMP = as.numeric(data$HEADER_TIME_STAMP)
  timestamp = np$array(data$HEADER_TIME_STAMP, dtype = "float64")
  x = np$array(data[["X"]], dtype="float64")
  y = np$array(data[["Y"]], dtype="float64")
  z = np$array(data[["Z"]], dtype="float64")
  list(timestamp = timestamp,
       x = x,
       y = y,
       z = z,
       orig_tz = orig_tz)
}

#' Preprocesses accelerometer bout to a common format.
#'
#' Resample 3-axial input signal to a predefined sampling rate and compute
#' vector magnitude.
#'
#' @param data A `data.frame` with a column for time in `POSIXct` (usually
#' `HEADER_TIMESTAMP`), and `X`, `Y`, `Z`
#' @param sample_rate sampling frequency, coercible to an integer.
#' This is the sampling rate you're sampling the data *into*.
#'
#' @return A list of `vm_bout`, which is an unnamed list of length 2,
#' containing times and VM (vector magnitude) interpolated.  This
#' will be passed into other `forest` functions.  The list also contains
#' `vm_data`, which transforms `vm_bout` into a `data.frame` after
#' creating the required times.
#' @export
#'
#' @examples
#' csv_file = system.file("test_data_bout.csv", package = "walking")
#' if (requireNamespace("readr", quietly = TRUE)) {
#'   x = readr::read_csv(csv_file)
#'   colnames(x)[colnames(x) == "UTC time"] = "time"
#'
#'   res = preprocess_bout(data = x)
#'   res2 = preprocess_bout_r(data = x)
#'   testthat::expect_equal(res$vm_bout, res2$vm_bout, tolerance = 1e-4)
#' }
preprocess_bout = function(data, sample_rate = 10L) {
  assertthat::assert_that(
    assertthat::is.count(sample_rate)
  )
  sample_rate = as.integer(sample_rate)

  oak = oak_base()

  py_data = pythonize_data(data)
  rm(data)

  vm_bout = oak$preprocess_bout(
    t_bout = py_data$timestamp,
    x_bout = py_data$x,
    y_bout = py_data$y,
    z_bout = py_data$z,
    fs = sample_rate)
  process_vm_bout(vm_bout, tz = py_data$orig_tz, sample_rate = sample_rate)
}


#' @rdname preprocess_bout
#' @export
preprocess_bout_r = function(data, sample_rate = 10L) {
  assertthat::assert_that(
    assertthat::is.count(sample_rate)
  )
  sample_rate = as.integer(sample_rate)

  oak = oak_base()
  np = reticulate::import("numpy")
  sp = reticulate::import("scipy")
  interpolate = sp$interpolate

  py_data = pythonize_data(data)
  rm(data)

  t_bout = py_data$timestamp
  x = py_data$x
  y = py_data$y
  z = py_data$z
  orig_tz = py_data$orig_tz
  rm(py_data)
  # xt_bout = t_bout

  t_bout_interp = t_bout - t_bout[1]
  t_bout_interp = np$arange(t_bout_interp[1],
                            t_bout_interp[length(t_bout_interp)],
                            (1/sample_rate))
  t_bout_interp = t_bout_interp + t_bout[1]

  f = interpolate$interp1d(t_bout, x)
  x_bout_interp = f(t_bout_interp)
  rm(x)

  f = interpolate$interp1d(t_bout, y)
  y_bout_interp = f(t_bout_interp)
  rm(y)

  f = interpolate$interp1d(t_bout, z)
  z_bout_interp = f(t_bout_interp)
  rm(z)
  rm(t_bout)

  # adjust bouts using designated function
  x_bout_interp = oak$adjust_bout(x_bout_interp)
  y_bout_interp = oak$adjust_bout(y_bout_interp)
  z_bout_interp = oak$adjust_bout(z_bout_interp)

  # number of full seconds of measurements
  num_seconds = np$floor(length(x_bout_interp)/sample_rate)

  # trim and decimate t
  index = 1:as.integer(num_seconds*sample_rate)
  t_bout_interp = t_bout_interp[index]
  t_bout_interp = t_bout_interp[seq(1, length(t_bout_interp), by = sample_rate)]

  # calculate vm
  vm_bout_interp = np$sqrt(x_bout_interp**2 +
                             y_bout_interp**2 +
                             z_bout_interp**2)

  # standardize measurement to gravity units (g) if its recorded in m/s**2
  if (np$mean(vm_bout_interp) > 5) {
    x_bout_interp = x_bout_interp/9.80665
    y_bout_interp = y_bout_interp/9.80665
    z_bout_interp = z_bout_interp/9.80665
  }

  # calculate vm after unit verification
  vm_bout_interp = np$sqrt(x_bout_interp**2 +
                             y_bout_interp**2 +
                             z_bout_interp**2) - 1

  vm_bout = list(
    t_bout_interp,
    vm_bout_interp
  )
  process_vm_bout(vm_bout, tz = orig_tz, sample_rate = sample_rate)
}




#' Standardize the Accelerometry Data
#'
#' @inheritParams preprocess_bout
#' @param subset should only the `HEADER_TIME_STAMP` (if available)
#' and `XYZ` be subset?
#'
#' @return A `data.frame` with `X/Y/Z` and a time in
#' `HEADER_TIME_STAMP` (if available).
#' @export
standardize_data = function(data, subset = TRUE) {
  HEADER_TIMESTAMP = TIME = HEADER_TIME_STAMP = X = Y = Z = NULL
  rm(list = c("HEADER_TIMESTAMP", "HEADER_TIME_STAMP", "X", "Y", "Z",
              "TIME"))
  if (is.matrix(data)) {
    if (is.numeric(data)) {
      stopifnot(ncol(data) == 3)
      data = as.data.frame(data)
      colnames(data) = c("X", "Y", "Z")
    } else {
      stop("data is a matrix and cannot be coerced to necessary structure")
    }
  }
  # uppercase
  colnames(data) = toupper(colnames(data))
  cn = colnames(data)
  if ("TIME" %in% cn && !"HEADER_TIME_STAMP" %in% cn) {
    data = data %>%
      dplyr::rename(HEADER_TIME_STAMP = TIME)
  }
  if ("HEADER_TIMESTAMP" %in% cn && !"HEADER_TIME_STAMP" %in% cn) {
    data = data %>%
      dplyr::rename(HEADER_TIME_STAMP = HEADER_TIMESTAMP)
  }
  if ("HEADER_TIME_STAMP" %in% colnames(data)) {
    if (is.unsorted(data$HEADER_TIME_STAMP)) {
      stop("Time in data must be sorted before running!")
    }
  }
  if (subset) {
    data = data %>%
      dplyr::select(dplyr::any_of("HEADER_TIME_STAMP"), X, Y, Z)
  }
  stopifnot(all(c("X", "Y", "Z") %in% colnames(data)))
  data
}

#' @export
#' @rdname standardize_data
standardise_data = standardize_data

xyz_data = function(data) {
  data = standardize_data(data)
  as.matrix(data[, c("X", "Y", "Z"), drop = FALSE])
}
