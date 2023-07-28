data = readr::read_csv("inst/test_data_bout.csv")

standardize_data = function(df, subset = TRUE) {
  HEADER_TIMESTAMP = TIME = HEADER_TIME_STAMP = X = Y = Z = NULL
  rm(list = c("HEADER_TIMESTAMP", "HEADER_TIME_STAMP", "X", "Y", "Z",
              "TIME"))
  # uppercase
  colnames(df) = toupper(colnames(df))
  cn = colnames(df)
  if ("TIME" %in% cn && !"HEADER_TIME_STAMP" %in% cn) {
    df = df %>%
      dplyr::rename(HEADER_TIME_STAMP = TIME)
  }
  if ("HEADER_TIMESTAMP" %in% cn && !"HEADER_TIME_STAMP" %in% cn) {
    df = df %>%
      dplyr::rename(HEADER_TIME_STAMP = HEADER_TIMESTAMP)
  }
  if (subset) {
    df = df %>%
      dplyr::select(HEADER_TIME_STAMP, X, Y, Z)
  }
  df
}

#' Preprocesses accelerometer bout to a common format.
#'
#' Resample 3-axial input signal to a predefined sampling rate and compute
#' vector magnitude.
#'
#' @param data A `data.frame` with a column for time in `POSIXct` (usually
#' `HEADER_TIMESTAMP`, and `X`, `Y`, `Z`
#' @param sample_rate sampling frequency, coercible to an integer
#'
#' @return A list of `vm_bout`, which is an unnamed list of length 2,
#' containing times and VM (vector magnitude) interpolated.  This
#' will be passed into other `forest` functions.  The list also contains
#' `vm_data`, which transforms `vm_bout` into a `data.frame` after
#' creating the required times.
#' @export
#'
#' @examples
preprocess_bout = function(data, sample_rate = 10L) {
  assertthat::assert_that(
    assertthat::is.count(sample_rate)
  )
  sample_rate = as.integer(sample_rate)

  oak = oak_base()
  np = reticulate::import("numpy")

  data = standardize_data(data, subset = TRUE)
  stopifnot(
    all(
      c("HEADER_TIME_STAMP", "X", "Y", "Z") %in% colnames(data)
    )
  )

  tz = lubridate::tz(data$HEADER_TIME_STAMP)
  data$HEADER_TIME_STAMP = as.numeric(data$HEADER_TIME_STAMP)
  timestamp = np$array(data$HEADER_TIME_STAMP, dtype = "float64")
  x = np$array(data[["X"]], dtype="float64")
  y = np$array(data[["Y"]], dtype="float64")
  z = np$array(data[["Z"]], dtype="float64")
  rm(data)

  vm_bout = oak$preprocess_bout(
    t_bout = timestamp,
    x_bout = x,
    y_bout = y,
    z_bout = z,
    fs = sample_rate)
  names(vm_bout) = c("time", "vm")
  # vm_data = reticulate::py_to_r(vm_bout)
  vm_data = vm_bout

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
