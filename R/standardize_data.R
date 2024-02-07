

#' Standardize the Accelerometry Data
#'
#' @inheritParams preprocess_bout
#' @param subset should only the `HEADER_TIMESTAMP` (if available)
#' and `XYZ` be subset?
#'
#' @return A `data.frame` with `X/Y/Z` and a time in
#' `HEADER_TIMESTAMP` (if available).
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
  if ("TIME" %in% cn && !"HEADER_TIMESTAMP" %in% cn) {
    data = data %>%
      dplyr::rename(HEADER_TIMESTAMP = TIME)
  }
  if ("HEADER_TIME_STAMP" %in% cn && !"HEADER_TIMESTAMP" %in% cn) {
    data = data %>%
      dplyr::rename(HEADER_TIMESTAMP = HEADER_TIME_STAMP)
  }
  if ("HEADER_TIMESTAMP" %in% colnames(data)) {
    if (is.unsorted(data$HEADER_TIMESTAMP)) {
      stop("Time in data must be sorted before running!")
    }
  }
  if (subset) {
    data = data %>%
      dplyr::select(dplyr::any_of("HEADER_TIMESTAMP"), X, Y, Z)
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
