testthat::test_that("standardize_data fails when needed", {
  # only 2 columns - fail
  data <- matrix(runif(500 * 2, min = -1.5, max = 1.5), ncol = 2)
  testthat::expect_error({
    standardize_data(data)
  })
  data <- matrix(runif(500 * 3, min = -1.5, max = 1.5), ncol = 3)
  class(data) = "character"
  testthat::expect_error({
    standardize_data(data)
  })

  csv_file = system.file("test_data_bout.csv", package = "walking")
  testthat::skip_if_not_installed("readr")
  data = readr::read_csv(csv_file)
  colnames(data)[colnames(data) == "UTC time"] = "time"
  data$z = NULL
  testthat::expect_error({
    standardize_data(data)
  })
  testthat::expect_error({
    standardize_data(data, subset = FALSE)
  })
})


testthat::test_that("standardize_data succeeds with resample_accel_data", {

  csv_file = system.file("test_data_bout.csv", package = "walking")
  testthat::skip_if_not_installed("readr")
  data = readr::read_csv(csv_file)
  colnames(data)[colnames(data) == "UTC time"] = "time"
  out = walking::standardize_data(data)

  testthat::expect_named(out, c("HEADER_TIMESTAMP", "X", "Y", "Z"))
  testthat::expect_true(nrow(out) == 98L)

  out = walking::resample_accel_data(data, sample_rate = 30L)

  testthat::expect_named(out, c("HEADER_TIMESTAMP", "X", "Y", "Z"))
  testthat::expect_true(nrow(out) == 292L)

})

