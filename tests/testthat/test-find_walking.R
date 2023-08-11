testthat::test_that("find_walking gives fixed answer", {
  csv_file = system.file("test_data_bout.csv", package = "walking")
  testthat::skip_if_not_installed("readr")
  data = readr::read_csv(csv_file)
  colnames(data)[colnames(data) == "UTC time"] = "time"
  suppressWarnings({out = find_walking(data, sample_rate_analysis = 10L)})
  testthat::expect_true(is.data.frame(out))
  testthat::expect_named(out, c("time", "steps"))
  testthat::expect_true(nrow(out) == 10)

  testthat::expect_identical(
    round(out$steps, 10),
    c(1.65, 1.6, 1.55, 1.6, 1.55, 1.85, 1.8, 1.75, 1.75, 1.7)
  )

  xdata = data

  data = xdata

  data = data %>%
    # need use case for standardize data
    dplyr::rename(HEADER_TIMESTAMP = time) %>%
    # need use case in mm/s^2
    dplyr::mutate(
      x = x * 9.80665,
      y = y * 9.80665,
      z = z * 9.80665
      )
  suppressWarnings({out2 = find_walking(data, sample_rate_analysis = 10L)})
  testthat::expect_identical(
    round(out$steps, 10),
    c(1.65, 1.6, 1.55, 1.6, 1.55, 1.85, 1.8, 1.75, 1.75, 1.7)
  )

})
