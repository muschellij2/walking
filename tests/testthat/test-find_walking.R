testthat::test_that("find_walking gives fixed answer", {
  csv_file = system.file("test_data_bout.csv", package = "walking")
  testthat::skip_if_not_installed("readr")
  data = readr::read_csv(csv_file)
  colnames(data)[colnames(data) == "UTC time"] = "time"
  suppressWarnings({out = find_walking(data, sample_rate = 10L)})
  testthat::expect_true(is.data.frame(out))
  testthat::expect_named(out, c("time", "steps"))
  testthat::expect_true(nrow(out) == 10)

  testthat::expect_identical(
    round(out$steps, 10),
    c(1.65, 1.6, 1.55, 1.6, 1.55, 1.85, 1.8, 1.75, 1.75, 1.7)
  )
})
