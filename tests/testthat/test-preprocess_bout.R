testthat::test_that("preprocess_bout gives same as preprocess_bout_r", {
  csv_file = system.file("test_data_bout.csv", package = "walking")
  testthat::skip_if_not_installed("readr")
  data = readr::read_csv(csv_file)
  colnames(data)[colnames(data) == "UTC time"] = "time"
  bout = preprocess_bout(data, sample_rate = 10)
  bout_r = preprocess_bout_r(data, sample_rate = 10)
  testthat::expect_equal(bout$vm_data$time, bout_r$vm_data$time)
  testthat::expect_true(
    all(abs(bout$vm_data$vm - bout_r$vm_data$vm) <= 1e-4)
  )
})
