test_that("verisense_count_steps peak_finder gives same", {
  input_data <- matrix(runif(500 * 3, min = -1.5, max = 1.5), ncol = 3)
  original = verisense_count_steps(input_data, sample_rate = 15L,
                                   peak_finder = "original")
  fast = verisense_count_steps(input_data, sample_rate = 15L,
                               peak_finder = "fast")
  testthat::expect_equal(original, fast)
})


test_that("verisense_count_steps peak_finder gives same with CSV", {
  csv_file = system.file("test_data_bout.csv", package = "walking")
  testthat::skip_if_not_installed("readr")
  data = readr::read_csv(csv_file)
  colnames(data)[colnames(data) == "UTC time"] = "time"
  original = verisense_count_steps(data, sample_rate = 10L,
                                   peak_finder = "original")
  fast = verisense_count_steps(data, sample_rate = 10L,
                               peak_finder = "fast")
  testthat::expect_equal(original, fast)
})
