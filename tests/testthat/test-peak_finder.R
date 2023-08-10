test_that("verisense_count_steps peak_finder gives same", {
  input_data <- matrix(runif(500 * 3, min = -1.5, max = 1.5), ncol = 3)
  original = verisense_count_steps(input_data, sample_rate = 15L,
                                   peak_finder = "original")
  fast = verisense_count_steps(input_data, sample_rate = 15L,
                               peak_finder = "fast")
  testthat::expect_equal(original, fast)
})
