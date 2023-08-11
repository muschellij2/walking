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


testthat::test_that("verisense_count_steps gives same  works", {
  set.seed(20230811)
  input_data <- matrix(runif(500 * 3, min = -1.5, max = 1.5), ncol = 3)
  res1 = verisense_count_steps(input_data, sample_rate = 15L)
  res2 = verisense_count_steps(input_data, sample_rate = 15L,
                               peak_finder = "fast")
  testthat::expect_equal(
    res1,
    c(0, 0, 0, 0, 0, 0, 1, 1, 2, 0, 2, 2, 1, 1, 1, 2, 1, 1, 1, 1,
      1, 2, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0))
  testthat::expect_equal(res1, res2)
  acc = sqrt(rowSums(input_data^2))
  testthat::expect_warning({
    res3 = verisense_count_steps(acc,
                                 sample_rate = 15L, peak_finder = "fast")
  })
  testthat::expect_equal(res1, res3)

  xacc = acc

  acc = xacc
  # reduce peaks deviation
  acc = rep(0, length(acc))
  acc[20] = 1
  testthat::expect_warning({
    res3 = verisense_count_steps(acc,
                                 sample_rate = 15L, peak_finder = "fast")
  })
  testthat::expect_equal(res3, rep(0, length(res3)))

  acc = xacc
  # reduce peaks deviation
  acc = rep(0, length(acc))
  acc[c(20, 30, 40, 50, 60)] = 2
  testthat::expect_warning({
    res3 = verisense_count_steps(acc,
                                 sample_rate = 15L, peak_finder = "fast")
  })
  testthat::expect_equal(res3, rep(0, length(res3)))

  # reduce standard deviation
  acc = xacc
  acc[1:496] = acc[1]
  testthat::expect_warning({
    res3 = verisense_count_steps(acc,
                                 sample_rate = 15L, peak_finder = "fast")
  })
  testthat::expect_equal(res3, rep(0, length(res3)))
})

