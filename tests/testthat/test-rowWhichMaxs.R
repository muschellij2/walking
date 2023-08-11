testthat::test_that("rowWhichMaxs works same as which.max", {
  set.seed(20230811)
  input_data <- matrix(runif(500 * 3, min = -1.5, max = 1.5), ncol = 3)
  input_data[1,] = 1
  out = apply(input_data, 1, which.max)
  out2 = walking:::rowWhichMaxs(input_data)
  testthat::expect_equal(out, out2)
})
