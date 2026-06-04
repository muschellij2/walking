testthat::test_that("estimate_steps_verisense validates inputs and dispatches methods", {
  testthat::expect_error(
    estimate_steps_verisense(1:10, sample_rate = 10L),
    "needs a data set/data.frame"
  )

  csv_file = system.file("test_data_bout.csv", package = "walking")
  data = utils::read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
  data$time = as.POSIXct(data$`UTC time`, tz = "UTC")
  data$`UTC time` = NULL

  original = estimate_steps_verisense(data, sample_rate = 10L, method = "original")
  revised = estimate_steps_verisense(data, sample_rate = 10L, method = "revised")

  testthat::expect_s3_class(original, "data.frame")
  testthat::expect_s3_class(revised, "data.frame")
  testthat::expect_named(original, c("time", "steps"))
  testthat::expect_named(revised, c("time", "steps"))
  testthat::expect_true(nrow(original) > 0L)
  testthat::expect_true(nrow(revised) > 0L)

  resample_data = data.frame(
    time = as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + c(0, 1, 2),
    X = c(0.1, 0.2, 0.3),
    Y = c(0.2, 0.1, 0.2),
    Z = c(1, 1, 1)
  )
  testthat::expect_warning(
    estimate_steps_verisense(
      resample_data,
      sample_rate = 10L,
      resample_to_15hz = TRUE,
      method = "original"
    ),
    "sample_rate will be ignored because resample_to_15hz is TRUE"
  )
})


testthat::test_that("estimate_steps_sdt matches sdt_count_steps", {
  times = as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + seq(0, by = 0.01, length.out = 100)
  data = data.frame(
    time = times,
    X = 0,
    Y = 0,
    Z = 0
  )

  wrist = sdt_count_steps(data, sample_rate = 100L, location = "wrist", verbose = FALSE)
  waist = sdt_count_steps(data, sample_rate = 100L, location = "waist", verbose = FALSE)
  wrapper = estimate_steps_sdt(data, sample_rate = 100L, location = "wrist", verbose = FALSE)

  testthat::expect_equal(wrist, wrapper)
  testthat::expect_equal(waist$steps, 0)
  testthat::expect_equal(wrist$steps, 0)
  testthat::expect_equal(wrist$time, as.POSIXct("2020-01-01 00:00:00", tz = "UTC"))
})


testthat::test_that("have_forest returns a logical scalar", {
  forest_available = suppressWarnings(have_forest())
  testthat::expect_type(forest_available, "logical")
  testthat::expect_length(forest_available, 1L)
})
