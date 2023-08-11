testthat::test_that("resample_accel_data gives fixed answer", {

  csv_file = system.file("test_data_bout.csv", package = "walking")
  testthat::skip_if_not_installed("readr")
  data = readr::read_csv(csv_file)
  colnames(data)[colnames(data) == "UTC time"] = "time"
  data = standardize_data(data)
  new_data = resample_accel_data(data, sample_rate = 5)


  suppressWarnings({out = find_walking(new_data, sample_rate_analysis = 10L)})
  testthat::expect_true(is.data.frame(out))
  testthat::expect_named(out, c("time", "steps"))
  testthat::expect_true(nrow(out) == 9)

  testthat::expect_identical(
    round(out$steps, 10),
    c(0, 0, 0, 0, 0, 1.8, 1.75, 1.75, 1.7)
  )

  # If you upsample - gives same result as original

  new_data = resample_accel_data(data, sample_rate = 20,
                                 method = "linear",
                                 rule = 2)
  remade_data = resample_accel_data_to_time(new_data,
                                            times = data$HEADER_TIME_STAMP,
                                            method = "linear",
                                            rule = 2)
  stopifnot(all(dim(remade_data) == dim(data)))
  # some are NA because the times are outside scope
  na_x = is.na(remade_data$X)
  dx = data[!na_x,]
  rx = remade_data[!na_x,]
  testthat::expect_true(abs(mean(rx$X - dx$X)) <= 1e-3)

  # If you upsample - gives same result as original
  new_data = resample_accel_data(data, sample_rate = 20)
  suppressWarnings({out = find_walking(new_data, sample_rate_analysis = 10L)})
  testthat::expect_true(is.data.frame(out))
  testthat::expect_named(out, c("time", "steps"))
  testthat::expect_true(nrow(out) == 10)

  testthat::expect_identical(
    round(out$steps, 10),
    c(1.65, 1.6, 1.55, 1.6, 1.55, 1.85, 1.8, 1.75, 1.75, 1.7)
  )

})
