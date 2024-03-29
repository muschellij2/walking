testthat::test_that("resample_accel_data gives fixed answer", {

  csv_file = system.file("test_data_bout.csv", package = "walking")
  testthat::skip_if_not_installed("readr")
  data = readr::read_csv(csv_file)
  colnames(data)[colnames(data) == "UTC time"] = "time"
  data = standardize_data(data)
  new_data = resample_accel_data(data, sample_rate = 5)
  testthat::expect_named(new_data, c("HEADER_TIMESTAMP", "X", "Y", "Z"))


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
                                            times = data$HEADER_TIMESTAMP,
                                            method = "linear",
                                            rule = 2)
  testthat::expect_named(remade_data, c("HEADER_TIMESTAMP", "X", "Y", "Z"))

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



testthat::test_that("resample_accel_data with splines gives fixed", {

  csv_file = system.file("test_data_bout.csv", package = "walking")
  testthat::skip_if_not_installed("readr")
  data = readr::read_csv(csv_file)
  colnames(data)[colnames(data) == "UTC time"] = "time"
  data = standardize_data(data)
  new_data = resample_accel_data(data, sample_rate = 1, method = "natural")
  testthat::expect_equal(
    new_data$X,
    c(-0.712326049804688, -0.712621490119049, -0.969534180827183,
      -0.503721073173929, -0.813009195052185, -0.694237790605315,
      -0.685506165744859,
      -0.702542313252479, -0.917206026054063, -0.768493243772211)
  )

  testthat::expect_equal(
    new_data$Y,
    c(-0.039520263671875, -0.0475473700022881, 0.295807109458185,
      -0.265249343186167, -0.143946899460751, 0.284004715360539,
      0.152206817288813,
      -0.00427613616967247, -0.207763176722145, -0.318512245527092)
  )

  testthat::expect_equal(
    new_data$Z,
    c(-0.483963012695312, -0.658997296105178, -0.400639379413525,
      -0.396496286614927, -0.839950960912041, -0.421181485795062,
      -1.75396689986857,
      -0.426912039518188, -0.44337841615456, -0.494501933453084)
  )
})
