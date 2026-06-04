testthat::test_that("process_vm_bout converts and expands time correctly", {
  times = as.numeric(as.POSIXct("2020-01-01 00:00:00", tz = "UTC")) + c(0.1234, 0.6234, 1.1234)
  vm_bout = list(times, c(0.1, 0.2, 0.3))

  out = process_vm_bout(vm_bout, tz = "UTC", sample_rate = 2L)

  testthat::expect_named(out, c("vm_bout", "vm_data"))
  testthat::expect_s3_class(out$vm_bout$time, "POSIXct")
  testthat::expect_equal(
    as.numeric(out$vm_bout$time),
    times,
    tolerance = 0.01
  )
  testthat::expect_equal(out$vm_data$time[1], as.POSIXct("2020-01-01 00:00:00", tz = "UTC"))
  testthat::expect_equal(as.numeric(diff(out$vm_data$time)), c(0.5, 0.5))
})


testthat::test_that("create_peak_info and rowWhichMaxIndex return expected values", {
  peak_info = create_peak_info(3L)
  testthat::expect_named(
    peak_info,
    c("peak_location", "acc_magnitude", "periodicity", "similarity", "continuity")
  )
  testthat::expect_true(all(is.na(peak_info)))

  index_mat = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
  value_mat = matrix(c(1, 4, 2, 7, 6, 5), nrow = 2, byrow = TRUE)

  testthat::expect_equal(rowWhichMaxIndex(index_mat, value_mat), c(2, 4))
})
