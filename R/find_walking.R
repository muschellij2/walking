#' Finds walking and calculate steps from raw acceleration data.
#'
#' Method finds periods of repetitive and continuous oscillations with
#' predominant frequency occurring within know step frequency range.
#' Frequency components are extracted with Continuous Wavelet Transform.
#'
#' @param data
#' @param sample_rate sampling frequency (in Hz) **for analyzing walking**.
#' Note, this is **NOT** the sampling frequency of the data.
#'
#' @param min_amplitude minimum amplitude (in g)
#' @param step_frequency step frequency range
#' @param alpha maximum ratio between dominant peak below
#' and within step frequency range
#' @param beta maximum ratio between dominant peak above
#' and within step frequency range
#' @param delta maximum difference between consecutive peaks
#' (in multiplication of 0.05Hz)
#' @param min_duration_peak minimum duration of peaks (in seconds)
#'
#' @return A vector of number of steps per second

#' @export
#'
#' @examples
#' csv_file = system.file("test_data_bout.csv", package = "walking")
#' x = readr::read_csv(csv_file)
#' colnames(x)[colnames(x) == "UTC time"] = "time"
#' res = find_walking(data = x)
find_walking = function(
    data,
    sample_rate = 10L,
    min_amplitude = 0.3,
    step_frequency = c(1.4, 2.3),
    alpha = 0.6,
    beta = 2.5,
    min_duration_peak = 3L,
    delta = 20L
) {

  assertthat::assert_that(
    assertthat::is.scalar(min_amplitude),
    assertthat::is.scalar(alpha),
    assertthat::is.scalar(beta),
    assertthat::is.count(min_duration_peak),
    assertthat::is.count(delta),
    assertthat::is.count(sample_rate),
    length(step_frequency) == 2
  )
  sample_rate = as.integer(sample_rate)
  min_duration_peak = as.integer(min_duration_peak)
  delta = as.integer(delta)


  # pp_out = preprocess_bout(data, sample_rate = sample_rate)
  pp_out = preprocess_bout_r(data, sample_rate = sample_rate)
  rm(data)
  vm_bout = pp_out$vm_bout$vm
  # step_frequency = do.call(reticulate::tuple, as.list(step_frequency))

  oak = oak_base()
  # np = reticulate::import("numpy")
  # vm_bout = np$asarray(vm_bout, dtype = "object")
  # oak = oak_base_noconvert()
  # oak$get_pp(vm_bout = vm_bout,
  #            fs = sample_rate)

  cadence_bout = oak$find_walking(
    vm_bout = vm_bout,
    fs = sample_rate,
    min_amp = min_amplitude,
    step_freq = step_frequency,
    alpha = alpha,
    beta = beta,
    min_t = min_duration_peak,
    delta = delta)

  vm_bout = pp_out$vm_bout
  vm_bout$steps = cadence_bout
  vm_bout$vm = NULL
  vm_bout = as.data.frame(vm_bout)

  #
  # expected_output = np.array([1.65, 1.6, 1.55, 1.6, 1.55, 1.85, 1.8, 1.75,
  #                             1.75, 1.7])
  # assert len(cadence_bout) == 10
  # assert np.array_equal(np.round(cadence_bout, 2), expected_output)
  return(vm_bout)
}
