# Calculate peak-to-peak metric in one-second time windows.
get_peak = function(
    data,
    sample_rate = 10L
) {

  assertthat::assert_that(
    assertthat::is.count(sample_rate)
  )
  sample_rate = as.integer(sample_rate)


  pp_out = preprocess_bout(data, sample_rate = sample_rate)
  vm_bout = pp_out$vm_bout$vm

  oak = oak_base()
  pp = oak$get_pp(vm_bout = vm_bout,
                      fs = sample_rate)
  list(
    preprocess_out = pp_out,
    peaks = pp
  )
}

find_walking_r = function(
    data,
    sample_rate = 10L,
    min_amplitude = 0.3,
    step_frequency = c(1.4, 2.3),
    alpha = 0.6,
    beta = 2.5,
    min_duration_peak = 3L,
    delta = 20L
) {
  sample_rate = as.integer(sample_rate)
  pp_out = preprocess_bout(data, sample_rate = sample_rate)
  vm_bout = pp_out$vm_bout$vm

  oak = oak_base()
  pp = oak$get_pp(vm_bout = vm_bout,
                  fs = sample_rate)


  # wavelet = ('gmw', {'beta': 90, 'gamma': 3})
  wavelet = list("gmw", list(beta = 90L, gamma = 3L))
  wavelet = reticulate::tuple(wavelet)
  # exclude low-intensity periods
  valid = pp >= min_amplitude

  oak = oak_base()
  np = reticulate::import("numpy")

  # compute cwt only if valid fragment is sufficiently long
  if (sum(valid) >= min_duration_peak) {

    valid_vector = np$`repeat`(valid, sample_rate)

    # trim bout to valid periods only
    tapered_bout = vm_bout[valid_vector]

    # compute and interpolate CWT
    out = oak$compute_interpolate_cwt(
      tapered_bout = tapered_bout,
      fs = sample_rate,
      wavelet = wavelet)
    names(out) = c("freqs_interp", "coefs_interp")

    # get map of dominant peaks
    dp = oak$identify_peaks_in_cwt(
      freqs_interp = out$freqs_interp,
      coefs_interp = out$coefs_interp,
      fs = sample_rate,
      step_freq = step_frequency,
      alpha = alpha,
      beta = beta)

    valid_peaks = array(0, dim = c(nrow(dp), length(valid)))
    valid_peaks[, valid] = dp

    cont_peaks = oak$find_continuous_dominant_peaks(
      valid_peaks = valid_peaks,
      min_t = min_duration_peak,
      delta = delta)

    cad = rep(0, ncol(valid_peaks))
    ind = which(cont_peaks > 0, arr.ind = TRUE)
    ind = as.data.frame(ind) %>%
      dplyr::group_by(col) %>%
      dplyr::slice(1L)
    cad[ind$col] = out$freqs_interp[ind$row]
  } else {
    cad = rep(0, as.integer(nrow(vm_bout)/sample_rate))
  }
  #
  #   # find peaks that are continuous in time (min_t) and frequency (delta)
  #   cont_peaks = find_continuous_dominant_peaks(valid_peaks, min_t, delta)
  #
  #   # summarize the results
  #   cad = np.zeros(valid_peaks.shape[1])
  #   for i in range(len(cad)):
  #     ind_freqs = np.where(cont_peaks[:, i] > 0)[0]
  #   if len(ind_freqs) > 0:
  #     cad[i] = freqs_interp[ind_freqs[0]]
  #
  #   else:
  #     cad = np.zeros(int(vm_bout.shape[0]/fs))
  #
  #   return cad
  # }
}
