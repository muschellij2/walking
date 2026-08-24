
<!-- README.md is generated from README.Rmd. Please edit that file -->

# walking

<!-- badges: start -->

[![R-CMD-check](https://github.com/jhuwit/walking/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jhuwit/walking/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `walking` is to provide some algorithms to detect walking in
tri-axial accelerometers.

## Installation

You can install the development version of walking from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("muschellij2/walking")
```

`walking` no longer tries to install `forest` for you. Install `forest`
in your own Python environment first, then load `walking` from R.

If you hit `clang++: error: unsupported option '-fopenmp'` or the LLVM
mismatch described in
[onnela-lab/forest#293](https://github.com/onnela-lab/forest/issues/293)
and
[numba/llvmlite#1389](https://github.com/numba/llvmlite/issues/1389),
use a Python environment where `forest` and its dependencies already
resolve cleanly, then point `reticulate` at that environment with
`use_condaenv()` or `use_python()`.

### Parallel processing and `ssqueezepy`

`forest`'s `oak` workflow uses [`ssqueezepy`](https://github.com/OverLordGoldDragon/ssqueezepy#gpu--cpu-acceleration)
for wavelet processing. `ssqueezepy` can parallelize CPU work, which can make
larger analyses faster but may conflict with an R/Python environment or its
threading and OpenMP configuration. `find_walking()` disables this back-end
parallelization by default (`disable_parallelization = TRUE`) for a more
reliable R-session default. Set `disable_parallelization = FALSE` only when
you want to use the acceleration and your environment supports it.

If an error occurs while loading `forest`, or while running the wavelet step,
set `SSQ_PARALLEL` before the first `forest` import. This disables
`ssqueezepy` parallelization for the R session:

``` r
Sys.setenv(SSQ_PARALLEL = 0)
walking::py_require_forest()
reticulate::import("forest")
```

See the [`ssqueezepy` GPU / CPU acceleration documentation](https://github.com/OverLordGoldDragon/ssqueezepy#gpu--cpu-acceleration)
for its parallelization settings.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(walking)
csv_file = system.file("test_data_bout.csv", package = "walking")
x = readr::read_csv(csv_file)
#> Rows: 98 Columns: 6
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (1): accuracy
#> dbl  (4): timestamp, x, y, z
#> dttm (1): UTC time
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
colnames(x)[colnames(x) == "UTC time"] = "time"
res = find_walking(data = x)
#> Preprocessing Bout
#> Bout is Preprocessed
#> OAK: Find walking is done
print(res)
#>                   time steps
#> 1  2020-02-25 18:18:31  1.65
#> 2  2020-02-25 18:18:32  1.60
#> 3  2020-02-25 18:18:33  1.55
#> 4  2020-02-25 18:18:34  1.60
#> 5  2020-02-25 18:18:35  1.55
#> 6  2020-02-25 18:18:36  1.85
#> 7  2020-02-25 18:18:37  1.80
#> 8  2020-02-25 18:18:38  1.75
#> 9  2020-02-25 18:18:39  1.75
#> 10 2020-02-25 18:18:40  1.70
```

## Potential Conflicts

### Running `forest` and [`stepcount`](https://github.com/jhuwit/stepcount).

The two Python modules (`forest` and `stepcount`) can have different
conflicts of python dependencies and python versions. You can do
specific call-outs with `callr` to try to resolve this:

## Running Walking with Different Python Environments

If you want to use both walking estimation from `forest` and
`stepcount`, you can run them in separate R processes using `callr` to
avoid Python package conflicts. Here’s how you can do it:

``` r
library(callr)
library(stepcount)
#> Warning in reticulate::py_require("stepcount>=3.11.0", python_version = "3.10"): Python version requirements cannot be changed after Python has been initialized.
#> * Python version request: '3.10' (from package:stepcount)
#> * Python version initialized: '3.11.15'
csv_file = system.file("test_data_bout.csv", package = "walking")
data = readr::read_csv(csv_file)
#> Rows: 98 Columns: 6
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (1): accuracy
#> dbl  (4): timestamp, x, y, z
#> dttm (1): UTC time
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
colnames(data)[colnames(data) == "UTC time"] = "time"

stepcount_callr = function(data,
                           ...) {
  
  reticulate::py_require("stepcount==3.11.0", python_version = "3.10")
  sc <- reticulate::import("stepcount")
  stepcount::stepcount_check()
  
  res = stepcount::stepcount(data, ...)
  return(res)
}

# 2. Run the isolated background R process
result <- callr::r(
  func = stepcount_callr,
  show = TRUE,
  args = list(data = data) # Safely injects data into the process
)
#> Loading model...
#> Downloading https://wearables-files.ndph.ox.ac.uk/files/models/stepcount/ssl-20230208.joblib.lzma...
#> Checking Data
#> Writing file to CSV...
#> Reading in Data for Stepcount
#> /Users/johnmuschelli/Library/Caches/org.R-project.R/R/reticulate/uv/cache/archive-v0/12AzDesQwp07_KC6/lib/python3.10/site-packages/actipy/processing.py:387: UserWarning: Skipping calibration: Insufficient stationary samples: 0 < 50
#>   warnings.warn(f"Skipping calibration: Insufficient stationary samples: {len(xyz)} < {calib_min_samples}")
#> Predicting from Model
#> Running step counter...
#> Gravity calibration...Gravity calibration... Done! (0.01s)
#> Nonwear detection...Nonwear detection... Done! (0.01s)
#> Resampling...Resampling... Done! (0.03s)
#> Defining windows...
#>   0%|          | 0/1 [00:00<?, ?it/s]100%|██████████| 1/1 [00:00<00:00, 506.07it/s]
#> /Users/johnmuschelli/Library/Caches/org.R-project.R/R/reticulate/uv/cache/archive-v0/12AzDesQwp07_KC6/lib/python3.10/site-packages/stepcount/models.py:467: UserWarning: No data to predict
#>   warnings.warn("No data to predict")
#> Processing Result
#> Using local /Users/johnmuschelli/Library/Caches/org.R-project.R/R/reticulate/uv/cache/archive-v0/12AzDesQwp07_KC6/lib/python3.10/site-packages/stepcount/torch_hub_cache/OxWearables_ssl-wearables_v1.0.0
#> /usr/local/Cellar/python@3.10/3.10.20_1/Frameworks/Python.framework/Versions/3.10/lib/python3.10/multiprocessing/resource_tracker.py:224: UserWarning: resource_tracker: There appear to be 1 leaked semaphore objects to clean up at shutdown
#>   warnings.warn('resource_tracker: There appear to be %d '
head(result)
#> $steps
#> # A tibble: 1 × 2
#>   time                steps
#>   <dttm>              <dbl>
#> 1 2020-02-25 18:18:31   NaN
#> 
#> $walking
#> # A tibble: 1 × 2
#>   time                walking
#>   <dttm>                <dbl>
#> 1 2020-02-25 18:18:31     NaN
#> 
#> $step_times
#> [1] time
#> <0 rows> (or 0-length row.names)
#> 
#> $info
#> $info$CalibNumSamples
#> [1] 0
#> 
#> $info$`CalibErrorBefore(mg)`
#> [1] NaN
#> 
#> $info$`CalibErrorAfter(mg)`
#> [1] NaN
#> 
#> $info$CalibOK
#> [1] 0
#> 
#> $info$`NonwearTime(days)`
#> [1] 0
#> 
#> $info$NumNonwearEpisodes
#> [1] 0
#> 
#> $info$`WearTime(days)`
#> [1] 0.0001126389
#> 
#> $info$NumInterrupts
#> [1] 0
#> 
#> $info$Covers24hOK
#> [1] 0
#> 
#> $info$ResampleRate
#> [1] 30
#> 
#> $info$NumTicksAfterResample
#> [1] 293
#> 
#> $info$Filename
#> [1] "/private/var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T/Rtmpteefb1/file4e0f3962f3e5.csv"
#> 
#> $info$Device
#> [1] ".csv"
#> 
#> $info$`Filesize(MB)`
#> [1] 0
#> 
#> $info$SampleRate
#> [1] 10
#> 
#> $info$StartTime
#> [1] "2020-02-25 18:18:31"
#> 
#> $info$EndTime
#> [1] "2020-02-25 18:18:40"

forest_callr = function(data,
                        ...) {
  reticulate::py_require(
    "git+https://github.com/onnela-lab/forest@45fb41038bd46c25d9e6a4442aa74fa03b501317", 
    python_version = "3.11")
  fr = reticulate::import("forest")
  oak = fr$oak$base
  oak
  res = walking::estimate_steps_forest(data, ...)
  return(res)
}

# 2. Run the isolated background R process
fresult <- callr::r(
  func = forest_callr,
  show = TRUE,
  args = list(data = data) # Safely injects data into the process
)
#> Preprocessing Bout
#> Bout is Preprocessed
#> OAK: Find walking is done
head(fresult)
#>                  time steps
#> 1 2020-02-25 18:18:31  1.65
#> 2 2020-02-25 18:18:32  1.60
#> 3 2020-02-25 18:18:33  1.55
#> 4 2020-02-25 18:18:34  1.60
#> 5 2020-02-25 18:18:35  1.55
#> 6 2020-02-25 18:18:36  1.85
```
