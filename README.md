
<!-- README.md is generated from README.Rmd. Please edit that file -->

# walking

<!-- badges: start -->

[![R-CMD-check](https://github.com/muschellij2/walking/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/muschellij2/walking/actions/workflows/R-CMD-check.yaml)
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
