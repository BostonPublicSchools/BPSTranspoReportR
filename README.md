
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RVersatransRP

<!-- badges: start -->
<!-- badges: end -->

The goal of RVersatransRP is to provide easy to use functions for
querying and formatting data from a Versatrans Routing & Planning
database.

## Installation

RVersatransRP is currently available from our [shared Google Drive
folder](https://drive.google.com/drive/folders/1e_C8c5epf8IqcKyP4g9TzaFm1Vnlr8wM?usp=sharing).
I don’t think there is any easy way to install or update directly from
there, so you will need to download/sync this shared drive to your local
disk and install from there. Once you have downloaded/synced the drive
directory you can install this package with

``` r
remotes::install_local("path/to/directory")
```

Eventually we will move this package to Github so you can install the
development version of RVersatransRP from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github(".../RVersatransRP")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(RVersatransRP)

student_monitor <- rp_report_student_monitor(database = "Sandbox", format = "long")

## example pickup data:
dplyr::glimpse(
dplyr::select(
  student_monitor,
  -dplyr::contains("ID"), -dplyr::contains("Last"), -dplyr::contains("First")))
#> Rows: 207,702
#> Columns: 10
#> $ `Program Description`  <chr> "DR DOOR FULL DAY", "DR DOOR FULL DAY", "DR DOO…
#> $ `INFO WC`              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ `INFO NEED MONITOR`    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ `School Abbrev`        <chr> "0335", "0335", "0420", "0420", "0420", "0420",…
#> $ `School Anchor Abbrev` <chr> "0335", "0335", "0420", "0420", "0420", "0420",…
#> $ `School Name`          <chr> "Farr Acad", "Farr Acad", "LANDMARK HS", "LANDM…
#> $ Day                    <chr> NA, NA, "M", "M", "T", "T", "W", "W", "H", "H",…
#> $ Shift                  <chr> "Pickup", "Dropoff", "Pickup", "Dropoff", "Pick…
#> $ Route                  <chr> NA, NA, "04201451", "04200451", "04201451", "04…
#> $ Bus                    <chr> NA, NA, "7D036", "7D036", "7D036", "7D036", "7D…
```
