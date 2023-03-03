
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BPSTranspoReportR

<!-- badges: start -->
<!-- badges: end -->

The goal of BPSTranspoReportR is to provide easy to use functions for
generating reports and data summaries from data stored in Versatrans
RP/Onscreen, and Zonar.

## Installation

BPSTranspoReportR is currently available from our [GitHub
page](https://github.com/BostonPublicSchools/BPSTranspoReportR). You can
the development version of BPSTranspoReportR from GitHub with:

``` r
# install.packages("remotes") ## if you don't have remotes installed already
remotes::install_github(c("BostonPublicSchools/RVersatransRP",
                          "BostonPublicSchools/RZonar",
                          "BostonPublicSchools/BPSTranspoReportR")
```

## Example

This is a basic example which shows you how to retrieve the student
monitor file from the Versatrans RP database:

``` r
library(BPSTranspoReportR)

## run report for yesterday
student_monitor <- otp_report(as.character(Sys.Date() -1))
```

``` r
## OTP data structure:
dplyr::glimpse(student_monitor[0, ])
#> Rows: 0
#> Columns: 34
#> $ RPVehicle           <chr> 
#> $ RPRoute             <chr> 
#> $ ExpectedTime        <dttm> 
#> $ Arrival             <dttm> 
#> $ delayTimeCombined   <int> 
#> $ ActualTime          <dttm> 
#> $ zonarActualTime     <dttm> 
#> $ DelayTime           <int> 
#> $ zonarDelayTime      <int> 
#> $ Direction           <chr> 
#> $ AnchorAbbrev        <chr> 
#> $ Date                <dttm> 
#> $ AMPM                <chr> 
#> $ OvernightGarage     <chr> 
#> $ Names               <chr> 
#> $ RouteName           <chr> 
#> $ load                <int> 
#> $ Days                <chr> 
#> $ RouteSetName        <chr> 
#> $ excludedShuttlesEtc <lgl> 
#> $ OriginalRPVehicle   <chr> 
#> $ Zone                <chr> 
#> $ zonarCategory       <chr> 
#> $ zonarDuration       <drtn>  secs
#> $ inZonar             <lgl> 
#> $ inOnscreen          <lgl> 
#> $ arrivalTimeSource   <chr> 
#> $ arrivalInWindow     <lgl> 
#> $ zoneInZonar         <lgl> 
#> $ OSVehicleOverride   <lgl> 
#> $ Uncovered           <lgl> 
#> $ cutoff_min          <dbl> 
#> $ cutoff_max          <dbl> 
#> $ TripCategory        <chr>
```

## Credentials and other private information

Usernames, passwords, and other private information will usually be
needed in order to access the data sources required to generate reports.
By default these are retrieved from environment variables. You can
either set these system-wide, or set up an `.Renviron` file as described
in
<https://support.posit.co/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf>.

Note that *this is just for convenience, you can also just pass
everything as arguemnts to your function calls*.

If you do set environment variables with private information, **make
sure that these values are not checked into git or otherwise
unintentionally exposed**.

The environment variables consulted by functions in this package are
shown in the table below.

| Variable           | Description                                                                  |
|--------------------|------------------------------------------------------------------------------|
| *RP_ODBC_NAME*     | name of the ODBC data source used to connect to Versatrans databases         |
| *RP_DATABASE*      | name of the RP database to use                                               |
| *OS_DATABASE*      | name of the Onscreen database to use                                         |
| *UNCOVERED_URL*    | URL of a google sheet containing uncovered trips list (required)             |
| *MAIL_TO*          | comma separated list of email addresses to send notifications to. (required) |
| *MAIL_FROM*        | gmail address from which notifications should be sent (required)             |
| *TZ*               | time zone, defaults to “America/New_York”,                                   |
| *CURL_SSL_BACKEND* | this should always be set to “openssl” (required)                            |
| *ZONAR_CUSTOMER*   | Zonar customer ID (required)                                                 |
| *ZONAR_USER*       | Zonar user name (required)                                                   |
| *ZONAR_PASS*       | ZOnar password (required)                                                    |
| ——————             | —————————————————————————-                                                   |
