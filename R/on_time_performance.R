#' Generate a on-time performance report
#'
#' @description
#' Retrieves and combines data from Zonar and Versatrans and generates full OTP
#' data. The full schedule and OTP data is returned, it will need to be summarized
#' for public reporting.
#'
#' @details TODO
#'
#' @param date Character vector of length 1 giving the date in YYY-MM-DD format.
#' @param ampm Character vector of length 1, either "AM", or "PM", use to
#' specify morning or evening OTP report.
#' @param include_zonar Logical vector of length 1. If `TRUE` zonar schedule data
#' will be retrieved and used for OTP calculations.
#' @param cutoff_min numeric vector of length one giving the minimum delay time in minutes
#' that will be considered a valid trip arrival. Usually this will be a negative value, e.g., -25 or so.
#' @param cutoff_max numeric vector of length one giving the maximum delay time in minutes
#' that will be considered a vlid trip arrival. Usuall this will be positive and relatively large, e.g., 60 or so.
#' @param rp_odbc_name Name of the Windows ODBC data source to use. Retrieved
#' from `RP_ODBC_NAME` environment variable by default but can be overridden
#' here, see RVersatransRP package for details.
#' @param uncovered_url URL of a google sheet containing a list of uncovered trips.
#' Must contain columns named "Date", "AM/PM", "Route Set", and "Bus".
#' @param rp_database Name of the RP database to connect to. Retrieved
#' from `RP_DATABASE` environment variable by default but can be overridden here,
#' see RVersatransRP package for details.
#' @param os_database Name of the Onscreen database to connect to.
#' @param mail_from GMail address used to send notification emails.
#' @param mail_to Character vector of email addresses to send notifications to.
#' @param TZ The timezone used by the database server.
#' @param test Logical vector of length one indicating whether to pull an abbreviated test data set.
#' Even with this the time needed to test this function will be long.
#'
#' @importFrom rlang !!
#' @importFrom rlang .data
#'
#' @export
#'
#' @examplesIf Sys.getenv("RP_ODBC_NAME")!=""&Sys.getenv("RP_DATABASE")!=""&Sys.getenv("ZONAR_PASS")!=""
#'
#' otp <- otp_report("2023-02-14", ampm = "AM", test = TRUE)
#' dplyr::glimpse(otp[0,])
#'
otp_report <- function(date = as.character(Sys.Date()),
                       ampm = c("AM", "PM"),
                       include_zonar = TRUE,
                       cutoff_min = -25,
                       cutoff_max = 60,
                       rp_database = Sys.getenv("RP_DATABASE"),
                       os_database = Sys.getenv("OS_DATABASE"),
                       rp_odbc_name = Sys.getenv("RP_ODBC_NAME"),
                       uncovered_url = Sys.getenv("UNCOVERED_URL"),
                       mail_to = strsplit(Sys.getenv("MAIL_TO"), ",")[[1]],
                       mail_from = Sys.getenv("MAIL_FROM"),
                       TZ = "America/New_York",
                       test = FALSE) {

  ## recursively compute both am and pm if both are requested
  if(all(c("AM", "PM") %in% ampm)) {
    amargs <- pmargs <- as.list(match.call())[-1]
    amargs$ampm <- "AM"
    pmargs$ampm <- "PM"
    return(dplyr::bind_rows(do.call(otp_report, amargs), do.call(otp_report, pmargs)))
  }

  options(gargle_oauth_email = TRUE)

  ## collect Onscreen OTP report and ancillary RP data
  startDate <- as.Date(date, tz = TZ)
  lubridate::tz(startDate) <- TZ

  conn_rp <- RVersatransRP::rp_connect(database = rp_database, rp_odbc_name = rp_odbc_name, TZ = TZ)
  on.exit(DBI::dbDisconnect(conn_rp), add = TRUE)
  conn_os <- RVersatransRP::rp_connect(database = os_database, rp_odbc_name = rp_odbc_name, TZ = TZ)
  on.exit(DBI::dbDisconnect(conn_os), add = TRUE)

  dataBuilding <- dplyr::collect(dplyr::select(dplyr::tbl(conn_rp, "Building"), "Name", "AnchorAbbrev"))
  dataBuilding <- dplyr::filter(dataBuilding, .data$AnchorAbbrev != "" & !is.na(.data$AnchorAbbrev))
  dataBuilding <- dplyr::mutate(dataBuilding, across(where(is.character), stringr::str_squish))


  dataRoute <- dplyr::collect(dplyr::select(dplyr::tbl(conn_rp, "Route"), "VisibleRouteID", "RouteSetID", "Name", "ActualLoad", "Days"))
  names(dataRoute) <- c("RPRoute", "RouteSetID", "RouteName", "ActualLoad", "Days")
  dataRouteSet <- dplyr::collect(dplyr::select(dplyr::tbl(conn_rp, "RouteSet"), "RecordID", "Name"))
  names(dataRouteSet) <- c("RouteSetID", "RouteSetName")
  dataRoute <- dplyr::mutate(dataRoute, across(where(is.character), stringr::str_squish))
  dataRouteSet <- dplyr::mutate(dataRouteSet, across(where(is.character), stringr::str_squish))
  dataRoute <- dplyr::left_join(dataRoute, dataRouteSet, by = "RouteSetID")

  dataUncovered <- googlesheets4::read_sheet(uncovered_url, col_types = "Dccccc")
  dataUncoveredOrig <- dataUncovered
  dataUncovered <- dplyr::transmute(
    dataUncovered,
    Date = {x <- as.Date(.data$Date, tz = !!TZ); lubridate::tz(x) <- !!TZ; x},
    AMPM = stringr::str_to_upper(.data[["AM/PM"]]),
    RouteSetName = .data[["Route Set"]],
    RPVehicle = .data$Bus,
    Uncovered = TRUE)
  dataUncovered <- dplyr::mutate(dataUncovered, across(where(is.character), stringr::str_squish))

  if(ampm[1] == "AM") {
    direction <- "I"
  } else {
    if(ampm[1] == "PM") {
      direction <- "O"
    } else {
      stop("`ampm` must be one of \"AM\" or \"PM\"")
    }
  }

  dataOS <- dplyr::collect(dplyr::tbl(conn_os, "VRW_v_VTOS_BusOnTimePerformance"))
  dataOS <- dplyr::filter(dataOS, .data$EffectiveDate == !!startDate & .data$Direction == !!direction)
  #if(ampm[1] == "AM") dataOS <- dplyr::mutate(dataOS, Planned.Start.Time = "")
  if(ampm[1] == "PM") {
    dataOS <- dplyr::left_join(
      dplyr::select(dataOS, -ExpectedTime, -ActualTime),
      dplyr::collect(dplyr::transmute(dplyr::tbl(conn_os, "RPScheduleByLocation"),
                                      .data$EffectiveDate,
                                      .data$RPRoute,
                                      .data$RPVehicle,
                                      .data$AnchorAbbrev,
                                      Direction = "O",
                                      ExpectedTime = .data$PlannedTime,
                                      ActualTime = .data$ActualTime)),
      by = c("RPVehicle", "RPRoute", "Direction", "AnchorAbbrev", "EffectiveDate"))
    }
  dataOS <- dplyr::mutate(dataOS, across(where(is.character), stringr::str_squish))
  dataOS <- dplyr::mutate(
    dataOS,
    ExpectedTime = {x <- .data$ExpectedTime; lubridate::date(x) <- startDate; lubridate::tz(x) <- !!TZ; x},
    # have to cleanup some anchors which do not have building names...
    AnchorAbbrev = ifelse(.data$AnchorAbbrev %in% !!dataBuilding$AnchorAbbrev,
                          .data$AnchorAbbrev, stringr::str_sub(.data$AnchorAbbrev, 1, 4))
  )
  dataOS <-
    dplyr::left_join(dataOS, dplyr::summarize(
      dplyr::group_by(dataBuilding, .data$AnchorAbbrev),
      Names = stringr::str_c(sort(unique(
        as.character(stats::na.omit(.data$Name))
      )), collapse = "; ")
    ), by = "AnchorAbbrev")
  dataOS <- dplyr::left_join(dataOS, dataRoute, by = "RPRoute")
  dataOS <- dplyr::mutate(
    dataOS,
    AMPM = stringr::str_extract(.data$RouteSetName, stringr::regex("\\b(AM|PM)\\b", ignore_case = TRUE)),
    AMPM = stringr::str_to_upper(stringr::str_squish(AMPM)),
    # some routes should be excluded, e.g. most all day shuttles
    excludedShuttlesEtc = dplyr::case_when(.data$ActualLoad == 0 & .data$RouteSetName != "CHARLESTOWN HS Combo AM" ~ TRUE,
                                           grepl("boston ballet", .data$RouteSetName, ignore.case = TRUE) ~ TRUE,
                                           grepl("strive.*shuttle", .data$RouteSetName, ignore.case = TRUE) ~ TRUE,
                                           grepl("horace mann afterschool pm", .data$RouteSetName, ignore.case = TRUE) ~ TRUE,
                                           grepl("fsb bos collab all day", .data$RouteSetName, ignore.case = TRUE) ~ TRUE,
                                           TRUE ~ FALSE)
  )

  ## examin Onscreen data for min and max times, these will be used to retrieve data for the interval from zonar.
  startTime <- min(dplyr::filter(dataOS, !.data$excludedShuttlesEtc)[["ExpectedTime"]]) - lubridate::dminutes(30)
  endTime <- max(dplyr::filter(dataOS, !.data$excludedShuttlesEtc)[["ExpectedTime"]]) + lubridate::dminutes(30)
  start <- as.character(startTime)
  end <- as.character(min(c(Sys.time(), endTime)))

  ## this is only for testing purposes, in order to have a version of this function that runs quickly.
  ## never set test=TRUE for production use.
  if(ampm[1] == "AM" && test) {
    start <- paste(as.character(date), "06:30:00")
    end <- paste(as.character(date), "07:00:00")
  }

  dataOverrides <- RVersatransRP::rp_report_vehicle_overrides()
  dataOverrides <- dplyr::mutate(dataOverrides, across(where(is.character), stringr::str_squish))
  dataOverrides <- dplyr::filter(dataOverrides, as.Date(.data$EffectiveDate, tz = !!TZ) == !!startDate)

  dataOS <- dplyr::left_join(dataOS, dataOverrides[, c("OriginalRPVehicle", "RPVehicle", "RPRoute")],
                             by = c("RPRoute", "RPVehicle"))

  ## get zonar data
  zones <- dplyr::mutate(RZonar::zonar_get_zones(), across(where(is.character), stringr::str_squish))
  zoneCategories <- dplyr::distinct(dplyr::select(zones, "category", "type"))

  if(include_zonar) {
  zonarSched <- RZonar::zonar_get_schedules(
    start = start,
    end = end,
    by = "zone",
    reformat = TRUE,
    omit_categories = dplyr::filter(zoneCategories, .data$type == "other")$category,
    test = test
  )
  } else {
    zonarSched = dplyr::mutate(
      dplyr::select(zones, zoneID = "id", Zone = "name", category),
      Asset = NA_character_,
      `Asset ID` = NA_character_,
      tgroup = NA_integer_,
      time_in = Sys.time(),
      time_out = Sys.time(),
      duration = lubridate::dseconds()
      )[0,]
  }
  zonarSched <- dplyr::mutate(zonarSched, across(where(is.character), stringr::str_squish))
  zonarSched <- dplyr::rename(zonarSched,
                              ZonarAssetID = "Asset ID",
                              ZonarCategory = "category",
                              ZonarActualTime = "time_in",
                              ZonarDuration = "duration")
  zones <- dplyr::inner_join(zones, dataBuilding, by = c(name = "Name"))

  zonarSched <- dplyr::left_join(zonarSched, dataBuilding, by = c(Zone = "Name"))

  dataSched <- dplyr::inner_join(
    dplyr::distinct(dplyr::select(dataOS, "AnchorAbbrev", "RPVehicle", "RPRoute", "ExpectedTime")),
    zonarSched, by = c(RPVehicle = "Asset", "AnchorAbbrev"))

  ## Calculate delay
  dataSched <- dplyr::mutate(dataSched, ZonarDelayTime = as.integer(difftime(.data$ZonarActualTime, .data$ExpectedTime, units = "mins")))
  dataSched <- dplyr::group_by(dataSched, .data$Zone, .data$RPVehicle, .data$RPRoute)
  dataSched <- dplyr::slice_min(dataSched, abs(.data$ZonarDelayTime), n = 1)

  dataFull <-
    dplyr::full_join(
      dataOS,
      dataSched,
      by = c("RPVehicle", "AnchorAbbrev", "RPRoute", "ExpectedTime"))

  dataFull <-
    dplyr::mutate(
      dataFull,
      OvernightGarage = stringr::str_remove(OvernightGarage, " .*$"),
      InZonar = !is.na(.data$ZonarActualTime),
      InOnscreen = !is.na(.data$ActualTime),
      ## the rule for choosing an arrival time are documented more fully in the package vignette. Basically
      ## we first try to take the one near the expected time, and failing that we the one closest to the expected time.
      Arrival = dplyr::case_when(is.na(.data$ActualTime) ~ .data$ZonarActualTime, TRUE ~ .data$ActualTime),
      Arrival = dplyr::case_when((.data$ZonarDelayTime > -10 & .data$ZonarDelayTime < !!cutoff_max) &
                                   (.data$DelayTime < -10 | .data$DelayTime > !!cutoff_max) ~ .data$ZonarActualTime,
                                 (.data$DelayTime > -10 & .data$DelayTime < !!cutoff_max) &
                                   (.data$ZonarDelayTime < -10 | .data$ZonarDelayTime > !!cutoff_max) ~ .data$ActualTime,
                                 (.data$ZonarDelayTime > !!cutoff_min & .data$ZonarDelayTime < !!cutoff_max) &
                                   (.data$DelayTime < !!cutoff_min | .data$DelayTime > !!cutoff_max) ~ .data$ZonarActualTime,
                                 (.data$DelayTime > !!cutoff_min & .data$DelayTime < !!cutoff_max) &
                                   (.data$ZonarDelayTime < !!cutoff_min | .data$ZonarDelayTime > !!cutoff_max) ~ .data$ActualTime,
                                 abs(.data$DelayTime) < abs(.data$ZonarDelayTime) ~ .data$ActualTime,
                                 TRUE ~ .data$ZonarActualTime),
      Arrival = dplyr::case_when(is.na(.data$Arrival) ~ .data$ZonarActualTime,
                                 TRUE ~ .data$Arrival),
      Arrival = dplyr::case_when(is.na(.data$Arrival) ~ .data$ActualTime,
                                 TRUE ~ .data$Arrival),
      ArrivalTimeSource = dplyr::case_when(.data$Arrival == ZonarActualTime ~ "Zonar",
                                           .data$Arrival == ActualTime ~ "Onscreen",
                                           TRUE ~ NA),
      DelayTimeCombined = as.integer(difftime(.data$Arrival, .data$ExpectedTime, units = "mins")),
      ArrivalInWindow = dplyr::case_when(!is.na(.data$DelayTimeCombined) &.data$DelayTimeCombined > cutoff_min & .data$DelayTimeCombined < cutoff_max ~ TRUE,
                                         !is.na(.data$DelayTimeCombined) ~ FALSE,
                                         TRUE ~ NA),
      ZoneInZonar = .data$AnchorAbbrev %in% !!zones$AnchorAbbrev
    )

  ## we may well have multiple matches per route at this point since we joined Zonar data
  ## by Zone/Anchor and Asset/RPVehicle. Here we take a temporally close match, prioritizing those
  ## that did not arrive long before the scheduled time and are therefore more likely to be the correct match.
  dataFull <- dplyr::arrange(dplyr::group_by(dataFull, .data$RPRoute, .data$EffectiveDate),
                             .data$RPRoute, .data$EffectiveDate, .data$DelayTimeCombined > -10, desc(.data$ArrivalInWindow), abs(.data$DelayTimeCombined))
  dataFull <- dplyr::rename(dataFull, Date = "EffectiveDate")
  dataFull <- dplyr::ungroup(dplyr::slice(dataFull,1))

  ## the rest of the code adds diagnostic information and cleans up variable names, order, etc.
  dataFull <- dplyr::mutate(
    dataFull,
    Date = {x <- as.Date(.data$Date, tz = !!TZ); lubridate::tz(x) <- !!TZ; x},
    OSVehicleOverride = !is.na(.data$OriginalRPVehicle),
    OriginalRPVehicle = dplyr::case_when(is.na(.data$OriginalRPVehicle) ~ .data$RPVehicle,
                           TRUE ~ .data$OriginalRPVehicle))
  dataFull <- dplyr::left_join(dataFull, dataUncovered, by = c(OriginalRPVehicle = "RPVehicle", "AMPM", "RouteSetName", "Date"))
  dataFull <- dplyr::mutate(dataFull, Uncovered = dplyr::case_when(is.na(.data$Uncovered) ~ FALSE,
                                                                   TRUE ~ .data$Uncovered))
  dataCovered <- dplyr::filter(dataFull, .data$Uncovered & (!is.na(.data$ActualTime) | !is.na(.data$ZonarActualTime)))
  ## delete trips incorrectly listed as uncovered
  if(nrow(dataCovered) > 0) {
    ducheck <- nrow(googlesheets4::read_sheet(uncovered_url, col_types = "Dccccc"))
    if(nrow(dataUncoveredOrig) != ducheck) {
      message("uncovered trips log was modified externally, skipping update")
    } else {
      coveredRows <- which(dataUncoveredOrig$Date %in% dataCovered$Date &
                             dataUncoveredOrig$`AM/PM` %in% dataCovered$AMPM &
                             dataUncoveredOrig$Bus %in% dataCovered$RPVehicle &
                             dataUncoveredOrig$`Route Set` %in% dataCovered$RouteSetName)

      if(length(coveredRows) > 0) {
        coveredRows <- sort(coveredRows, decreasing = TRUE)
        for(row in as.character(coveredRows + 1)) googlesheets4::range_delete(uncovered_url, range = paste0(row, ":", row))
        txt <- paste("This automated notice is to inform you that one or more trips in the uncovered",
                      "list at ", uncovered_url, " have been removed. These trips were determined to have been covered after all.",
                      "The following records were removed:\n\n<pre><code>",
                      paste0(gsub("`", " ", capture.output(print(dataUncoveredOrig[coveredRows, ]))[-c(1, 3)], fixed = TRUE), collapse = "\n"),
                      "</code></pre>\n\nIf you no longer wish to receive this notice reply \"unsubscribe\".",
                      collapse = " ", sep = " ")
        email <- gmailr::gm_mime()
        email <- gmailr::gm_to(email, mail_to)
        email <- gmailr::gm_from(email, mail_from)
        email <- gmailr::gm_subject(email, "Uncovered trips updated")
        email <- gmailr::gm_html_body(email, txt)
        gmailr::gm_send_message(email)
      }
    }
  }
  dataFull <- dplyr::mutate(
    dataFull,
    CutoffMin = !!cutoff_min, CutoffMax = !!cutoff_max,
    Uncovered = dplyr::case_when(!is.na(.data$ActualTime) | !is.na(.data$ZonarActualTime) ~ FALSE,
                                 TRUE ~ .data$Uncovered),
    TripCategory = dplyr::case_when(
      .data$excludedShuttlesEtc ~ "Excluded",
      .data$Uncovered ~ "Uncovered",
      !.data$ArrivalInWindow ~ "Arrived outside window",
      !is.na(.data$DelayTimeCombined) ~ "Reported",
      is.na(.data$DelayTimeCombined) ~ "Unreported",
      TRUE ~ "Unknown")
    )
  lubridate::tz(dataFull$Date) <- TZ
  class(dataFull) <- c("otpdata", class(dataFull))
  dataFull <- dplyr::select(dataFull, -"Driver", -"RouteSetID", -"zoneID", -"ZonarAssetID", -"time_out", -"tgroup", -"PackageNumber")
  dataFull <- dplyr::relocate(dataFull,
                              "Arrival", "DelayTimeCombined", "ActualTime", "ZonarActualTime", "DelayTime", "ZonarDelayTime",
                              .after = "ExpectedTime")
  dataFull <- dplyr::relocate(dataFull, "AMPM", .after = "Date")
  dataFull
}

#'
#' @export
unreported_trips <- function(data) {
  dplyr::filter(data, .data$excludedShuttlesEtc | (is.na(.data$DelayTimeCombined) & !.data$Uncovered))
}

#'
#' @export
prepare_raw <- function(data, oldstyle = FALSE, ...) {
  if(inherits(data, "otpformatted")) return(data)
  if(!inherits(data, "otpdata")) stop("`data` must be an otpdata object or a otpformatted object")
  #otp_data <- dplyr::filter(
  #  data,
  #  !.data$excludedShuttlesEtc & (!is.na(.data$DelayTimeCombined) | .data$Uncovered))
  otp_data <- dplyr::rename(
    data,
    Delay.Minutes. = "DelayTimeCombined",
    Planned.Anchor.Time = "ExpectedTime",
    Vehicle = "RPVehicle",
    Riders = "ActualLoad")
  otp_data <- dplyr::mutate(
    otp_data,
    OnTimeRateByAnchor = ifelse(.data$Uncovered | .data$Delay.Minutes. > 0, 0,1),
    Within5MinOfAnchor = ifelse(.data$Uncovered | .data$Delay.Minutes. > 5, 0,1),
    Within10MinOfAnchor = ifelse(.data$Uncovered |.data$Delay.Minutes. > 10, 0,1),
    TrueOnTime = ifelse(.data$Uncovered |.data$Delay.Minutes. > 15, 0,1),
    Late10Min = ifelse(.data$Uncovered | .data$Delay.Minutes. > 25, 0,1),
    Late15Min = ifelse(.data$Uncovered | .data$Delay.Minutes. > 30, 0,1),
    Late25Min = ifelse(.data$Uncovered | .data$Delay.Minutes. > 40, 0,1),
    Late30Min = ifelse(.data$Uncovered | .data$Delay.Minutes. > 45, 0,1),
    AnchorTimeConverted = round(lubridate::interval(.data$Date, .data$Planned.Anchor.Time)/lubridate::dminutes(1), digits = 0),
    TimeTier = dplyr::case_when(.data$AnchorTimeConverted < 451 ~ "7:30",
                                .data$AnchorTimeConverted < 511 ~ "8:30",
                                .data$AnchorTimeConverted < 651 ~ "9:30",
                                TRUE ~ ""),
    TypeOfBus = substr(.data$Vehicle,1,1),
    Date = format(Date, "%m/%d/%Y"),
    concatRouteDate = paste(.data$RPRoute, Date, sep=': '),
    BusDay = paste(.data$Vehicle, .data$Date,sep=": ")
  )
  ## There is some useless stuff in here, but we need it for consistency with legacy reports.
  out <- dplyr::transmute(
    otp_data,
    .data$Date,
    .data$concatRouteDate,
    Route = as.integer(.data$RPRoute),
    .data$Vehicle,
    Anchor.Location = .data$AnchorAbbrev,
    Planned.Start.Time = "",
    Planned.Anchor.Time = format(.data$Planned.Anchor.Time, "%H:%M"),
    Actual.Arrival.Time = format(.data$Arrival, "%H:%M"),
    .data$Delay.Minutes.,
    .data$concatRouteDate,
    .data$OnTimeRateByAnchor,
    .data$Within5MinOfAnchor,
    .data$Within10MinOfAnchor,
    .data$TrueOnTime,
    .data$Late10Min,
    .data$Late15Min,
    .data$Late25Min,
    .data$Late30Min,
    .data$AMPM,
    .data$TimeTier,
    .data$TypeOfBus,
    .data$BusDay,
    Yard = as.character(factor(.data$OvernightGarage,
                               levels = c("FREEPORT", "READVILLE", "WASHINGTON"),
                               labels = c("Frpt", "Read", "Wash"))),
    X.1 = "",
    X.2 = "",
    .data$AnchorTimeConverted,
    X.5 = "",
    .data$Uncovered,
    .data$excludedShuttlesEtc
  )
  if(oldstyle) {
    out <- out[, c("Date", "concatRouteDate", "Route", "Vehicle", "Anchor.Location",
                 "Planned.Start.Time", "Planned.Anchor.Time", "Actual.Arrival.Time",
                 "Delay.Minutes.", "OnTimeRateByAnchor", "Within5MinOfAnchor",
                 "Within10MinOfAnchor", "TrueOnTime", "Late10Min", "Late15Min",
                 "Late25Min", "Late30Min", "AMPM", "TimeTier", "TypeOfBus",
                 "BusDay", "Yard", "X.1", "X.2", "AnchorTimeConverted", "X.5")]
  }
  class(out) <- c("otpformatted", setdiff(class(out), "otpdata"))
  out
}


#'
#' @export
summary.otpformatted <- function(otp_data, internal=FALSE, ...) {
  df <- dplyr::filter(otp_data, !.data$excludedShuttlesEtc)
  df$X.2 <- as.numeric(df$X.2)

  if(length(unique(otp_data$Date)) > 1) stop("Multiple dates detected, please iterate explicitly.")

  otp <- data.frame(OTP_Date                                = as.character(unique(otp_data$Date)),
                    OTP_AM                                  = mean(df[df$AMPM == "AM", ][["TrueOnTime"]], na.rm = TRUE),
                    OTP_PM                                  = mean(df[df$AMPM == "PM", ][["Within10MinOfAnchor"]], na.rm = TRUE),
                    AM.by.Time.Tier_7.30                    = mean(df[df$TimeTier=="7:30",][["TrueOnTime"]], na.rm = TRUE),
                    AM.by.Time.Tier_8.30                    = mean(df[df$TimeTier=="8:30",][["TrueOnTime"]], na.rm = TRUE),
                    AM.by.Time.Tier_9.30                    = mean(df[df$TimeTier=="9:30",][["TrueOnTime"]], na.rm = TRUE),
                    AM.by.Yard_READ                         = mean(df[df$AMPM == "AM" & df$Yard=="Read",][["TrueOnTime"]], na.rm = TRUE),
                    AM.by.Yard_WASH                         = mean(df[df$AMPM == "AM" & df$Yard=="Wash",][["TrueOnTime"]], na.rm = TRUE),
                    AM.by.Yard_FRPT                         = mean(df[df$AMPM == "AM" & df$Yard=="Frpt",][["TrueOnTime"]], na.rm = TRUE),
                    AM.OTP_Within.Anchor                    = mean(df[df$AMPM == "AM",][["OnTimeRateByAnchor"]], na.rm = TRUE),
                    AM.OTP_Within.15.Minutes.of.On.Time     = mean(df[df$AMPM == "AM",][["Late15Min"]], na.rm = TRUE),
                    AM.OTP_Within.30.Minutes.of.School.Time = mean(df[df$AMPM == "AM",][["Late30Min"]], na.rm = TRUE),
                    PM.by.Yard_READ                         = mean(df[df$AMPM == "PM" & df$Yard=="Read", ][["Within10MinOfAnchor"]], na.rm = TRUE),
                    PM.by.Yard_WASH                         = mean(df[df$AMPM == "PM" & df$Yard=="Wash", ][["Within10MinOfAnchor"]], na.rm = TRUE),
                    PM.by.Yard_FRPT                         = mean(df[df$AMPM == "PM" & df$Yard=="Frpt", ][["Within10MinOfAnchor"]], na.rm = TRUE),
                    PM.OTP_Within.Anchor                    = mean(df[df$AMPM == "PM",][["OnTimeRateByAnchor"]], na.rm = TRUE),
                    PM.OTP_Within.15.Minutes.of.On.Time     = mean(df[df$AMPM == "PM",][["Late10Min"]], na.rm = TRUE),
                    PM.OTP_Within.30.Minutes.of.School.Time = mean(df[df$AMPM == "PM",][["Late25Min"]], na.rm = TRUE))

  class(otp) <- c("otpsummary", class(otp))
  otp
}

#'
#' @export
summary.otpdata <- function(data) {
  data <- prepare_raw(data)
  summary(data)
}


#'
#' @export
update_otp_report = function(data,
                             id=c(Sys.getenv("OTP_REPORT_ID"), Sys.getenv("OTP_REPORT_INTERNAL_ID")),
                             sheet = Sys.getenv("OTP_SHEET"),
                             internal=FALSE,  allow_unmatched = FALSE,
                             ...) {
  UseMethod("update_otp_report")
}

#'
#' @export
update_otp_report.otpdata <- function(data,
                                      id=c(Sys.getenv("OTP_REPORT_ID"), Sys.getenv("OTP_REPORT_INTERNAL_ID")),
                                      sheet = Sys.getenv("OTP_SHEET"),
                                      internal=FALSE,  allow_unmatched = FALSE,
                                      ...) {

  al <- as.list(match.call())[-1]
  al$data <- summary(data)
  do.call(update_otp_report, al)
}

#'
#' @export
update_otp_report.otpsummary <- function(data,
                                         id=c(Sys.getenv("OTP_REPORT_ID"), Sys.getenv("OTP_REPORT_INTERNAL_ID")),
                                         sheet = Sys.getenv("OTP_SHEET"),
                                         internal=FALSE,  allow_unmatched = FALSE,
                                         ...) {

  browser()
  if(nrow(data) != 1) stop("Input 'data' must be a single row, please iterate explicity if you need to update multiple rows")

  options(gargle_oauth_email = TRUE)

  if(missing(id)) {
    if(internal) id = Sys.getenv("OTP_REPORT_ID")
    if(!internal) id = Sys.getenv("OTP_REPORT_INTERNAL_ID")
  }

  ## Matching the data to the sheet column headers is a really pain, but otherwise we just
  ## have to stick it in there and hope it matches... better to try checking it, as painful as it is.
  suppressMessages(sht <- googlesheets4::read_sheet(ss=id, sheet = sheet, skip=1, col_names = FALSE, n_max = 3, col_types = "c", trim_ws = TRUE))
  sn <- dplyr::as_tibble(t(sht))
  sn <- tidyr::fill(sn, V1, .direction = "down")[c(-2,-1), ]
  sn[is.na(sn$V1), "V1"] <- ""
  sn[sn$V1 != "", "V1"] <- paste0(sn[sn$V1 != "", ][["V1"]], "_")
  snames <- make.names(paste0(sn$V1, sn$V2))

  missingNames <- setdiff(snames, names(data))
  if(length(missingNames) > 0) {
    msg <- paste("Could not match input data names to all the sheet column names, input is missing these columns:\n", missingNames)
    if(allow_unmatched){
      warning(msg)
    } else {
      stop(msg)
    }
  }

  ## Make sure we have a date column
  suppressMessages(gsreport <- googlesheets4::read_sheet(ss=id, sheet = sheet, skip=2, col_names = TRUE, trim_ws = TRUE))
  DateCol <- which(names(gsreport) == "Date")
  if(length(DateCol) != 1) stop("Could not find 'Date' column in row 3 of google sheet", id, "please check your sheet id and sheet name")
  gsreport <- gsreport[, DateCol:ncol(gsreport)]
  ## ugh, now we can actually update the report...
  gsreport <- gsreport[, 1:length(snames)]
  names(gsreport) <- snames
  input <- tail(dplyr::bind_rows(gsreport, data), nrow(data))[, snames]
  row <- which(gsreport$OTP_Date %in% input$OTP_Date) + 3 ## yes, the data starts on row 3. Ugh.
  if(length(row) !=1) stop(paste0("Could not determine which row to update, please check that the 'Date' columns in 'data' and google sheet ", id, " match."))
  range <- paste0(LETTERS[DateCol], as.character(row))
  googlesheets4::range_write(input, ss = id, sheet = sheet, range = range, col_names = FALSE, reformat = FALSE)

  return(TRUE)
}

