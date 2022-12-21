
#' Guess DNS name
#'
#' @export
rp_guess_dsn <- function() {
  txt <- paste0(system2("powershell", args = c("-NoLogo", "-NonInteractive", "-NoProfile", "-Command Get-OdbcDsn"), stdout = TRUE), collapse = "\n")
  txt <- strsplit(txt, "\n\n")[[1]]
  txt <- txt[c(-1, -length(txt))]
  txt <- strsplit(txt, "\n")
  txt <- lapply(txt, \(x) {
    m <- do.call(rbind, strsplit(x, "\\W+:\\W+"))
    m2 <- m[, 2]
    names(m2) <- m[, 1]
  })
  values <- dplyr::bind_rows(txt)
  best_guess <- dplyr::filter(values, grepl("SQL2", .data$DriverName))$Name
  if(length(best_guess) < 1) stop("`rp_odbc_name` must specify the ODBC data source pointing to the RP database")
  message("`rp_odbc_name` not specified, using \"", best_guess[[1]], "\"")
  best_guess[[1]]
}

#' Connect to RP database
#'
#' @description  Establish a connection to a Versatrans Routing and Planning database using
#' the DBI/odbc packages in R.
#'
#' @details You must set up a ODBC data source for the Versatrans RP database. You must
#' be connected to the BPS network (e.g., via VPN if you are remote). See
#' https://support.microsoft.com/en-us/office/administer-odbc-data-sources-b19f856b-5b9b-48c9-8b93-07484bfab5a7
#'
#' #' Connection parameters `rp_odbc_name` and `database` should usually be
#' specified in a .Renviron file, see
#' https://support.posit.co/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf
#' if you are not familiar with seting envionment variables in R.
#'
#' @param rp_odbc_name Name of the Windows ODBC data source to use. Retrieved
#' from `RP_ODBC_NAME` environment variable by default but can be overridden here,
#'  see details.
#' @param database Name of the default database to connect to. Retrieved
#' from `RP_DATABASE` environment variable by default but can be overridden here,
#'  see details.
#' @param TZ The timezone used by the database server.
#' @export
#' @examplesIf Sys.getenv("RP_ODBC_NAME")!=""&Sys.getenv("RP_DATABASE")!=""
#'
#' conn <- rp_connect()
#' {tbls <- DBI::dbListTables(conn); head(tbls)}
#' DBI::dbDisconnect(conn)
#'
rp_connect <- function(rp_odbc_name = Sys.getenv("RP_ODBC_NAME"),
                       database = Sys.getenv("RP_DATABASE"),
                       TZ = Sys.timezone()) {
  if(rp_odbc_name == "") rp_odbc_name <- rp_guess_dsn()
  if(database == "") {
    database <- "Sandbox"
    message("`database` not specified, using \"Sandbox\"")
  }
  DBI::dbConnect(odbc::odbc(), rp_odbc_name, database = database, timezone = TZ, timezone_out = TZ)
}

#' Retrieve RP table
#'
#' @param conn DBI connection to use, see `rp_connect`.
#' @inheritParams rp_connect
#' @param tablename The name of the table to retrieve
#' @export
#' @examplesIf Sys.getenv("RP_ODBC_NAME")!=""&Sys.getenv("RP_DATABASE")!=""
#'
rp_get_table <- function(conn=NULL,
                         tablename = c("v_crosstab_student_calc",
                                       "StudentInfoFields"),
                         database = Sys.getenv("RP_DATABASE")) {
  if(database == "") database <- "Sandbox"
  if(!inherits(conn, "Microsoft SQL Server")) {
    if(is.null(conn)) conn <- rp_connect(database = database)
    if(is.character(conn)) conn <- rp_connect(conn, database = database)
    on.exit(DBI::dbDisconnect(conn))
  } else {
    if(DBI::dbGetInfo(conn)$dbname != database) {
      conn <- rp_connect(database = database)
      on.exit(DBI::dbDisconnect(conn))
    }
  }
  if(is.character(tablename)) tablename <- tablename[[1]]
  DBI::dbReadTable(conn, tablename)
}

#' Create student monitor report
#'
#' @description Equivalent to *Import/Export => Student => Monitors student file*
#'
#' @inheritParams rp_get_table
#' @param format Character vector of length one specifying how the data should be
#' formatted. If `"long"` (the default) return separate *rows* for each weekday/pickup/dropoff.
#' If `"wide"` return separate *columns* for each weekday/pickup/dropoff.
#' @importFrom rlang .data
#' @export
#' @examplesIf Sys.getenv("RP_ODBC_NAME")!=""&Sys.getenv("RP_DATABASE")!=""
#'
#' student_monitor <- rp_report_student_monitor()
#'
#' ## data fields:
#' dplyr::glimpse(student_monitor[0, ])
#'
#' ## example pickup data:
#' dplyr::glimpse(
#' dplyr::select(
#'   student_monitor,
#'   -dplyr::contains("ID"), -dplyr::contains("Last"), -dplyr::contains("First")))
#'
rp_report_student_monitor <- function(conn=NULL,
                                      database=Sys.getenv("RP_DATABASE"),
                                      format = c("long", "wide")) {

  format <- format[[1]]
  if(!format %in% c("long", "wide")) stop("`format` must be one of \"long\" or \"wide\"")
  ## get and prepare student data
  student <- rp_get_table(conn=conn, "v_crosstab_student", database = database)
  student_data <- dplyr::select(
    student,
    "RecordID",
    "Student.ID",
    "Last..First",
    "Program.Description",
    School.Abbrev = "Default.School.Abbrev",
    School.Anchor.Abbrev = "Default.School.Anchor.Abbrev" ,
    School.Name = "Default.School.Name"
  )
  student_data <- dplyr::filter(
    student_data,
    tolower(trimws(.data$Program.Description)) %in%
      tolower(trimws(
        c("AC ACCOMMODATED CR FULL D",
          "ACA ACCOMMODATED CR AM",
          "ACA ACCOMMODATED CR PM",
          "CA CORNER AM",
          "CA CORNER PM",
          "CR CORNER FULL DAY",
          "DA DOOR AM",
          "DA DOOR PM",
          "DR DOOR FULL DAY",
          "M MEDICAL FULL DAY",
          "MA MEDICAL AM",
          "MA MEDICAL PM",
          "NA NONAMBULATORY FULL DAY",
          "NAP NONAMBULATORY PM")))
  )

  ## get and prepare info data
  info <- rp_get_table(conn, "StudentInfoFields", database = database)
  info_data <- dplyr::mutate(
    info,
    INFO_WC = ifelse(.data$InfoID %in% c(1, 15) & .data$Description == "WC", .data$Description, NA),
    INFO_NEED_MONITOR = ifelse(.data$InfoID %in% c(1, 15) & .data$Description %in% c("1", "M"), .data$Description, NA))
  info_data <- dplyr::select(info_data, "StudentID", "INFO_WC", "INFO_NEED_MONITOR")
  info_data <- dplyr::filter(info_data, !is.na(.data$INFO_WC) | !is.na(.data$INFO_NEED_MONITOR))
  info_data <- dplyr::distinct(info_data)
  info_data <- dplyr::summarize(
    dplyr::group_by(info_data, .data$StudentID),
    INFO_WC = paste0(.data$INFO_WC[!is.na(.data$INFO_WC)], collapse=","),
    INFO_NEED_MONITOR = paste0(.data$INFO_NEED_MONITOR[!is.na(.data$INFO_NEED_MONITOR)], collapse = ","))
  info_data <- dplyr::mutate(info_data, dplyr::across(dplyr::everything(), \(x) ifelse(x == "", NA, x)))

  ## get and prepare pickup and dropoff data)
  pickup <- rp_get_table(conn, "v_crosstab_PickUpRouteBus", database = database)
  pickup_data <- dplyr::rename(dplyr::distinct(pickup), Pickup.Route = "Route", Pickup.Bus = "Bus")
  dropoff <- rp_get_table(conn, "v_crosstab_DropOffRouteBus", database = database)
  dropoff_data <- dplyr::rename(dplyr::distinct(dropoff), Dropoff.Route = "Route", Dropoff.Bus = "Bus")

  ## merge
  full_data <- dplyr::left_join(student_data, info_data, by = c(RecordID = "StudentID"))
  full_data <- dplyr::left_join(full_data, pickup_data, by = c(RecordID = "StudentID"))
  full_data <- dplyr::left_join(full_data, dropoff_data, by = c(RecordID = "StudentID", "Days"))
  names(full_data) <- gsub(".", "_", names(full_data), fixed = TRUE)
  full_data <- dplyr::relocate(full_data, "INFO_WC", "INFO_NEED_MONITOR", .after = "Program_Description")

  if(format == "wide") {
    ## reformat with separate bus and route columns by day
    wide_data <- dplyr::mutate(full_data, Days = factor(.data$Days, levels = c("M", "T", "W", "H", "F"), labels = c("Mon.", "Tue.", "Wed.", "Thu.", "Fri.")))
    wide_data <- tidyr::pivot_wider(
      wide_data,
      names_from = "Days",
      values_from = c("Pickup_Bus", "Pickup_Route", "Dropoff_Bus",  "Dropoff_Route"),
      names_vary = "slowest",
      names_sort = TRUE,
      names_glue = "{Days}_{.value}")

    # cleanup missing days (we could just drop entirely, but keeping for compatibility with RP export)
    wide_data <- dplyr::select(wide_data, -"RecordID", -dplyr::starts_with("NA_"), -dplyr::starts_with("_"))

    final_data <- wide_data
  }
  if(format == "long") {
    final_data <- tidyr::pivot_longer(
      full_data,
      cols = c("Pickup_Route", "Pickup_Bus", "Dropoff_Route", "Dropoff_Bus"),
      names_to = c("Shift", ".value"),
      names_sep = "_")
  }
  # cleanup names, for compatibility with RP export
  names(final_data) <- gsub("_", " ", names(final_data), fixed = TRUE)
  names(final_data) <- gsub("  ", ", ", names(final_data), fixed = TRUE)
  final_data <- dplyr::rename(final_data, Day = "Days")
  final_data
}
