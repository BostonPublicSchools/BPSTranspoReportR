#' Create student monitor report
#'
#' @description Equivalent to *Import/Export => Student => Monitors student file*
#'
#' @inheritParams rp_get_table
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
#' dplyr::glimpse(dplyr::select(student_monitor, dplyr::starts_with("Pickup")))
#'
rp_report_student_monitor <- function(conn=NULL, database=Sys.getenv("RP_DATABASE")) {

  ## get and prepare student data
  student <- rp_get_table(conn, "v_crosstab_student", database = database)
  student_data <- dplyr::select(
    student,
    "RecordID",
    "Student.ID",
    "Last.Name", "First.Name",
    "Program.Description",
    "Default.School.Abbrev",
    "Default.School.Anchor.Abbrev",
    "Default.School.Name"
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

  ## reformat with separate bus and route columns by day
  wide_data <- tidyr::pivot_wider(
    full_data,
    names_from = "Days",
    values_from = c("Pickup.Bus", "Dropoff.Bus", "Pickup.Route", "Dropoff.Route"))

  # cleanup missing days (we could just drop entirely, but keeping for compatibility with RP export)
  dplyr::select(wide_data, -dplyr::ends_with("_NA"), -dplyr::ends_with("_"))
}
