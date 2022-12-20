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
  } else {
    if(DBI::dbGetInfo(conn)$dbname != database) conn <- rp_connect(database = database)
  }
  if(is.character(tablename)) tablename <- tablename[[1]]
  DBI::dbReadTable(conn, tablename)
}
