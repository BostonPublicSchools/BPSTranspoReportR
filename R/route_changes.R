
#' Old greensheet categorization
#'
#' Internal function used to re-create the old green sheet categorization
#' for comparison with the updated algorithm.

old_greensheets <- function(pend, plan) {
  #pend<-read.xlsx("Routes Pending.xlsx", sheet = "Routes Pending")
  #pend<-pend[,c(3,2,5,7,8,9,10,11,12)]
  #colnames(pend)<-c("Route","RouteSet","Bus","Load","AnchorLocation","AnchorTime","RouteDist","RouteTime","Days")
  pend<-pend[,c(1,3,4,6,7,8,9)]

  #plan<-read.xlsx("Routes Planning.xlsx", sheet = "Routes Planning")
  #sheetURL <- unlist(fileURLs[5,2])
  #sheetRef<-gs4_get(sheetURL)
  #range_write(sheetRef, plan, sheet = "Trip Sheet", range = "A2", col_names = FALSE, reformat = FALSE)

  #plan<-plan[,c(3,2,5,7,8,9,10,11,12)]
  #colnames(plan)<-c("Route","RouteSet","Bus","Load","AnchorLocation","AnchorTime","RouteDist","RouteTime","Days")
  plan<-plan[,c(1,3,4,6,7,8,9)]

  temp<-sqldf::sqldf("select n.*,o.* from plan n left join pend o on n.Route=o.Route")
  temp[is.na(temp)]<-""

  temp[,15]<-0
  y<-temp[,2]!=temp[,9]
  temp[y,15]<-1

  temp[,16]<-0
  temp[temp$Load.1=="",10]<-0
  y<-as.integer(temp[,3])>=as.integer(temp[,10])+5
  z<-as.integer(temp[,3])<=as.integer(temp[,10])-5
  x<-y|z

  temp[x,16]<-1

  temp[,17]<-0
  temp[temp$RouteTime.1=="",13]<-temp[temp$RouteTime.1=="",6]
  y<-as.integer(substr(temp[,6],1,unlist(gregexpr(':',temp[,6]))-1))>=(as.integer(substr(temp[,13],1,unlist(gregexpr(':',temp[,13]))-1))+3)
  z<-as.integer(substr(temp[,6],1,unlist(gregexpr(':',temp[,6]))-1))<=(as.integer(substr(temp[,13],1,unlist(gregexpr(':',temp[,13]))-1))-3)
  x<-y|z
  temp[x,17]<-1


  temp[,18]<-0
  temp[,18]<-rowSums(temp[,c(seq(15,17))])
  z<-temp[,18]!=0
  diffDF<-temp[z,c(1,2)]

  #busYard<-read.csv("BusYard.csv",stringsAsFactors = FALSE)
  #diffDF<-sqldf("Select d.*, y.Yard from diffDF d left join busYard y on d.Bus=y.Vehicle2")
  #diffDF<-unique(diffDF[,c(3,2,1)])

  notAssigned<-diffDF[diffDF$Bus=="",]
  diffDF<-diffDF[diffDF$Bus!="",]
  #noYard<-diffDF[is.na(diffDF$Yard),]
  #diffDF<-diffDF[!(is.na(diffDF$Yard)),]
  diffDF
}


#' Flag trips that have changed
#'
#' @export
#'
tripchange_report <- function(database_pend = Sys.getenv("RP_DATABASE_PEND"),
                              database_plan = Sys.getenv("RP_DATABASE_PLAN"),
                              load_threshold = 5,
                              time_threshold = 3,
                              distance_threshold = 100) {

  con_pend <- RVersatransRP::rp_connect(database = database_pend)
  on.exit(DBI::dbDisconnect(con_pend), add = TRUE)
  con_plan <- RVersatransRP::rp_connect(database = database_plan)
  on.exit(DBI::dbDisconnect(con_plan), add = TRUE)

  ## Below we retrieve pick up and dropoff locations from the original RP tables. Alternatively
  ## we could use the v_TSatPickupDropOff view which has pretty much everything. I've done it
  ## this way to have more control over the result and a better understanding of where the data
  ## comes from.

  time_threshold <- time_threshold*60 # convert seconds to minutes

  route_pend <- dplyr::collect(
    dplyr::select(dplyr::tbl(con_pend, "Route"),
                  "RecordID":"ActualLoad", "RouteTime", "RouteDistance", "AnchorTime",
                  "VehicleID", "Days", "OriginPointID", "DestinationPointID", "AnchorTimeAtOrigin"))
  route_plan <- dplyr::collect(
    dplyr::select(dplyr::tbl(con_plan, "Route"), "RecordID":"ActualLoad", "RouteTime", "RouteDistance", "AnchorTime",
                  "VehicleID", "Days", "OriginPointID", "DestinationPointID", "AnchorTimeAtOrigin"))

  route_pend <- dplyr::left_join(
    route_pend,
    dplyr::collect(dplyr::select(dplyr::tbl(con_pend, "v_VehicleList"), RecordID="recordid", Vehicle = "vehicle", Yard="OvernightGarage")),
    by = c(VehicleID = "RecordID"))
  route_plan <- dplyr::left_join(
    route_plan,
    dplyr::collect(dplyr::select(dplyr::tbl(con_plan, "v_VehicleList"), RecordID="recordid", Vehicle = "vehicle", Yard="OvernightGarage")),
    by = c(VehicleID = "RecordID"))

  route_pend <- dplyr::left_join(
    route_pend,
    dplyr::collect(dplyr::select(dplyr::tbl(con_pend, "RouteSet"), "RecordID", RouteSet = "Name")),
    by = c(RouteSetID = "RecordID"))
  route_plan <- dplyr::left_join(
    route_plan,
    dplyr::collect(dplyr::select(dplyr::tbl(con_plan, "RouteSet"), "RecordID", RouteSet = "Name")),
    by = c(RouteSetID = "RecordID"))

  ##colnames(pend)<-c("Route","RouteSet","Bus","Load","AnchorLocation","AnchorTime","RouteDist","RouteTime","Days")
  oldgs <- old_greensheets(pend = dplyr::transmute(route_pend,
                                            Route=RecordID, RouteSet, Bus=Vehicle, Load=ActualLoad, AnchorLocation=Name,
                                            AnchorTime = as.character(hms::as_hms(AnchorTime)), RouteDist=RouteDistance,
                                            RouteTime = paste(RouteTime/60, RouteTime %% 60, sep = ":"), Days),
                           plan = dplyr::transmute(route_plan,
                                            Route=RecordID, RouteSet, Bus=Vehicle, Load=ActualLoad, AnchorLocation=Name,
                                            AnchorTime = as.character(hms::as_hms(AnchorTime)), RouteDist=RouteDistance,
                                            RouteTime = paste(RouteTime/60, RouteTime %% 60, sep = ":"), Days))

  route_detail_pend <- dplyr::collect(
    dplyr::select(dplyr::tbl(con_pend, "RouteDetail"), "RecordID", "RouteID", "PointID", "DesignatedStop"))
  route_detail_plan <- dplyr::collect(
    dplyr::select(dplyr::tbl(con_plan, "RouteDetail"), "RecordID", "RouteID", "PointID", "DesignatedStop"))

  route_detail_pend <- dplyr::left_join(route_detail_pend, route_pend, by = c(RouteID = "RecordID"), suffix = c(".routedetail", ".route"))
  route_detail_plan <- dplyr::left_join(route_detail_plan, route_plan, by = c(RouteID = "RecordID"), suffix = c(".routedetail", ".route"))

  transreq_pend <- dplyr::collect(
    dplyr::select(dplyr::tbl(con_pend, "TransportationRequest"),
                  "RecordID", "StudentID", "OriginPointID", "DestinationPointID", "RequestType"))
  transreq_plan <- dplyr::collect(
    dplyr::select(dplyr::tbl(con_plan, "TransportationRequest"),
                  "RecordID", "StudentID", "OriginPointID", "DestinationPointID", "RequestType"))

  transsat_pend <- dplyr::collect(
    dplyr::select(dplyr::tbl(con_pend, "TransportationSatisfaction"),
                  "TRexID", "PickupRouteDetailID", "DropoffRouteDetailID"))
  transsat_plan <-dplyr::collect(
    dplyr::select(dplyr::tbl(con_plan, "TransportationSatisfaction"),
                  "TRexID", "PickupRouteDetailID", "DropoffRouteDetailID"))

  trans_pend <- dplyr::inner_join(transreq_pend, transsat_pend, by = c(RecordID = "TRexID"), multiple = "all", suffix = c(".transreq", ".transsat"))
  trans_plan <- dplyr::inner_join(transreq_plan, transsat_plan, by = c(RecordID = "TRexID"), multiple = "all", suffix = c(".transreq", ".transsat"))

  trans_pend <- dplyr::inner_join(distinct(trans_pend),
                           distinct(select(route_detail_pend,
                                           "RecordID", "VisibleRouteID", op="OriginPointID", dp="DestinationPointID")),
                           by = c(PickupRouteDetailID = "RecordID"), suffix = c(".trans", ".route"))
  trans_plan <- dplyr::inner_join(distinct(trans_plan),
                           distinct(select(route_detail_plan,
                                           "RecordID", "VisibleRouteID", op="OriginPointID", dp="DestinationPointID")),
                           by = c(PickupRouteDetailID = "RecordID"), suffix = c(".trans", ".route"))

  trans_pend <- dplyr::mutate(trans_pend,
                       DestinationPointID = dplyr::case_when(RequestType == "I" ~ dp, TRUE ~ DestinationPointID),
                       OriginPointID = dplyr::case_when(RequestType == "O" ~ op, TRUE ~ OriginPointID),
                       InPending = TRUE,
                       op=NULL,
                       dp=NULL)
  trans_plan <- dplyr::mutate(trans_plan,
                       DestinationPointID = dplyr::case_when(RequestType == "I" ~ dp, TRUE ~ DestinationPointID),
                       OriginPointID = dplyr::case_when(RequestType == "O" ~ op, TRUE ~ OriginPointID),
                       InPlanning = TRUE,
                       op = NULL,
                       dp = NULL)

  trans_stops_pend <- distinct(dplyr::select(trans_pend, "VisibleRouteID", "RequestType", "OriginPointID", "DestinationPointID", "InPending"))
  trans_stops_plan <- distinct(dplyr::select(trans_plan, "VisibleRouteID", "RequestType", "OriginPointID", "DestinationPointID", "InPlanning"))

  points_pend <- dplyr::collect(dplyr::select(dplyr::tbl(con_pend, "Point"), "RecordID", "Description", "CustomDescription"))
  points_plan <- dplyr::collect(dplyr::select(dplyr::tbl(con_plan, "Point"), "RecordID", "Description", "CustomDescription"))

  points_pend <- dplyr::transmute(
    points_pend,
                           RecordID,
                           CustomDescription = dplyr::case_when(is.na(.data$CustomDescription) | .data$CustomDescription == "" ~ .data$Description,
                                                         TRUE ~ .data$CustomDescription))
  points_plan <- dplyr::transmute(
    points_plan,
                                  .data$RecordID,
                           CustomDescription = dplyr::case_when(is.na(.data$CustomDescription) | .data$CustomDescription == "" ~ .data$Description,
                                                         TRUE ~ .data$CustomDescription))

  trans_stops_pend <- dplyr::mutate(
    trans_stops_pend,
                             AnchorPoint = dplyr::case_when(.data$RequestType == "I" ~ .data$DestinationPointID,
                                                            .data$RequestType == "O" ~ .data$OriginPointID),
                             NonAnchorPoint = dplyr::case_when(.data$RequestType == "I" ~ .data$OriginPointID,
                                                               .data$RequestType == "O" ~ .data$DestinationPointID))
  trans_stops_plan <- dplyr::mutate(
    trans_stops_plan,
                             AnchorPoint = dplyr::case_when(.data$RequestType == "I" ~ .data$DestinationPointID,
                                                            .data$RequestType == "O" ~ .data$OriginPointID),
                             NonAnchorPoint = dplyr::case_when(.data$RequestType == "I" ~ .data$OriginPointID,
                                                               .data$RequestType == "O" ~ .data$DestinationPointID))

  trans_stops_pend <- dplyr::left_join(
    trans_stops_pend,
                                dplyr::select(points_pend, "RecordID", StopDescription = "CustomDescription"),
                                by = c(NonAnchorPoint = "RecordID"))
  trans_stops_plan <- dplyr::left_join(
    trans_stops_plan,
                                dplyr::select(points_plan, RecordID, StopDescription = "CustomDescription"),
                                by = c(NonAnchorPoint = "RecordID"))

  trans_stops_all <- dplyr::full_join(
    trans_stops_pend, trans_stops_plan,
                               by = c("VisibleRouteID", "NonAnchorPoint"),
                               suffix = c(".pend", ".plan"))

  trans_stops_all <- dplyr::mutate(
    trans_stops_all,
                            InPending = dplyr::case_when(is.na(.data$InPending) ~ FALSE, TRUE ~ .data$InPending),
                            InPlanning = dplyr::case_when(is.na(.data$InPlanning) ~ FALSE, TRUE ~ .data$InPlanning))

  trans_changed <- dplyr::transmute(
    dplyr::group_by(dplyr::filter(trans_stops_all, !.data$InPlanning | !.data$InPending),
             VisibleRouteID, InPending, InPlanning),
    StopsRemoved = dplyr::case_when(!InPlanning ~ paste(StopDescription.pend, sep = "", collapse = " :: "),
                             TRUE ~ ""),
    StopsAdded = dplyr::case_when(!InPending ~ paste(StopDescription.plan, sep = "", collapse = " :: "),
                           TRUE ~ ""))

  trans_summ <- dplyr::summarize(dplyr::group_by(trans_changed, .data$VisibleRouteID),
                          StopsRemoved = paste(unique(StopsRemoved), sep = "", collapse = ""),
                          StopsAdded = paste(unique(StopsAdded), sep = "", collapse = "")
  )
  trans_summ <- dplyr::mutate(trans_summ,
                       n_StopsRemoved = as.integer(nchar(StopsRemoved) > 0) + stringr::str_count(StopsRemoved, pattern = stringr::fixed(" :: ")),
                       n_StopsAdded = as.integer(nchar(StopsAdded) > 0) + stringr::str_count(StopsAdded, pattern = stringr::fixed(" :: ")),
                       n_StopsChanged = n_StopsRemoved + n_StopsAdded)


  route_pend <- dplyr::mutate(route_pend,
                       AnchorPointID = dplyr::case_when(AnchorTimeAtOrigin == "Y" ~ OriginPointID,
                                                 AnchorTimeAtOrigin == "Y" ~ DestinationPointID))
  route_plan <- dplyr::mutate(route_plan,
                       AnchorPointID = dplyr::case_when(AnchorTimeAtOrigin == "Y" ~ OriginPointID,
                                                 AnchorTimeAtOrigin == "Y" ~ DestinationPointID))

  route_pend <- dplyr::left_join(route_pend, dplyr::select(points_pend, "RecordID", AnchorLocation = "CustomDescription"),
                          by = c(AnchorPointID = "RecordID"))
  route_plan <- dplyr::left_join(route_plan, dplyr::select(points_plan, "RecordID", AnchorLocation = "CustomDescription"),
                          by = c(AnchorPointID = "RecordID"))

  route <- dplyr::full_join(route_pend, route_plan, by = "VisibleRouteID", suffix = c(".pend", ".plan"))
  route <- dplyr::left_join(route, trans_summ, by = c("VisibleRouteID"))
  route <- dplyr::mutate(route,
                  Direction.pend = as.character(factor(AnchorTimeAtOrigin.pend, levels = c("N", "Y"), labels = c("I", "O"))),
                  Direction.plan = as.character(factor(AnchorTimeAtOrigin.pend, levels = c("N", "Y"), labels = c("I", "O"))))

  route_changes <- dplyr::transmute(
    route,
    VisibleRouteID,
    Route.added = dplyr::case_when(!is.na(RecordID.plan) & is.na(RecordID.pend) ~ as.character(RecordID.plan), TRUE ~ ""),
    Route.removed = dplyr::case_when(!is.na(RecordID.pend) & is.na(RecordID.plan) ~ as.character(RecordID.pend), TRUE ~ ""),
    Route.diff = as.integer(Route.added != "" | Route.removed != ""),
    Direction.diff = as.numeric(Direction.plan != Direction.pend),
    Direction.added = dplyr::case_when(Direction.diff > 0 ~ Direction.plan, TRUE ~ ""),
    Direction.removed = dplyr::case_when(Direction.diff > 0 ~ Direction.pend, TRUE ~ ""),
    RouteName.diff = as.numeric(Name.plan != Name.pend),
    RouteName.added  = dplyr::case_when(RouteName.diff > 0 ~ Name.plan, TRUE ~ ""),
    RouteName.removed = dplyr::case_when(RouteName.diff > 0 ~ Name.pend, TRUE ~ ""),
    RouteSet.diff = as.numeric(RouteSetID.plan != RouteSetID.pend),
    RouteSet.added = dplyr::case_when(RouteSet.diff > 0 ~ RouteSet.plan, TRUE ~ ""),
    RouteSet.removed = dplyr::case_when(RouteSet.diff > 0 ~ RouteSet.pend, TRUE ~ ""),
    Days.diff = as.numeric(Days.plan != Days.pend),
    Days.added = dplyr::case_when(Days.diff > 0 ~ Days.plan, TRUE ~ ""),
    Days.removed = dplyr::case_when(Days.diff > 0 ~ Days.pend, TRUE ~ ""),
    AnchorLocation.diff = as.numeric(AnchorPointID.plan != AnchorPointID.pend),
    AnchorLocation.added = dplyr::case_when(AnchorLocation.diff > 0 ~ AnchorLocation.plan, TRUE ~ ""),
    AnchorLocation.removed = dplyr::case_when(AnchorLocation.diff > 0 ~ AnchorLocation.pend, TRUE ~ ""),
    AnchorTime.diff = as.integer(AnchorTime.plan - AnchorTime.pend),
    AnchorTime.added = dplyr::case_when(AnchorTime.diff > 0 ~ as.character(hms::as_hms(AnchorTime.plan)), TRUE ~ ""),
    AnchorTime.removed = dplyr::case_when(AnchorTime.diff > 0 ~ as.character(hms::as_hms(AnchorTime.pend)), TRUE ~ ""),
    Vehicle.diff = as.numeric(Vehicle.plan != Vehicle.pend),
    Vehicle.added = dplyr::case_when(Vehicle.diff > 0 ~ Vehicle.plan, TRUE ~ ""),
    Vehicle.removed = dplyr::case_when(Vehicle.diff > 0 ~ Vehicle.pend, TRUE ~ ""),
    Yard.diff = as.numeric(Yard.plan != Yard.pend),
    Yard.added = dplyr::case_when(Yard.diff > 0 ~ Yard.plan, TRUE ~ ""),
    Yard.removed = dplyr::case_when(Yard.diff > 0 ~ Yard.pend, TRUE ~ ""),
    Stops.diff = dplyr::case_when(is.na(n_StopsChanged) ~ 0, TRUE ~ n_StopsChanged),
    Stops.added = dplyr::case_when(is.na(StopsAdded) ~ "", TRUE ~ StopsAdded),
    Stops.removed = dplyr::case_when(is.na(StopsRemoved) ~ "", TRUE ~ StopsRemoved),
    DesiredLoad.diff = DesiredLoad.plan - DesiredLoad.pend,
    DesiredLoad.added = dplyr::case_when(DesiredLoad.diff > 0 ~ as.character(DesiredLoad.plan), TRUE ~ ""),
    DesiredLoad.removed = dplyr::case_when(DesiredLoad.diff > 0 ~ as.character(DesiredLoad.pend), TRUE ~ ""),
    ActualLoad.diff = ActualLoad.plan - ActualLoad.pend,
    ActualLoad.added = dplyr::case_when(abs(ActualLoad.diff) > load_threshold ~ as.character(ActualLoad.plan), TRUE ~ ""),
    ActualLoad.removed = dplyr::case_when(abs(ActualLoad.diff) > load_threshold ~ as.character(ActualLoad.pend), TRUE ~ ""),
    RouteDistance.diff = RouteDistance.plan - RouteDistance.pend,
    RouteDistance.added = dplyr::case_when(abs(RouteDistance.diff) > distance_threshold ~ as.character(RouteDistance.plan), TRUE ~ ""),
    RouteDistance.removed = dplyr::case_when(abs(RouteDistance.diff) > distance_threshold ~ as.character(RouteDistance.pend), TRUE ~ ""),
    RouteTime.diff = RouteTime.plan - RouteTime.pend,
    RouteTime.added = dplyr::case_when(abs(RouteTime.diff) > time_threshold ~ as.character(RouteTime.plan), TRUE ~ ""),
    RouteTime.removed = dplyr::case_when(abs(RouteTime.diff) > time_threshold ~ as.character(RouteTime.pend), TRUE ~ ""))

  route_changes$AnyChange  <- rowSums(dplyr::select(route_changes, dplyr::ends_with(".diff")), na.rm = TRUE) > 0
  route_changes$InOldGreensheet = as.integer(route_changes$VisibleRouteID) %in% oldgs$Route

  route_changes <- dplyr::filter(route_changes, .data$AnyChange)
  route_long <- tidyr::pivot_longer(route_changes,
                                    cols=c(dplyr::ends_with(".diff"), dplyr::ends_with(".added"), dplyr::ends_with(".removed")),
                                    names_to = c("Variable", ".value"),
                                    names_sep = "\\.")
  route_long <- dplyr::mutate(
    route_long,
    Notes = dplyr::case_when(added != "" | removed != "" ~ paste(Variable, paste(removed, added, sep = " => ")), TRUE ~ "")
  )
  route_long <- dplyr::filter(route_long, .data$Notes != "")

  route_notes <- dplyr::summarize(dplyr::group_by(route_long, .data$VisibleRouteID),
                           Notes = paste(Notes, sep = "", collapse = ";\n"))
  route_changes <- dplyr::inner_join(route_changes, route_notes, by = "VisibleRouteID")
  route_changes
}
