#' get effort by port, box, fleet for data sets
#'
#' Effort is measured as Days at Sea at the trip level
#'
#' @param fleetData data frame. Data which contains the effort by box, and fleet data
#' @param ports vector, list of ports, by PORTID for which effort is requred
#'
#' @return data frame of port effort aggregated by year, box
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fleetData <- readRDS(here::here("data/NEGroundfishDataCAMS.rds"))
#' get_effort(fleetData,
#'            ports=c(220101, 320201, 240207, 240115, 240301, 240403, 420209, 350635, 320901, 240813, 240601),
#'            combine = data.frame(main = c(320201, 240115, 240301), associated = c(320901,240813, 240601) ))
#' }

get_effort <- function(fleetData,ports,combine) {

  fleets <- fleetData |>
    tidyr::separate(col=Area,into = c("text","Box"),sep = "ID_") |>
    dplyr::mutate(Box = as.numeric(Box)) |>
    dplyr::filter(PORTID %in% ports) |>
    dplyr::group_by(Year,Box,newport,STATEABB,PORTID) |>
    dplyr::summarise(effort = sum(InsideDAS),
                     .groups = "drop")

  otherFleet <- fleetData |>
    tidyr::separate(col=Area,into = c("text","Box"),sep = "ID_") |>
    dplyr::mutate(Box = as.numeric(Box)) |>
    dplyr::filter(!(PORTID %in% ports)) |>
    dplyr::group_by(Year,Box) |>
    dplyr::summarise(effort = sum(InsideDAS),
                     .groups = "drop") |>
    dplyr::mutate(PID = NA,
                  newport = "OTHER",
                  STATEABB = "NA") |>
    dplyr::relocate(Year,Box,PID,effort,newport,STATEABB)


  portNames <- fleets |>
    dplyr::distinct(newport,STATEABB,PORTID)

  # now combine some of the fleets
  # relabel port ids
  fleets$PID <- fleets$PORTID
  for (iport in 1:nrow(combine)) {
    dep <- combine$associated[iport]
    main <- combine$main[iport]

    ind <- fleets$PORTID == dep
    fleets$PID[ind] <- main
  }

  # now aggregate
  effortByBox <- fleets |>
    dplyr::group_by(Year,Box,PID) |>
    dplyr::summarise(effort = sum(effort),
                     .groups = "drop") |>
    dplyr::left_join(portNames, by = c("PID"="PORTID"))

  effortByBox <- rbind(effortByBox,otherFleet)

  return(effortByBox)

}


