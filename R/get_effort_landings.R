#' get effort by port, box, fleet for data sets
#'
#' Effort is measured as Days at Sea at the trip level
#'
#' @param fleetData data frame. Data which contains the effort by box, and fleet data. Make sure you understand the units of the landings
#' @param ports vector, list of ports, by PORTID for which effort is requred
#'
#' @return data frame of port effort aggregated by year, box
#'
#'
#' @export
#'
#'@family modelData
#'
#' @examples
#' \dontrun{
#' fleetData <- readRDS(here::here("data/NEGroundfishDataCAMS.rds"))
#' get_effort(fleetData,
#'            ports=c(220101, 320201, 240207, 240115, 240301, 240403, 420209, 350635, 320901, 240813, 240601),
#'            combine = data.frame(main = c(320201, 240115, 240301), associated = c(320901,240813, 240601) ))
#' }

get_effort_landings <- function(fleetData,ports,combine,speciesCodes) {

  message("Calculating trip effort for main ports")
  # distinct trips
  tripEff <- fleetData |>
    tidyr::separate(col=Area,into = c("text","Box"),sep = "ID_") |>
    dplyr::mutate(Box = as.numeric(Box)) |>
    dplyr::filter(PORTID %in% ports) |>
    dplyr::select(Year,Box,newport,STATEABB,PORTID,TRIPID,InsideDAS) |>
    dplyr::distinct()

  # true effort summed over trips and not species/trips
  das <- tripEff |>
    dplyr::group_by(Year,Box,newport,STATEABB,PORTID) |>
    dplyr::summarise(effort = sum(InsideDAS),
                     .groups="drop")

  message("calculating landings for main ports")
  # landings
  fleets <- fleetData |>
    tidyr::separate(col=Area,into = c("text","Box"),sep = "ID_") |>
    dplyr::mutate(Box = as.numeric(Box)) |>
    dplyr::filter(PORTID %in% ports,
                  Code %in% speciesCodes) |>
    dplyr::group_by(Year,Box,newport,STATEABB,PORTID,Code) |>
    dplyr::summarise(landings = sum(InsideLANDED),
                     .groups = "drop")

  message("calculating effort for other ports")
  ## repeat for other fleet effort
  otherFleetTrip <- fleetData |>
    tidyr::separate(col=Area,into = c("text","Box"),sep = "ID_") |>
    dplyr::mutate(Box = as.numeric(Box)) |>
    dplyr::filter(!(PORTID %in% ports)) |>
    dplyr::select(Year,Box,newport,STATEABB,PORTID,TRIPID,InsideDAS) |>
    dplyr::distinct()

  otherdas <- otherFleetTrip |>
    dplyr::group_by(Year,Box) |>
    dplyr::summarise(effort = sum(InsideDAS),
                     .groups="drop") |>
    dplyr::mutate(PID = NA,
                  newport = "OTHER",
                  STATEABB = "NA") |>
    dplyr::relocate(Year,Box,PID,effort,newport,STATEABB)

  message("calculating landings for other ports")
  ## other fleet landings
  otherFleetLandings <- fleetData |>
    tidyr::separate(col=Area,into = c("text","Box"),sep = "ID_") |>
    dplyr::mutate(Box = as.numeric(Box)) |>
    dplyr::filter(!(PORTID %in% ports),
                  Code %in% speciesCodes) |>
    dplyr::group_by(Year,Box) |>
    dplyr::summarise(landings = sum(InsideLANDED),
                     .groups = "drop") |>
    dplyr::mutate(PID = NA,
                  newport = "OTHER",
                  STATEABB = "NA",
                  Code = "NA") |>
    dplyr::relocate(Year,Box,PID,landings,newport,STATEABB)


  portNames <- fleets |>
    dplyr::distinct(newport,STATEABB,PORTID)

  message("combining all data")
  # now combine some of the fleets
  # relabel port ids
  fleets$PID <- fleets$PORTID
  das$PID <- das$PORTID
  for (iport in 1:nrow(combine)) {
    dep <- combine$associated[iport]
    main <- combine$main[iport]

    # combine landings
    ind <- fleets$PORTID == dep
    fleets$PID[ind] <- main
    # combine effort
    ind2 <- das$PORTID == dep
    das$PID[ind2] <- main
  }

  # now aggregate main ports and other port
  # landings
  landings <- fleets |>
    dplyr::group_by(Year,Box,PID,Code) |>
    dplyr::summarise(landings = sum(landings),
                     .groups = "drop") |>
    dplyr::left_join(portNames, by = c("PID"="PORTID"))

  landingsByBox <-  rbind(landings,otherFleetLandings)

  #effort
  effort <- das |>
    dplyr::group_by(Year,Box,PID) |>
    dplyr::summarise(effort = sum(effort),
                     .groups = "drop") |>
    dplyr::left_join(portNames, by = c("PID"="PORTID"))


  effortByBox <- rbind(effort,otherdas)

  return(list(effort=effortByBox,landings=landingsByBox))

}


