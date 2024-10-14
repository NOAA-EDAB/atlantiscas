#' get effort by port, box, fleet for data sets
#'
#' Effort is measured as Days at Sea at the trip level
#'
#' @param fleetData data frame. Data which contains the effort by box, and fleet data. Make sure you understand the units of the landings
#' @param ports vector, list of ports, by PORTID for which effort is requred
#' @param combine data frame. main ports and associated ports (one to one)
#' @param speciesCodes character vector. Atlantis species codes
#' @param lbs Boolean. Are landings in lbs? (Default = T)
#'
#'
#' @return data frame of port effort and landings aggregated by year, box, Gear category, species.
#' if in lbs, landings are converted to metric tons
#'
#' @export
#'
#'@family modelData
#'
#' @examples
#' \dontrun{
#' fleetData <- readRDS(here::here("data/NEGroundfishDataCAMS.rds"))
#' get_effort_landings2(fleetData$neus,
#'            ports=c(240403,240207,220101,240115,420209,240301,320201,350635),
#'            combine = data.frame(main = c(320201, 240115, 240301), associated = c(320901,240813, 240601) ),
#'            speciesCodes = c("COD","HAD","YTF","POL","PLA","WTF","WHK","WIF","RED","HAL","WPF","OPT","WOL"),
#'            lbs = T)
#'}

get_effort_landings <- function(fleetData,ports,combine,speciesCodes,lbs=T) {

  lbstotons <- 2204.62
  message("Are landings in lbs or metric tons?")

  message("Calculating trip effort for main ports")
  # distinct trips
  tripEff <- fleetData |>
    tidyr::separate(col=Area,into = c("text","Box"),sep = "ID_") |>
    dplyr::mutate(Box = as.numeric(Box),
                  Year = as.integer(Year)) |>
    dplyr::filter(PORTID %in% ports) |>
    dplyr::select(Year,Box,GEARCAT,newport,STATEABB,PORTID,TRIPID,InsideDAS) |>
    dplyr::distinct()

  # true effort summed over trips and not species/trips
  das <- tripEff |>
    dplyr::group_by(Year,Box,GEARCAT,newport,STATEABB,PORTID) |>
    dplyr::summarise(effort = sum(InsideDAS),
                     .groups="drop")

  message("calculating landings for main ports")
  # landings
  fleets <- fleetData |>
    tidyr::separate(col=Area,into = c("text","Box"),sep = "ID_") |>
    dplyr::mutate(Box = as.numeric(Box)) |>
    dplyr::filter(PORTID %in% ports,
                  Code %in% speciesCodes) |>
    dplyr::group_by(Year,Box,GEARCAT,newport,STATEABB,PORTID,Code) |>
    dplyr::summarise(landings = sum(InsideLANDED),
                     .groups = "drop")

  message("calculating effort for other ports")
  ## repeat for other fleet effort
  otherFleetTrip <- fleetData |>
    tidyr::separate(col=Area,into = c("text","Box"),sep = "ID_") |>
    dplyr::mutate(Box = as.numeric(Box)) |>
    dplyr::filter(!(PORTID %in% ports)) |>
    dplyr::select(Year,Box,GEARCAT,newport,STATEABB,PORTID,TRIPID,InsideDAS) |>
    dplyr::distinct()

  otherdas <- otherFleetTrip |>
    dplyr::group_by(Year,Box,GEARCAT) |>
    dplyr::summarise(effort = sum(InsideDAS),
                     .groups="drop") |>
    dplyr::mutate(PID = NA,
                  newport = "OTHER",
                  STATEABB = "NA") |>
    dplyr::relocate(Year,Box,GEARCAT,PID,effort,newport,STATEABB)

  message("calculating landings for other ports")
  ## other fleet landings
  otherFleetLandings <- fleetData |>
    tidyr::separate(col=Area,into = c("text","Box"),sep = "ID_") |>
    dplyr::mutate(Box = as.numeric(Box)) |>
    dplyr::filter(!(PORTID %in% ports),
                  Code %in% speciesCodes) |>
    dplyr::group_by(Year,Box,GEARCAT,Code) |>
    dplyr::summarise(landings = sum(InsideLANDED),
                     .groups = "drop") |>
    dplyr::mutate(PID = NA,
                  newport = "OTHER",
                  STATEABB = "NA") |>
    dplyr::relocate(Year,Box,GEARCAT,PID,Code,landings,newport,STATEABB)


  portNames <- fleets |>
    dplyr::distinct(newport,STATEABB,PORTID)

  message("combining all data")
  # now combine some of the fleets (associated ports with main ports)
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
    dplyr::group_by(Year,Box,GEARCAT,PID,Code) |>
    dplyr::summarise(landings = sum(landings),
                     .groups = "drop") |>
    dplyr::left_join(portNames, by = c("PID"="PORTID"))

  landingsByBox <-  rbind(landings,otherFleetLandings)

  if (lbs) {
    # convert to mt
    landingsByBox <- landingsByBox |>
      dplyr::mutate(landings = landings/lbstotons)
  }


  #effort
  effort <- das |>
    dplyr::group_by(Year,Box,GEARCAT,PID) |>
    dplyr::summarise(effort = sum(effort),
                     .groups = "drop") |>
    dplyr::left_join(portNames, by = c("PID"="PORTID"))


  effortByBox <- rbind(effort,otherdas)

  message("Are landings in lbs or metric tons?")

  return(list(effort=effortByBox,landings=landingsByBox))

}


