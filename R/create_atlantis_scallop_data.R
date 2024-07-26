#' get processed scallop data for atlantis use
#'
#' Reads in scallop data and returns effort and landings by box for main ports
#'
#' @return data frames
#' @export

create_atlantis_scallop_data <- function() {

  message("Reading in vignette data")
  lbstotons <- 2204.62
  scallopData <- readRDS(here::here("data/scallopDataCAMS.rds"))

  # convert to metric tons
  sca <- scallopData$neus |>
    dplyr::mutate(Year = as.integer(Year),
                  InsideLANDED = InsideLANDED/lbstotons)

  mainPortsIDs = c(240403,330309,490910)
  associatedPorts = data.frame(main=c(240403,330309,330309,490910,490910),associated=c(242203,331627,330127,490869,490118))


  ports <- unique(c(mainPortsIDs,associatedPorts$associated))

  speciesCodes <- "SCA"

  effortByBox <- get_effort_landings(sca,ports,associatedPorts,speciesCodes)


  return(effortByBox)

}

