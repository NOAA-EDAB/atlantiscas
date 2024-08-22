#' get processed groundfish data for atlantis use
#'
#' Reads in groundfish data and returns effort and landings by box for main ports.
#' input data obtained from create_vignete_dataset
#'
#' @return data frames
#'
#' @family modelData
#'
#' @export

create_atlantis_groundfish_data <- function() {

  message("Reading in vignette data")
  lbstotons <- 2204.62
  gfData <- readRDS(here::here("data/NEGroundfishDataCAMS.rds"))

  # convert to metric tons
  gf <- gfData$neus |>
    dplyr::mutate(Year = as.integer(Year)) |>
    dplyr::mutate(InsideLANDED = InsideLANDED/lbstotons)

  mainPortsIDs = c(240403,240207,220101,240115,420209,240301,320201,350635)
  associatedPorts = data.frame(main=c(320201,240115,240301),associated=c(320901,240813,240601))

  ports <- unique(c(mainPortsIDs,associatedPorts$associated))

  speciesCodes <- c("COD","HAD","YTF","POL","PLA","WTF","WHK","WIF","RED","HAL","WPF","OPT","WOL")

  effortByBox <- get_effort_landings(gf,ports,associatedPorts,speciesCodes)


  return(effortByBox)

}

