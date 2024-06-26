#' Filter/create data sets for use in vignettes
#'
#' Read in the fully processed data and filter by gear type
#'
#' @param fleetType character (scallop, groundfish)
#'
#' @return data frame
#'
#' @export

create_dataset <- function(fleetType) {

  cleanData <- readRDS(here::here("data-raw/fishing/processedData.rds"))
  # just select commercial fleets within the NEUS domain
  cleanData <- cleanData$data


  if (tolower(fleetType) == "scallop") {
    fleetData <- cleanData |>
      dplyr::filter(GEARCAT == "Scallop Gear")

  } else if (tolower(fleetType) == "groundfish") {
    fleetData <-  cleanData |>
      dplyr::filter(GEARCAT %in% c("Bottom Trawl","Sink Gillnet"))


  } else {
    stop("Please select an appropriate fleet type")
  }

  return(fleetData)

}

