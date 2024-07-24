#' Filter/create data sets for use in vignettes
#'
#' Read in the fully processed data and filter by gear type
#'
#' @param fleetType character (scallop, groundfish)
#'
#' @return list of 3 data frames
#' \item{neus}{commercial data from states within NEUS footprint}
#' \item{outside}{commercial data from states outside NEUS footprint}
#' \item{rec}{recreational data from all states}
#'
#' @export

create_dataset <- function(fleetType) {

  cleanData <- readRDS(here::here("data-raw/fishing/processedData.rds"))

  fleetData <- list()
  # pick out fleets fo neus states, non neus states (for commercial data), and rec data
  if (tolower(fleetType) == "scallop") {
    fleetData$neus <- cleanData$data |>
      dplyr::filter(GEARCAT == "Scallop Gear")
    fleetData$outsideneus <- cleanData$outside |>
      dplyr::filter(GEARCAT == "Scallop Gear")
    fleetData$rec <- cleanData$rec |>
      dplyr::filter(GEARCAT == "Scallop Gear")


  } else if (tolower(fleetType) == "groundfish") {
    fleetData$neus <-  cleanData$data |>
      dplyr::filter(GEARCAT %in% c("Bottom Trawl","Sink Gillnet"),
                    squidTrip == F)
    fleetData$outsideneus <-  cleanData$outside |>
      dplyr::filter(GEARCAT %in% c("Bottom Trawl","Sink Gillnet"))
    fleetData$rec <-  cleanData$rec |>
      dplyr::filter(GEARCAT %in% c("Bottom Trawl","Sink Gillnet"))


  } else {
    stop("Please select an appropriate fleet type")
  }

  return(fleetData)

}

