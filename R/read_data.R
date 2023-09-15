#' Read in processed fleet data
#'
#' Read in fishing data, fix problems, add lat, lon to ports and other
#' data cleaning#'
#'
#' @return list of commercial and rec data
#'
#' @export

#channel <- dbutils::connect_to_database("sole","abeet")

read_data <- function(){

  fishingData <- readRDS(here::here("data-raw/fishing/processedData.rds"))

  return(fishingData)

}
