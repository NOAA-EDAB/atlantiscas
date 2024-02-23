#' Read in processed fleet data
#'
#' Processing in process_data function
#'
#' @return list of commercial and rec data
#'
#' @export

#channel <- dbutils::connect_to_database("nefscdb1","user")

read_data <- function(){

  fishingData <- readRDS(here::here("data-raw/fishing/processedData.rds"))

  return(fishingData)

}
