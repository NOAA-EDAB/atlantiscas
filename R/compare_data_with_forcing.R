#' Compare landing from Geret to data used to calibrate atlantis
#'
#' @export


compare_data <- function(){

  boundaryBoxes <- c(0,23:29)
  lbstotons <- 2204.62

  #source(here::here("other/communitiesatsea/R/read_data.r"))

  allData <- read_data()
  data <- allData$data

  # read in atlantis forcing files


  # group Gerets data into atlantis groups
  fgs <- readr::read_csv(here::here("data/functionalGroupNames.csv"))

  data |>
    dplyr::select(SPPNM,NESPP3) |>
    dplyr::distinct() |>
    tidyr::separate(SPPNM,into = c("fi","la"),sep = ",", remove = F )


    dplyr::filter(grepl("DRUM",SPPNM))
}
