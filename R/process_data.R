#' Process Gerets fleet data. CAMS only
#'
#' Read in fishing data, fix problems, add lat, lon to ports and other
#' data cleaning
#'
#' @return list of commercial and rec data
#'
#'
#' @export


process_data <- function(){

  message("Reading in CAMS data")
  ## Read in commercial fishing data
  # CAMS data
  data1 <- readRDS(here::here("data-raw/fishing","REVENUEFILE_NEUS_ATLANTIS_CAMS_1996_2021_portids.rds")) |>
    tibble::as_tibble() |>
    dplyr::rename(PORTID = CAMS_PORT,
                  STATE = STATE1,
                  PORT = PORTLND1)|>
    dplyr::select(-DOCID)

  ## clean fishing data
  # need to clean data for duplicate species names. eg. DRUM, BLACK and BLACK DRUM
  # SQUID (LOLIGO), SHRIMP (NK) etc
  # split names by comma and reorder
  message("Cleaning species names")
  commercialData <- data1  |>
    dplyr::mutate(SPPNM = stringr::str_remove(SPPNM,"\\(BIG\\)"))  |>
    dplyr::mutate(SPPNM = stringr::str_replace_all(SPPNM," \\(",", "))  |>
    dplyr::mutate(SPPNM = stringr::str_remove(SPPNM,"\\)"))  |>
    dplyr::mutate(SPPNM = stringr::str_replace_all(SPPNM,"LOBSTER","AMERICAN LOBSTER"))  |>
    dplyr::mutate(SPPNM = stringr::str_replace_all(SPPNM,"LOLIGO SQUID","LONGFIN SQUID"))  |>
    dplyr::mutate(SPPNM = stringr::str_replace_all(SPPNM,"ROSEFISH,BLK BELLIED","BLK BELLIED ROSEFISH"))  |>
    tidyr::separate(col = SPPNM,into = c("last","first"),sep=", ",remove=F)  |>
    dplyr::mutate(SP = dplyr::case_when(is.na(first) ~ SPPNM,
                                        TRUE ~ paste(first,last)))  |>
    dplyr::select(-first,-last,-SPPNM) |>
    dplyr::rename(SPPNM = SP)

  rm(data1)

  # convert to numeric and reorder NK species
  message("Renaming NK species")
  commercialData <- commercialData |>
    dplyr::mutate(Year = as.numeric(Year),
                  TRIPID = as.numeric(TRIPID)) |>
    tidyr::separate(col = SPPNM,into = c("first","last"),sep=" NK",remove=F) |>
    dplyr::mutate(SP = dplyr::case_when(is.na(last) ~ SPPNM,
                                        TRUE ~ paste("NK",first))) |>
    dplyr::select(-first,-last,-SPPNM) |>
    dplyr::rename(SPPNM = SP) |>
    dplyr::mutate(NESPP3 =as.numeric(NESPP3)) |>
    dplyr::mutate(Area = gsub(" ","_",Area))

  # fix data types for future joins
  commercialData <- commercialData |>
    dplyr::mutate(NESPP3 = as.double(NESPP3))

   #saveRDS(commercialData,here::here("data-raw/commercialdata.rds"))
  #commercialData <- readRDS(here::here("data-raw/commercialdata.rds"))
   #dat <- readRDS(here::here("data-raw/commercialdata.rds"))

  # Add Atlantis codes to NESPP3 codes
  commercialData <- atlantiscas::assign_species_codes(commercialData)
  # Deal with "catch all" groups
  commercialData <- atlantiscas::assign_unclassified_groups(commercialData)

  # assign lat and lon to ports
  cleanedData <- atlantiscas::assign_latlon_ports(commercialData,saveToFile=T)

  commercialData <- cleanedData$neus
  commercialDataOutside <- cleanedData$noneus

  # deal with missing codes (OTHER FISH?)
  # Ignore NS SQUID, NK DOGFISH, NK CLAM
  # SKATES need to be split up
  # NK SHARKS need to be split up


  ## read in rec landings
  rec <-  readRDS(here::here("data-raw/fishing","REVENUEFILE_Neus_Atlantis_rec_1996_2021_Sept2023.rds"))  |>
    tibble::as_tibble()  |>
    dplyr::rename(PORTID = PORT,
                  STATE = STATE1,
                  PORT = PORTLND1)  |>
    dplyr::mutate(Inside = NA,
                  DAY = NA,
                  QTYKEPT = NA,
                  REVENUE = NA,
                  InsideDAS=NA,
                  nominal_revenue=NA)  |>
    dplyr::relocate(names(data))

  ## clean rec landings



  processedData <- list(data = commercialData,
                        outside= commercialDataOutside,
                        rec = rec)

  saveRDS(processedData,here::here("data-raw/fishing/processedData.rds"))

  return(processedData)


}
