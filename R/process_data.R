#' Process Gerets fleet data
#'
#' Read in fishing data, fix problems, add lat, lon to ports and other
#' data cleaning#'
#'
#' @return list of commercial and rec data
#'
#'
#' @export

#channel <- dbutils::connect_to_database("sole","abeet")

process_data <- function(){

  ## Read in commercial fishing data
  # VTR data
  data1 <- readRDS(here::here("data-raw/fishing","REVENUEFILE_Neus_Atlantis_com_1996_2007_Sept2023.rds")) |>
    tibble::as_tibble() |>
    dplyr::rename(PORTID = PORT,
                  STATE = STATE1,
                  PORT = PORTLND1)

  # DMIS DATA
  data2 <- readRDS(here::here("data-raw/fishing","REVENUEFILE_Neus_Atlantis_com_2008_2021_Sept2023.rds")) |>
    tibble::as_tibble()  |>
    dplyr::mutate(NESPP3 = as.double(NESPP3))  |>
    dplyr::rename(PORTID = PORT,
                  STATE = STATE1,
                  PORT = PORTLND1)  |>
    dplyr::select(-DOCID)

  ## clean fishing data
  # Convert VTR data to match DMIS
  # need to clean data for duplicate species names. eg. DRUM, BLACK and BLACK DRUM
  # SQUID (LOLIGO), SHRIMP (NK) etc
  # split names by comma and reorder
  fixdata1 <- data1  |>
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


  # join data
  # change data type
  # reorder NK species from CLAM NK to NK CLAM
  commercialData <- rbind(fixdata1,data2) |>
    dplyr::mutate(Year = as.numeric(Year),
                  TRIPID = as.numeric(TRIPID)) |>
    tidyr::separate(col = SPPNM,into = c("first","last"),sep=" NK",remove=F) |>
    dplyr::mutate(SP = dplyr::case_when(is.na(last) ~ SPPNM,
                                        TRUE ~ paste("NK",first))) |>
    dplyr::select(-first,-last,-SPPNM) |>
    dplyr::rename(SPPNM = SP)

  ## clean up species
  # read in neus groups # pull from neus-atlantis repo

  atlantisGroups <- readr::read_csv("https://raw.githubusercontent.com/NEFSC/READ-EDAB-neusAtlantis/dev_branch/data-raw/data/Atlantis_1_5_groups_svspp_nespp3.csv")

  if (nrow(commercialData |>
    dplyr::filter(is.na(NESPP3)))>0){
    stop("There will be a join problem due to NAs")
  }
  a <- dplyr::left_join(commercialData,atlantisGroups ,by ="NESPP3") |>
    dplyr::select(-SVSPP)


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



  processedData <- list(data = commercialData, rec = recData)

  saveRDS(processedData,here::here("data-raw/fishing/processedData.rds"))

  return(processedData)


}
