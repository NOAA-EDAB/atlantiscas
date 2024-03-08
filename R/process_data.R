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

  rm(data1)
  # join data
  # change data type
  # reorder NK species from CLAM NK to NK CLAM
  commercialData <- rbind(fixdata1,data2)
  rm(data2)
  rm(fixdata1)

  commericalData <- commercialData |>
    dplyr::mutate(Year = as.numeric(Year),
                  TRIPID = as.numeric(TRIPID)) |>
    tidyr::separate(col = SPPNM,into = c("first","last"),sep=" NK",remove=F) |>
    dplyr::mutate(SP = dplyr::case_when(is.na(last) ~ SPPNM,
                                        TRUE ~ paste("NK",first))) |>
    dplyr::select(-first,-last,-SPPNM) |>
    dplyr::rename(SPPNM = SP)

  #saveRDS(commercialData,here::here("data-raw/commercialdata.rds"))
  commercialData <- readRDS(here::here("data-raw/commercialdata.rds"))

  commercialData <- assign_species_codes(commercialData)
  ## clean up species
  # read in neus groups. pull from neus-atlantis repo

  #atlantisGroups <- readr::read_csv("https://raw.githubusercontent.com/NEFSC/READ-EDAB-neusAtlantis/dev_branch/data-raw/data/Atlantis_2_0_groups_svspp_nespp3.csv")

  ## save intermediate data for test scallops
  scallopData <- fullData |>
    dplyr::filter(Code == "SCA")
  saveRDS(scallopData,here::here("data/scallopData.rds"))



  # current data not assigned to an atlantis group
  dataToSplit <-  fullData |> dplyr::filter(is.na(Code))
  # plot of this data
  dataToSplit |>
    dplyr::group_by(Year,NESPP3,SPPNM) |>
    dplyr::summarise(lbs = sum(InsideLANDED),
                     .groups = "drop") |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=as.numeric(Year),y=lbs)) +
    ggplot2::facet_wrap(~as.factor(SPPNM),scales = "free_y")

  # look a skates
  fullData |>
    dplyr::filter(grepl("SKATE",SPPNM)) |>
    dplyr::group_by(Year,NESPP3,SPPNM) |>
    dplyr::summarise(lbs = sum(InsideLANDED),
                     .groups = "drop") |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=as.numeric(Year),y = lbs)) +
    ggplot2::facet_wrap(~SPPNM)

  # look a squid
  fullData |>
    dplyr::filter(grepl("SQUID",SPPNM)) |>
    dplyr::group_by(Year,NESPP3,SPPNM) |>
    dplyr::summarise(lbs = sum(InsideLANDED),
                     .groups = "drop") |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=as.numeric(Year),y = lbs)) +
    ggplot2::facet_wrap(~SPPNM)



  # deal with missing codes
  #


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
