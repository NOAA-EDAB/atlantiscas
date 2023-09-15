#' Process Gerets fleet data
#'
#' Read in fishing data, fix problems, add lat, lon to ports and other
#' data cleaning#'
#'
#' @return list of commercial and rec data
#'
#' @export

#channel <- dbutils::connect_to_database("sole","abeet")

process_data <- function(){

  # read in portname, lat, lon data
  ports <- readxl::read_xlsx(here::here("data-raw/ports/Lat Lon Info (ME-FL) 091323.xlsx")) |>
    dplyr::mutate(MAPNAME = toupper(MAPNAME))
  ports <- readxl::read_xlsx(here::here("data-raw/ports/Lat Lon Info_Ports (ME-NC) 091323.xlsx")) |>
    dplyr::mutate(portsinc = stringr::str_replace_all(portsinc,"\\^","")) |>
    dplyr::select(-GEOTYPE,-areakey) |>
    dplyr::mutate(portsinc = toupper(portsinc))

  adddf <- NULL
  for (i in 1:nrow(ports)) {
    line <- ports[i,]
    port <- ports$placenm[i]
    other <- ports$portsinc[i]
    others <- unlist(strsplit(other,", "))
    for (j in 1:length(others)) {
      newline <- line
      newline$placenm <- others[j]
      adddf <- rbind(adddf,newline)
    }
  }

  #oracleports <- comlandr::get_ports(channel)$data


  data1 <- readRDS(here::here("data-raw/fishing","REVENUEFILE_1996_2007.rds")) |>
    tibble::as_tibble() |>
    dplyr::rename(STATE = STATE1,
                  PORT = PORTLND1)

  data2 <- readRDS(here::here("data-raw/fishing","REVENUEFILE_2008_2021.rds")) |>
    tibble::as_tibble() |>
    dplyr::mutate(NESPP3 = as.double(NESPP3)) |>
    dplyr::rename(STATE = STATE1,
                  PORT = PORTLND1) |>
    dplyr::select(-DOCID)

  # need to clean data for duplicate species names. eg. DRUM, BLACK and BLACK DRUM
  # SQUID (LOLIGO), SHRIMP (NK) etc
  # split names by comma and reorder
  fixdata1 <- data1 |>
    dplyr::mutate(SPPNM = stringr::str_remove(SPPNM,"\\(BIG\\)")) |>
    dplyr::mutate(SPPNM = stringr::str_replace_all(SPPNM," \\(",", ")) |>
    dplyr::mutate(SPPNM = stringr::str_remove(SPPNM,"\\)")) |>
    dplyr::mutate(SPPNM = stringr::str_replace_all(SPPNM,"LOBSTER","AMERICAN LOBSTER")) |>
    dplyr::mutate(SPPNM = stringr::str_replace_all(SPPNM,"LOLIGO SQUID","LONGFIN SQUID")) |>
    dplyr::mutate(SPPNM = stringr::str_replace_all(SPPNM,"ROSEFISH,BLK BELLIED","BLK BELLIED ROSEFISH")) |>
    tidyr::separate(col = SPPNM,into = c("last","first"),sep=", ",remove=F) |>
    dplyr::mutate(SP = dplyr::case_when(is.na(first) ~ SPPNM,
                                        TRUE ~ paste(first,last))) |>
    dplyr::select(-first,-last,-SPPNM) |>
    dplyr::rename(SPPNM = SP)


    # a <- fixdata1 |>
    #   dplyr::mutate(SPPNM = stringr::str_remove(SPPNM,"\\(BIG\\)")) |>
    #   dplyr::mutate(SPPNM = stringr::str_replace_all(SPPNM," \\(",", ")) |>
    #   dplyr::mutate(SPPNM = stringr::str_remove(SPPNM,"\\)")) |>
    #   dplyr::distinct(SPPNM,NESPP3)
    #
    # length(a$NESPP3)
    # length(unique(a$NESPP3))
  # join data
  data <- rbind(fixdata1,data2)

  # a2 <- data |>
  #   dplyr::select(PORTLANDED, PORT, STATE) |>
  #   dplyr::distinct() |>
  #   dplyr::left_join(adddf,by = c("PORT" = "placenm","STATE"="placest"))
  #
  # a <- data |>
  #   dplyr::select(PORTLANDED, PORT, STATE) |>
  #   dplyr::distinct() |>
  #   dplyr::left_join(oracleports,by = c("PORT" = "PORTNM","STATE"="STATEABB"))
  #
  # missports <- data |>
  #   dplyr::select(PORTLANDED, PORT, STATE) |>
  #   dplyr::distinct() |>
  #   dplyr::left_join(ports,by = c("PORT" = "MAPNAME","STATE"="STATEABBR"))
  #
  # missports |>
  #   dplyr::filter(is.na(PRIMARY_LATITUDE))


  rec <-  readRDS(here::here("data-raw/fishing","REVENUEFILE_Neus_Atlantis_rec_1996_2021.rds")) |>
    tibble::as_tibble() |>
    dplyr::rename(STATE = STATE1,
                  PORT = PORTLND1) |>
    dplyr::mutate(Inside = NA,
                  DAY = NA,
                  QTYKEPT = NA,
                  REVENUE = NA,
                  InsideDAS=NA,
                  nominal_revenue=NA) |>
    dplyr::relocate(names(data))

  processedData <- list(data = data, rec = rec)

  saveRDS(processedData,here::here("data-raw/fishing/processedData.rds"))

  return(processedData)


}
