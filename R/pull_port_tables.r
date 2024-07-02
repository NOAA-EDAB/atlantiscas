#' Create PortTables
#'
#' From VTR and CFDBS
#'
#' @param channel A connection object from dbutils::connect_to_database`
#' @param write Boolean. Write output to file
#'
#'@export

pull_port_tables <- function(channel,saveToFile = F) {
  # pull from vtr
  portTableVTR <- DBI::dbGetQuery(channel,"select * from VTR.VLPORTSYN")
  # this is a mess!
  if(saveToFile) {
    saveRDS(portTableVTR,here::here("data-raw/portTableVTR.rds"))
  }


  # pull port table from cams and clean names


  portTableCAMS <- DBI::dbGetQuery(channel,"select * from CAMS_GARFO.CFG_PORT")
  portTableCAMS$PORT_NAME <- gsub("\\(\\(","(",portTableCAMS$PORT_NAME)

  portTableCAMS  <-   portTableCAMS |>
    dplyr::as_tibble() |>
    tidyr::separate(PORT_NAME,
                    into=c("NewPORT","extracol"),
                    sep = "\\(",
                    fill="right") |>
    dplyr::select(-extracol,-PSREGION,-DOC) |>
    dplyr::as_tibble() |>
    dplyr::rename(PORTID = PORT,
                  PORT= NewPORT,
                  STATE=STATE_ABB,
                  COUNTY = COUNTY_NAME) |>
    dplyr::mutate(PORT=trimws(PORT)) |>
    dplyr::distinct()

  # FIX SPELLING MISTAKES IN PORT TABLES
  portTableCAMS$PORT[portTableCAMS$PORT == "ORRS ISLAND" & portTableCAMS$STATE=="ME"] <- "ORR'S ISLAND"
  portTableCAMS$PORT[portTableCAMS$PORT == "LONG ISLAND CUMBERLAND" & portTableCAMS$STATE=="ME"] <- "LONG ISLAND"
  portTableCAMS$PORT[portTableCAMS$PORT == "STUEBEN" & portTableCAMS$STATE=="ME"] <- "STEUBEN"
  portTableCAMS$PORT[portTableCAMS$PORT == "SALISBURY COVE" & portTableCAMS$STATE=="ME"] <- "SALSBURY COVE"
  portTableCAMS$PORT[portTableCAMS$PORT == "WESTERLEY" & portTableCAMS$STATE=="RI"] <- "WESTERLY"
  portTableCAMS$PORT[portTableCAMS$PORT == "BARINGTON" & portTableCAMS$STATE=="RI"] <- "BARRINGTON"
  portTableCAMS$COUNTY[portTableCAMS$PORT == "PERKINS COVE" & portTableCAMS$STATE=="ME"] <- "YORK"
  portTableCAMS$PORT[portTableCAMS$PORT == "PERKINS COVE" & portTableCAMS$STATE=="ME"] <- "OGUNQUIT"
  portTableCAMS$COUNTY[portTableCAMS$PORT == "OGUNQUIT" & portTableCAMS$STATE=="ME"] <- "YORK"
  portTableCAMS$PORT[portTableCAMS$PORT == "DYERS BAY" & portTableCAMS$STATE=="ME"] <- "STEUBEN"
  portTableCAMS$PORT[portTableCAMS$PORT == "MATHAIS POINT" & portTableCAMS$STATE=="VA"] <- "DAHLGREN"
  portTableCAMS$PORT[portTableCAMS$PORT == "HAMPTON/SEABROOK" & portTableCAMS$STATE=="NH"] <- "SEABROOK"
  portTableCAMS$PORT[portTableCAMS$PORT == "HARRIMANS POINT" & portTableCAMS$STATE=="ME"] <- "BROOKLIN"
  portTableCAMS$COUNTY[portTableCAMS$PORT == "BROOKLIN" & portTableCAMS$STATE=="ME"] <- "HANCOCK"


  if(saveToFile) {
    saveRDS(portTableCAMS,here::here("data-raw/portTableCAMS.rds"))
  }


  # # pull from CFDBS
  portTableCFDBS <- DBI::dbGetQuery(channel,"select * from CFDBS.PORT")
  # clean table
  # remove all content in parentheses in port name

  portTableCFDBS$PORT <- gsub("\\(\\(","(",portTableCFDBS$PORT)

  portTableCFDBS <- portTableCFDBS %>%
    tidyr::separate(.,PORTNM,
                    into=c("NewPORT","extracol"),
                    sep = "\\(",
                    fill="right") %>%
    dplyr::select(-extracol,-PSREGION,-DOC) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(PORTID = PORT,
                  PORT= NewPORT,
                  STATE=STATEABB) %>%
    dplyr::mutate(PORT=trimws(PORT)) %>%
    dplyr::distinct()

  # FIX SPELLING MISTAKES IN PORT TABLES
  portTableCFDBS$PORT[portTableCFDBS$PORT == "ORRS ISLAND" & portTableCFDBS$STATE=="ME"] <- "ORR'S ISLAND"
  portTableCFDBS$PORT[portTableCFDBS$PORT == "LONG ISLAND CUMBERLAND" & portTableCFDBS$STATE=="ME"] <- "LONG ISLAND"
  portTableCFDBS$PORT[portTableCFDBS$PORT == "STUEBEN" & portTableCFDBS$STATE=="ME"] <- "STEUBEN"
  portTableCFDBS$PORT[portTableCFDBS$PORT == "SALISBURY COVE" & portTableCFDBS$STATE=="ME"] <- "SALSBURY COVE"
  portTableCFDBS$PORT[portTableCFDBS$PORT == "WESTERLEY" & portTableCFDBS$STATE=="RI"] <- "WESTERLY"
  portTableCFDBS$PORT[portTableCFDBS$PORT == "BARINGTON" & portTableCFDBS$STATE=="RI"] <- "BARRINGTON"
  portTableCFDBS$COUNTY[portTableCFDBS$PORT == "PERKINS COVE" & portTableCFDBS$STATE=="ME"] <- "YORK"
  portTableCFDBS$PORT[portTableCFDBS$PORT == "PERKINS COVE" & portTableCFDBS$STATE=="ME"] <- "OGUNQUIT"
  portTableCFDBS$COUNTY[portTableCFDBS$PORT == "OGUNQUIT" & portTableCFDBS$STATE=="ME"] <- "YORK"
  portTableCFDBS$PORT[portTableCFDBS$PORT == "DYERS BAY" & portTableCFDBS$STATE=="ME"] <- "STEUBEN"
  portTableCFDBS$PORT[portTableCFDBS$PORT == "MATHAIS POINT" & portTableCFDBS$STATE=="VA"] <- "DAHLGREN"
  portTableCFDBS$PORT[portTableCFDBS$PORT == "HAMPTON/SEABROOK" & portTableCFDBS$STATE=="NH"] <- "SEABROOK"
  portTableCFDBS$PORT[portTableCFDBS$PORT == "HARRIMANS POINT" & portTableCFDBS$STATE=="ME"] <- "BROOKLIN"
  portTableCFDBS$COUNTY[portTableCFDBS$PORT == "BROOKLIN" & portTableCFDBS$STATE=="ME"] <- "HANCOCK"



  #replace OTHER PORT with name of PORT
  portTableCFDBS$PORT <- gsub("OTHER ","",portTableCFDBS$PORT)

  if(saveToFile) {
    saveRDS(portTableCFDBS,here::here("data-raw/portTableCFDBS.rds"))
  }

  return(list(portsCFDBS=portTableCFDBS,portsVTR=portTableVTR,portsCAMS=portTableCAMS))
}
