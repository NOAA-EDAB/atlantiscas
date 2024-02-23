#' Create PortTables
#'
#' From VTR and CFDBS
#'
#' @param channel A connection object from dbutils::connect_to_database`
#' @param write. Boolean. Write output to file
#'

pull_port_tables <- function(channel,write = F) {
  # pull from vtr
  portTableVTR <- DBI::dbGetQuery(channel,"select * from VTR.VLPORTSYN")
  # this is a mess!
  if(write) {
    saveRDS(portTableVTR,here::here("data-raw/portTableVTR"))
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

  if(write) {
    saveRDS(portTableCFDBS,here::here("data-raw/portTableCFDBS.rds"))
  }

  return(list(portsCFDBS=portTableCFDBS,portsVTR=portTableVTR))
}
