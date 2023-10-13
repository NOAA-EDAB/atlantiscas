#' Create PortTables
#'
#' From VTR and CFDBS
#'
#' @param channel A connection object from dbutils::connect_to_data_base`
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
    dplyr::distinct()

  #replace OTHER PORT with name of PORT
  portTableCFDBS$PORT <- gsub("OTHER ","",portTableCFDBS$PORT)

  if(write) {
    saveRDS(portTableCFDBS,here::here("data-raw/portTableCFDBS"))
  }

  return(list(portsCFDBS=portTableCFDBS,portsVTR=portTableVTR))
}
