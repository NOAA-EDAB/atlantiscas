#' Create PortTables
#'
#' From VTR and CFDBS
#'
#'
#'

pull_port_tables <- function() {
  # pull from vtr
  porttableVTR <- DBI::dbGetQuery(channel,"select * from VTR.VLPORTSYN")
  # pull from CFDBS
  porttableCFDBS <- DBI::dbGetQuery(channel,"select * from CFDBS.PORT")

  saveRDS(porttableVTR,here::here("data-raw/portTableVTR"))
  saveRDS(porttableCFDBS,here::here("data-raw/portTableCFDBS"))
}
