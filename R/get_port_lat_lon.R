#' Pull port lat and long address
#'
#' Uses tidygeocoder
#'
#' @param portData Data frame. Each row is Portname, County, State
#'
#' @return Data frame with lat and lon appended
#'
#' @export



get_port_lat_lon <- function(portData,city=NULL,county=NULL,state=NULL){

  osm <- tidygeocoder::geocode(portData,
                        city=city,
                        county=county,
                        state = state,
                        lat=lat,
                        long= lon,
                        return_input = F,
                        method = "osm")

  return(osm)

}
