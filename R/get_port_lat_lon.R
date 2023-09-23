#' Pull port lat and long address
#'
#' Uses tidygeocoder
#'
#' @param portData Data frame. Each row is Portname, County, State
#'
#' @return Data frame with lat and lon appended
#'
#' @export



get_port_lat_lon <- function(portData){

  osm <- tidygeocoder::geocode(portData,
                        city=portData$PORT,
                        county=portData$PORT_COUNTY_NAME,
                        state = STATE,
                        lat=lat,
                        long= lon,
                        return_input = F,
                        method = "osm")

  census <- tidygeocoder::geocode(portData,
                          city=portData$PORT,
                          county=portData$PORT_COUNTY_NAME,
                          state = STATE,
                          lat=lat,
                          long= lon,
                          return_input = F,
                          method = "census")

  arcgis <- tidygeocoder::geocode(portData,
                              city=portData$PORT,
                              county=portData$PORT_COUNTY_NAME,
                              state = STATE,
                              lat=lat,
                              long= lon,
                              return_input = F,
                              method = "arcgis")

  return(list(osm=osm,census=sensus,arcgis=arcgis))

}
