#' Plot ports on maps with coastline
#'
#' Landings in pounds
#'
#'
#'

library(magrittr)
#plot_ports <- function() {
  # Read in landings by port
  neusData <- readRDS(here::here("data-raw/REVENUE_cleanports.rds"))
  # tempfix for rhode island
  indri <- neusData$PORTID == 420999
  neusData$lat[indri] <- 41.55246
  neusData$lon[indri] <- -71.27038

  # aggregate total landings by port
  dd <- neusData %>%
    dplyr::group_by(PORTID,PORTNM,STATEABB,lat,lon) %>%
    dplyr::summarise(totalLandings = sum(InsideLANDED),
                     .groups = "drop")


  # plot total landings by location on map. use circle area to determine scale
  # calculate radius to ensure circle sizes are comparable.


  shoreline <- sf::st_read(here::here("data-raw/gis/EASTCoastShoreline.shp"))
  box <- sf::st_bbox(shoreline)
  crs <-  sf::st_crs(shoreline)
  states <- sf::st_read(here::here("data-raw/gis/tl_2012_us_state.shp")) %>%
    sf::st_transform(.,crs=crs) %>%
    sf::st_crop(.,y=box)
  neus <- NEFSCspatial::Neus_atlantis

  h <- list()
  # plot by state
  for (astate in unique(dd$STATEABB)) {
    print(astate)
    state <- states %>% dplyr::filter(STUSPS == astate)
    dddata <- dd %>% dplyr::filter(STATEABB == astate)

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data=shoreline,color = ggplot2::alpha("grey",.8)) +
      ggplot2::geom_sf(data = neus,color=ggplot2::alpha("grey",0.4)) +
      ggplot2::geom_sf(data = state) +
      ggplot2::geom_point(data = dddata,
                          mapping=ggplot2::aes(x=lon,y=lat,size=totalLandings),
                          color="blue") +
      ggplot2::ggtitle(paste0("Landings in ",astate," ports"))

      ggplot2::ggsave(here::here("data-raw",paste0(astate,".png")))

  }
