#' Plot NEUS footprint along with NAFO areas
#'
#' @export
#'
#'


plot_nafo <- function() {

  atlantis <- NEFSCspatial::Neus_atlantis |>
    dplyr::select(BOX_ID,X,Y,geometry)

  nafo <- NEFSCspatial::NAFO_Divisions_2021_poly_clipped |>
    dplyr::select(Division,X,Y,geometry) |>
    sf::st_transform(sf::st_crs(atlantis))

  ggplot2::ggplot(data=atlantis)+
    ggplot2::geom_sf() +
    ggplot2::geom_sf(data=nafo,alpha = 0.5)+
    ggplot2::coord_sf(ylim = c(34, 50),xlim = c(-80,-60)) +
    ggplot2::geom_sf_text(data=nafo,ggplot2::aes(label = Division))

}
