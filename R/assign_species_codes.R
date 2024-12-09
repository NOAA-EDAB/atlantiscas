#' assign species codes to atlantis groups
#'
#' Reads species from dev branch of neus-atlantis
#'
#' @family processData
#'
#'@export

assign_species_codes <- function(commercialData){

  message("Assigning species codes")
  #atlantisGroups <- readr::read_csv("https://raw.githubusercontent.com/NEFSC/READ-EDAB-neusAtlantis/andy_speciesupdate/data-raw/data/Atlantis_2_0_groups_svspp_nespp3.csv")
  atlantisGroups <- readr::read_csv("https://raw.githubusercontent.com/NEFSC/READ-EDAB-neusAtlantis/dev_branch/data-raw/data/Atlantis_2_0_groups_svspp_nespp3.csv")

  # finds duplicates
  message("Species with multiple NESPP3 codes")
  atlantisGroups |>
    dplyr::group_by(SVSPP) |>
    dplyr::filter(!is.na(SVSPP)) |>
    dplyr::filter(dplyr::n()>1)

  message("Species with multiple SVSSP codes")
  atlantisGroups |>
    dplyr::group_by(NESPP3) |>
    dplyr::filter(!is.na(NESPP3)) |>
    dplyr::filter(dplyr::n()>1)

  message("Species with multiple species names")
  atlantisGroups |>
    dplyr::group_by(Species) |>
    dplyr::filter(dplyr::n()>1)

  ss <- atlantisGroups |>
    dplyr::group_by(NESPP3) |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(NESPP3)) |>
    dplyr::filter(dplyr::n()>1)
  if (nrow(ss) > 0) {
    message("Species with the same NESPP3 code")
    ss |> print(n=30)
  }



  # check for NAs of NESPP3 codes in data
  if (nrow(commercialData |>
           dplyr::filter(is.na(NESPP3)))>0){
    stop("There will be a join problem due to NAs")
  }

  fullData <- dplyr::left_join(commercialData,atlantisGroups ,by ="NESPP3") |>
    dplyr::select(-SVSPP)

  return(fullData)





}
