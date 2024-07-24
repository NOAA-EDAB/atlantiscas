#' Define trips targeting squid
#'
#' Define trips as squid trips based on the percentage of squid caught in each trip
#' A new column is created containing a boolean
#'
#' @param allData Data frame
#' @param p Numeric. Value used to define the proportion of squid (by landings) in a trip to identify as a squid trip
#'
#'
#'@export

assign_squid_trips <- function(allData, p = 0.3){

  message("Assigning trips to squid trips")

  # identify trips with certain %ae of squid catch
  # sum landings by trip, gear, atlantis code
  message("Gear types filtered out first: Other Pot, Lobster Pot, Clam Dredge, Shrimp Trawl, Scallop Gear")
  lan <- allData |>
    dplyr::filter(!(GEARCAT %in% c("Other Pot","Lobster Pot", "Clam Dredge", "Shrimp Trawl","Scallop Gear"))) |>
    dplyr::group_by(TRIPID,GEARCAT,Code) |>
    dplyr::summarise(land= sum(InsideLANDED),
                     .groups="drop")

  # identify all trips that catch any squid
  tripids <- lan |>
    dplyr::filter(Code %in% c("ISQ","LSQ")) |>
    dplyr::distinct(TRIPID) |>
    dplyr::pull(TRIPID)

  # filter all trips catching squid
  squidTrips <- lan |>
    dplyr::filter(TRIPID %in% tripids)

  landings <- squidTrips |>
    dplyr::mutate(squid = dplyr::case_when(Code %in% c("ISQ","LSQ") ~ land,
                                           .default = NA))
  squidTot <- landings |>
    dplyr::group_by(TRIPID,GEARCAT,) |>
    dplyr::summarise(tot = sum(land),
                     .groups = "drop")

  squidLand <- landings |>
    dplyr::filter(Code %in% c("LSQ","ISQ")) |>
    dplyr::group_by(TRIPID,GEARCAT) |>
    dplyr::summarise(squid= sum(squid),
                     .groups = "drop")

  # all trips with landings of squid, total trip landings and % of landings that are squid
  squidTripLand <- squidLand |>
    dplyr::left_join(squidTot, by = c("TRIPID","GEARCAT")) |>
    dplyr::mutate(pcent = squid/tot)

  # trip ids that we will consider as squid trips.
  # Any trip that has >= 30% squid
  ids <- squidTripLand |>
    dplyr::filter(pcent >= p) |>
    dplyr::pull(TRIPID)

  a <- allData |>
    dplyr::mutate(squidTrip = dplyr::case_when(TRIPID %in% ids ~ TRUE,
                                               .default = FALSE))


  return(a)
}
