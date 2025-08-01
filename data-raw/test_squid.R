# 
channel <- dbutils::connect_to_database("NEFSC_USERS","abeet")

sql1 <- "select distinct CAMSID, SUBTRIP, VTR_IMGID, VTR_MESH, VES_LEN, VES_GTONS from CAMS_GARFO.CAMS_SUBTRIP"
subtrip <- DBI::dbGetQuery(channel,sql1)
sql2 <- "select distinct CAMSID, SUBTRIP, FISHERY_GROUP from CAMS_GARFO.CAMS_FISHERY_GROUP"
fisheryGroup <- DBI::dbGetQuery(channel,sql2)

subtrip2 <- subtrip |>
  dplyr::left_join(fisheryGroup,by=c("CAMSID","SUBTRIP")) |>
  dplyr::filter(!is.na(VTR_IMGID))

saveRDS(subtrip2,here::here("subtrip2.rds"))



subtrip2
ls()
rm(list=ls())
a <- atlantiscas::read_data()$data
fishingData <- readRDS(here::here("data-raw/fishing/processedData.rds"))
fishingData <- readRDS(here::here("data-raw/fishing/processedData.rds"))
fishingData <- fishingData$data
lan <- fishingdata |>
  dplyr::group_by(TRIPID,Code) |>
  dplyr::summarise(land= sum(InsideLANDED))
lan <- fishingData |>
  dplyr::group_by(TRIPID,Code) |>
  dplyr::summarise(land= sum(InsideLANDED))
lan <- fishingData |>
  dplyr::group_by(TRIPID,Code) |>
  dplyr::summarise(land= sum(InsideLANDED),
                   .groups="drop")
tripids <- lan |>
  dplyr::filter(Code %in% c("ISQ","LSQ")) |>
  dplyr::distinct(TRIPID) |>
  dplyr::pull(TRIPID)
gc()
squidTrips <- lan |>
  dplyr::filter(TRIPID %in% tripids)
squidTrips |>
  dplyr::group_by(TRIPID) |>
  dplyr::mutate(tot = sum(land)) |>
  dplyr::mutate(squid = dplyr::case_when(Code %in% c("ISQ","LSQ") ~ sum(land),
                                         .default = 0))
squidLand <- squidTrips |>
  dplyr::group_by(TRIPID) |>
  dplyr::mutate(tot = sum(land)) |>
  dplyr::mutate(squid = dplyr::case_when(Code %in% c("ISQ","LSQ") ~ land,
                                         .default = NA))
squidLand |>
  dplyr::filter(TRIPID == 149580)
squidLand <- squidTrips |>
  dplyr::group_by(TRIPID) |>
  dplyr::mutate(tot = sum(land)) |>
  dplyr::mutate(squid = dplyr::case_when(Code %in% c("ISQ","LSQ") ~ land,
                                         .default = NA)) |>
  dplyr::filter(!is.na(squid))  |>
  dplyr::mutate(pcent=squid/tot)
squidLand
hist(squidLand$pcent)
squidLand <- squidTrips |>
  dplyr::group_by(TRIPID) |>
  dplyr::mutate(tot = sum(land)) |>
  dplyr::mutate(squid = dplyr::case_when(Code %in% c("ISQ","LSQ") ~ land,
                                         .default = NA)) |>
  dplyr::filter(!is.na(squid))
squidLand
lan |> dplyr::filter(TRIPID == 155346)
squidLand |>
  dplyr::group_by(TRIPID) |>
  dplyr::summarise(tot = sum(tot),squid = squid) |>
  dplyr::mutate(pcent=squid/tot)
squidLand <- squidTrips |>
  dplyr::mutate(tot = sum(land)) |>
  dplyr::mutate(squid = dplyr::case_when(Code %in% c("ISQ","LSQ") ~ land,
                                         .default = NA)) |>
  dplyr::filter(!is.na(squid))
squidLand
squidLand |>
  dplyr::group_by(TRIPID) |>
  dplyr::summarise(tot = sum(tot),
                   squid = squid,
                   .groups = "drop") |>
  dplyr::mutate(pcent=squid/tot)
squidTrips
squidLand <- squidTrips |>
  dplyr::group_by(TRIPID) |>
  dplyr::summarise(tot = sum(land),
                   .groups = "drop") |>
  dplyr::mutate(squid = dplyr::case_when(Code %in% c("ISQ","LSQ") ~ land,
                                         .default = NA)) |>
  dplyr::filter(!is.na(squid))
squidLand <- squidTrips |>
  dplyr::mutate(squid = dplyr::case_when(Code %in% c("ISQ","LSQ") ~ land,
                                         .default = NA)) |>
  dplyr::group_by(TRIPID) |>
  dplyr::summarise(tot = sum(land),
                   .groups = "drop") |>
  dplyr::filter(!is.na(squid))
squidLand <- squidTrips |>
  dplyr::mutate(squid = dplyr::case_when(Code %in% c("ISQ","LSQ") ~ land,
                                         .default = NA)) |>
  dplyr::group_by(TRIPID) |>
  dplyr::summarise(tot = sum(land),
                   squid = squid,
                   .groups = "drop") |>
  dplyr::filter(!is.na(squid))
squidLand <- squidTrips |>
  dplyr::group_by(TRIPID,Code) |>
  dplyr::summarise(tot = sum(land),
                   .groups = "drop")
squidLand
squidTrips
squidTrip
squidTrips
squidLand <- squidTrips |>
  dplyr::mutate(squid = dplyr::case_when(Code %in% c("ISQ","LSQ") ~ land,
                                         .default = NA)) |>
  dplyr::group_by(TRIPID,Code) |>
  dplyr::summarise(tot = sum(land),
                   .groups = "drop") |>
  dplyr::mutate(squid = dplyr::case_when(Code %in% c("ISQ","LSQ") ~ land,
                                         .default = NA)) |>
  dplyr::filter(!is.na(squid))
squidLand <- squidTrips |>
  dplyr::mutate(squid = dplyr::case_when(Code %in% c("ISQ","LSQ") ~ land,
                                         .default = NA))
squidLand
squidTot <- squidTrips |>
  dplyr::group_by(TRIPID) |>
  dplyr::summarise(tot = sum(land),
                   .groups = "drop")
squidTo
squidTot
squidLand |>
  dplyr::left_join(squidTot,, by = "TRIPID")
squidLand |>
  dplyr::left_join(squidTot,, by = "TRIPID") |>
  dplyr::filter(TRIPID == 155346)
squidLand
landings <- squidTrips |>
  dplyr::mutate(squid = dplyr::case_when(Code %in% c("ISQ","LSQ") ~ land,
                                         .default = NA))
landings
squidTot <- landings |>
  dplyr::group_by(TRIPID) |>
  dplyr::summarise(tot = sum(land),
                   .groups = "drop")
squidTot
squidLand <- landings |>
  dplyr::filter(Code %in% c("LSQ","ISQ"))
squidLand
squidLand <- landings |>
  dplyr::filter(Code %in% c("LSQ","ISQ")) |>
  dplyr::group_by(TRIPID) |>
  dplyr::summarise(squid= sum(squid),
                   .groups = "drop")
squidLand
squidTot
squidTot <- landings |>
  dplyr::group_by(TRIPID) |>
  dplyr::summarise(tot = sum(land),
                   .groups = "drop")
squidLand |>
  dplyr::left_join(squidTot, by = "TRIPID") |>
  dplyr::filter(TRIPID == 155346)
squidLand |>
  dplyr::left_join(squidTot, by = "TRIPID")
squidLand |>
  dplyr::left_join(squidTot, by = "TRIPID") |>
  dplyr::mutate(pcent = squid/tot)
squidLand |>
  dplyr::left_join(squidTot, by = "TRIPID") |>
  dplyr::mutate(pcent = squid/tot) |>
  hist(pcent)
squidLand |>
  dplyr::left_join(squidTot, by = "TRIPID") |>
  dplyr::mutate(pcent = squid/tot) |>
  dplyr::pull(pcent) |>
  hist(pcent)
squidLand |>
  dplyr::left_join(squidTot, by = "TRIPID") |>
  dplyr::mutate(pcent = squid/tot) |>
  dplyr::pull(pcent)
squidLand |>
  dplyr::left_join(squidTot, by = "TRIPID") |>
  dplyr::mutate(pcent = squid/tot) |>
  dplyr::pull(pcent) |>
  hist()
squidLand |>
  dplyr::left_join(squidTot, by = "TRIPID") |>
  dplyr::mutate(pcent = squid/tot) |>
  dplyr::pull(pcent) |>
  hist(title="dfd")
squidTripLand <- squidLand |>
  dplyr::left_join(squidTot, by = "TRIPID") |>
  dplyr::mutate(pcent = squid/tot) |>
  dplyr::pull(pcent)
hist(squidTripLand, title="dfd")
?hist
hist(squidTripLand, main="Proportion of landings that are squid",
     xlab = "proportion",ylab = "frequency")