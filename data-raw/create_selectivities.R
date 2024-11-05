#' Find all groundfish species length data caught by groundfish fleets.
#'
#' Pick trips identified as groundfish to find species lengths.
#' Use to estimate selectivity
#'
#' This should be pretty similar to the contnent found in groundfishSelectivity.rmd


# pull groundfish data
lbstotons <- 2204.62
boundaryBoxes <- c(0,23:29)
gfData <- readRDS(here::here("data/NEGroundfishDataCAMS.rds"))
gf <- gfData$neus |>
  dplyr::mutate(Year = as.integer(Year)) |>
  dplyr::mutate(InsideLANDED = InsideLANDED/lbstotons)

if(0) {
  # attempts to join tables with GARFO
  channel <- dbutils::connect_to_database("NEFSC_USERS","abeet")

  sql1 <- "select distinct CAMSID, VTR_IMGID
  from CAMS_GARFO.CAMS_SUBTRIP
  where YEAR >= 1996
  and VTR_IMGID IS NOT NULL"
  subtrip <- DBI::dbGetQuery(channel,sql1)

  # this table is a list of camsid and original vtr ids
  saveRDS(subtrip,here::here("data-raw/subtrip.rds"))
}

# distinct camsid, vtr_imgid pairs
subtrip <- readRDS(here::here("data-raw/subtrip.rds")) |>
  dplyr::as_tibble() |>
  dplyr::mutate(VTR_IMGID = as.character(VTR_IMGID))

# select trips
# IDNUM in our data = VTR_IMGID
trips <- gf |>
  dplyr::distinct(IDNUM) |>
  dplyr::pull()

# trips in groundfish data
pairedtrips <-  subtrip |>
  dplyr::filter(VTR_IMGID %in% trips)

gftripgear <- gf |>
  dplyr::select(IDNUM,GEARCAT) |>
  dplyr::distinct()

idsbyGear <- gftripgear |>
  dplyr::left_join(pairedtrips,by=c("IDNUM"="VTR_IMGID")) |>
  dplyr::filter(!is.na(CAMSID))

camstrips <- idsbyGear |>
  dplyr::pull(CAMSID)

# now pull lengths
if(0) {
  sqlLengths <- "select CAMSID, NESPP3, LENGTH, NUMLEN
  from CAMS_GARFO.cams_cflen_aa
  where YEAR >= 1996"

  lengths <- DBI::dbGetQuery(channel,sqlLengths) |>
    dplyr::as_tibble()

  saveRDS(lengths,here::here("data-raw/gflengths.rds"))
}

lengths <- readRDS(here::here("data-raw/gflengths.rds"))

# filter trips based on GF trips
gflengths <- lengths |>
  dplyr::filter(CAMSID %in% camstrips) |>
  dplyr::mutate(NESPP3 = as.double(NESPP3))

gfcodes <- c("RED", "WHK", "COD" ,"HAD" ,"POL", "WTF", "WIF", "HAL", "PLA", "YTF" ,"WPF" ,"WOL", "OPT")

atlantisGroups <- readr::read_csv("https://raw.githubusercontent.com/NEFSC/READ-EDAB-neusAtlantis/dev_branch/data-raw/data/Atlantis_2_0_groups_svspp_nespp3.csv")
gfgroups <- atlantisGroups |>
  dplyr::filter(Code %in% gfcodes)


allgflengths <- gflengths |>
  dplyr::left_join(gfgroups,by = "NESPP3") |>
  dplyr::filter(Code %in% gfcodes) |>
  dplyr::filter(LENGTH < 200)


replicatelengths <- allgflengths |>
  tidyr::uncount(NUMLEN)

saveRDS(replicatelengths,here::here("data/gfTripLengths.rds"))

# over all fleets.
# to do this by fleet we 'd need to filter out tripid's
# plot histogram of lengths
replicatelengths |>
  ggplot2::ggplot() +
  ggplot2::geom_histogram(ggplot2::aes(x=LENGTH),bins=25) +
  ggplot2::facet_wrap(~Code,scale = "free_x") +
  ggplot2::ylab("Number of fish") +
  ggplot2::xlab("length (cm)")

# plot prob curve by species
replicatelengths |>
  dplyr::select(Code,LENGTH) |>
  dplyr::arrange(Code,LENGTH) |>
  dplyr::group_by(Code) |>
  dplyr::mutate(prob = (1:dplyr::n())/dplyr::n()) |>
  ggplot2::ggplot(ggplot2::aes(x=LENGTH,y=prob)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 0.5) +
  ggplot2::facet_wrap(~Code,scale = "free_x") +
  ggplot2::ylab("Probability") +
  ggplot2::xlab("length (cm)")

# plot all on same figure
p <- replicatelengths |>
  dplyr::select(Code,LENGTH) |>
  dplyr::arrange(Code,LENGTH) |>
  dplyr::group_by(Code) |>
  dplyr::mutate(prob = (1:dplyr::n())/dplyr::n()) |>
  ggplot2::ggplot(ggplot2::aes(x=LENGTH,y=prob, col=Code)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 0.5) +
  ggplot2::ylab("Probability") +
  ggplot2::xlab("length (cm)")

plotly::ggplotly(p)

## combine all lengths for aglobal selectivity
alllengths <- replicatelengths |>
  dplyr::select(Code,LENGTH) |>
  dplyr::mutate(Code = "ALL") |>
  dplyr::arrange(Code,LENGTH) |>
  dplyr::group_by(Code) |>
  dplyr::mutate(prob = (1:dplyr::n())/dplyr::n())

alllengths |>
  dplyr::filter(prob>=0.5)


p  <-  alllengths
  ggplot2::ggplot(ggplot2::aes(x=LENGTH,y=prob)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 0.5) +
  ggplot2::ylab("Probability") +
  ggplot2::xlab("length (cm)")

plotly::ggplotly(p)

## separate by gear type and repeat plots to
# determine if there is a difference in selectivity based on
# fishing gear
gill <- idsbyGear |>
  dplyr::filter(GEARCAT == "Sink Gillnet") |>
  dplyr::distinct(GEARCAT,CAMSID)
trawl <- idsbyGear |>
  dplyr::filter(GEARCAT == "Bottom Trawl") |>
  dplyr::distinct(GEARCAT,CAMSID)

# NAs introduced from CAMS trips associated with other GEARCAT type
gilllengths <- allgflengths |>
  dplyr::left_join(gill,by = "CAMSID") |>
  tidyr::uncount(NUMLEN) |>
  dplyr::filter(!is.na(GEARCAT))
trawllengths <- allgflengths |>
  dplyr::left_join(trawl,by = "CAMSID") |>
  tidyr::uncount(NUMLEN) |>
  dplyr::filter(!is.na(GEARCAT))


combinedlengths <- rbind(gilllengths,trawllengths) |>
  dplyr::filter(!is.na(GEARCAT))


# plot selectivities by gear
trawllengths |>
  dplyr::select(Code,LENGTH) |>
  dplyr::arrange(Code,LENGTH) |>
  dplyr::group_by(Code) |>
  dplyr::mutate(prob = (1:dplyr::n())/dplyr::n()) |>
  ggplot2::ggplot(ggplot2::aes(x=LENGTH,y=prob)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 0.5) +
  ggplot2::facet_wrap(~Code,scale = "free_x") +
  ggplot2::ylab("Probability") +
  ggplot2::xlab("length (cm)")

gilllengths |>
  dplyr::select(Code,LENGTH) |>
  dplyr::arrange(Code,LENGTH) |>
  dplyr::group_by(Code) |>
  dplyr::mutate(prob = (1:dplyr::n())/dplyr::n()) |>
  ggplot2::ggplot(ggplot2::aes(x=LENGTH,y=prob)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 0.5) +
  ggplot2::facet_wrap(~Code,scale = "free_x") +
  ggplot2::ylab("Probability") +
  ggplot2::xlab("length (cm)")

# plot prob curve by species
combinedlengths |>
  dplyr::select(Code,GEARCAT,LENGTH) |>
  dplyr::arrange(Code,GEARCAT,LENGTH) |>
  dplyr::group_by(Code,GEARCAT) |>
  dplyr::mutate(prob = (1:dplyr::n())/dplyr::n()) |>
  ggplot2::ggplot(ggplot2::aes(x=LENGTH,y=prob,col=GEARCAT)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 0.5) +
  ggplot2::facet_wrap(~Code,scale = "free_x") +
  ggplot2::ylab("Probability") +
  ggplot2::xlab("length (cm)")

saveRDS(combinedlengths,here::here("data/groundfishLengthsByGear.rds"))
