#' map ports
#'
#'


# 18175111 records
# 19358 missing port ids

library(magrittr)

# read in landings data
a <- atlantiscas::read_data()

# deal with ports in states that lie within model
neusData <- a$data %>%
  dplyr::filter(STATE %in% c("ME","NH","MA","RI","CT","NY","NJ","PA","DE","MD","VA"))
neusNoData <- a$data %>%
  dplyr::filter(!(STATE %in% c("ME","NH","MA","RI","CT","NY","NJ","PA","DE","MD","VA")))

# read in port lookup table
portTableCFDBS <- readRDS(here::here("data-raw/portTableCFDBS.rds")) %>%
  dplyr::as_tibble() %>%
  dplyr::select(PORT, PORTNM,STATEABB,COUNTY)

# join landings with ports by port id to find ports in data set
pts <- neusData %>%
  dplyr::left_join(.,portTableCFDBS, by =c("PORTID"="PORT")) %>%
  { . ->> dd} %>%
  dplyr::distinct(PORTNM,COUNTY,STATEABB)

# create new port and county fields with "Clean" names
pts2 <- pts %>%
  dplyr::mutate(newport = dplyr::case_when(grepl("OTHER",PORTNM) ~ gsub("OTHER ","",PORTNM),
                                             TRUE ~ PORTNM)) %>%
  dplyr::mutate(newport = gsub("\\(COUNTY\\)","",newport)) %>%
  dplyr::mutate(newport = gsub("\\(TOWN OF\\)","",newport)) %>%
  dplyr::mutate(newport = gsub("\\/LYNNHAVEN","",newport)) %>%
  dplyr::mutate(newport = gsub("\\(CAPTREE\\)","",newport)) %>%
  dplyr::mutate(newport = gsub("\\(PORT\\)","",newport)) %>%
  dplyr::mutate(newport = gsub("\\(AQUINNAH\\)","",newport))

# county
pts2 <- pts2 %>%
  dplyr::mutate(newcounty = dplyr::case_when(grepl("CITY OF",COUNTY) ~ gsub("CITY OF ","",COUNTY),
                                           TRUE ~ COUNTY)) %>%
  dplyr::mutate(newcounty = gsub("\\(COUNTY\\)","",newcounty)) %>%
  dplyr::mutate(newcounty = gsub("NOT-SPECIFIED","",newcounty))

# remove trips with no specied portid, port, county or state. All missing
pts2 <- pts2 %>%
  dplyr::mutate(newport = dplyr::case_when(newcounty == "" ~ "",
                                           TRUE ~ newport))

# run names of port, county, state through open street maps to get lat and lon
osmpts <- tidygeocoder::geocode(pts2,
                             city=newport,
                             county=newcounty,
                             state = STATEABB,
                             lat=lat,
                             long= lon,
                             return_input = T,
                             method = "osm")
# select records with no matches
tryagain <- osmpts %>%
  dplyr::filter(is.na(lat) & !(is.na(COUNTY))) %>%
  dplyr::rename(PORT=newport,
                OLDCOUNTY = COUNTY,
                COUNTY = newcounty,
                STATE = STATEABB)

#run these through a different geocoder
bb <- get_port_lat_lon_web(tryagain)
newmatches <- bb %>%
  dplyr::filter(!is.na(lat) & lat!=999)

nine <- bb %>%
  dplyr::filter(lat==999)


keeposmpts <- osmpts
osmpts <- keeposmpts
## add new matched to osm
for (i in 1:nrow(newmatches)) {
  print(i)
  ind <- which(osmpts$PORTNM == newmatches$PORTNM[i] & osmpts$STATEABB == newmatches$STATEABB[i] & osmpts$COUNTY == newmatches$COUNTY[i])
  if (length(ind)>1){
    stop("More than one match")
  }
  osmpts$lat[ind] <- newmatches$lat[i]
  osmpts$lon[ind] <- newmatches$lon[i]
}

osmpts %>%
  dplyr::filter(is.na(lat))

## hard code a few ports
osmpts$lat[osmpts$PORTNM == "ORRS ISLAND"] <- 43.7606388
osmpts$lon[osmpts$PORTNM == "ORRS ISLAND"] <- -69.9761584
osmpts$lat[osmpts$PORTNM == "LONG ISLAND CUMBERLAND"] <- 43.691554350000004
osmpts$lon[osmpts$PORTNM == "LONG ISLAND CUMBERLAND"] <- -70.15805924008598
osmpts$lat[osmpts$PORTNM == "STUEBEN"] <- 44.5106339
osmpts$lon[osmpts$PORTNM == "STUEBEN"] <- -67.9652809



bb <- get_port_lat_lon_web(nine[1,])
tryagain

