#' Assign lat and long to ports
#'
#' We use PORTID, PORTNM, STATE in the data and cdfbs.port table
#'
#' Need to deal with Recreational data
#'
#'@param dat data frame. This is output from process_data
#'@param saveToFile boolean. Save data set to rds file
#'
#'
#' @return NULL
#' files are written to data-raw
#' 1. clean PORT Table with lat an lons
#' 2. REVENUE file for ports within NEUS footprint
#' 3. REVENUE flle for ports Outside NEUS footprint

# 18175111 records
# 19358 missing port ids


assign_latlon_ports <- function(dat,saveToFile=F) {

  # read in landings data
  #dat <- atlantiscas::read_data()

  # deal with ports in states that lie within model
  neusData <- dat$data %>%
    dplyr::filter(STATE %in% c("ME","NH","MA","RI","CT","NY","NJ","PA","DE","MD","VA")) %>%
    dplyr::mutate(PORTID = as.numeric(PORTID)) %>%
    dplyr::mutate(PORT = dplyr::case_when(PORT == "HAMPTON BAY" ~ "HAMPTON BAYS",
                                          PORT == "STUEBEN" ~ "STEUBEN",
                                          PORT == "OTHER NORTHAMPTON" ~ "NORTHAMPTON",
                                          TRUE ~ PORT))

  neusDataNA <- neusData %>%
    dplyr::filter(is.na(PORTID))

  neusOutside <- dat$data %>%
    dplyr::filter(!(STATE %in% c("ME","NH","MA","RI","CT","NY","NJ","PA","DE","MD","VA"))) %>%
    dplyr::mutate(PORTID = as.numeric(PORTID))

  # read in port lookup table
  portTableCFDBS <- readRDS(here::here("data-raw/portTableCFDBS.rds")) %>%
    dplyr::as_tibble() %>%
    dplyr::select(PORTID,PORT,STATE,COUNTY) %>%
    dplyr::rename(PORTNM = PORT,
                  STATEABB = STATE) %>%
    dplyr::mutate(PORTID = as.numeric(PORTID))

  #find all ports in data and filter from port table
  portids <- unique(neusData$PORTID)
  neusPortTable <- portTableCFDBS %>%
    dplyr::filter(PORTID %in% portids)


  # Replace all PORT, STATE names for known PORTIDs with names from PORT table
  # Assumes reported PORTID is correct.
  # There are some PORTIDs in data with multiple descriptions. Use the one in the port table
  ns <- neusData
  for (irecord in 1:nrow(neusPortTable)) {
    id <- neusPortTable$PORTID[irecord]
    port <- neusPortTable$PORTNM[irecord]
    state <- neusPortTable$STATEABB[irecord]
    ind <- neusData$PORTID == id
    ns$PORT[ind] <- port
    ns$STATE[ind] <- state
  }
  neusData <- ns


  # join landings with ports by port id to find ports in data set
  pts <- neusData %>%
    dplyr::left_join(.,portTableCFDBS, by =c("PORTID")) %>%
    { . ->> dd} %>%
    dplyr::distinct(PORTID,PORT,PORTNM,COUNTY,STATE,STATEABB)%>%
    dplyr::mutate(ORIGPORT = PORT,
                  ORIGSTATE = STATE) %>%
    dplyr::relocate(PORTID,ORIGPORT,ORIGSTATE,PORTNM,COUNTY,STATEABB) %>%
    dplyr::select(-PORT,-STATE)

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

  ## Fix other names
  pts2$newport[pts2$PORTNM == "DUKES" & pts2$STATEABB=="MA"] <- NA
  pts2$newport[pts2$PORTNM == "CREEK" & pts2$STATEABB=="MD"] <- NA
  pts2$newport[pts2$PORTNM == "NORTHUMBERLAND" & pts2$STATEABB=="VA"] <- NA
  pts2$newport[pts2$PORTNM == "WASHINGTON" & pts2$STATEABB=="NY"] <- NA
  pts2$newport[pts2$PORTNM == "WASHINGTON" & pts2$STATEABB=="RI"] <- NA
  pts2$newport[pts2$PORTNM == "WASHINGTON" & pts2$STATEABB=="ME"] <- NA
  pts2$newport[pts2$PORTNM == "CITY OF CHESAPEAKE" & pts2$STATEABB=="VA"] <- NA
  pts2$newport[pts2$PORTNM == "WILLIAMS CREEK" & pts2$STATEABB=="VA"] <- NA


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

  keep <- osmpts %>%
    dplyr::filter(!is.na(lat))

  osmm <- keep
  # select records with no no city
  tryagain <- osmpts %>%
    dplyr::filter(is.na(lat)) %>%
    dplyr::select(-lat,-lon)

  osmpt2 <- tidygeocoder::geocode(tryagain,
                          county=newcounty,
                          state = STATEABB,
                          lat=lat,
                          long= lon,
                          return_input = T,
                          method = "osm")

  keep <- osmpt2 %>%
    dplyr::filter(!is.na(lat))

  osmm <- rbind(osmm,keep)

  ## trip id's without port info in tables
  ## use port info in data
  missingports <- osmpt2 %>%
    dplyr::filter(is.na(lat)) %>%
    dplyr::distinct(PORTID) %>%
    dplyr::pull()

  step3 <- neusData %>%
    dplyr::filter(PORTID %in% missingports) %>%
    dplyr::distinct(PORTID,PORT,STATE) %>%
    dplyr::mutate(COUNTY = NA,
                  newcounty = PORT,
                  newport = NA,
                  ORIGPORT = PORT,
                  ORIGSTATE = STATE,
                  PORTNM = NA,
                  STATEABB = NA,) %>%
    dplyr::relocate(PORTID,ORIGPORT,ORIGSTATE,PORTNM,COUNTY,STATEABB,newport,newcounty) %>%
    dplyr::select(-PORT,-STATE)


  osmpt3 <- tidygeocoder::geocode(step3,
                                  county=newcounty,
                                  state = ORIGSTATE,
                                  lat=lat,
                                  long= lon,
                                  return_input = T,
                                  method = "osm")

  keep <- osmpt3 %>%
    dplyr::filter(!is.na(lat))

  osmm <- rbind(osmm,keep)

  ## hand fix ports ...again
  missingports <- osmpt3 %>%
    dplyr::filter(is.na(lat))

  # these ports exist in the data already
  # replace PORT name and STATE with corrct nam

  # missingports$newcounty[missingports$ORIGPORT =="STUEBEN" & missingports$ORIGSTATE =="ME"] <- "STEUBEN"
  #
  # missingports$PORTID[missingports$ORIGPORT =="LONG BEACH" & missingports$ORIGSTATE =="NJ"] <- 331627
  # missingports$lat[missingports$ORIGPORT =="LONG BEACH" & missingports$ORIGSTATE =="NJ"] <- 39.97782
  # missingports$lon[missingports$ORIGPORT =="LONG BEACH" & missingports$ORIGSTATE =="NJ"] <- -74.33193
  # missingports$newport[missingports$ORIGPORT =="LONG BEACH" & missingports$ORIGSTATE =="NJ"] <- "LONG BEACH"
  # missingports$newcounty[missingports$ORIGPORT =="LONG BEACH" & missingports$ORIGSTATE =="NJ"] <- "OCEAN"
  #
  # missingports$PORTID[missingports$ORIGPORT =="HAMPTON BAY" & missingports$ORIGSTATE =="NY"] <- 350735
  # missingports$lat[missingports$ORIGPORT =="HAMPTON BAY" & missingports$ORIGSTATE =="NY"] <- 40.86899
  # missingports$lon[missingports$ORIGPORT =="HAMPTON BAY" & missingports$ORIGSTATE =="NY"] <- -72.5175893
  # missingports$newcounty[missingports$ORIGPORT =="HAMPTON BAY" & missingports$ORIGSTATE =="NY"] <- "SUFFOLK"
  # missingports$newport[missingports$ORIGPORT =="HAMPTON BAY" & missingports$ORIGSTATE =="NY"] <- "HAMPTON BAYS"
  #
  # missingports$PORTID[missingports$ORIGPORT =="OTHER NORTHAMPTON" & missingports$ORIGSTATE =="VA"] <- 490945
  # missingports$lat[missingports$ORIGPORT =="OTHER NORTHAMPTON" & missingports$ORIGSTATE =="VA"] <- 37.04934
  # missingports$lon[missingports$ORIGPORT =="OTHER NORTHAMPTON" & missingports$ORIGSTATE =="VA"] <- -76.4244302197637
  # missingports$newcounty[missingports$ORIGPORT =="OTHER NORTHAMPTON" & missingports$ORIGSTATE =="VA"] <- "NORTHAMPTON"
  # missingports$newport[missingports$ORIGPORT =="OTHER NORTHAMPTON" & missingports$ORIGSTATE =="VA"] <- "NORTHAMPTON"
  #
  # missingports$lat[missingports$ORIGPORT =="OTHER NEW LONDON" & missingports$ORIGSTATE =="CT"] <- 41.4915
  # missingports$lon[missingports$ORIGPORT =="OTHER NEW LONDON" & missingports$ORIGSTATE =="CT"] <- -72.1237666
  # missingports$newcounty[missingports$ORIGPORT =="OTHER NEW LONDON" & missingports$ORIGSTATE =="CT"] <- "NEW LONDON"
  #
  # missingports$lat[missingports$ORIGPORT =="OTHER DELAWARE" & missingports$ORIGSTATE =="DE"] <- 38.69205
  # missingports$lon[missingports$ORIGPORT =="OTHER DELAWARE" & missingports$ORIGSTATE =="DE"] <- -75.4013315
  # missingports$newcounty[missingports$ORIGPORT =="OTHER DELAWARE" & missingports$ORIGSTATE =="DE"] <- NA
  #
  # missingports$lat[missingports$ORIGPORT =="OTHER SUSSEX" & missingports$ORIGSTATE =="DE"] <- 38.69081
  # missingports$lon[missingports$ORIGPORT =="OTHER SUSSEX" & missingports$ORIGSTATE =="DE"] <- -75.36914
  # missingports$newcounty[missingports$ORIGPORT =="OTHER SUSSEX" & missingports$ORIGSTATE =="DE"] <- "SUSSEX"
  #
  # missingports$lat[is.na(missingports$ORIGPORT) & missingports$ORIGSTATE =="NH"] <- 43.48491
  # missingports$lon[is.na(missingports$ORIGPORT) & missingports$ORIGSTATE =="NH"] <- -71.6554

  ## Check again for PORTID dups

  ##############################

  # just select valid PORTS
  osm <- osmm %>%
    dplyr::filter(!is.na(PORTID))

  osmwrite <- osm #rbind(osm,missingports)
  osmwrite <- osm %>%
    dplyr::select(-ORIGPORT,-ORIGSTATE) %>%
    dplyr::distinct()


  saveRDS(osmwrite,here::here("data-raw/portLatLon.rds"))

  #osmm <- readRDS(here::here("data-raw/portLatLon.rds"))

  # missing ports removed. deal with later
  neusData2 <- neusData %>%
    dplyr::filter(!is.na(PORTID))


  ## DONE for data with ports that have port ids.
  # Now deal witj data with missing ports. Assign them portids, then overwrite PORT and state with correect infor from
  # port table


  ## After all of this there are many trips with no port id
  # Try to fix this
  newmiss <- osmm %>%
    dplyr::filter(is.na(PORTID))
  ## for records that say (COUNTY) remove and make portname. this is equivaluent to OTHER
  newmisss <- newmiss %>%
    dplyr::mutate(newcounty = dplyr::case_when(grepl("\\(COUNTY\\)",newcounty) ~ gsub("\\(COUNTY\\)","",newcounty),
                                               TRUE ~ newcounty)) %>%
    dplyr::mutate(newcounty = trimws(newcounty))

  ## match reported port with other ports with same name or counties with the reported port name
  fillin <- newmisss
  for(ir in 1:nrow(newmisss)) {
    record <- newmisss[ir,]

    p <- record$newcounty
    s <- record$ORIGSTATE
    record <- osmm %>%
      dplyr::filter(newport == p & STATEABB == s)
    if (nrow(record) == 0) {
      record <- osmm %>%
        dplyr::filter(newcounty == p & ORIGSTATE == s)
    }

    if (nrow(record) == 0) {
      print("ahhh")
    } else {
      # take first entry
      fillin$PORTID[ir] <- record$PORTID[1]
      fillin$lat[ir] <- record$lat[1]
      fillin$lon[ir] <- record$lon[1]
      fillin$newport[ir] <- record$newport[1]
      fillin$newcounty[ir] <- record$newcounty[1]
    }

  }


  ## remove ports with NA Ids. Deal with this separately

  ### Now deal with NA data
  # match orig ports and assign trip id
  naports <- neusDataNA %>%
    dplyr::distinct(PORT,STATE)

  useports <- fillin
  #  now grab port id in neusData to assign to missing ports
  neusDataNA2 <- neusDataNA
  for (iport in 1:nrow(useports)) {
    record <- useports %>%
      dplyr::slice(iport)
    ind <- ((neusDataNA2$PORT == record$ORIGPORT) & (neusDataNA2$STATE == record$ORIGSTATE))
    neusDataNA2$PORTID[ind] <- record$PORTID
  }
  # these ports that are duplicates of ports in neusdata
  # remove ports that have missing port or state. toss data
  missingports <- missingports %>%
    dplyr::filter( !is.na(ORIGSTATE) & !is.na(ORIGPORT))

  neusDataNA2 %>%
    dplyr::filter(is.na(PORTID)) %>% dplyr::distinct(PORT,STATE)


  for (iport in 1:nrow(missingports)) {
    record <- missingports %>%
      dplyr::slice(iport)
    ind <- ((neusData2$PORT == record$ORIGPORT) & (neusData2$STATE == record$ORIGSTATE))
    indna <- ((neusDataNA2$PORT == record$ORIGPORT) & (neusDataNA2$STATE == record$ORIGSTATE))
    portid <- unique(neusData2$PORTID[ind])
    neusDataNA2$PORTID[indna] <- portid
  }

  ## toss data with missing ports. very few records from small ports
  neusDataNA2 <- neusDataNA2 %>%
    dplyr::filter(!is.na(PORTID))

  neusDataFinal <- rbind(neusData2,neusDataNA2)

  ## now bind this all back to neusData
  az <- neusDataFinal %>%
    dplyr::left_join(.,osmwrite, by = "PORTID")

  if (saveToFile) {
    saveRDS(az,here::here("data-raw/REVENUE_cleanports.rds"))
    saveRDS(neusOutside,here::here("data-raw/REVENUE_outsidemodel.rds"))
  }

  return(list(neus=az,noneus=neusOutside))

  ## Deal with rec data

}
