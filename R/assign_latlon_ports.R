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
  neusData <- dat |>
    dplyr::mutate(PORTID = as.numeric(PORTID)) |>
    dplyr::select(-CFG_PORT,-PORT_NAME,-STATE_ABB)


  neusDataNA <- neusData |>
    dplyr::filter(is.na(PORTID))

  # # remove all ports with NA id
  # neusData <- neusData |>
  #   dplyr::filter(!is.na(PORTID))


  # read in CAMS port lookup table
  portTableCAMS <- readRDS(here::here("data-raw/portTableCAMS.rds")) |>
    dplyr::as_tibble() |>
    dplyr::select(PORTID,PORT,STATE,COUNTY) |>
    dplyr::rename(PORTNM = PORT,
                  STATEABB = STATE) |>
    dplyr::mutate(PORTID = as.numeric(PORTID))

  # join data with cleaned port table
  neusData <- neusData |>
    dplyr::left_join(portTableCAMS,by = "PORTID")

  # ports outside of model
  neusOutside <- neusData |>
    dplyr::filter(!(STATEABB %in% c("ME","NH","MA","RI","CT","NY","NJ","PA","DE","MD","VA"))) |>
    dplyr::mutate(PORTID = as.numeric(PORTID))

  # filter all ports within model domain
  neusData <- neusData |>
    dplyr::filter(STATEABB %in% c("ME","NH","MA","RI","CT","NY","NJ","PA","DE","MD","VA",NA))

  #find all ports in data and filter from port table
  portids <- unique(neusData$PORTID)
  neusPortTable <- portTableCAMS |>
    dplyr::filter(PORTID %in% portids)


  # Fix some names.
  # There are port names in the landings data which do not match port names in the port tables.
  # Further, the port tables in the landings table have spelling errors etc. We correct these now.
  # we ignore the names in the landings data since they should be tied to the port id number

  # join landings with ports by port id to find ports in data set
  pts <- neusData |>
    dplyr::distinct(PORTID,PORT,PORTNM,COUNTY,STATE,STATEABB) |>
    dplyr::mutate(ORIGPORT = PORT,
                  ORIGSTATE = STATE) |>
    dplyr::relocate(PORTID,ORIGPORT,ORIGSTATE,PORTNM,COUNTY,STATEABB) |>
    dplyr::select(-PORT,-STATE)

  # create new port and county fields with "Clean" names
  pts2 <- pts |>
    dplyr::mutate(newport = dplyr::case_when(grepl("OTHER",PORTNM) ~ gsub("OTHER ","",PORTNM),
                                               TRUE ~ PORTNM)) |>
    dplyr::mutate(newport = gsub("\\(COUNTY\\)","",newport)) |>
    dplyr::mutate(newport = gsub("\\(TOWN OF\\)","",newport)) |>
    dplyr::mutate(newport = gsub("\\/LYNNHAVEN","",newport)) |>
    dplyr::mutate(newport = gsub("\\(CAPTREE\\)","",newport)) |>
    dplyr::mutate(newport = gsub("\\(PORT\\)","",newport)) |>
    dplyr::mutate(newport = gsub("\\(AQUINNAH\\)","",newport))


  # county
  pts2 <- pts2 |>
    dplyr::mutate(newcounty = dplyr::case_when(grepl("CITY OF",COUNTY) ~ gsub("CITY OF ","",COUNTY),
                                             TRUE ~ COUNTY)) |>
    dplyr::mutate(newcounty = gsub("\\(COUNTY\\)","",newcounty)) |>
    dplyr::mutate(newcounty = gsub("NOT-SPECIFIED","",newcounty))

  # Manual Fixes for "OTHER KINGS"
  pts2$newport[pts2$newcounty == "KINGS" & pts2$STATEABB=="NY"] <- "BROOKLYN"
  # manual fix for portid with 2 differnt ports that are not in port lookup table
  index <- pts2$PORTID == 224107 & !is.na(pts2$PORTID)
  pts2[index,]$ORIGPORT <- "BOOTHBAY HARBOR"

  message("Running PORT, COUNTY, STATE through open street maps to get lat and lons")
  # run names of port, county, state through open street maps to get lat and lon
  osmpts <- tidygeocoder::geocode(pts2,
                               city=newport,
                               county=newcounty,
                               state = STATEABB,
                               lat=lat,
                               long= lon,
                               return_input = T,
                               method = "osm")

  keep <- osmpts |>
    dplyr::filter(!is.na(lat))

  osmm <- keep
  # select records with no no city
  tryagain <- osmpts |>
    dplyr::filter(is.na(lat)) |>
    dplyr::select(-lat,-lon)

  message("Running COUNTY, STATE (from port table) through open street maps to get lat and lons")
  osmpt2 <- tidygeocoder::geocode(tryagain,
                          county=newcounty,
                          state = STATEABB,
                          lat=lat,
                          long= lon,
                          return_input = T,
                          method = "osm")

  keep <- osmpt2 |>
    dplyr::filter(!is.na(lat))

  osmm <- rbind(osmm,keep)

  ## trip id's without port info in tables
  ## use port info in origianl data, not assigned to a portid
  missingports <- osmpt2 |>
    dplyr::filter(is.na(lat) & !is.na(PORTID)) |>
    dplyr::mutate(ORIGPORT = dplyr::case_when(grepl("OTHER",ORIGPORT) ~ gsub("OTHER ","",ORIGPORT),
                                             TRUE ~ ORIGPORT)) |>
    dplyr::mutate(newcounty = dplyr::case_when(grepl("CUNDY",ORIGPORT) ~ "CUMBERLAND",
                                              TRUE ~ COUNTY)  ) |>
    dplyr::mutate(ORIGPORT = dplyr::case_when(grepl("HAMPTON BAY",ORIGPORT) ~ "HAMPTON BAYS",
                                               TRUE ~ ORIGPORT)  ) |>
    dplyr::select(-c(lat,lon))




  message("Running COUNTY, STATE (from data) through open street maps to get lat and lons.")
  osmpt3 <- tidygeocoder::geocode(missingports,
                                  city = ORIGPORT,
                                  #county=newcounty,
                                  state = ORIGSTATE,
                                  lat=lat,
                                  long= lon,
                                  return_input = T,
                                  method = "osm")

  keep <- osmpt3 |>
    dplyr::filter(!is.na(lat))

  osmm <- rbind(osmm,keep)

  ports <- osmm |>
    dplyr::select(PORTID,newport,newcounty,lat,lon) |>
    dplyr::distinct()

  ### ALL data with a port ID now rectified. Join with data

  neusData |>
    dplyr::filter(!is.na(PORTID)) |>
    dim()

  notNA <- neusData |>
    dplyr::filter(!is.na(PORTID)) |>
    dplyr::left_join(ports,by="PORTID")

  saveRDS(notNA,here::here("notNA.rds"))

  # Deal with records that dont have a port id. give them a portid and join to neusData

  neusDataNA <- neusData |>
    dplyr::filter(is.na(PORTID))

  missingports <- osmpt3 |>
    dplyr::filter(is.na(lat))

  missing  <-  osmpt2 |>
    dplyr::filter(is.na(lat) & is.na(PORTID)) |>
    dplyr::filter(is.na(lat))

  missingports <- rbind(missingports,missing)

  missingports <- missingports |>
    dplyr::filter(is.na(lat))  |>
    dplyr::mutate(ORIGPORT = dplyr::case_when(grepl("OTHER",ORIGPORT) ~ gsub("OTHER ","",ORIGPORT),
                                              TRUE ~ ORIGPORT)) |>
    dplyr::mutate(newcounty = dplyr::case_when(grepl("CUNDY",ORIGPORT) ~ "CUMBERLAND",
                                               TRUE ~ COUNTY)  ) |>
    dplyr::mutate(ORIGPORT = dplyr::case_when(grepl("HAMPTON BAY",ORIGPORT) ~ "HAMPTON BAYS",
                                              TRUE ~ ORIGPORT)  )


  # match existing data
  fixed <- NULL
  for (irecord in 1:nrow(missingports)) {
    print(irecord)
    record <- missingports[irecord,]
    port <- record$ORIGPORT
    county <- record$newcounty
    state <- record$ORIGSTATE
    # record$newport <- record$ORIGPORT
    # record$STATEABB <- record$ORIGSTATE
    if (is.na(port) & is.na(county) & is.na(state)) {
      next
    }
    # pick out lat and lon for this port
    match <- osmm |>
      dplyr::filter(newport == port,
                    #newcounty == county,
                    STATEABB == state) |>
      dplyr::distinct(PORTID,lat,lon,newport,newcounty,STATEABB) |>
      dplyr::slice(1)
    if (nrow(match) ==0) {
      match <- osmm |>
        dplyr::filter(ORIGPORT == port,
                      #newcounty == county,
                      STATEABB == state) |>
        dplyr::distinct(PORTID,lat,lon,newport,newcounty,STATEABB) |>
        dplyr::slice(1)
    }
    record$lat <- match$lat
    record$lon <- match$lon
    record$PORTID <- match$PORTID
    record$newport <- match$newport
    record$newcounty <- match$newcounty
    record$STATEABB <- match$STATEABB
    fixed <- rbind(fixed,record)
  }

  fixed <- fixed |>
    dplyr::distinct() |>
    dplyr::select(ORIGPORT,ORIGSTATE,newport,newcounty,lat,lon)

  # join these to data
  neusDataNA |>
    dplyr::left_join(fixed, by = c("PORT"="ORIGPORT","STATE"="ORIGSTATE"))

osmm <- rbind(osmm,fixed)


  ##############################


  osmwrite <- osmm |>
    dplyr::select(-ORIGPORT,-ORIGSTATE)  |>
    dplyr::distinct()

  if(saveToFile) {
    saveRDS(osmwrite,here::here("data-raw/portLatLon.rds"))
  }

  #osmm <- readRDS(here::here("data-raw/portLatLon.rds"))

  ## DONE for data with ports that have port ids.
  # Now deal witj data with missing ports. Assign them portids, then overwrite PORT and state with correect infor from
  # port table


  ## After all of this there are many trips with no port id
  # Try to fix this
  newmiss <- osmm  |>
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
