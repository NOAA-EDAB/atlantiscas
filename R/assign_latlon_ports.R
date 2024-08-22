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
#'
#' @family processData
#'
#' @export

# 18175111 records
# 19358 missing port ids


assign_latlon_ports <- function(dat,saveToFile=F) {

  # read in landings data
  #dat <- atlantiscas::read_data()

  message("Assigning lat and lons")
  # deal with ports in states that lie within model
  neusData <- dat |>
    dplyr::mutate(PORTID = as.numeric(PORTID)) |>
    dplyr::select(-CFG_PORT,-PORT_NAME,-STATE_ABB)

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
    dplyr::filter(!(STATEABB %in% c("ME","NH","MA","RI","CT","NY","NJ","PA","DE","MD","VA",NA))) |>
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
  ## need NEW LONDON COUNTY
  pts2 <- pts2 |>
    dplyr::mutate(newcounty = dplyr::case_when(newcounty == "NEW LONDON" ~ "NEW LONDON COUNTY",
                                               newcounty == "NEW HAVEN" ~ "NEW HAVEN COUNTY",
                                               TRUE ~ newcounty))


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

  # check to makes sure we have distinct(PORTIDS)
  tab <- osmm |>
    dplyr::select(PORTID,newport,newcounty,STATEABB,lat,lon) |>
    dplyr::distinct() |>
    dplyr::group_by(PORTID) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::filter(n > 1)
  if (nrow(tab) > 0) {
    stop("duplicate PORTID")
  }

  # select records with no no city
  tryagain <- osmpts |>
    dplyr::filter(is.na(lat)) |>
    dplyr::select(-lat,-lon)

  # check if any portid in missing already have matches
  matches <- intersect(tryagain$PORTID,osmm$PORTID)
  if (length(matches) > 0){
    stop("Extra work needed")
  }


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

  # check to makes sure we have distinct(PORTIDS)
  tab <- osmm |>
    dplyr::select(PORTID,newport,newcounty,STATEABB,lat,lon) |>
    dplyr::distinct() |>
    dplyr::group_by(PORTID) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::filter(n > 1)
  if (nrow(tab) > 0) {
    stop("duplicate PORTID")
  }


  ## now ignore county and try port with state

  # select records with no no city
  tryagain <- osmpt2 |>
    dplyr::filter(is.na(lat)) |>
    dplyr::select(-lat,-lon)

  message("Running PORT, STATE (from port table) through open street maps to get lat and lons")
  osmpt2b <- tidygeocoder::geocode(tryagain,
                                  city = newport,
                                  #county=newcounty,
                                  state = STATEABB,
                                  lat=lat,
                                  long= lon,
                                  return_input = T,
                                  method = "osm")

  keep <- osmpt2b |>
    dplyr::filter(!is.na(lat))

  osmm <- rbind(osmm,keep)

  # check to makes sure we have distinct(PORTIDS)
  tab <- osmm |>
    dplyr::select(PORTID,newport,newcounty,STATEABB,lat,lon) |>
    dplyr::distinct() |>
    dplyr::group_by(PORTID) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::filter(n > 1)
  if (nrow(tab) > 0) {
    stop("duplicate PORTID")
  }

  ## trip id's without port info in tables
  ## use port info in origianl data, not assigned to a portid
  missingports <- osmpt2b |>
    dplyr::filter(is.na(lat) & !is.na(PORTID)) |>
    dplyr::mutate(ORIGPORT = dplyr::case_when(grepl("OTHER",ORIGPORT) ~ gsub("OTHER ","",ORIGPORT),
                                             TRUE ~ ORIGPORT)) |>
    dplyr::mutate(newcounty = dplyr::case_when(grepl("CUNDY",ORIGPORT) ~ "CUMBERLAND",
                                              TRUE ~ COUNTY)  ) |>
    dplyr::mutate(ORIGPORT = dplyr::case_when(grepl("HAMPTON BAY",ORIGPORT) ~ "HAMPTON BAYS",
                                               TRUE ~ ORIGPORT)  ) |>
    dplyr::select(-c(lat,lon)) |>
    dplyr::distinct()

  # check if any portid in missing already have matches
  matches <- intersect(missingports$PORTID,osmm$PORTID)
  if (length(matches) > 0){
    stop("Extra work needed")
  }

  ## Try to match from designated ports already in port tables
  fixed1 <- NULL
  for (irecord in 1:nrow(missingports)) {
    record <- missingports[irecord,]
    port <- record$ORIGPORT
    state <- record$ORIGSTATE
    if (is.na(port) & is.na(state)) {
      next
    }
    # pick out lat and lon for this port
    match <- osmm |>
      dplyr::filter(newport == port,
                    STATEABB == state) |>
      dplyr::distinct(PORTID,lat,lon,newport,newcounty,STATEABB) |>
      dplyr::slice(1)
    if (nrow(match) == 0) {

    }
    record$lat <- match$lat
    record$lon <- match$lon
    #record$PORTID <- match$PORTID
    record$newport <- match$newport
    record$newcounty <- match$newcounty
    record$STATEABB <- match$STATEABB
    fixed1 <- rbind(fixed1,record)
  }

  osmm <- rbind(osmm,fixed1)

  ## Make fix for "ports" that have no information except for the state
  states <- osmm |>
    dplyr::filter(newcounty == "") |>
    dplyr::select(-lat,-lon)

  osmptstates <- tidygeocoder::geocode(states,
                                   state = newport,
                                   lat=lat,
                                   long= lon,
                                   return_input = T,
                                   method = "osm")

  nonstates <- osmm |>
    dplyr::filter(newcounty != "")

  osmm <- rbind(nonstates,osmptstates)

  # select distinct ports
  ports <- osmm |>
    dplyr::select(PORTID,newport,newcounty,STATEABB,lat,lon) |>
    dplyr::distinct()


  ### ALL data with a port ID now rectified. Join with data

  notNA <- neusData |>
    dplyr::select(-STATEABB) |>
    dplyr::filter(!is.na(PORTID)) |>
    dplyr::left_join(ports,by="PORTID") |>
    dplyr::relocate(PORTID)

  #saveRDS(notNA,here::here("notNA.rds"))

  ###################################################### NA Portss ##################################
  ## Deal with records that dont have a port id. give them a portid and join to neusData
  message("Dealing with Data that have no PORTID. Map to existing ports")

  neusDataNA <- neusData |>
    dplyr::filter(is.na(PORTID))


  missingports  <-  osmpt2b |>
    dplyr::filter(is.na(lat) & is.na(PORTID))


  missingports <- missingports |>
    dplyr::filter(is.na(lat))  |>
    dplyr::mutate(ORIGPORT = dplyr::case_when(grepl("OTHER",ORIGPORT) ~ gsub("OTHER ","",ORIGPORT),
                                              TRUE ~ ORIGPORT)) |>
    dplyr::mutate(newcounty = dplyr::case_when(grepl("CUNDY",ORIGPORT) ~ "CUMBERLAND",
                                               TRUE ~ COUNTY)  ) |>
    dplyr::mutate(ORIGPORT = dplyr::case_when(grepl("HAMPTON BAY",ORIGPORT) ~ "HAMPTON BAYS",
                                              TRUE ~ ORIGPORT)  ) |>
    dplyr::distinct()

  neusDataNA <- neusDataNA |>
    dplyr::mutate(PORT = dplyr::case_when(grepl("OTHER",PORT) ~ gsub("OTHER ","",PORT),
                                              TRUE ~ PORT)) |>
    dplyr::mutate(PORT = dplyr::case_when(grepl("HAMPTON BAY",PORT) ~ "HAMPTON BAYS",
                                              TRUE ~ PORT)  )

  # match existing data
  # assign existing port id's, newport and new county to ports without an id
  fixed <- NULL
  for (irecord in 1:nrow(missingports)) {
    #print(irecord)
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
    if (nrow(match) == 0) {
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
    dplyr::distinct()

  # osmm <- rbind(osmm,fixed) |>
  #   dplyr::distinct()


  newfixed <- fixed |>
    dplyr::select(PORTID,ORIGPORT,ORIGSTATE,newport,newcounty,STATEABB,lat,lon)

  # join these to data
  dataNA <- neusDataNA |>
    dplyr::select(-PORTID,-STATEABB) |>
    dplyr::left_join(newfixed, by = c("PORT"="ORIGPORT","STATE"="ORIGSTATE")) |>
    dplyr::relocate(PORTID) |>
    dplyr::filter(!is.na(PORTID))

  allData <- rbind(notNA,dataNA)





  ##############################
# SAve processed output

  allData |>
    dplyr::select(PORTID,STATEABB,newport,newcounty,lat,lon) |>
    dplyr::distinct()

  osmwrite <- osmm |>
    dplyr::select(-ORIGPORT,-ORIGSTATE)  |>
    dplyr::distinct()

  if(saveToFile) {
    saveRDS(osmwrite,here::here("data/portLatLon.rds"))
  }

  #osmm <- readRDS(here::here("data-raw/portLatLon.rds"))


  if (saveToFile) {
    saveRDS(allData,here::here("data-raw/REVENUE_cleanports_CAMS.rds"))
    saveRDS(neusOutside,here::here("data-raw/REVENUE_outsidemodel.rds"))
  }

  return(list(neus=allData,noneus=neusOutside))

  ## Deal with rec data

}
