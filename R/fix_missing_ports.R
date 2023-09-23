#' Fix missing ports or incomplete ports
#'
#' Some ports are not defined or have missing state or county.
#' However by looking at the permit holder and where they land you can identify some ports
#'
#' @export

fix_missing_ports <- function(channel){

  data <- atlantiscas::read_data()$data
  # link portid with port table from VTR database
  sql <- "select * from vtr.vlportsyn"
  portTable <- DBI::dbGetQuery(channel,sql) %>%
    tibble::as_tibble() %>%
    dplyr::rename(PORTID = PORT) %>%
    dplyr::select(PORTID,PORTNM,STATEABB) %>%
    dplyr::distinct()

  data %>%
    dplyr::distinct(PORT,PORT_COUNTY_NAME,STATE,PORTID)

  d2 <- data %>%
    dplyr::left_join(.,portTable,by="PORTID")

  d2 %>%
    dplyr::select(STATE,PORT,PORTNM,STATEABB,PORT_COUNTY_NAME,PORTID) %>%
    dplyr::distinct() %>%
    dplyr::filter(PORT != PORTNM) %>%
    print(n=100)
  ################################

  test <- data
  permits <- data %>%
    dplyr::distinct(PERMIT) %>%
    dplyr::pull()

  for(ipermit in 3507:length(permits)) {
    apermit <- permits[ipermit]
    pd <- data %>%
      dplyr::filter(PERMIT == apermit)

    pdd <- pd %>%
      dplyr::select(PORT,PORT_COUNTY_NAME,STATE)

    # if port is missing but county is not
    # assign the most frequent port permit holder lands in (based on trips)
    if (any(is.na(pdd$PORT))) {
      print(c(apermit,ipermit))
      pcn <- pd %>%
        dplyr::filter(is.na(PORT)) %>%
        {. ->> missingPorts} %>%
        dplyr::filter(!is.na(PORT_COUNTY_NAME))

      # for each port with just county listed
      countys <- unique(pcn$PORT_COUNTY_NAME)
      yrs <- missingPorts %>%
        dplyr::distinct(Year) %>%
        dplyr::pull()

      for (acounty in countys) {
        for (iyr in yrs) {
          # All ports in year and county where missing port is recorded
          allPorts <- pd %>%
            dplyr::filter(PORT_COUNTY_NAME == acounty,
                          !is.na(PORT),
                          Year == iyr) %>%
            dplyr::group_by(PORT,PORT_COUNTY_NAME,STATE) %>%
            dplyr::summarise(n = dplyr::n(),.groups="drop")

          if (nrow(allPorts) == 0) {
            # Look in neighbouring years
            allPorts <- pd %>%
              dplyr::filter(PORT_COUNTY_NAME == acounty,
                            !is.na(PORT),
                            Year %in% ((iyr-1):(iyr+1))) %>%
              dplyr::group_by(PORT,PORT_COUNTY_NAME,STATE) %>%
              dplyr::summarise(n = dplyr::n(),.groups="drop")
            # still no matching ports. infrequent fisher?
            if(nrow(allPorts) == 0) {
              allPorts <- pd %>%
                dplyr::filter(PORT_COUNTY_NAME == acounty,
                              !is.na(PORT)) %>%
                dplyr::group_by(PORT,PORT_COUNTY_NAME,STATE) %>%
                dplyr::summarise(n = dplyr::n(),.groups="drop")
            }
          }


          if (nrow(allPorts) == 1) {
            # This is the only port in this year that the permit holder landed in.
            # Assign the missing port to this port
            test <- test %>%
              dplyr::mutate(PORT = dplyr::case_when( ((PERMIT == apermit)&
                                                      (Year==iyr)&
                                                      (PORT_COUNTY_NAME==acounty)&
                                                      (is.na(PORT))) ~ allPorts$PORT,
                                                     TRUE ~ PORT))


          } else if (nrow(allPorts)==0){
            # there are still no landings for this year.
            print("No landings for this year")
            stop()


          } else {
            # pick the most common port for this year
            print("Many options to choose from")
            print(allPorts)
            allPorts <- allPorts %>%
              dplyr::filter(n==max(n))

            test <- test %>%
              dplyr::mutate(PORT = dplyr::case_when( ((PERMIT == apermit)&
                                                        (Year==iyr)&
                                                        (PORT_COUNTY_NAME==acounty)&
                                                        (is.na(PORT))) ~ allPorts$PORT,
                                                     TRUE ~ PORT))


          }

        }

      }

    }
  }




}
