#' Reproduce mismatch of portid from VTR tables and
#' reported landed port, state from fishing data

# get port table from VTR database
channel <- dbutils::connect_to_database("server","user")
# dbutils is a package we developed
# https://github.com/andybeet/dbutils
sql <- "select * from vtr.vlportsyn"
portTable <- DBI::dbGetQuery(channel,sql) %>%
  tibble::as_tibble() %>%
  dplyr::rename(PORTID = PORT) %>%
  dplyr::select(PORTID,PORTNM,STATEABB) %>%
  dplyr::distinct()

# read in fishing data


data2 <- readRDS(here::here("data-raw/fishing","REVENUEFILE_Neus_Atlantis_com_2008_2021_Sept2023.rds")) %>%
  tibble::as_tibble()  %>%
  dplyr::mutate(NESPP3 = as.double(NESPP3))  %>%
  dplyr::rename(PORTID = PORT,
                STATE = STATE1,
                PORT = PORTLND1)  %>%
  dplyr::select(-DOCID) %>%
  dplyr::left_join(.,portTable,by="PORTID")

# example
data2 %>%
  dplyr::filter(PORTID=="350635") %>%
  dplyr::distinct(PORTID,PORT,STATE,STATEABB,PORTNM)

data2 %>%
  dplyr::filter(PORTID=="350635",STATE == "MA")


# newbedford portid = 240403
portTable %>%
  dplyr::filter(grepl("NEW BEDFORD",PORTNM))
