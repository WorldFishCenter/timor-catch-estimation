# Read PESKADAT species sheet
clean_peskasdat_species <- function(path){
  path %>%
    readr::read_csv() %>%
    dplyr::mutate(species_code = as.character(species_code))
}


clean_peskasdat_boats <- function(path){
  path %>%
    readr::read_csv(col_types = readr::cols(.default = readr::col_character())) %>%
    janitor::clean_names() %>%
    dplyr::mutate(imei_long = imei,
                  installation_date = dplyr::if_else(stringr::str_detect(installation_date, "/[0-9]{2}$"),
                                                      lubridate::mdy(installation_date),
                                                      lubridate::dmy(installation_date))) %>%
    dplyr::mutate(municipality_name = dplyr::if_else(municipality_name == "Manatutu",
                                                     "Manatuto", municipality_name))

}

clean_peskadat_municipalities <- function(path){
  path %>%
    readr::read_csv() %>%
    janitor::clean_names() %>%
    dplyr::filter(!is.na(municipality_code)) %>%
    dplyr::mutate(municipality_code = as.character(municipality_code))
}

clean_peskadat_stations <- function(path){
  path %>%
    readr::read_csv() %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(.cols = c(station_code, municipality_code), .fns = as.character))
}
