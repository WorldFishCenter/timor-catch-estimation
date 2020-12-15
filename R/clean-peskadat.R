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
                                                      lubridate::dmy(installation_date)))

}

clean_peskadat_municipalities <- function(path){
  path %>%
    readr::read_csv() %>%
    janitor::clean_names() %>%
    dplyr::filter(!is.na(municipality_code))
}
