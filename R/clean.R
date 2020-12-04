clean_boats <- function(path){
  path %>%
    readr::read_csv2() %>%
    janitor::clean_names() %>%
    dplyr::mutate(imei_long = as.character(imei),
                  last_seen = lubridate::ymd_hms(last_seen)) %>%
    dplyr::mutate_if(is.character, ~ dplyr::if_else(. == "--", NA_character_, .))
}
