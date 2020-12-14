clean_boats <- function(path){
  path %>%
    readr::read_csv2() %>%
    janitor::clean_names() %>%
    dplyr::mutate(imei_long = as.character(imei),
                  last_seen = lubridate::ymd_hms(last_seen)) %>%
    dplyr::mutate_if(is.character, ~ dplyr::if_else(. == "--", NA_character_, .))
}

clean_trips <- function(path){
  path %>%
    readr::read_delim(delim = ";", col_types = "cccccccdcc") %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      dplyr::across(.cols = tidyselect:::where(is.character),
                    .fns = ~ dplyr::if_else(. == "--", NA_character_, .)),
      dplyr::across(.cols = c(start, end),
                    .fns = lubridate::ymd_hms),
      dplyr::across(.cols = c(distance, range),
                    .fns = ~stringr::str_extract(., "[0-9.]+")),
      dplyr::across(.cols = c(distance, range),
                    .fns = as.numeric),
      # To conform with ISO 8601 in which 'M' is for minutes and 'm' for months
      duration = stringr::str_replace(duration, "m", "M"),
      duration = stringr::str_replace(duration, "h", "H"),
      duration = lubridate::duration(duration),
      trip_id = as.character(trip_id))
}
