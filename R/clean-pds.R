clean_trips_from_points <- function(path){
  readr::read_csv(path) %>%
    dplyr::mutate(trip_end_date_pds = lubridate::as_date(end, tz = "Asia/Dili"),
                  dplyr::across(c(imei, boat_id, trip), as.character)) %>%
    dplyr::rename(download_date_pds = day, trip_id_pds = trip,
                  boat_id_pds = boat_id) %>%
    # If there are more than a trip a day just choose the latest one
    dplyr::group_by(imei, trip_end_date_pds, download_date_pds) %>%
    dplyr::arrange(desc(end)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()
}
