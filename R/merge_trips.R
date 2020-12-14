merge_trips <- function(kobo_trips_2, trips_from_points){

  require(dplyr)

  trips_with_imei <- trips_from_points %>%
    filter(!is.na(imei))

  kobo_trips_2 %>%
    dplyr::full_join(trips_with_imei, by = c("trip_imei" = "imei",
                                               "trip_date" = "trip_end_date_pds"))
}


