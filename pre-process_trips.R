files <- list.files("data/raw/peskas-tracking/",
                    pattern = "pelagic-data-daily_.+csv$",
                    full.names = TRUE)

library(magrittr)
library(data.table)

process_track_file_dplyr <- function(x){
  m <- readr::read_csv(x, col_types = "Tccdddddcccc")
  on.exit(remove("m"))

  if (isFALSE("imei" %in% colnames(m))) {

  }

  m %>%
    janitor::clean_names() %>%
    dplyr::group_by(trip) %>%
    dplyr::summarise(start = min(time),
                     end = max(time),
                     range = max(range_meters),
                     dplyr::across(c(boat, boat_name, community, imei), dplyr::first),
                     .groups = "drop")
}

process_track_file_dt <- function(x){

  this_day <- stringr::str_extract(x, "[0-9]{4}-[0-9]{2}-[0-9]{2}")

  m0 <- fread(x, nrows = 5)

  if (nrow(m0) == 0 | ncol(m0) == 1) {

    empty_out <- data.table(start = NA,
                            end = NA,
                            range = NA,
                            boat_id = NA, boat_name = NA,
                            community = NA, imei = NA,
                            day = this_day)

    return(empty_out)
  }

  m <- fread(x, colClasses = c("POSIXct", "character", "character",
                               "numeric","numeric", "numeric", "numeric", "numeric",
                               "character", "character", "character", "character")) %>%
    janitor::clean_names()

  on.exit(remove("m"))

  if (isFALSE("imei" %in% colnames(m))) {
    m <- m[, imei := NA]
  }

  m[, by = trip, .(start = min(time), end = max(time), range = max(range_meters),
                   boat_id = data.table::first(boat),
                   boat_name = data.table::first(boat_name),
                   community = data.table::first(community),
                   imei = data.table::first(imei),
                   day = this_day)]
}

o <- files %>%
  purrr::map_dfr(purrr::auto_browse(process_track_file_dt))

fwrite(o, "data/processed/trips.csv")
