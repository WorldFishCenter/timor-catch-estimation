library(DBI)
library(bigrquery)
library(tidyverse)

bq_auth(use_oob = TRUE)

# catch -------------------------------------------------------------------

catch_connection <- dbConnect(
  bigrquery::bigquery(),
  project = "peskas",
  dataset = "timor_catch",
  billing = "peskas"
)

dbListTables(catch_connection)

catch <- tbl(catch_connection, "timor_catch_raw_csv")
catch_raw <- catch %>%
  collect()

catch_raw %>%
  write_csv("data/raw/timor-catch.csv")

# points ------------------------------------------------------------------

tracks_connection <- dbConnect(
  bigrquery::bigquery(),
  project = "peskas",
  dataset = "pelagic_data",
  billing = "peskas"
)

dbListTables(tracks_connection)

tracks_dataset <- bq_dataset("peskas", "pelagic_data")

tracks_query <- readLines("points-witth-catch.sql") %>% paste(collapse = " ")

peksas_timor_points <- bq_table("peskas", "pelagic_data", "peskas_timor_points")

bq_dataset_query(tracks_dataset,
                 query = tracks_query,
                 destination_table = peksas_timor_points)

dest_uri_gstorage <- paste0("gs://timor/timor-all_points_",
                            as.character(Sys.Date()),
                            "_*.csv.gz")

bq_table_save(peksas_timor_points,
              destination_format = "CSV",
              compression = "GZIP",
              destination_uris = dest_uri_gstorage)

bq_table_delete(peksas_timor_points)
