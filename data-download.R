library(DBI)
library(bigrquery)
library(tidyverse)

# This script helps download the data used for this project but manual input is
# necessary. Authentication with a Google account with access to the datasets in
# needed. Catch data is downloaded automatically. Track points data is exported
# from BigQuery into into a series of files in Google Cloud Storage and then
# should be downloaded manually. Trip data is downloaded manually from
# https://analytics.pelagicdata.com/ filtering for MAF/WorldFish as customer and
# date after 2019-04-01 which is roughly when catch collection starts

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

tracks_query <-
"SELECT *
FROM `pelagic_data_raw`
WHERE (`time` >= '2019-04-20T00:00:00Z')"

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

bq_table_delete(peskas_timor_points)

# Download manualluy using gsutil:
# gsutil cp \
# gs://timor/timor-all_points_2020-12-03_000000000000.csv.gz \
# gs://timor/timor-all_points_2020-12-03_000000000001.csv.gz \
# gs://timor/timor-all_points_2020-12-03_000000000002.csv.gz \
# gs://timor/timor-all_points_2020-12-03_000000000003.csv.gz \
# gs://timor/timor-all_points_2020-12-03_000000000004.csv.gz \
# gs://timor/timor-all_points_2020-12-03_000000000005.csv.gz \
# gs://timor/timor-all_points_2020-12-03_000000000006.csv.gz \
# gs://timor/timor-all_points_2020-12-03_000000000007.csv.gz \
# gs://timor/timor-all_points_2020-12-03_000000000008.csv.gz \
# gs://timor/timor-all_points_2020-12-03_000000000009.csv.gz \
# gs://timor/timor-all_points_2020-12-03_000000000010.csv.gz \
# gs://timor/timor-all_points_2020-12-03_000000000011.csv.gz \
# gs://timor/timor-all_points_2020-12-03_000000000012.csv.gz \
# gs://timor/timor-all_points_2020-12-03_000000000013.csv.gz \
# .
