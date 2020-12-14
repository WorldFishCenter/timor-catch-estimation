# Prepare workspace -------------------------------------------------------

library(magrittr)
library(drake)

# load functions
f <- lapply(list.files(path = here::here("R"), full.names = TRUE,
                       include.dirs = TRUE, pattern = "*.R"), source)

# Plan analysis ------------------------------------------------------------

read_data <- drake_plan(
  boats_pds = clean_boats(file_in("data/raw/timor-boats.csv")),
  trips_pds = clean_trips(file_in("data/raw/timor-trips.csv")),
  trips_from_points = clean_trips_from_points(file_in("data/processed/trips.csv")),
  peskadat_species = clean_peskasdat_species(file_in("data/raw/peskaDAT-species.csv")),
  peskadat_boats = clean_peskasdat_boats(file_in("data/raw/peskaDAT-boats.csv")),
  kobo_survey_2 = read_kobo_survey_2(file_in("data/raw/catch_timor_structured.csv")),
)

pre_process_data <- drake_plan(
  kobo_catch_2 = clean_catch_kobo2(kobo_survey_2, peskadat_species),
  kobo_trips_2 = clean_trips_kobo2(kobo_survey_2, kobo_catch_2, peskadat_boats),
  all_trips = merge_trips(kobo_trips_2, trips_from_points),
)

fit_models <- drake_plan(
  vessel_activity_model = model_vessel_activity(trips_from_points,
                                                 peskadat_boats,
                                                 boats_pds),
)

full_plan <- rbind(
  read_data, pre_process_data, fit_models
)

# Execute plan ------------------------------------------------------------

if (!is.null(full_plan)) {
  make(full_plan)
}
