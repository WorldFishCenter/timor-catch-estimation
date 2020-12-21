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
  peskadat_municipalities = clean_peskadat_municipalities(file_in("data/raw/peskaDAT-municipalities.csv")),
  kobo_survey_2 = read_kobo_survey_2(file_in("data/raw/catch_timor_structured.csv")),
)

pre_process_data <- drake_plan(
  kobo_catch_2 = clean_catch_kobo2(kobo_survey_2, peskadat_species),
  kobo_trips_2 = clean_trips_kobo2(kobo_survey_2, kobo_catch_2, peskadat_boats),
  kobo_catch = kobo_catch_2,
  kobo_trips = kobo_trips_2,
  kobo_trip_catch_wide = widen_trip_catch(kobo_trips, kobo_catch, n_top_species = 2),
  all_trips = merge_trips(kobo_trips_2, trips_from_points),
)

fit_models <- drake_plan(
  vessel_activity_bernoulli = format_vessel_activity(trips_from_points, peskadat_boats, boats_pds,
                                                     include_last_seen_info = T,
                                                     correct_pelagic_empty_days = T,
                                                     period_static_unit = "week",
                                                     period_seasonal_function = lubridate::week),
  vessel_activity_bernoulli_m = format_vessel_activity(trips_from_points, peskadat_boats, boats_pds,
                                                     include_last_seen_info = T,
                                                     correct_pelagic_empty_days = T,
                                                     period_static_unit = "month",
                                                     period_seasonal_function = lubridate::month),
  vessel_activity_bernoulli_q = format_vessel_activity(trips_from_points, peskadat_boats, boats_pds,
                                                     include_last_seen_info = T,
                                                     correct_pelagic_empty_days = T,
                                                     period_static_unit = "quarter",
                                                     period_seasonal_function = lubridate::quarter),
  vessel_activity_model = model_vessel_activity_binomial(vessel_activity_bernoulli),
  vessel_activity_model_brms = model_vessel_activity_binomial_brms(vessel_activity_bernoulli_m),
  vessel_activity_model_brms2 = model_vessel_activity_binomial_brms2(vessel_activity_bernoulli_m),
  # vessel_activity_model_brms3 = model_vessel_activity_binomial_brms3(vessel_activity_bernoulli_m),
  vessel_activity_model_m = model_vessel_activity_binomial(vessel_activity_bernoulli_m),
  vessel_activity_model_q = model_vessel_activity_binomial(vessel_activity_bernoulli_q),
  species_price_model = target(command = model_species_price(kobo_trip_catch_wide,
                                                             peskadat_municipalities),
                               trigger = trigger(depend = T))
)

notebooks <- drake_plan(
  vessel_activity_nb = target(rmarkdown::render(knitr_in("notebooks/vessel-activity.Rmd"))),
  catch_value_nb = target(rmarkdown::render(knitr_in("notebooks/catch-value.Rmd"))),
)

full_plan <- bind_plans(
  read_data, pre_process_data, fit_models, notebooks
)

# Execute plan ------------------------------------------------------------

if (!is.null(full_plan)) {
  make(full_plan, lock_envir = F)
}
