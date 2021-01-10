model_trip_gear <- function(kobo_trips,
         peskadat_stations,
         period_static_unit = "month",
         period_seasonal_function = lubridate::month){

  suppressPackageStartupMessages({
    library(tidyverse)
    library(brms)
  })

  # drake::loadd(kobo_trips, peskadat_stations)

  gear_freq_data <- kobo_trips %>%
    mutate(period_static = lubridate::floor_date(trip_date,
                                                 unit = period_static_unit),
           period_seasonal = period_seasonal_function(trip_date),
           period_seasonal = as.character(period_seasonal),
           period_static = as.character(period_static),
           wday = lubridate::wday(trip_date, label = T)) %>%
    left_join(peskadat_stations, by = c("trip_landing_site" = "station_code")) %>%
    filter(!is.na(municipality_name), !is.na(trip_gear), !is.na(boat_type)) %>%
    group_by(municipality_name, boat_type, period_static, period_seasonal, trip_gear) %>%
    tally() %>%
    group_by(municipality_name, boat_type, period_static, period_seasonal) %>%
    mutate(size = sum(n)) %>%
    pivot_wider(names_from = "trip_gear", names_prefix = "tg.",
                values_from = "n", values_fill = 0) %>%
    ungroup() %>%
    mutate(trip_gear = cbind(tg.GN, tg.CN, tg.HL, tg.LL, tg.SG, tg.BS, tg.MC, tg.SN, tg.TP)) %>%
    slice_sample(prop = 1)

  m0 <- brm(bf(trip_gear | trials(size)  ~  (1 | a | municipality_name) +
                 (1 | b | period_static) +
                 (1 | c | boat_type)), data = gear_freq_data,
            family = multinomial(),
            chains = 2,
            cores = 4,
            backend = "cmdstanr", threads = threading(2),
            iter = 1500,
            # control = list(max_treedepth = 12, adapt_delta = 0.98),
            warmup = 1000)

  # add_fitted_draws(newdata = tibble(size = 1),
  #                  model = m0) %>%
  #   ungroup() %>%
  #   mutate(.category = fct_reorder(.category, .value)) %>%
  #   ggplot(aes(x = .category, y = .value)) +
  #   stat_pointinterval() +
  #   scale_y_continuous(labels = scales::label_percent()) +
  #   theme_minimal()

  m0
}
