widen_trip_catch <- function(kobo_trips,
                             kobo_catch,
                             n_top_species = 2){

  suppressPackageStartupMessages({
    library(tidyverse)
  })

  catch_wide <- kobo_catch %>%
    group_by(record_id) %>%
    arrange(desc(weight), .by_group = T) %>%
    mutate(species_rank = 1:n(),
           species_rank = as.character(species_rank)) %>%
    ungroup() %>%
    mutate(species_rank = fct_lump_n(species_rank, n_top_species, other_level = "other")) %>%
    group_by(record_id, species_rank) %>%
    mutate(species_code = if_else(species_rank == "other" & n() > 1,
                                  "other", species_code)) %>%
    group_by(record_id, species_rank, species_code) %>%
    summarise(weight = sum(weight, na.rm = T), .groups = "drop") %>%
    pivot_wider(record_id, names_from = species_rank,
                values_from = c(species_code, weight),
                values_fill = list(species_code = "none", weight = 0))

  kobo_trips %>%
    ungroup() %>%
    mutate(trip_price = trip_value / trip_catch) %>%
    left_join(catch_wide, by = "record_id")
}


model_species_price <- function(kobo_trip_catch_wide,
                                peskadat_stations,
                                period_static_unit = "month",
                                period_seasonal_function = lubridate::month){

  suppressPackageStartupMessages({
    library(brms)
  })

  model_data <- kobo_trip_catch_wide %>%
    dplyr::filter(is.finite(trip_price),
                  !is.na(trip_price),
                  trip_platform == "boat") %>%
    dplyr::left_join(peskadat_stations, by = c("trip_landing_site" = "station_code")) %>%
    dplyr::mutate(period_static = lubridate::floor_date(trip_date,
                                                        unit = period_static_unit),
                  period_static = as.character(period_static),
                  period_seasonal = period_seasonal_function(trip_date),
                  period_seasonal = as.character(period_seasonal),
                  wday = lubridate::wday(trip_date, label = T))

  brm(trip_price ~
              (1 | mm(species_code_1,
                      species_code_2,
                      species_code_other,
                      weights = cbind(weight_1,
                                      weight_2,
                                      weight_other))),
            data = model_data,
            family = student(link = "log"),
            control = list(max_treedepth = 12),
            cores = 4,
            iter = 2000,
            warmup = 1000)

}

add_price_flags <- function(kobo_trip_catch_wide, species_price_model){

  suppressPackageStartupMessages({
    library(dplyr)
  })

  pred_data <- kobo_trip_catch_wide %>%
    filter(is.finite(trip_price),
           !is.na(trip_price)) %>%
    arrange(desc(trip_price))

  residuals(species_price_model,
            pred_data[1:10, ],
            allow_new_levels = T,
            scale = "response")

  predicted_prices <- fitted(species_price_model,
                             pred_data,
                             allow_new_levels = T,
                             scale = "response")

  residual_factor <- predicted_prices %>%
    as_tibble() %>%
    bind_cols(pred_data) %>%
    mutate(trip_price_residual_factor = pmin(abs(log(trip_price/Q2.5)),
                                             abs(log(trip_price/Q97.5))),
           trip_price_residual_factor2 = trip_price_residual_factor *
             sign(-abs(log(trip_price/Q97.5))+abs(log(trip_price/Q2.5))),
           trip_price_residual_factor = exp(trip_price_residual_factor2)) %>%
    select(record_id, trip_price_residual_factor) %>%
    mutate(price_flag = case_when(trip_price_residual_factor > 5  ~
                                    "trip_catch__price_per_kg_extremely_large",
                                  trip_price_residual_factor < 1/5 ~
                                    "trip_catch__price_per_kg_extremely_small",
                                  trip_price_residual_factor > 3  ~
                                    "trip_catch__price_per_kg_too_large",
                                  trip_price_residual_factor < 1/3 ~
                                    "trip_catch__price_per_kg_too_small",
                                  TRUE ~ NA_character_))

  kobo_trip_catch_wide %>%
    left_join(residual_factor, by = "record_id") %>%
    rowwise() %>%
    mutate(flags = paste(na.omit(c(flags,
                                   price_flag)), collapse = ";")) %>%
    ungroup() %>%
    select(-ends_with("_flag")) %>%
    mutate(flags = if_else(flags == "", NA_character_, flags))
}

model_catch <- function(kobo_trips_with_price_tags,
                        peskadat_stations,
         period_static_unit = "month",
         period_seasonal_function = lubridate::month){

  suppressPackageStartupMessages({
    library(tidyverse)
    library(brms)
  })

  model_data <- kobo_trips_with_price_tags %>%
    filter(!str_detect(flags, "price_per_kg_") | is.na(flags)) %>%
    mutate(period_static = lubridate::floor_date(trip_date,
                                                 unit = period_static_unit),
           period_seasonal = period_seasonal_function(trip_date),
           period_seasonal = as.character(period_seasonal),
           wday = lubridate::wday(trip_date, label = T)) %>%
    left_join(peskadat_stations, by = c("trip_landing_site" = "station_code")) %>%
    filter(!is.na(municipality_name), !is.na(trip_gear), !is.na(boat_type)) %>%
    mutate(trip_catch_po = round(trip_catch*10),
           trip_catch_po_3 = round((trip_catch*1000)^(1/3)),
           period_static = as.character(period_static)) %>%
    slice_sample(prop = 1)
#
#   zi3 <- brm(trip_catch_po_3 ~ (1 | trip_gear) +
#                (1 | municipality_name) +
#                (1 | boat_type) + (1 | period_static),
#            data = model_data,
#            prior = c(prior(normal(3, 3), class = Intercept),
#                      prior(student_t(3, 0, 1.5), class = sd)),
#            family = zero_inflated_poisson,
#            chains = 2, cores = 2,
#            backend = "cmdstanr", threads = threading(2),
#            iter = 1500,
#            control = list(max_treedepth = 15, adapt_delta = 0.95),
#            warmup = 1000)
#
#   p3o <- brm(trip_catch_po_3 ~ (1 | trip_gear) +
#                 (1 | municipality_name) +
#                 (1 | boat_type) + (1 | period_static) + (1| record_id),
#               data = model_data,
#               prior = c(prior(normal(3, 2), class = Intercept),
#                         prior(student_t(3, 0, 1.5), class = sd)),
#               family = poisson,
#               chains = 2, cores = 2,
#               backend = "cmdstanr", threads = threading(2),
#               iter = 1500,
#               control = list(max_treedepth = 15, adapt_delta = 0.95),
#               warmup = 1000)

  # zi3o <-
  brm(trip_catch_po_3 ~ (1 | trip_gear) +
        (1 | municipality_name) +
        (1 | boat_type) + (1 | period_static) + (1| record_id),
      data = model_data,
      prior = c(prior(normal(3, 2), class = Intercept),
                prior(student_t(3, 0, 1.5), class = sd)),
      family = zero_inflated_poisson,
      chains = 2,
      cores = 4,
      backend = "cmdstanr", threads = threading(2),
      iter = 1500,
      control = list(max_treedepth = 12, adapt_delta = 0.98),
      warmup = 1000)

#
#   add_fitted_draws(expand_grid(boat_type = unique(cm2[[3]]$data$boat_type)),
#                    cm2[[3]], re_formula = ~ (1 | boat_type)) %>%
#     ungroup() %>%
#     mutate(x = boat_type) %>%
#     mutate(x = fct_reorder(x, .value)) %>%
#     ggplot(aes(x = (.value^3)/1000, y = x)) +
#     stat_pointinterval()
#
#   gather_draws(cm2[[3]], r_boat_type[x,Intercept]) %>%
#     ungroup() %>%
#     # mutate(x = str_extract(x, "-[0-9]{2}-")) %>%
#     mutate(x = fct_reorder(as.character(x), .value)) %>%
#     ggplot(aes(x = .value, y = x)) +
    # stat_pointinterval()

  # list(zi3, p3o, zi3o)

}

model_value <- function(kobo_trips_with_price_tags,
                        peskadat_stations,
                        period_static_unit = "month",
                        period_seasonal_function = lubridate::month){

  suppressPackageStartupMessages({
    library(tidyverse)
    library(brms)
  })

  model_data <- kobo_trips_with_price_tags %>%
    filter(!str_detect(flags, "price_per_kg_") | is.na(flags)) %>%
    mutate(period_static = lubridate::floor_date(trip_date,
                                                 unit = period_static_unit),
           period_seasonal = period_seasonal_function(trip_date),
           period_seasonal = as.character(period_seasonal),
           wday = lubridate::wday(trip_date, label = T)) %>%
    left_join(peskadat_stations, by = c("trip_landing_site" = "station_code")) %>%
    filter(!is.na(municipality_name), !is.na(trip_gear), !is.na(boat_type)) %>%
    mutate(trip_catch_po = round(trip_catch*10),
           trip_catch_po_3 = round((trip_catch*1000)^(1/3)),
           period_static = as.character(period_static),
           trip_value_po_3 = round((trip_value*1000)^(1/3))) %>%
    slice_sample(prop = 1)


    # zi3 <- brm(trip_value_po_3 ~ (1 | trip_gear) +
    #              (1 | municipality_name) +
    #              (1 | boat_type) + (1 | period_static),
    #          data = model_data,
    #          prior = c(prior(normal(3, 3), class = Intercept),
    #                    prior(student_t(3, 0, 1.5), class = sd)),
    #          family = zero_inflated_poisson,
    #          chains = 2, cores = 2,
    #          backend = "cmdstanr", threads = threading(2),
    #          iter = 1500,
    #          control = list(max_treedepth = 15, adapt_delta = 0.95),
    #          warmup = 1000)
    #
    # p3o <- brm(trip_value_po_3 ~ (1 | trip_gear) +
    #               (1 | municipality_name) +
    #               (1 | boat_type) + (1 | period_static) + (1| record_id),
    #             data = model_data,
    #             prior = c(prior(normal(3, 2), class = Intercept),
    #                       prior(student_t(3, 0, 1.5), class = sd)),
    #             family = poisson,
    #             chains = 2, cores = 2,
    #             backend = "cmdstanr", threads = threading(2),
    #             iter = 1500,
    #             control = list(max_treedepth = 15, adapt_delta = 0.95),
    #             warmup = 1000)

  # zi3o <-
  brm(trip_value_po_3 ~ (1 | trip_gear) +
        (1 | municipality_name) +
        (1 | boat_type) + (1 | period_static) + (1| record_id),
      data = model_data,
      prior = c(prior(normal(3, 2), class = Intercept),
                prior(student_t(3, 0, 1.5), class = sd)),
      family = zero_inflated_poisson,
      chains = 2, cores = 2,
      backend = "cmdstanr", threads = threading(2),
      iter = 1500,
      control = list(max_treedepth = 15, adapt_delta = 0.98),
      warmup = 1000)

  # list(zi3, p3o, zi3o)

}

function(kobo_trips_2,
         kobo_catch_2,
         peskadat_municipalities,
         kobo_trips_with_price_tags,
         period_static_unit = "month",
         period_seasonal_function = lubridate::month){
  kobo_trips <- kobo_trips_2
  kobo_catch <- kobo_catch_2

  library(lme4)
  library(brms)
  library(tidyverse)

  d <- kobo_trips %>%
    filter(trip_platform == "boat") %>%
    ungroup() %>%
    mutate(period_static = lubridate::floor_date(trip_date,
                                                 unit = period_static_unit),
           period_seasonal = period_seasonal_function(trip_date),
           period_seasonal = as.character(period_seasonal),
           wday = lubridate::wday(trip_date, label = T)) %>%
    filter(!is.na(trip_catch) & !is.na(trip_value)) %>%
    left_join(peskadat_municipalities, by = c("trip_landing_site" = "municipality_code")) %>%
    mutate(trip_catch_gr = round(trip_catch*10),
           period_static = as.character(period_static),
           price_per_kg = trip_value/trip_catch)


  catch_wide <- kobo_catch_2 %>%
    group_by(record_id) %>%
    arrange(desc(weight), .by_group = T) %>%
    mutate(species_rank = 1:n(),
           species_rank = as.character(species_rank)) %>%
    ungroup() %>%
    mutate(species_rank = fct_lump_n(species_rank, 2, other_level = "other")) %>%
    group_by(record_id, species_rank) %>%
    mutate(species_code = if_else(species_rank == "other" & n() > 1,
                                  "other", species_code)) %>%
    group_by(record_id, species_rank, species_code) %>%
    summarise(weight = sum(weight, na.rm = T), .groups = "drop") %>%
    pivot_wider(record_id, names_from = species_rank,
                values_from = c(species_code, weight), values_fill = list(species_code = "none", weight = 0))

  set.seed(123)
  d2 <- d %>%
    left_join(catch_wide) %>%
    group_by(municipality_name, trip_gear, boat_type, period_static) %>%
    # slice_sample(prop = 0.25) %>%
    filter(is.finite(price_per_kg))


  # mpg0 <- brm(price_per_kg ~ (1|species_code_1), data = d2)
  # mpgl0 <- brm(price_per_kg ~ (1|species_code_1), data = d2, family = gaussian(link = "log"))
  # mps0 <- brm(price_per_kg ~ (1|species_code_1), data = d2, family = student)
  mpsl0 <- brm(price_per_kg ~ (1|mm(species_code_1, species_code_2, species_code_other, weights = cbind(weight_1, weight_2, weight_other))),
               data = d2,
               family = student(link = "log"),
               cores = 4,
               iter = 2000,
               # control = list(max_treedepth = 15slice_sample),
               warmup = 1000)


  library(tidybayes)
  library(ggdist)

  ?posterior_epred()

  add_fitted_draws(mpsl0,
                   newdata = tibble(species_code_1 = c(unique(d2$species_code_1), "other"),
                                    species_code_2 = "999",
                                    species_code_other = "other",
                                    weight_1 = 1,
                                    weight_2 = 0,
                                    weight_other = 0),
                   allow_new_levels = T,
                   prediction = ".value") %>%
    left_join(peskadat_species, by = c("species_code_1" = "species_code")) %>%
    ungroup() %>%
    mutate(category = fct_reorder(category, .value)) %>%
    ggplot(aes(x = .value, y = category)) +
    stat_pointinterval(.width = c(0.66, 0.95)) +
    theme_minimal()

  add_fitted_draws(mpsl0,
                   newdata = tibble(species_code_1 = c(unique(d2$species_code_1), "other"),
                                    species_code_2 = "999",
                                    species_code_other = "other",
                                    weight_1 = 1,
                                    weight_2 = 0,
                                    weight_other = 0))  %>%
    mean_qi()



  m <- brm(trip_catch_gr ~
             (1 | trip_gear) +
             (1 | municipality_name) +
             (1 | boat_type) +
             (1 | period_static),
           data = d2,
           family = poisson,
           cores = 4,
           iter = 1250,
           # control = list(max_treedepth = 15slice_sample),
           warmup = 1000)
}
