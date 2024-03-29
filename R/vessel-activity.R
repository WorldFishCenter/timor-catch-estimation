format_vessel_activity <- function(trips_from_points, peskadat_boats, boats_pds,
                                   include_last_seen_info = T,
                                   correct_pelagic_empty_days = T,
                                   period_static_unit = "month",
                                   period_seasonal_function = lubridate::month){

  suppressPackageStartupMessages({
    require(dplyr)
    require(tidyr)
  })

  boat_info <- peskadat_boats %>%
    select(boat_id, imei, owner, boat_code, municipality_name, installation_date,
           primary_gear, secondary_gear)

  last_seen_info <- boats_pds %>%
    select(imei, last_seen) %>%
    mutate(trip_end_date_pds = lubridate::as_date(last_seen, tz = "Asia/Dili"),
           last_seen = TRUE)

  vessel_data <- trips_from_points %>%
    filter(!is.na(trip_id_pds)) %>%
    left_join(boat_info, by = c(imei = "imei")) %>%
    group_by(trip_id_pds) %>%
    mutate(n_rows = n()) %>%
    # If there are multiple rows for a pelagic trip, it can be because the same
    # imei device was used in multiple boats. Here chose the one that matches
    # best
    filter(n_rows == 1 |
             ((trip_end_date_pds > installation_date) &
                (trip_end_date_pds - installation_date) ==
                min(trip_end_date_pds - installation_date))) %>%
    select(imei, boat_id_pds, boat_code, municipality_name, trip_start_date_pds,
           trip_end_date_pds, trip_id_pds) %>%
    # If there are multiple trips on a day just select one of them
    group_by(imei, boat_id_pds, trip_end_date_pds) %>%
    slice_head(n = 1)

  # Complete data with absences if last_seen_info is not included, it uses from
  # first to last tracking
  vessel_data_complete <- vessel_data %>%
    {if (include_last_seen_info) { bind_rows(., last_seen_info) } else {.}} %>%
    group_by(imei) %>%
    arrange(trip_end_date_pds, .by_group = TRUE) %>%
    fill(boat_id_pds, boat_code, municipality_name) %>%
    filter(!is.na(boat_code)) %>%
    group_by(imei, boat_id_pds) %>%
    complete(nesting(imei, boat_id_pds, boat_code, municipality_name),
             trip_end_date_pds = full_seq(trip_end_date_pds, 1)) %>%
    mutate(individual_boat = paste(imei, boat_id_pds))

  # There are weird stuff from the Pelagic data when things were not downloaded
  potential_pds_outages <- trips_from_points %>%
    filter(is.na(imei)) %>%
    mutate(previous_day = download_date_pds - 1) %>%
    {c(.$download_date_pds, .$previous_day)} %>%
    unique()

  vessel_data_pds_corrected <- vessel_data_complete %>%
    {if (correct_pelagic_empty_days) {
      filter(., !trip_end_date_pds %in% potential_pds_outages)
    } else {.}}

  vessel_data_pds_corrected %>%
    ungroup() %>%
    mutate(trip_activity = !is.na(trip_id_pds),
           trip_activity = as.numeric(trip_activity)) %>%
    mutate(period_static = lubridate::floor_date(trip_end_date_pds,
                                                 unit = period_static_unit),
           period_seasonal = period_seasonal_function(trip_end_date_pds),
           period_seasonal = as.character(period_seasonal),
           wday = lubridate::wday(trip_end_date_pds, label = T))
}

model_vessel_activity <- function(vessel_activity_bernoulli){

  suppressPackageStartupMessages({
    require(tidyverse)
    require(lme4)
  })

  boat_type <- list(canoe = 1,
                    motor = 2)

  boat_type %>%
    map(~ filter(vessel_activity_bernoulli, boat_code == .)) %>%
    map(~ glmer(trip_activity ~
                  (1 | municipality_name) +
                  (1 | wday) +
                  (1 | period_static) +
                  (1 | municipality_name : period_static) +
                  (1 | period_seasonal) +
                  (1 | individual_boat),
                data = .,
                family = "binomial"))
}

model_vessel_activity_binomial <- function(vessel_activity_bernoulli){

  suppressPackageStartupMessages({
    require(tidyverse)
    require(lme4)
  })

  vessel_data_binomial <- vessel_activity_bernoulli %>%
    group_by(period_static, individual_boat, boat_code, municipality_name,
             period_seasonal) %>%
    summarise(n_days = n(), n_trips = sum(trip_activity), .groups = "drop") %>%
    filter(!is.na(municipality_name))

  boat_type <- list(canoe = 1,
                    motor = 2)

    boat_type %>%
      map(~ filter(vessel_data_binomial, boat_code == .)) %>%
      map(~ glmer(cbind(n_trips, n_days - n_trips) ~
                    (1 | municipality_name) +
                    (1 | period_static) +
                    (1 | municipality_name : period_static) +
                    (1 | period_seasonal) +
                    (1 | individual_boat),
                  data = .,
                  family = "binomial"))

}


model_vessel_activity_binomial_brms <- function(vessel_activity_bernoulli){

  suppressPackageStartupMessages({
    require(brms)
    require(tidyverse)
  })

  vessel_data_binomial <- vessel_activity_bernoulli %>%
    group_by(period_static, individual_boat, boat_code, municipality_name,
             period_seasonal) %>%
    summarise(n_days = n(), n_trips = sum(trip_activity), .groups = "drop") %>%
    filter(!is.na(municipality_name)) %>%
    mutate(period_static = as.character(period_static))

  boat_type <- list(canoe = 1,
                    motor = 2)

  boat_type %>%
    map(~ filter(vessel_data_binomial, boat_code == .)) %>%
    map(~ brms::brm(n_trips | trials(n_days) ~
                      (1 | municipality_name) +
                      (1 | period_static) +
                      (1 | municipality_name : period_static) +
                      (1 | period_seasonal) +
                      (1 | individual_boat),
                    data = .,
                    cores = 4,
                    iter = 1250,
                    warmup = 1000,
                    control = list(adapt_delta = 0.9,
                                   max_treedepth = 12),
                    family = zero_inflated_binomial))

}

model_vessel_activity_binomial_brms2 <- function(vessel_activity_bernoulli){

  suppressPackageStartupMessages({
    require(brms)
    require(tidyverse)

  })

  vessel_data_binomial <- vessel_activity_bernoulli %>%
    group_by(period_static, individual_boat, boat_code, municipality_name,
             period_seasonal) %>%
    summarise(n_days = n(), n_trips = sum(trip_activity), .groups = "drop") %>%
    filter(!is.na(municipality_name)) %>%
    mutate(period_static = as.character(period_static))

  boat_type <- list(canoe = 1,
                    motor = 2)

  boat_type %>%
    map(~ filter(vessel_data_binomial, boat_code == .)) %>%
    map(~ brms::brm(n_trips | trials(n_days) ~
                      (1 | municipality_name) +
                      (1 | period_static) +
                      (1 | municipality_name : period_static) +
                      (1 | period_seasonal) +
                      (1 | individual_boat),
                    data = .,
                    cores = 4,
                    iter = 1250,
                    warmup = 1000,
                    family = binomial))

}

model_vessel_activity_binomial_brms3 <- function(vessel_activity_bernoulli){

  suppressPackageStartupMessages({
    require(brms)
    require(tidyverse)

  })

  vessel_data_binomial <- vessel_activity_bernoulli %>%
    group_by(period_static, individual_boat, boat_code, municipality_name,
             period_seasonal) %>%
    summarise(n_days = n(), n_trips = sum(trip_activity), .groups = "drop") %>%
    filter(!is.na(municipality_name)) %>%
    mutate(period_static = as.character(period_static))

  d_multivariate <- vessel_data_binomial %>%
    pivot_wider(id_cols = c(period_static, individual_boat, municipality_name, period_seasonal),
                names_from = boat_code,
                values_from = c(n_days, n_trips))

  canoe_formula <- bf(n_trips_1 | trials(n_days_1) ~
                        (1 | a | municipality_name) +
                        (1 | b | period_static) +
                        (1 | c | municipality_name : period_static) +
                        (1 | d | period_seasonal) +
                        (1 | individual_boat))

  motor_formula <- bf(n_trips_2 | trials(n_days_2) ~
                        (1 | a | municipality_name) +
                        (1 | b | period_static) +
                        (1 | c | municipality_name : period_static) +
                        (1 | d | period_seasonal) +
                        (1 | individual_boat))

  brm(formula = mvbf(canoe_formula, motor_formula),
      data = d_multivariate,
      cores = 4,
      iter = 1250,
      warmup = 1000,
      control = list(adapt_delta = 0.9,
                                   max_treedepth = 12),
      family = zero_inflated_binomial)
}



function(all_trips, peskadat_boats, boats, last_seen_info = TRUE){



  nd <- vessel_data_binomial %>%
    filter(boat_code == 1) %>%
    distinct(boat_code, period_static, municipality_name, period_seasonal)


  predict_1 <- function(x){


    predict(x,
            newdata = nd,
            re.form = ~
              (1 | municipality_name) +
              (1 | period_static) +
              (1 | municipality_name : period_static) +
              (1 | period_seasonal),
            allow.new.levels = T)
  }

  system.time({
    confint_u <- bootMer(m0f_b1, predict_1, nsim = 100, parallel = "multicore",
                         ncpus = 4, use.u = T)
  })

  system.time({
    confint <- bootMer(m0f_b1, predict_1, nsim = 100, parallel = "multicore",
                       ncpus = 4, use.u = F)
  })










  vessel_data_binomial <- vessel_data_complete %>%
    group_by(month_static, imei, boat_code, municipality_name, month) %>%
    summarise(n_days = n(), n_trips = sum(trip_activity)) %>%
    filter(!is.na(municipality_name)) %>%
    ungroup()

  vessel_data_binomial2 <- vessel_data_binomial %>%
    mutate(month_static = as.character(month_static))

  require(brms)


  m0 <- brm(trip_activity ~ (1 | imei) +
              (boat_code | municipality_name : month_static) +
              (1 | month) + (1 | wday),
            data = vessel_data_complete,
            family = bernoulli,
            cores = 4)

  m1 <- brm(trip_activity ~
              (boat_code | municipality_name : month_static) +
              (1 | month) + (1 | wday),
            data = vessel_data_complete,
            family = bernoulli,
            cores = 4)

  m2 <- brm(trip_activity ~
              (1 | imei) +
              # (1 | municipality_name) +
              # (1 | month_static) +
              # (1 | municipality_name : month_static) +
              (1 | month),
            data = vessel_data_complete,
            family = bernoulli,
            cores = 4)

  m3_1 <- brm(n_trips | trials(n_days) ~
                (1 | municipality_name) +
                (1 | month_static) +
                (1 | municipality_name : month_static) +
                (1 | month) +
                (1 | imei),
              data = filter(vessel_data_binomial2, boat_code == 1),
              cores = 4,
              family = binomial)

  m3_2 <- brm(n_trips | trials(n_days) ~
                (1 | municipality_name) +
                (1 | month_static) +
                (1 | municipality_name : month_static) +
                (1 | month) +
                (1 | imei),
              data = filter(vessel_data_binomial2, boat_code == 2),
              cores = 4,
              family = binomial)

  m3 <- brm(n_trips | trials(n_days) ~ boat_code +
              (boat_code | municipality_name) +
              (boat_code | month_static) +
              (boat_code | municipality_name : month_static) +
              (boat_code | month) +
              (1 | imei),
            data = vessel_data_binomial2,
            cores = 4,
            family = binomial)

  require(lme4)

  m0f_b1 <- glmer(cbind(n_trips, n_days - n_trips) ~  (1 | municipality_name) +
                    (1 | period_static) +
                    (1 | municipality_name : period_static) +
                    (1 | period_seasonal) +
                    (1 | imei),
               data = filter(vessel_data_binomial, boat_code == 1),
               family = "binomial")

  m0f_b2 <- glmer(cbind(n_trips, n_days - n_trips) ~  (1 | municipality_name) +
                    (1 | period_static) +
                    (1 | municipality_name : period_static) +
                    (1 | period_seasonal) +
                    (1 | imei),
                  data = filter(vessel_data_binomial, boat_code == 2),
                  family = "binomial")

  m0f_b2 <- glmer(cbind(n_trips, n_days - n_trips) ~  (1 | municipality_name) +
                    (1 | period_static) +
                    (1 | municipality_name : period_static) +
                    (1 | period_seasonal) +
                    (1 | imei),
                  data = filter(vessel_data_binomial, boat_code == 2),
                  family = "binomial")

  nd_b1 <- vessel_data_binomial %>%
    distinct(boat_code, month_static, municipality_name, month) %>%
    filter(boat_code == 1)

  p_1 <- predict(m0f_b1,
          newdata = nd_b1,
          re.form = ~
            (1 | municipality_name) +
            (1 | month_static) +
            (1 | municipality_name : month_static) +
            (1 | month),
          allow.new.levels = T)

  a <- posterior_epred(m3_1, newdata = nd_b1,
                   re_formula = ~ (1 | municipality_name) +
                     (1 | month_static) +
                     (1 | municipality_name : month_static) +
                     (1 | month))

  nd_b2 <- vessel_data_binomial %>%
    distinct(boat_code, month_static, municipality_name, month) %>%
    filter(boat_code == 2)

  p_2 <- predict(m0f_b2,
                 newdata = nd_b2,
                 re.form = ~
                   (1 | municipality_name) +
                   (1 | month_static) +
                   (1 | municipality_name : month_static) +
                   (1 | month),
                 allow.new.levels = T)


  require(ggplot2)

  nd_b1 %>%
    bind_rows(nd_b2) %>%
    mutate(estimate = plogis(c(p_1, p_2))) %>%
    ggplot(aes(x = lubridate::as_date(month_static), y = estimate)) +
    geom_line(aes(colour = as.factor(boat_code))) +
    facet_wrap(~ municipality_name)

}
