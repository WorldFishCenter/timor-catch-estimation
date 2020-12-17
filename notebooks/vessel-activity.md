Vessel activity
================

Here we look, on a high level, at the tracks data used to determine
vessel activity. As well as the results from the vessel activity
coefficient modelling.

## Summary

  - There are multiple issues with the trip detection algorithm and
    device operation that could affect the activity estimates. Until
    these issues are resolved, activity estimates should be seen as
    provisional.
  - In most locations, activity of motor boats appears to be higher than
    Canoe boats.
  - Canoe boats are under-represented from tracking. About 40% of
    trackers devices have been installed in canoes, but they constitute
    about 70% of the fishing fleet. The disparity is larger in some
    municipalities.
  - The number of operational tracking units is steadily declining.
    Sampling in some locations for at least one boat type is already
    nil. At the current rate, without new trackers added, the ability to
    generate activity estimates at the national level might be severely
    compromised in 1 to 3 years time. 
  - There are large gaps in data availability in tracking data. The
    origin of these gaps is unknown and until we know more about it, it
    may call to question the reliability of the service. 
  - A significant number of boat trackers stopped being operational (last
    heard of) simultaneously. These events are prominent at the
    beginning of the first quarter 2019 and the beginning of the second
    quarter 2020. The reason for this decline is currently unknown and
    warrants further investigation of the failure rate of tracking
    devices. #did you ask PDS about this when you contacted them? I dont remember seeing it
  - New devices should be deployed aiming to improve coverage of
    under-sampled municipalities and boat types. #great yes - we tried to do this with Joctan previously in a non-quantitative way, but having this specific guidance would be very useful to guide further installations
  - The tracking data highlights some potential inaccuracies with the
    fisher/boat counts per municipality. In one of the locations the
    number of tracking devices installed is larger than the recorded
    number of boats. #haha brilliant. I never noticed this.
  - Substantial improvements in the PDS detection algorithm or
    sufficient post-processing methods are needed before it can be used
    ready for fisher-level analytics. #do you think these are things that we should be working on, or do you think we could provide them with some of these tasks for the Inspire challenge work?

## Plots

``` r
drake::loadd(vessel_activity_bernoulli)

vessel_activity_bernoulli %>%
  group_by(imei, boat_id_pds, boat_code, municipality_name) %>%
  summarise(installation = min(trip_end_date_pds), 
            last_heard = max(trip_end_date_pds), .groups = "drop") %>%
  mutate(boat_id_pds = fct_reorder(boat_id_pds, installation, .fun = min, .desc = T),
         municipality_name = fct_reorder(municipality_name, installation, .fun = min, .desc = F)) %>%
  ggplot(aes(y = boat_id_pds, colour = boat_code)) +
  geom_segment(aes(x = installation, xend = last_heard, yend = boat_id_pds)) +
  facet_grid(municipality_name ~ ., scales = "free", space = "free", margins = F) +
  scale_color_brewer(palette = "Set1", labels = c("Canoe", "Motor"), name = "Boat type") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), 
        panel.grid.major.y = element_blank(), 
        legend.position = "top", 
        axis.title.x = element_blank()) +
  labs(title = "Operation time for installed PDS trackers in Timor", 
       caption = "Operation time goes from the time of first trip until the time the tracker was last detected",
       legend = "Boat type",
       y = "Individual boat")
```

![](vessel-activity_files/figure-gfm/operation-per-boat-1.png)<!-- -->

``` r
vessel_activity_bernoulli %>%
  complete(period_static, boat_code) %>%
  group_by(period_static, boat_code) %>%
  summarise(n_boats = n_distinct(boat_id_pds, na.rm = T), .groups = "drop") %>%
  mutate(boat_code = fct_reorder(boat_code, n_boats)) %>%
  ggplot(aes(x = period_static, y = n_boats, fill = boat_code)) +
  geom_col() +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1", labels = c("Canoe", "Motor"), name = "Boat type") +
  labs(title = "Number of boats tracked")
```

![](vessel-activity_files/figure-gfm/operation-all-boats-1.png)<!-- -->

``` r
drake::loadd(peskadat_municipalities)

census <- peskadat_municipalities %>%
  select(-fishers) %>%
  pivot_longer(c(canoes, motors), "boat_code") %>%
  mutate(boat_code = case_when(boat_code == "canoes" ~ "1", 
                               boat_code == "motors" ~ "2"))

boats_location_data <-vessel_activity_bernoulli %>%
  complete(municipality_name, period_static, boat_code) %>%
  group_by(municipality_name, period_static, boat_code) %>%
  summarise(n_boats = n_distinct(boat_id_pds, na.rm = T), .groups = "drop") %>%
  left_join(census, by = c("municipality_name", "boat_code")) %>%
  mutate(prop_boats = n_boats / value, 
         municipality_name = fct_reorder(municipality_name, n_boats,.fun = max, .desc = T))

proportion_plot <- boats_location_data %>%
  ggplot(aes(x = period_static, y = prop_boats, colour = boat_code)) +
  geom_step() +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap("municipality_name", ncol = 1, scales = "free") +
  scale_colour_brewer(palette = "Set1", labels = c("Canoe", "Motor"), name = "Boat type") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Proportion of boats tracked")

count_plot <- boats_location_data %>%
  ggplot(aes(x = period_static, y = n_boats, fill = boat_code)) +
  geom_col() +
  facet_wrap("municipality_name", ncol = 1, scales = "free") +
  scale_fill_brewer(palette = "Set1", labels = c("Canoe", "Motor"), name = "Boat type") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Number of boats tracked")

cowplot::plot_grid(count_plot, proportion_plot, ncol = 2)
```

![](vessel-activity_files/figure-gfm/operation-per-location%20-1.png)<!-- -->

``` r
drake::loadd(vessel_activity_model_brms)

monthly_predictions_overall <- function(x, y){
  nd <- x$data %>%
    distinct(period_static) %>%
    mutate(data_available = TRUE, 
           period_static = lubridate::as_date(period_static)) %>%
    complete(period_static = seq(min(period_static), 
                                                    max(period_static), "month"), 
             fill = list(data_available = FALSE)) %>%
    mutate(period_seasonal = lubridate::month(period_static), 
           n_days = lubridate::days_in_month(period_seasonal),
           boat_code = y,
           prediction_id = as.character(1:n())) 
  
  x %>%
    posterior_epred(nd, 
                    re_formula = ~ (1 | period_static) +
                      (1 | period_seasonal), 
                    allow_new_levels = TRUE) %>% 
    set_colnames(as.character(nd$prediction_id)) %>%
    as.data.frame.table() %>%
    dplyr::mutate(prediction_id = Var2) %>%
    dplyr::rename(Estimate = Freq, sample = Var1) %>%
    dplyr::inner_join(nd, by = "prediction_id") %>%
    tibble::as_tibble() %>%
    dplyr::select(-Var2)
}

imap_dfr(vessel_activity_model_brms, monthly_predictions_overall) %>%
  ggplot(aes(x = period_static, y = Estimate, fill = boat_code, colour = boat_code)) + 
  stat_lineribbon(aes(alpha = forcats::fct_rev(ordered(stat(.width)))), 
                  .width = c(0.05, 0.66, 0.95)) +
  facet_grid(cols = vars(boat_code)) +
  scale_fill_brewer(palette = "Set1", aesthetics = c("colour", "fill"), 
                    name = "Boat type") +
  scale_alpha_manual(values = c(0.05, 0.33, 0.95), name = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Estimated average number of trips per month - Overall", 
       caption = "Based on 1000 Markov Chain Monte Carlo simulations of the Dynamic Vessel Activity Coefficient")
```

![](vessel-activity_files/figure-gfm/model-predictions-overall-1.png)<!-- -->

``` r
monthly_predictions_site <- function(x, y){
  nd <- x$data %>%
    distinct(municipality_name, period_static) %>%
    mutate(data_available = TRUE, 
           period_static = lubridate::as_date(period_static)) %>%
    complete(municipality_name, period_static = seq(min(period_static), 
                                                    max(period_static), "month"), 
             fill = list(data_available = FALSE)) %>%
    mutate(period_seasonal = lubridate::month(period_static), 
           n_days = lubridate::days_in_month(period_seasonal),
           boat_code = y,
           prediction_id = as.character(1:n())) 
  
  x %>%
    posterior_epred(nd, 
                    re_formula = ~ (1 | municipality_name) + (1 | period_static) +
                      (1 | municipality_name : period_static) + 
                      (1 | period_seasonal), 
                    allow_new_levels = TRUE) %>% 
    set_colnames(as.character(nd$prediction_id)) %>%
    as.data.frame.table() %>%
    dplyr::mutate(prediction_id = Var2) %>%
    dplyr::rename(Estimate = Freq, sample = Var1) %>%
    dplyr::inner_join(nd, by = "prediction_id") %>%
    tibble::as_tibble() %>%
    dplyr::select(-Var2)
}

imap_dfr(vessel_activity_model_brms, monthly_predictions_site) %>%
  ggplot(aes(x = period_static, y = Estimate, fill = boat_code, colour = boat_code)) + 
  stat_lineribbon(aes(alpha = forcats::fct_rev(ordered(stat(.width)))), 
                  .width = c(0.05, 0.66, 0.95)) +
  facet_wrap(vars(municipality_name), ncol = 3) +
  scale_fill_brewer(palette = "Set1", aesthetics = c("colour", "fill"), 
                    name = "Boat type") +
  scale_alpha_manual(values = c(0.05, 0.33, 0.95), name = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Estimated average number of trips per month - Per site", 
       caption = "Based on 1000 Markov Chain Monte Carlo simulations")
```

![](vessel-activity_files/figure-gfm/model-predictions-persite-1.png)<!-- -->

``` r
predictions_site <- function(x, y){
  nd <- x$data %>%
    distinct(municipality_name) %>%
    mutate(data_available = TRUE) %>%
    complete(municipality_name) %>%
    mutate(n_days = 30,
           boat_code = y,
           prediction_id = as.character(1:n())) 
  
  x %>%
    posterior_epred(nd, 
                    re_formula = ~ (1 | municipality_name), 
                    allow_new_levels = TRUE) %>% 
    set_colnames(as.character(nd$prediction_id)) %>%
    as.data.frame.table() %>%
    dplyr::mutate(prediction_id = Var2) %>%
    dplyr::rename(Estimate = Freq, sample = Var1) %>%
    dplyr::inner_join(nd, by = "prediction_id") %>%
    tibble::as_tibble() %>%
    dplyr::select(-Var2)
}

imap_dfr(vessel_activity_model_brms, predictions_site) %>%
  mutate(municipality_name = fct_reorder(municipality_name, Estimate)) %>%
  ggplot(aes(y = municipality_name, x = Estimate, fill = boat_code, colour = boat_code)) + 
  stat_pointinterval(.width = c(0.66, 0.95), position = position_dodge(0.4)) +
  scale_fill_brewer(palette = "Set1", aesthetics = c("colour", "fill"), 
                    name = "Boat type") +
  scale_alpha_manual(values = c(0.05, 0.33, 0.95), name = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Estimated average number of trips per month - Per site", 
       caption = "Based on 1000 Markov Chain Monte Carlo simulations")
```

![](vessel-activity_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->