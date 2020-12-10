# Takes the CSV of the current survey and makes some very basic cleaning
read_kobo_survey_2 <- function(path){

  number_cols <- read.csv(path, nrows = 10) %>%
    ncol()

  path %>%
    # To make sure automatic type selection doesn't screw anything we read it
    # all as character
    readr::read_csv(file = .,
                    col_types = paste(rep("c", number_cols), collapse = "")) %>%
    janitor::clean_names() %>%
    # Sometimes an uuid has more than one column when the survey has been
    # manually edited.
    dplyr::group_by(uuid) %>%
    dplyr::arrange(end, .by_group = TRUE) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()
}


# Takes the survey and cleans the catch. Calculates the weight and highlights if
# there are any issues there. One row per species caught in each trip
clean_catch_kobo2 <- function(kobo_survey_2, peskadat_species){

  suppressPackageStartupMessages({
    require(dplyr)
    require(tidyr)
    require(purrr)
  })

  # Ignore NAs when adding numbers except if everythinh is an NA
  na_mindful_sum <- function(x){
    if (all(is.na(x))) {
      NA_real_
    } else {
      sum(x, na.rm = TRUE)
    }
  }

  clean_lengths <- kobo_survey_2 %>%
    select(starts_with("species_group"), uuid) %>%
    select(-ends_with("species_name"), -ends_with("photo")) %>%
    mutate(record_id = uuid) %>%
    select(-uuid) %>%
    pivot_longer(!record_id, names_to = c("record", "question"),
                 names_pattern = "species_group_(.)_species_group_(.*)") %>%
    # Make sure all have the same questions
    complete(record_id, record, question) %>%
    group_by(record_id, record) %>%
    filter(!all(is.na(value))) %>%
    mutate(species_code = value[question == "species"],
           use = value[question == "food_or_sale"],
           length_over_60 = value[question ==
                                    "no_fish_by_length_group_fish_length_over60"]) %>%
    filter(!question %in% c("species",
                            "food_or_sale",
                            "no_fish_by_length_group_fish_length_over60")) %>%
    mutate(mean_length = question,
           mean_length = stringr::str_extract_all(mean_length,
                                                  "((?<=_|r)[0-9]+)"),
           mean_length = map(mean_length, as.numeric),
           mean_length = map_dbl(mean_length, mean),
           mean_length = if_else(mean_length == 60 & !is.na(length_over_60),
                                 as.numeric(length_over_60), mean_length),
           n_individuals = as.numeric(value)) %>%
    select(-value, -question, -length_over_60)

  clean_lengths %>%
    left_join(peskadat_species, by = "species_code") %>%
    # Here we use ~20000 individuals as the threshold. This should improve in
    # the future
    mutate(n_individuals_flag = case_when(n_individuals > exp(10) ~
                                            "species_number__too_large",
                                          n_individuals < 0 ~
                                            "species_number__negative",
                                          na_mindful_sum(n_individuals) == 0 &
                                            first(species_code) != "0" ~
                                            "species_number__zero",
                                          TRUE ~NA_character_),
           n_individuals = case_when(n_individuals > exp(10) ~ NA_real_,
                                     n_individuals < 0 ~ n_individuals * (-1),
                                     na_mindful_sum(n_individuals) == 0 &
                                       first(species_code) != "0" ~ NA_real_,
                                     TRUE ~ n_individuals),
           size_flag = case_when(isTRUE(n_individuals > 0) &
                                   mean_length - 2.5 > maxlength ~
                                   "species_size__too_large",
                                 isTRUE(n_individuals > 0) &
                                   mean_length + 2.5 < minlength ~
                                   "species_size__too_small",
                                 TRUE ~ NA_character_)) %>%
    # Get weight
    mutate(mean_weight = (a * mean_length ^ b)/1000,
           weight = mean_weight * n_individuals) %>%
    group_by(record_id, species_code, use) %>%
    summarise(mean_length = weighted.mean(x = mean_length,
                                          w = replace_na(n_individuals, 0)),
              # Make the totals NA only if all of the components are NA
              across(c(n_individuals, weight), na_mindful_sum),
              species_flags = paste(unique(na.omit(c(n_individuals_flag,
                                                     size_flag))),
                                    collapse = ";"),
              .groups = "drop") %>%
    mutate(species_flags = if_else(species_flags == "",
                                   NA_character_, species_flags),
           mean_length = round(mean_length, digits = 1))

}

# Takes the survey and cleans the hell out of it. One row per trip
clean_trips_kobo2 <- function(kobo_survey_2, kobo_catch_2, boats){

  suppressPackageStartupMessages({
    require(dplyr)
    require(fuzzyjoin)
    require(stringr)
  })

  # Summarise catch by trip
  trip_catch_2 <- kobo_catch_2 %>%
    group_by(record_id) %>%
    arrange(desc(weight), .by_group = TRUE) %>%
    summarise(trip_catch_use = if_else(n_distinct(use) == 1,
                                       first(use), "both"),
              trip_n_species = n_distinct(species_code[species_code != "0"],
                                          na.rm = TRUE),
              trip_catch = sum(weight, na.rm = T),
              species_flag = paste(unique(na.omit(c(species_flags))),
                                   collapse = ";"),
              .groups = "drop") %>%
    mutate(species_flag = if_else(species_flag == "", NA_character_,
                                  species_flag))

  imei_list <- boats %>%
    select(imei_long)

  kobo_survey_2 %>%
    transmute(boat_imei_r = trip_group_imei,
              boat_registration_r = trip_group_boat_reg_no,
              boat_type_r = trip_group_boat_type,
              trip_duration_r = trip_group_duration,
              trip_gear_r = trip_group_gear_type,
              trip_habitat_r = trip_group_habitat_boat,
              trip_platform_r = trip_group_has_boat,
              trip_date_r = date,
              trip_landing_site_r = landing_site_name,
              trip_value_r = total_catch_value,
              gear_mesh_size_a_r = trip_group_mesh_size,
              gear_mesh_size_b_r = trip_group_mesh_size_other,
              fisher_number_men_r = trip_group_no_fishers_no_men_fishers,
              fisher_number_women_r = trip_group_no_fishers_no_women_fishers,
              fisher_number_children_r = trip_group_no_fishers_no_child_fishers,
              record_device_id_r = deviceid,
              record_date_r = end,
              trip_happiness_r = happiness_rating,
              record_id_r = uuid) %>%
    # CLEAN IMEI FIELD
    mutate(boat_imei = as.numeric(boat_imei_r),
           # If it was negative make it positive as it was probably a typo
           boat_imei = if_else(boat_imei < 0,
                               boat_imei * -1,
                               boat_imei),
           # Optimistically we need at least 5 digits to work with,
           boat_imei = if_else(boat_imei < 9999,
                               NA_real_,
                               boat_imei),
           # back to character for further treatment
           boat_imei = as.character(boat_imei),
           imei_regex = paste0(boat_imei, "$")) %>%
    {
      fuzzyjoin::regex_right_join(x = imei_list,
                                  y = .,
                                  by = c("imei_long" = "imei_regex"))
    } %>%
    mutate(boat_imei_flag = if_else(is.na(imei_long) & !is.na(boat_imei_r),
                                    "imei__not_found", NA_character_),
           boat_imei = imei_long) %>%
    select(-imei_long, -imei_regex, -boat_imei_r) %>%
    # CLEAN BOAT REGISTRATION
    mutate(boat_registration = tolower(boat_registration_r),
           # Only valid if it has characters, then numbers, then
           # characters again. Last set of characters is optional,
           # although don't know if it should
           boat_registration = if_else(str_detect(boat_registration,
                                                  "^[a-z]{2,4} ?[0-9]{2,3}( ?[a-z]{2,3})?$"),
                                       boat_registration,
                                       NA_character_),
           # Make sure there is one space between segments
           boat_registration = str_replace(string = boat_registration,
                                           pattern = "([a-z]{2,4}) ?([0-9]{2,3})( ?)([a-z]{2,3})?$",
                                           replacement = "\\1 \\2 \\4"),
           boat_registration = toupper(boat_registration),
           boat_registration_flag = if_else(is.na(boat_registration) &
                                              !is.na(boat_registration_r),
                                            "boat_registration__invalid",
                                            NA_character_)) %>%
    select(-boat_registration_r) %>%
    # CLEAN TRIP DURATION
    mutate(trip_duration = as.numeric(trip_duration_r),
           # If it was negative make it positive as it was probably a typo
           trip_duration = if_else(trip_duration < 0,
                                   trip_duration * -1,
                                   trip_duration),
           # Trip duration cannot be zero...
           trip_duration = if_else(trip_duration == 0,
                                   NA_real_,
                                   trip_duration),
           # Outright reject trips that are longer than 60 days
           trip_duration = if_else(trip_duration > 60*24,
                                   NA_real_,
                                   trip_duration),
           # trip durations larger than 48 hours are rarer than 1 in a thousand
           # (checked using quantile function) so for safety we're removing them
           # now
           trip_duration = if_else(trip_duration > 3*24,
                                   NA_real_,
                                   trip_duration),
           trip_duration_flag = case_when(as.numeric(trip_duration_r) == 0 ~
                                            "trip_duration__zero_hours",
                                          is.na(trip_duration) &
                                            !is.na(trip_duration_r) ~
                                            "trip_duration__too_long",
                                          TRUE ~ NA_character_)) %>%
    select(-trip_duration_r) %>%
    # CLEAN TRIP TYPE
    mutate(trip_platform = ifelse(trip_platform_r, "boat", "land")) %>%
    select(-trip_platform_r) %>%
    # CLEAN DATES
    mutate(trip_date = lubridate::ymd(trip_date_r, tz = "Asia/Dili"),
           recording_datetime = lubridate::ymd_hms(record_date_r),
           recording_date = lubridate::as_date(recording_datetime,
                                               tz = "Asia/Dili"),
           trip_date_flag = case_when(trip_date > recording_date ~
                                        "trip_date__trip_is_in_future",
                                      trip_date < recording_date - 30 ~
                                        "trip_date__trip_date_too_old",
                                      TRUE ~ NA_character_)) %>%
    select(-trip_date_r, -record_date_r, -recording_date) %>%
    # MESH SIZE
    mutate(gear_mesh_size_b_w = as.numeric(gear_mesh_size_b_r),
           gear_mesh_size_a_w = if_else(gear_mesh_size_a_r == "seluk",
                                        gear_mesh_size_b_r,
                                        gear_mesh_size_a_r),
           gear_mesh_size = as.numeric(gear_mesh_size_a_w),
           gear_mesh_size_flag = case_when(is.na(gear_mesh_size_b_w) &
                                             !is.na(gear_mesh_size_b_r) ~
                                             "gear_mesh_size__invalid",
                                           TRUE ~ NA_character_)) %>%
    select(-gear_mesh_size_b_r, -gear_mesh_size_a_r,
           -gear_mesh_size_b_w, -gear_mesh_size_a_w) %>%
    # NUMBER OF FISHERS
    mutate(fisher_number_men = as.numeric(fisher_number_men_r),
           fisher_number_women = as.numeric(fisher_number_women_r),
           fisher_number_children = as.numeric(fisher_number_children_r),
           fisher_number_total_w = `+`(`+`(fisher_number_men,
                                           fisher_number_women),
                                       fisher_number_children),
           fisher_number_total = if_else(fisher_number_total_w == 0,
                                         NA_real_, fisher_number_total_w),
           fisher_number_flag = case_when(fisher_number_total_w == 0 &
                                            !is.na(trip_platform) ~
                                            "fisher_number__zero_fishers",
                                          TRUE ~ NA_character_)) %>%
    select(-fisher_number_men_r, -fisher_number_women_r,
           -fisher_number_children_r, -fisher_number_total_w) %>%
    # VARIABLES THAT NEED LITTLE-NO CLEANING
    mutate(record_device_id = record_device_id_r,
           record_id = record_id_r,
           trip_happiness = as.numeric(trip_happiness_r),
           boat_type = boat_type_r,
           trip_gear = trip_gear_r,
           trip_habitat = trip_habitat_r,
           trip_landing_site = trip_landing_site_r) %>%
    select(-record_device_id_r, -record_id_r, -trip_happiness_r, -boat_type_r,
           -trip_gear_r, -trip_habitat_r, -trip_landing_site_r) %>%
    # CATCH
    left_join(trip_catch_2, by = "record_id") %>%
    # CLEAN VALUES (left for after because it's good to know the catch too)
    mutate(trip_value_w = as.numeric(trip_value_r),
           trip_value = case_when(trip_value_w > 20000 ~ NA_real_,
                                  trip_value_w == 0 ~ NA_real_,
                                  TRUE ~ trip_value_w),
           trip_value_flag = case_when(trip_value_w > 20000 ~
                                         "trip_value__too_large",
                                       trip_value_w == 0 &
                                         !is.na(trip_platform) &
                                         trip_catch != 0 ~
                                         "trip_value__is_zero",
                                       TRUE ~ NA_character_)) %>%
    select(-trip_value_w, -trip_value_r) %>%
    # FLAGS
    rowwise() %>%
    mutate(flags = paste(na.omit(c(boat_imei_flag,
                                   boat_registration_flag,
                                   trip_duration_flag,
                                   trip_date_flag,
                                   trip_value_flag,
                                   gear_mesh_size_flag,
                                   species_flag,
                                   fisher_number_flag)), collapse = ";")) %>%
    ungroup() %>%
    select(-ends_with("_flag")) %>%
    mutate(flags = if_else(flags == "", NA_character_, flags))

}
