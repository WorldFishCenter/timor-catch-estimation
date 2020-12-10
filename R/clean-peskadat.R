# Read PESKADAT species sheet
clean_peskasdat_species <- function(path){
  path %>%
    readr::read_csv() %>%
    dplyr::mutate(species_code = as.character(species_code))
}
