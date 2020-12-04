# Prepare workspace -------------------------------------------------------

library(magrittr)
library(drake)

# load functions
f <- lapply(list.files(path = here::here("R"), full.names = TRUE,
                       include.dirs = TRUE, pattern = "*.R"), source)

# Plan analysis ------------------------------------------------------------

read_data <- drake_plan(
  boats = clean_boats(file_in("data/raw/timor-boats.csv"))
)

full_plan <- rbind(
  read_data
)

# Execute plan ------------------------------------------------------------

if (!is.null(full_plan)) {
  make(full_plan)
}
