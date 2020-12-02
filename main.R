# Prepare workspace -------------------------------------------------------

library(magrittr)
library(drake)

# load functions
f <- lapply(list.files(path = here::here("R"), full.names = TRUE,
                       include.dirs = TRUE, pattern = "*.R"), source)

# Plan analysis ------------------------------------------------------------

full_plan <- rbind()

# Execute plan ------------------------------------------------------------

if (!is.null(full_plan)) {
  make(full_plan)
}
