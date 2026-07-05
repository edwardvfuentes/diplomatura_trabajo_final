library(tidyverse)
library(stringr)

source("./input/load_source_data.R")
source("./input/udf.R")

famecon_list <- list()
i <- 1

# Una lista con los dataframes relevantes
for (famecon_df in str_subset(ls(), "famecon\\d{4}")) {
  print(famecon_df)
  # famecon_list <- append(famecon_list, get(famecon_df))
  # famecon_list[[i]] <- get(famecon_df)
  famecon_list[[famecon_df]] <- get(famecon_df)
  i <- i + 1
}

famecon_map <- map_dfr(
  famecon_list,
  function(x) famecon_cleaner(x, variables = c(
    "fincome1",
    "fincome1_per",
    "pce",
    "food",
    "dress",
    "house",
    "daily",
    "med",
    "trco"
  )
  ),
  .id = "df_year"
)

## Limpieza adicional después del merge

# Extraemos el año como una variable
famecon_complete <- famecon_map %>%
    mutate(year = str_extract(df_year, "\\d{4}"))