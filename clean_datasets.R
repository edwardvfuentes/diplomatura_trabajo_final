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
  famecon_list[[i]] <- get(famecon_df)
  i <- i + 1
}


# for (famecon_df in str_subset(ls(), "famecon")){
#   print("Empezamos a limpiar el dataset " + famecon_df + "...")
#   famecon_cleaner(
#     get(famecon_df),
#        c(
#          "fincome1",
#          "fincome1_per",
#          "pce",
#          "food",
#          "dress",
#          "house",
#          "daily",
#          "med",
#          "trco"
#        )
#   )
# }


map(
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
        )
)