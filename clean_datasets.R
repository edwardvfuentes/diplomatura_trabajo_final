library(tidyverse)
library(stringr)

source("./input/load_source_data.R")
source("./input/udf.R")

for (famecon_df in str_subset(ls(), "famecon")){
  print("Empezamos a limpiar el dataset " + famecon_df)
  famecon_cleaner(
    get(famecon_df),
       c(
         "fid16",
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
}
