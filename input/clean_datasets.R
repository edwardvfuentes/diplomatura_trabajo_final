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
    "trco",
    "eec",
    "other",
    "eptran",
    "epwelf",
    "mortage",
    "expense"
  )
  ),
  .id = "df_year"
)

# Resumen de datos después del merge
summary(famecon_map)
nrow(famecon_map) # 95185 filas en bruto

## == Limpieza adicional después del merge ==

# Extraemos el año como una variable ==
famecon_family_df <- famecon_map %>%
    mutate(year = str_extract(df_year, "\\d{4}"))

# Eliminar missing values de las provincias (esto es, códigos negativos o
# iguales a 99, y los NAs que se encuentran en algunos datos)
famecon_family_df_2 <- famecon_family_df %>% filter(provcd >= 0)
nrow(famecon_family_df_2) # 92638: Son 2547 filas menos

famecon_family_df %>% 
  anti_join(famecon_family_df_2)

# Insertar medians en el resto de variables numéricas donde hay un NA.
# Estas medianas son por año y provincia para mayor representatividad.
famecon_family_df <- famecon_family_df %>%
  group_by(provcd, year) %>%
  mutate(
    across(where(is.numeric), ~ replace_na(., median(., na.rm = TRUE)))
  ) %>%
  ungroup()

# Se eliminan las 6 familias de Xinjiang en 2014 que no tienen datos de varias
# variables
famecon_family_df <- famecon_family_df %>% filter(!is.na(fincome1))

# Eliminamos de las variables numéricas los outliers
famecon_family_df <- famecon_family_df %>%
  filter(!es_outlier(fincome1))

# Elaboraremos otra versión del dataframe con promedios para todas las variables
# según año y provincia
famecon_grouped_df <- famecon_family_df %>%
  group_by(year, provcd) %>% 
  summarise(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
  )
  
