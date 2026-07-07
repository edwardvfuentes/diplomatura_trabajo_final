library(tidyverse)
library(stringr)

source("./input/household_type.R")
source("./input/udf.R")

famecon_list <- list()
i <- 1

# Una lista con los dataframes relevantes
for (famecon_df in str_subset(ls(), "famecon\\d{4}")) {
  famecon_list[[famecon_df]] <- get(famecon_df)
  i <- i + 1
}

famecon_map <- map_dfr(
  famecon_list,
  function(x) famecon_cleaner(x, variables = c(
    "fincome1",
    "fincome1_per",
    "pce",
    "pce_per",
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
    "expense",
    "tipo_familia"
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

# Eliminamos las siguientes provincias por falta de datos o muy pocos datos:
# Hainan, Tibet, Qinghai, Ningxia, Mongolia Interior y Xinjiang
famecon_family_df <- famecon_family_df %>% filter(provcd %notin% c(46, 54, 63, 64, 65)) 

# Eliminar missing values de las provincias (esto es, códigos negativos o
# iguales a 99, y los NAs que se encuentran en algunos datos)
famecon_family_df <- famecon_family_df %>% filter(provcd >= 0)
# Y convertimos la variable de provincias a factores
famecon_family_df <- famecon_family_df %>% mutate(
  provcd = as_factor(provcd)
)
nrow(famecon_family_df) # 92638: Son 2547 filas menos

# Insertar medians en el resto de variables numéricas donde hay un NA.
# Estas medianas son por año y provincia para mayor representatividad.
famecon_family_df <- famecon_family_df %>%
  group_by(provcd, year) %>%
  mutate(
    across(where(is.numeric), ~ replace_na(., median(., na.rm = TRUE)))
  ) %>%
  ungroup()

# Eliminamos de las variables numéricas los outliers
famecon_family_df <- famecon_family_df %>%
  filter(!es_outlier(fincome1))

# Añadimos una variable dummy, para reflejar la reforma del hukou en 2014
famecon_family_df <- famecon_family_df %>% 
  mutate(
    hukou_reform = as.logical(ifelse(as.numeric(year) >= 2014, 1, 0))
    )


# Elaboraremos otra versión del dataframe con promedios para todas las variables
# según año y provincia
famecon_grouped_df <- famecon_family_df %>%
  group_by(year, provcd) %>% 
  summarise(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
  )

  
