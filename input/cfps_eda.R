library(tidyverse)
library(e1071)
library(modelsummary)
library(skimr)

source("./input/clean_datasets.R")

# == Tablas estadísticas == 


## Muestras por provincias y por años antes de las transformaciones

## Muestras por años después de transformaciones
# year_summary <- famecon_family_df %>% 
#   group_by(year) %>% 
#   summarise(Registros = n()) %>% 
#   rename(
#     "Año" = year,
#     )
# 
# datasummary_df(
#   year_summary,
#   fmt = 0,
#   title = "Tabla 1: Registros por año después de las transformaciones",
#   notes = "Fuente: Elaboración propia en base a CFPS",
#   output = "./output/year_summary.png"
# )


provcd_year_summary <- famecon_family_df %>% 
  group_by(provcd, year) %>% 
  summarise(Registros = n()) %>% 
  rename(
    "Año" = year,
    "Provincia" = provcd
  )

datasummary_df(
  provcd_year_summary %>% pivot_wider(names_from = "Año", values_from = Registros),
  fmt = 0,
  title = "Tabla 1: Registros por provincia y año después de las transformaciones",
  notes = "Fuente: Elaboración propia en base a CFPS",
  output = "./output/provcd_year_summary.png"
)

# Configuración para que skim obtenga etiquetas
skim_with(
  numeric = skimr::sfl(
    label = ~attr(., "label")  # Extrae la etiqueta
  )
)

skimmed_family_df <- famecon_family_df %>%
  select(where(is.numeric)) %>%
  skim() %>% 
  as_tibble()

skimmed_family_table <- skimmed_family_df %>% 
  select(-skim_type, -numeric.hist, -complete_rate) %>%
  mutate(
    n_registros = 85799
  ) %>% 
  select(skim_variable, n_registros, everything()) %>% 
  rename_with(~ str_remove(., "^numeric\\."), starts_with("numeric.")) %>% 
  rename(
    "Variable" = skim_variable,
    "Registros_faltantes" = n_missing,
    "Nº registros" = n_registros
  )
  

datasummary_df(
  skimmed_family_table,
  fmt = 0,
  title = "Tabla 2: Resumen estadístico de las variables",
  notes = "Fuente: Elaboración propia en base a CFPS",
  output = "./output/variable_summary.png"
)



# == Gráficas ggplot ==
famecon_family_df %>% 
  ggplot(aes(x = fincome1_per)) +
  geom_histogram() +
  facet_wrap(~provcd)


famecon_family_df
