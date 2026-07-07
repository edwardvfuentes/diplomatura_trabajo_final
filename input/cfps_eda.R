library(tidyverse)
library(e1071)
library(modelsummary)

source("./input/clean_datasets.R")

# == Tablas estadísticas == 


## Muestras por provincias y por años antes de las transformaciones

## Muestras por años después de transformaciones
year_summary <- famecon_family_df %>% 
  group_by(year, provcd) %>% 
  summarise(Registros = n()) %>% 
  rename("Año" = year)


datasummary_df(
  year_summary,
  fmt = 0,
  title = "Registros por año después de transformaciones",
  notes = "Fuente: Elaboración propia en base a CFPS",
  output = "./output/year_summary.png"
)

# == Gráficas ggplot ==
famecon_family_df %>% 
  ggplot(aes(x = fincome1)) +
  geom_histogram() +
  facet_wrap(~provcd)