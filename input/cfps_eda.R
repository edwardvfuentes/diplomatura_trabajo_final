library(tidyverse)
library(e1071)
library(modelsummary)

source("./input/clean_datasets.R")

# == Tablas estadísticas == 

## Muestras por provincias y por años
year_summary <- famecon_family_df %>% 
  group_by(year) %>% 
  summarise(Registros = n())


datasummary_df(
  year_summary,
  title = "Número de registros por año",
  notes = "Fuente: Miami me lo confirmó",
  output = "./output/year_summary.png"
)

# == Gráficas ggplot ==
famecon_family_df %>% 
  ggplot(aes(x = fincome1)) +
  geom_histogram() +
  facet_wrap(~provcd)