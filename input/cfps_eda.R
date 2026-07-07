library(tidyverse)
library(e1071)
library(modelsummary)
library(skimr)
library(ggthemes)
library(ggsci)
library(envalysis)

source("./input/clean_datasets.R")

# == Tablas estadísticas == 


## Tabla 1: Muestras por provincias y por años antes de las transformaciones
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

## Tabla 2: Resumen estadístico de las variables
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

## Tabla 3: Registros por tipo de familia
datasummary_df(
  famecon_family_df %>% 
    group_by(tipo_familia) %>% 
    summarise(
      "Nº Registros" = n(),
      "Ingresos Medios" = mean(fincome1_per),
      "Ingresos Medianos" = median(fincome1_per),
      "Consumo Medio" = mean(pce),
      "Consumo Mediano" = median(fincome1_per)
    )
  ,
  fmt = 0,
  title = "Tabla 3: Nº de registros por tipo de familia",
  notes = "Fuente: Elaboración propia en base a CFPS",
  output = "./output/household_type_summary.png"
)

# =========

## Figura 1: Evolución ingresos y consumo por año y tipo de familia
family_household_year <- famecon_family_df %>% 
  group_by(year, tipo_familia) %>% 
  summarise(
    ingreso_medianos = median(fincome1_per),
    consumo_mediano = median(pce)
  ) %>% 
  mutate(year = as.integer(year))

family_household_year %>%
  pivot_longer(
    cols = c("ingreso_medianos", "consumo_mediano"),
    names_to = "Variable", values_to = "Valor"
    ) %>% 
  ggplot(
    aes(x = year, y = Valor, col = Variable)
    ) + 
  geom_line(linewidth = 1.5) +
  geom_vline(xintercept = 2014, linetype = 2) +
  facet_wrap(.~tipo_familia) +
  scale_x_continuous(breaks = unique(family_household_year$year)) +
  theme_minimal()

famecon_family_df %>% 
  group_by(year, tipo_familia) %>% 
  summarise(
    ingresos_medianos = median(fincome1_per),
    consumo_mediano = median(pce)
  ) %>% 
  mutate(year = as.integer(year)) %>% 
  ggplot(aes())




famecon_family_df %>% 
  ggplot(aes(x = fincome1_per)) +
  geom_histogram() +
  facet_wrap(~provcd) +
  theme_publish() +
  labs(
    x = "Renta neta per cápita",
    caption = expression(italic("Figura 1: Distribución de renta per capita por provincias (2010-2022)"))
    )


  

