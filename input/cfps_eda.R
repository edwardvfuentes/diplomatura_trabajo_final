library(tidyverse)
library(e1071)
library(modelsummary)
library(skimr)
library(ggthemes)
library(ggsci)
library(envalysis)
library(scales)

source("./input/clean_datasets.R")

# Diccionario para los nombres de las variables
dict <- c(
  "daily"= "Necesidades diarias",
  "dress"= "Ropa",
  "eec"  = "Educación y entretenimiento",
  "food" = "Comida",
  "house"= "Casa",
  "med"  = "Gastos médicos",
  "other"= "Otros",
  "trco" = "Correos y telecomunicaciones"
)

# Valores únicos tipo_de_familia según fid
famecon_family_df %>% 
  group_by(fid) %>% 
  summarise(valores_unicos = list(unique(tipo_familia)))


# Conteos de transición de un tipo_famili a otro entre 2010 y 2012
family_transition_full <- famecon_family_df %>%
  # filter(year %in% c(2010, 2012)) %>%
  mutate(year = as.integer(year)) %>% 
  select(fid, year, tipo_familia) %>% 
  arrange(fid, year) %>% 
  group_by(fid) %>% 
  mutate(
    estado_actual = tipo_familia,
    estado_siguiente = lead(tipo_familia),
    anio_actual = year,
    anio_siguiente = lead(year)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(estado_siguiente)) %>% 
  mutate(periodo = paste0(anio_actual, "/", anio_siguiente)) %>% 
  count(periodo, estado_actual, estado_siguiente)

family_transition_1012_table <- family_transition_full %>% 
  filter(periodo == "2010/2012") %>% 
  pivot_wider(
    id_cols = estado_actual,
    names_from = estado_siguiente,
    values_from = n,
    values_fill = 0
  ) %>% rename('Estado Familiar'=estado_actual)

datasummary_df(
  family_transition_1012_table,
  fmt = 0,
  title = "Tabla 5: Transiciones de estados de familia de 2010 a 2012",
  notes = "Fuente: Elaboración propia en base a CFPS",
  output = "./output/transition_1012_table.png"
)


family_transition_1416_table <- family_transition_full %>% 
  filter(periodo == "2014/2016") %>% 
  pivot_wider(
    id_cols = estado_actual,
    names_from = estado_siguiente,
    values_from = n,
    values_fill = 0
  ) %>% rename('Estado Familiar'=estado_actual)

datasummary_df(
  family_transition_1416_table,
  fmt = 0,
  title = "Tabla 6: Transiciones de estados de familia de 2014 a 2016",
  notes = "Fuente: Elaboración propia en base a CFPS",
  output = "./output/transition_1416_table.png"
)

family_transition_2022_table <- family_transition_full %>% 
  filter(periodo == "2020/2022") %>% 
  pivot_wider(
    id_cols = estado_actual,
    names_from = estado_siguiente,
    values_from = n,
    values_fill = 0
  ) %>% rename('Estado Familiar'=estado_actual)

datasummary_df(
  family_transition_2022_table,
  fmt = 0,
  title = "Tabla 7: Transiciones de estados de familia de 2020 a 2022",
  notes = "Fuente: Elaboración propia en base a CFPS",
  output = "./output/transition_2022_table.png"
)


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
  title = "Tabla 3: Ingresos y consumo por tipo de familia",
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


## Figura 2: Distribución de la renta pc por provincias (2010-2022)
famecon_family_df %>% 
  filter(!es_outlier(fincome1_per)) %>% 
  ggplot(aes(x = fincome1_per)) +
  geom_histogram() +
  facet_wrap(~provcd) +
  theme_minimal() +
  labs(
    x = "Renta neta per cápita",
    caption = expression(italic("Figura 2: Distribución de renta per capita por provincias (2010-2022). \n Se ha escogido un tamaño de 30 para el ancho de las barras y se han eliminado atípicos. Elaboración propia con datos de CFPS"))
    )

## Figura 3: Distribución del consumo pc por provincias (2010-2022)
famecon_family_df %>%
  filter(!es_outlier(pce)) %>% 
  ggplot(aes(x = pce)) +
  geom_histogram() +
  facet_wrap(~provcd) +
  theme_minimal() +
  labs(
    x = "Consumo per cápita",
    caption = expression(italic("Figura 3: Distribución del consumo per cápita por provincias (2010-2022). Se ha escogido  \n un tamaño de 30 para el ancho de las barras. Elaboración propia con datos de CFPS"))
  )


## Figura 4: Distribución de gastos por tipo de household (año 2022)
familia_cat_gasto_2022 <- famecon_family_df %>%
  filter(year == 2022) %>%
  group_by(tipo_familia) %>% 
  summarise(
    food  = sum(food),
    daily = sum(daily),
    dress = sum(dress),
    eec   = sum(eec),
    house = sum(house),
    med   = sum(med),
    other = sum(other),
    trco  = sum(trco)
  ) %>% 
  pivot_longer(
    cols = c("food", "dress", "house", "daily", "med", "trco", "eec", "other"),
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  mutate(tipo_familia = as.factor(tipo_familia)) %>% 
  group_by(tipo_familia) %>% 
  mutate(pct_gasto = Valor / sum(Valor))

  familia_cat_gasto_2022 %>% 
  ggplot(aes(x = Variable, y=pct_gasto)) +
  geom_col( aes(fill = tipo_familia), position = "dodge", width = 0.5) +
  theme_publish() +
  theme(
    panel.grid.major.y = element_line(
      color = "gray80", 
      linewidth = 0.5,
      linetype = "solid"   
    )
  ) +
  scale_y_continuous(
    labels = scales::percent,
    n.breaks = 4
    ) +
  labs(
    y = "% Gasto total en la categoría",
    x = "Categoría de gasto",
    fill = "Tipo familia",
    caption = expression(italic("Figura 4: Distribución de gastos según tipo de familia (2022). Elaboración propia con datos de CFPS"))
  )




famecon_family_df %>%
  filter(year == 2022) %>% 
  select(tipo_familia, food, dress, house, daily, med, trco, eec, other) %>% 
  pivot_longer(
    cols = c("food", "dress", "house", "daily", "med", "trco", "eec", "other"),
    names_to = "Variable",
    values_to = "Valor"
  ) %>% 
  mutate(Variable=recode(Variable, !!!dict, .default="Otro")) %>% 
  ggplot(aes(x = Variable, y=Valor)) +
  geom_col( aes(fill = tipo_familia), position = "dodge") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme_publish() +
  theme(
    panel.grid.major.y = element_line(
      color = "gray80", 
      linewidth = 0.5,  
      linetype = "solid"   
    )
  ) +
  labs(
    y = "Renta neta per cápita",
    x = "Categoría de gasto",
    caption = expression(italic("Figura 4: Distribución de gastos según tipo de familia (2022). Elaboración propia con datos de CFPS"))
  )

  

