library(tidyverse)
library(plm)
library(modelsummary)
library(fixest)
library(kableExtra)

source("./input/clean_datasets.R")

# Para determinar el Rural como categoría base en tipo_familia
famecon_family_df$tipo_familia <- factor(famecon_family_df$tipo_familia, levels = c("Rural", "Urbano", "Migrante (Rural a Urbano)", "Migrante (Urbano a Rural)"))

# Modelos OLS
# Consumo ~ Ingresos + Activos

ols_estandar_1 <- lm(log(pce + 1) ~ log(fincome1_per + 1) + log(resivalue + 1), data=famecon_family_df)
ols_estandar_2 <- lm(log(pce + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + tipo_familia, data=famecon_family_df)
ols_estandar_3 <- lm(log(pce + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + tipo_familia + categoria_renta, data=famecon_family_df)
ols_estandar_4 <- lm(log(pce + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + tipo_familia + categoria_renta + hukou_reform, data=famecon_family_df)


lista_estandar_models <- list(
  "Modelo 1" = ols_estandar_1,
  "Modelo 2" = ols_estandar_2,
  "Modelo 3" = ols_estandar_3,
  "Modelo 4" = ols_estandar_4
)

modelsummary(
  lista_estandar_models,
  stars = TRUE,
  # title = "MCO para el logaritmo de consumo total",
  gof_map = c("r.squared", "adj.r.squared", "nobs", "rmse"),
  notes = "Fuente: Elaboración propia en base a CFPS",
  output = "./output/Tablas/resumen_ols_consumo.png"
)

# Mínimos Cuadrados Ordinarios para el logaritmo de consumo total

# Consumo ~ Ingresos + Activos 2010-2014 frente a 2016-2022
ols_year_1 <- lm(log(pce + 1) ~ log(fincome1_per + 1) + tipo_familia + categoria_renta,
                 data=famecon_family_df %>% filter(year %in% c(2010, 2012, 2014)))
ols_year_2 <- lm(log(pce + 1) ~ log(fincome1_per + 1) + tipo_familia + categoria_renta,
                 data=famecon_family_df %>% filter(year %in% c(2016, 2018, 2020, 2022)))

lista_year_models <- list(
  "Pre-reforma" = ols_year_1,
  "Post-reforma" = ols_year_2
)

modelsummary(
  lista_year_models,
  stars = TRUE,
  # title = "MCO para el logaritmo de consumo: Antes y después de la reforma de 2014",
  gof_map = c("r.squared", "adj.r.squared", "nobs", "rmse"),
  notes = "Fuente: Elaboración propia en base a CFPS",
  output = "./output/Tablas/resumen_ols_consumo_reforma.png"
)

## MCO para el logaritmo de consumo: Antes y después de la reforma de 2014


# Modelos para categoría de gasto general
ols_food  <- lm(log(food + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) +tipo_familia + hukou_reform, data=famecon_family_df )
ols_dress <- lm(log(dress + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + tipo_familia + hukou_reform, data=famecon_family_df)
ols_house <- lm(log(house + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + tipo_familia + hukou_reform, data=famecon_family_df)
ols_daily <- lm(log(daily + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + tipo_familia + hukou_reform, data=famecon_family_df)
ols_med   <- lm(log(med + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + tipo_familia + hukou_reform, data=famecon_family_df)
ols_trco  <- lm(log(trco + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + tipo_familia + hukou_reform, data=famecon_family_df)
ols_eec   <- lm(log(eec + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + tipo_familia + hukou_reform, data=famecon_family_df)

lista_categorias_models <- list(
  "Comida" = ols_food,
  "Ropa" = ols_dress,
  "Casa" = ols_house,
  "Diario" = ols_daily,
  "Médicos" = ols_med,
  "Teleco" = ols_trco,
  "Educación y Entretenimiento" = ols_eec
)

modelsummary(
  lista_categorias_models,
  stars = TRUE,
  # title = "MCO para el logaritmo de categorías de consumo",
  gof_map = c("r.squared", "adj.r.squared", "nobs", "rmse"),
  notes = "Fuente: Elaboración propia en base a CFPS",
  output = "./output/Tablas/resumen_ols_categorías.png"
)


## MCO para el logaritmo de categorías de consumo

# Modelos para categorías de gasto post-reforma hukou
ols_post_food  <- lm(log(food + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + tipo_familia + hukou_reform, data=famecon_family_df %>%filter(year %in% c(2016, 2018, 2020, 2022)))
ols_post_dress <- lm(log(dress + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + tipo_familia + hukou_reform, data=famecon_family_df %>% filter(year %in% c(2016, 2018, 2020, 2022)))
ols_post_house <- lm(log(house + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + tipo_familia + hukou_reform, data=famecon_family_df %>% filter(year %in% c(2016, 2018, 2020, 2022)))
ols_post_daily <- lm(log(daily + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + tipo_familia + hukou_reform, data=famecon_family_df %>% filter(year %in% c(2016, 2018, 2020, 2022)))
ols_post_med   <- lm(log(med + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + tipo_familia + hukou_reform, data=famecon_family_df %>% filter(year %in% c(2016, 2018, 2020, 2022)))
ols_post_trco  <- lm(log(trco + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + tipo_familia + hukou_reform, data=famecon_family_df %>%filter(year %in% c(2016, 2018, 2020, 2022)))
ols_post_eec   <- lm(log(eec + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + tipo_familia + hukou_reform, data=famecon_family_df %>% filter(year %in% c(2016, 2018, 2020, 2022)))

lista_categorias_post_models <- list(
  "Comida" = ols_post_food,
  "Ropa" = ols_post_dress,
  "Casa" = ols_post_house,
  "Diario" = ols_post_daily,
  "Médicos" = ols_post_med,
  "Teleco" = ols_post_trco,
  "Educación y Entretenimiento" = ols_post_eec
)

modelsummary(
  lista_categorias_models,
  stars = TRUE,
  title = "MCO para el logaritmo de categorías de consumo",
  gof_map = c("r.squared", "adj.r.squared", "nobs", "rmse"),
  notes = "Fuente: Elaboración propia en base a CFPS",
  output = "./output/Tablas/resumen_ols_categorías.png"
)


# Convertimos famecon_grouped_df en un pdataframe
pfamecon_grouped_df <- pdata.frame(famecon_grouped_df, index=c("provcd","year"), drop.index=TRUE, row.names=TRUE)

plm_estandar_1 <- plm(log(pce + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) ,
                      data=pfamecon_grouped_df, model = "within")
plm_estandar_2 <- plm(log(pce + 1) ~ log(fincome1_per + 1) + log(resivalue + 1) + hukou_reform,
                      data=pfamecon_grouped_df, model = "within")

lista_plm_models <- list(
  "Modelo 1" = plm_estandar_1,
  "Modelo 2" = plm_estandar_2
)

modelsummary(
  lista_plm_models,
  stars = TRUE,
  # title = "MCO de Panel con efectos fijos para el logaritmo de consumo total",
  gof_map = c("r.squared", "adj.r.squared", "nobs", "rmse"),
  notes = "Fuente: Elaboración propia en base a CFPS",
  output = "./output/Tablas/resumen_plm_consumo.png"
)

## MCO de Panel con efectos fijos para el logaritmo de consumo total



plm_fe_1 <- fixef(plm_estandar_1, type = "dmean")
plm_fe_2 <- fixef(plm_estandar_2, type = "dmean")


tabla_fe <- data.frame(
  id = names(plm_fe_1),
  efecto_fijo = as.numeric(plm_fe_1)
)

tidy(plm_fe_1)

modelsummary(tabla_fe)

kable(tabla_fe,
      digits = 3,
      col.names = c("Unidad", "Efecto fijo"))


lista_efectos_fijos <- list(
  "Modelo 1" = plm_fe_1,
  "Modelo 2" = plm_fe_2
)

datasummary_df(
  tidy(plm_fe_1) %>% summary(),
  stars = TRUE,
  title = "Efectos Fijos de MCO de Panel para log(consumo)",
  gof_map = c("r.squared", "adj.r.squared", "nobs", "rmse"),
  notes = "Fuente: Elaboración propia en base a CFPS",
  output = "./output/Tablas/resumen_plm_consumo.png"
)




