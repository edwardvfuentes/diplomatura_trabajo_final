library(dplyr)
library(tidyr)
library(stringr)
library(rlang)


#' Obtiene el nombre fid relevante para el famecon. Normalmente es el que se corresponde
#' con el año en el que está el dataset
#' 
#' @param famecon_df Un dataframe de famecon
#' @returns nombre del fid correcto
get_relevant_fid <- function(famecon_df) {
  all_fids <- famecon_df %>%
    select(contains("fid")) %>%
    colnames() %>%
    str_subset("^fid\\d*$") %>%
    sort(decreasing=TRUE)
  
  relevant_fid <- all_fids[1]
  
  return (relevant_fid)
}


#' Obtiene un dataset de famecon y lo prepara para ser mezclado junto con otros
#' famecons de otros años
#' 
#' @param famecon_df Un dataframe de famecon
#' @param variables Vector con los nombres de las variables que se han de extraer
#'                  para realizar la estimación de los modelos
#' @returns Dataframe con las variables seleccionadas y los nombres de las mismas
#'          variables corregidos
#' @examples 
#' 
#' famecon_cleaner(
#'   famecon2022,
#'   c(
#'     "fid22",
#'     "fincome1",
#'     "fincome1_per",
#'     "pce",
#'     "food",
#'     "dress",
#'     "house",
#'     "daily",
#'     "med",
#'     "trco"
#'   )
#' )
#' 
#' famecon_cleaner(
#'   famecon2016,
#'   c(
#'     "fid16",
#'     "fincome1",
#'     "fincome1_per",
#'     "pce",
#'     "food",
#'     "dress",
#'     "house",
#'     "daily",
#'     "med",
#'     "trco"
#'   )
#' )
famecon_cleaner <- function(famecon_df, variables) {

  # Si hay una var llamada faminc_net, renombrarla a fincome1
  if(any(str_detect(names(famecon_df), "faminc_net"))){

    names(famecon_df) <- str_replace(names(famecon_df), "faminc_net", "fincome1")
    
  }
  
  # Si no hay fincome1_per, la calcularemos
  if(!any(str_detect(names(famecon_df), "fincome1_per"))){
    
    famecon_df <- famecon_df %>%
      mutate(
        fincome1_per = fincome1 / familysize
      )
    
  }
  
  # == Generamos el Subconjunto ==
  # Subconjunto de famecon con las variables para la estimación
  famecon_sub <- famecon_df %>%
    select(
      contains("fid"),
      contains("provcd"),
      all_of(variables)
    )
 
  # La variable de provincias, provcd, no debería tener sufijo
  # numérico.
  famecon_sub <- famecon_sub %>% 
    rename_with( ~str_remove(., "\\d+$"), .cols = matches("^provcd\\d+$"))
  
  # La variable fid* debe estar sin el sufijo del año
  # Obtén fid del año presente
  relevant_fid <- get_relevant_fid(famecon_sub)
  
  famecon_sub <- famecon_sub %>%
    rename(fid = relevant_fid)
  
  # La variable fid no debería ser un integer, sino un character
  famecon_sub$fid <- as.character(famecon_sub$fid)
  
  # Devolvemos el dataset final
  famecon_sub <- famecon_sub %>% select(fid, provcd, variables)
  
  return (famecon_sub)
  
}

# Ejecución de prueba
 # famecon_cleaner(
 #   famecon2020_raw,
 #   c(
 #     "fincome1",
 #     "fincome1_per",
 #     "pce",
 #     "food",
 #     "dress",
 #     "house",
 #     "daily",
 #     "med",
 #     "trco"
 #   )
 # )

 #' Filtra un numeric para quitarle sus outliers mediante cuantiles
 #' 
 #' @param x Un vector numérico
 #' @returns El vector numérico sin los outliers en cuestión
 #' 
 es_outlier <- function(x) {
   q1 <- quantile(x, 0.25, na.rm = TRUE)
   q3 <- quantile(x, 0.75, na.rm = TRUE)
   iqr <- q3 - q1
   limite_inferior <- q1 - 1.5 * iqr
   limite_superior <- q3 + 1.5 * iqr
   return(x < limite_inferior | x > limite_superior)
 }
 
 
 #' Utiliza los dataframes correspondientes de famecon y famcomf para clasificar
 #' el status de una familia en el dataset de CFPS: Sea rural, urbano, o migrante hacia rural
 #' o urbano
 #' 
 #' @param df_famecon Un dataset cargado de famecon.
 #' @param df_famconf Un dataset de famconf. Debe ser del mismo año que el dataset de famecon
 #' @param variable_urban Nombre de la variable que en famecon identifica el tipo de lugar donde vive la familia (Urban o Rural)
 #' @param variable_hukou Nombre de la variable que en famconf identifica el hukou del invididuo.
 #' @returns El dataframe de famecon pero con la variable tipo_familia, que clasifica a la familia como urbana, rural o migrante
 #' @examples 
 #' 
 clasificar_familia <- function(df_famecon, df_famconf, variable_urban, variable_hukou) {
   
   relevant_fid <- get_relevant_fid(df_famecon)
   
   # Para el dataset de 2010, hacemos una excepción determinando la clasificación familiar
   if (variable_hukou == "tb601_a_p") {
   
     household_person <- df_famecon %>% 
       inner_join(df_famconf, by = relevant_fid) %>% 
       select(pid, relevant_fid, !!sym(variable_urban), !!sym(variable_hukou)) %>% 
       mutate(
         tipo_individuo = case_when(
           !!sym(variable_urban) == 0 & !!sym(variable_hukou) == 2 ~ "Migrante (Rural a Urbano)",
           !!sym(variable_urban) == 1 & !!sym(variable_hukou) == 2 ~ "Migrante (Urbano a Rural)",
           !!sym(variable_urban) == 0 & !!sym(variable_hukou) != 2 ~ "Rural",
           !!sym(variable_urban) == 1 & !!sym(variable_hukou) != 2 ~ "Urbano",
           TRUE                        ~ NA 
         ) %>% as.factor()
       )
   } else {
     household_person <- df_famecon %>% 
       inner_join(df_famconf, by = relevant_fid) %>% 
       select(pid, relevant_fid, !!sym(variable_urban), !!sym(variable_hukou)) %>% 
       mutate(
         tipo_individuo = case_when(
           !!sym(variable_urban) == 0 & (!!sym(variable_hukou) == 3 | !!sym(variable_hukou) == 7) ~ "Migrante (Urbano a Rural)",
           !!sym(variable_urban) == 1 & !!sym(variable_hukou) == 1 ~ "Migrante (Rural a Urbano)",
           !!sym(variable_urban) == 0 & !!sym(variable_hukou) == 1 ~ "Rural",
           !!sym(variable_urban) == 1 & (!!sym(variable_hukou) == 3 | !!sym(variable_hukou) == 7) ~ "Urbano",
           TRUE                        ~ NA 
         ) %>% as.factor()
       )
   }
   
   ## Teniendo cada individuo, clasificamos a las familias según un tipo u otro
   household_family <- household_person %>%
     group_by(!!sym(relevant_fid)) %>% 
     summarise(
       tipo_familia = names(sort(table(tipo_individuo), decreasing = TRUE))[1]
     )
   
   ## El resultado se tiene que aplicar al dataframe final de famecon
   famecon_family <- df_famecon %>% 
     left_join(household_family, by = relevant_fid) 
   
   return (famecon_family)
 }

 # Ejemplo para clasificar familia
 # clasificar_familia(
 #   famecon2010_raw,
 #   famconf2010_raw,
 #   variable_urban = "urban",
 #   variable_hukou = "tb601_a_p"
 # )
 
 # clasificar_familia(
 #   famecon2014_raw,
 #   famconf2014_raw,
 #   variable_urban = "urban14.x",
 #   variable_hukou = "qa301_a14_p"
 # )

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 