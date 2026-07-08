library(dplyr)
library(tidyr)
library(glue)
library(stringr)
library(rlang)


#' Obtiene el nombre de una var relevante para el famecon. Normalmente es el que se corresponde
#' con el año en el que está el dataset. Pensado para variables que tienen nombres diferentes de
#' una muestra a otra (ej provcd frente a provcd14)
#' 
#' @param famecon_df Un dataframe de famecon
#' @param var_name Nombre de la variable que se desea capturar entre múltiples. Algunos
#'  ejemplos son provcd, familysize o fid
#' @returns Nombre de la variable correcta, que corresponde al año del famecon analizado
get_relevant_name <- function(famecon_df, var_name) {
  
  # Se busca un nombre que opcionalmente puede contener dígitos después
  regex_pattern <- glue("^{var_name}\\d*$")
  
  all_names <- famecon_df %>%
    select(contains(var_name)) %>%
    colnames() %>%
    str_subset(regex_pattern) %>%
    sort(decreasing=TRUE)
  
  relevant_name <- all_names[1]
  
  return (relevant_name)
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

  # En primer lugar obtenemos nombres relevantes que pueden contener un sufijo
  # numérico
  relevant_fid        <- get_relevant_name(famecon_df, var_name = "fid")
  relevant_familysize <- get_relevant_name(famecon_df, var_name = "familysize")
  
  # Si hay una var llamada faminc_net, renombrarla a fincome1
  if(any(str_detect(names(famecon_df), "faminc_net"))){

    names(famecon_df) <- str_replace(names(famecon_df), "faminc_net", "fincome1")
    
  }
  
  # Si hay una var llamada resivalue_new, renombrar a resivalue
  if(any(str_detect(names(famecon_df), "resivalue_new"))){
    
    names(famecon_df) <- str_replace(names(famecon_df), "resivalue_new", "resivalue")
    
  }
  
  # Si no hay fincome1_per, la calcularemos
  if(!any(str_detect(names(famecon_df), "fincome1_per"))){
    
    famecon_df <- famecon_df %>%
      mutate(
        fincome1_per = fincome1 / !!sym(relevant_familysize)
      )
    
  }
  
  # pce se ha de calcular en términos per capita también
  # if(!any(str_detect(names(famecon_df), "pce_per"))){
  # 
  #   famecon_df <- famecon_df %>%
  #     mutate(
  #       pce_per = pce / !!sym(relevant_familysize)
  #     )
  # 
  # }
  
  # En realidad tenemos que calcular el per capita de todas las variables
  # numericas
  famecon_df <- famecon_df %>% 
    mutate(
      across(
        c("pce", "food", "dress", "house", "daily", "med", "trco", "total_asset"),
        ~ . / !!sym(relevant_familysize)
      )
    )
  
  
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
 #   famecon2016_raw,
 #   c(
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
   
   relevant_fid <- get_relevant_name(df_famecon, var_name = "fid")
   
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
           !!sym(variable_urban) == 0 & (!!sym(variable_hukou) == 3 | !!sym(variable_hukou) == 7) ~ "Migrante (Urbano a Rural)", # Vive en Rural, y hukou urbano
           !!sym(variable_urban) == 1 & !!sym(variable_hukou) == 1 ~ "Migrante (Rural a Urbano)", # Vive en Urbano, y hukou Rural
           !!sym(variable_urban) == 0 & !!sym(variable_hukou) == 1 ~ "Rural", # Vive en Rural y hukou Rural
           !!sym(variable_urban) == 1 & (!!sym(variable_hukou) == 3 | !!sym(variable_hukou) == 7) ~ "Urbano", # vive en urbano y hukou urbano
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

 # clasificar_familia(
 #   famecon2022_raw,
 #   famconf2022_raw,
 #   variable_urban = "urban22",
 #   variable_hukou = "hukou_a22_p"
 # ) 

 
 
 
 
 
 
 
 
 
 
 