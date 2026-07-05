library(dplyr)
library(tidyr)
library(stringr)

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
  all_fids <- famecon_sub %>%
    select(contains("fid")) %>%
    colnames() %>%
    str_subset("^fid\\d*$") %>%
    sort(decreasing=TRUE)
  
  relevant_fid <- all_fids[1]
  
  famecon_sub <- famecon_sub %>%
    rename(fid = relevant_fid)
  
  # La variable fid no debería ser un integer, sino un character
  famecon_sub$fid <- as.character(famecon_sub$fid)
  
  # Devolvemos el dataset final
  famecon_sub <- famecon_sub %>% select(fid, provcd, variables)
  
  return (famecon_sub)
  
}

# Ejecución de prueba
 famecon_cleaner(
   famecon2020,
   c(
     "fincome1",
     "fincome1_per",
     "pce",
     "food",
     "dress",
     "house",
     "daily",
     "med",
     "trco"
   )
 )

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
