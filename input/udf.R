library(dplyr)
library(tidyr)

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
  
  # Subconjunto de famecon con las variables para la estimación
  famecon_sub <- famecon_df %>%
    select(
      variables
    )
 
  # La variable fid* debe estar sin el sufijo del año
  # Obtén fid del año presente
  all_fids <- famecon_sub %>%
    select(contains("fid")) %>%
    colnames() %>% 
    sort(decreasing=TRUE)
  
  relevant_fid <- all_fids[1]
  
  famecon_sub <- famecon_sub %>% rename(fid = relevant_fid)
  
  # La variable fid no debería ser un integer, sino un character
  famecon_sub$fid <- as.character(famecon_sub$fid)
  
  return (famecon_sub)
  
}

