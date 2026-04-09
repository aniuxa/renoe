#' Eliminar sufijo '_tri' y el prefijo 'cve' de nombres de variables
#'
#' Esta función renombra las variables eliminando el sufijo '_tri' y el prefijo cve si existe. Para compatibilidad de la serie
#' @encoding UTF-8
#' @param data Un data.frame o tibble con nombres de variables posiblemente terminados en '_tri' y que inicien con 'cve_',
#' @return Un data.frame con los nombres de variables modificados.
#' @export
drop_tri <- function(data) {
  data %<>% rename_with(~ stringr::str_remove_all(.x, "_tri"), .cols = everything())
  data %<>% rename_with(~ stringr::str_remove_all(.x, "cve_"), .cols = everything())
return(data)
  }
