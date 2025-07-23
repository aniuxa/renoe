#' Eliminar sufijo '_tri' de nombres de variables
#'
#' Esta función renombra las variables eliminando el sufijo '_tri' si existe.
#' @encoding UTF-8
#' @param data Un data.frame o tibble con nombres de variables posiblemente terminados en '_tri'.
#' @return Un data.frame con los nombres de variables modificados.
#' @export
drop_tri <- function(data) {
  data %>% rename_with(~ stringr::str_remove_all(.x, "_tri"), .cols = everything())
}
