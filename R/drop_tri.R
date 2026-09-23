#' Eliminar sufijo '_tri' y el prefijo 'cve' de nombres de variables
#'
#' Esta funcion renombra las variables eliminando el sufijo '_tri' y el prefijo cve si existe. Para compatibilidad de la serie
#' @encoding UTF-8
#' @param data Un data.frame o tibble con nombres de variables posiblemente terminados en '_tri' y que inicien con 'cve_',
#' @return Un data.frame con los nombres de variables modificados.
#' @export
drop_tri <- function(data) {
  nombres_originales <- names(data)
  nombres_nuevos <- nombres_originales |>
    stringr::str_remove("_tri$") |>
    stringr::str_remove("^cve_")

  duplicados <- unique(nombres_nuevos[duplicated(nombres_nuevos)])
  if (length(duplicados)) {
    origenes <- vapply(duplicados, function(nombre) {
      paste(nombres_originales[nombres_nuevos == nombre], collapse = " y ")
    }, character(1L))
    stop(
      "La normalizacion de nombres produciria columnas duplicadas: ",
      paste0(duplicados, " <- ", origenes, collapse = "; "),
      call. = FALSE
    )
  }

  names(data) <- nombres_nuevos
  data
}
