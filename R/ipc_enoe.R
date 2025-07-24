#' Añadir IPC al conjunto fusionado de ENOE
#'
#' Esta función agrega una columna llamada `ipc` al objeto fusionado de la ENOE,
#' correspondiente al promedio trimestral del Índice de Precios al Consumidor (IPC).
#'
#' El archivo `ipc.rds` debe estar ubicado en `inst/extdata/` y contener
#' las columnas `anio`, `trim` e `ipc`.
#'
#' @param datos_fusionados Un data.frame ya fusionado con `fusion_enoe()`.
#' @param anio Año del trimestre (numérico).
#' @param trimestre Trimestre (1 a 4).
#'
#' @return El mismo `data.frame` con una nueva columna `ipc`.
#' @export
#'
#' @examples
#' \dontrun{
#' datos <- fusion_enoe(2023, 1)
#' datos <- ipc_enoe(datos, 2023, 1)
#' }
#' @family procesamiento_enoe

ipc_enoe <- function(datos_fusionados, anio, trimestre) {
  if (!is.data.frame(datos_fusionados)) {
    stop("El objeto proporcionado debe ser un data.frame fusionado de ENOE")
  }
  if (!is.numeric(anio) || !is.numeric(trimestre)) {
    stop("anio y trimestre deben ser numéricos")
  }

  ipc_path <- system.file("extdata", "ipc.rds", package = "renoe")
  if (ipc_path == "") stop("No se encontró el archivo ipc.rds en inst/extdata/")

  ipc_tabla <- readRDS(ipc_path)

  ipc_valor <- ipc_tabla[ipc_tabla$anio == anio & ipc_tabla$trim == trimestre,]$ipc

  if (length(ipc_valor) != 1) {
    warning("No se encontró valor único de IPC para ", anio, " trimestre ", trimestre)
    datos_fusionados$ipc <- NA_real_
  } else {
    datos_fusionados$ipc <- ipc_valor
  }

  return(datos_fusionados)
}
