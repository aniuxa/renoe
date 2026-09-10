.leer_ipc_enoe <- function() {
  ipc_path <- system.file("extdata", "ipc.rds", package = "renoe")
  if (length(ipc_path) != 1L || !nzchar(ipc_path)) {
    stop("No se encontró el archivo ipc.rds en inst/extdata/.", call. = FALSE)
  }
  readRDS(ipc_path)
}

#' Añadir IPC al conjunto fusionado de ENOE
#'
#' Esta función agrega una columna llamada `ipc` al objeto fusionado de la ENOE,
#' correspondiente al promedio trimestral del Índice de Precios al Consumidor (IPC).
#'
#' El archivo `ipc.rds` debe estar ubicado en `inst/extdata/` y contener
#' las columnas numéricas `anio`, `trim` e `ipc`, una fila por trimestre. La
#' función se detiene si el recurso tiene claves duplicadas, valores inválidos o
#' no contiene el periodo solicitado; así se evita propagar ingresos
#' deflactados ausentes sin advertencia suficiente.
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
    stop("El objeto proporcionado debe ser un data.frame fusionado de ENOE.",
         call. = FALSE)
  }
  if (length(anio) != 1L || !is.numeric(anio) || is.na(anio) ||
      !is.finite(anio) || anio != floor(anio)) {
    stop("`anio` debe ser un número escalar, finito e íntegro.", call. = FALSE)
  }
  if (length(trimestre) != 1L || !is.numeric(trimestre) || is.na(trimestre) ||
      !is.finite(trimestre) || trimestre != floor(trimestre) ||
      !trimestre %in% 1:4) {
    stop("`trimestre` debe ser un número escalar e íntegro entre 1 y 4.",
         call. = FALSE)
  }

  ipc_tabla <- .leer_ipc_enoe()
  requeridas <- c("anio", "trim", "ipc")
  if (!is.data.frame(ipc_tabla) ||
      !all(requeridas %in% names(ipc_tabla)) ||
      !all(vapply(ipc_tabla[requeridas], is.numeric, logical(1))) ||
      anyNA(ipc_tabla[requeridas]) ||
      any(!is.finite(ipc_tabla$anio)) ||
      any(ipc_tabla$anio != floor(ipc_tabla$anio)) ||
      any(!is.finite(ipc_tabla$trim)) ||
      any(ipc_tabla$trim != floor(ipc_tabla$trim)) ||
      any(!ipc_tabla$trim %in% 1:4) ||
      any(!is.finite(ipc_tabla$ipc)) ||
      any(ipc_tabla$ipc <= 0)) {
    stop(
      "ipc.rds debe contener anio, trim e ipc numéricos, íntegros donde ",
      "corresponde, sin faltantes y con IPC positivo.",
      call. = FALSE
    )
  }
  if (anyDuplicated(ipc_tabla[c("anio", "trim")])) {
    stop("ipc.rds contiene claves duplicadas de anio y trim.", call. = FALSE)
  }

  coincide <- ipc_tabla$anio == anio & ipc_tabla$trim == trimestre
  if (!any(coincide)) {
    stop(
      "No se encontró un valor de IPC para ", anio, "-T", trimestre, ".",
      call. = FALSE
    )
  }
  ipc_valor <- ipc_tabla$ipc[coincide]
  datos_fusionados$ipc <- rep(ipc_valor, nrow(datos_fusionados))
  datos_fusionados
}
