#' Procesar el perfil reproducible de productos academicos
#'
#' Este wrapper no mantiene reglas propias. Si recibe microdatos sin el
#' contrato SINCO, ejecuta la ruta canonica completa; si recibe una salida ya
#' armonizada, aplica unicamente los consumidores reproducibles.
#'
#' @param data Microdatos ENOE o salida canonica ya armonizada.
#' @param anio Año requerido cuando `data` aun no fue procesado.
#' @param trimestre Trimestre requerido cuando `data` aun no fue procesado.
#' @param escenario Escenario explicito de armonizacion y consumidores.
#' @param ... Argumentos adicionales de [procesar_variables_enoe()].
#' @return El data frame con el perfil reproducible, su escenario y la marca
#'   `academic_reproducible` cuando se solicita `analysis_legacy`. Esta marca
#'   identifica la ruta que puede alimentar productos académicos congelados;
#'   no sustituye un manifiesto, `run_id` ni hashes del bundle.
#' @export
procesar_productos_academicos <- function(
    data,
    anio = NULL,
    trimestre = NULL,
    escenario = c("integrated_accepted", "official_strict", "analysis_legacy"),
    ...) {
  escenario <- match.arg(escenario)
  armonizada <- all(c(
    "sinco2011_comparable", "sinco_escenario", "campo_arm8_horizontal"
  ) %in% names(data))
  if (!armonizada) {
    if (is.null(anio) || is.null(trimestre)) {
      stop("Declare `anio` y `trimestre` para ejecutar la ruta can\u00F3nica.",
           call. = FALSE)
    }
    salida <- procesar_variables_enoe(
      data, anio = anio, trimestre = trimestre,
      escenario_clasificadores = escenario, ...
    )
  } else {
    escenario_actual <- unique(as.character(data$sinco_escenario))
    if (length(escenario_actual) != 1L || escenario_actual != escenario) {
      stop("El escenario solicitado no coincide con la armonizaci\u00F3n recibida.",
           call. = FALSE)
    }
    salida <- procesar_clasificaciones_reproducibles(
      data, escenario = escenario
    )
  }
  salida$perfil_productos_reproducibles <- rep(
    "productos_reproducibles_v1", nrow(salida)
  )
  salida$productos_academicos_escenario <- rep(escenario, nrow(salida))
  salida$perfil_publicacion <- rep(
    if (identical(escenario, "analysis_legacy")) {
      "academic_reproducible"
    } else {
      NA_character_
    },
    nrow(salida)
  )
  salida
}
