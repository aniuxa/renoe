# Actualizar solo despues de validar la publicacion y sus cinco tablas.
.ultimo_periodo_enoe <- c(anio = 2026L, trimestre = 2L)

.validar_periodo_enoe <- function(anio, trimestre, exigir_disponible = TRUE) {
  entero_escalar <- function(x) {
    is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) && x == floor(x)
  }
  if (!entero_escalar(anio) || !entero_escalar(trimestre) ||
      anio < 2005 || !trimestre %in% 1:4) {
    stop("A\u00F1o y trimestre deben ser valores escalares enteros: a\u00F1o >= 2005 y trimestre entre 1 y 4", call. = FALSE)
  }
  if (anio == 2020L && trimestre == 2L) {
    stop("No existe 2020-T2 por la suspensi\u00F3n del levantamiento regular de la ENOE", call. = FALSE)
  }
  disponible <- anio < .ultimo_periodo_enoe[['anio']] ||
    (anio == .ultimo_periodo_enoe[['anio']] && trimestre <= .ultimo_periodo_enoe[['trimestre']])
  if (!disponible && exigir_disponible) {
    stop(sprintf("Periodo %d-T%d no habilitado; el ultimo validado es %d-T%d",
                 anio, trimestre, .ultimo_periodo_enoe[['anio']],
                 .ultimo_periodo_enoe[['trimestre']]), call. = FALSE)
  }
  disponible
}

.validar_opciones_descarga <- function(intentos, timeout_sec, verificar_url, cache) {
  if (!is.numeric(intentos) || length(intentos) != 1L || is.na(intentos) ||
      !is.finite(intentos) || intentos < 1 || intentos != floor(intentos)) {
    stop('`intentos` debe ser un entero positivo.', call. = FALSE)
  }
  if (!is.numeric(timeout_sec) || length(timeout_sec) != 1L || is.na(timeout_sec) ||
      !is.finite(timeout_sec) || timeout_sec <= 0) {
    stop('`timeout_sec` debe ser un numero positivo y finito.', call. = FALSE)
  }
  if (!is.logical(verificar_url) || length(verificar_url) != 1L || is.na(verificar_url) ||
      !is.logical(cache) || length(cache) != 1L || is.na(cache)) {
    stop('`verificar_url` y `cache` deben ser valores logicos escalares.', call. = FALSE)
  }
  invisible(NULL)
}
