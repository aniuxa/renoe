#' Armonizar SCIAN-Hogares antes de la ocupacion
#'
#' El periodo se infiere de `anio` y `trim` cuando no se declara expresamente.
#' Conserva `p4a` observado y toda la procedencia del adaptador SCIAN.
#' @param datos Microdatos ENOE con `anio`, `trim` y `p4a`.
#' @param ... Argumentos adicionales para el adaptador SCIAN-Hogares interno.
#' @export
armonizar_scian <- function(datos, ...) {
  argumentos <- list(...)
  if (is.null(argumentos$periodo) && is.null(argumentos$version)) {
    requeridas <- c("anio", "trim")
    faltantes <- setdiff(requeridas, names(datos))
    if (length(faltantes)) {
      stop("Faltan variables para inferir el regimen SCIAN: ",
           paste(faltantes, collapse = ", "), call. = FALSE)
    }
    trimestre <- sub("^t", "", tolower(as.character(datos$trim)))
    if (any(is.na(trimestre) | !trimestre %in% as.character(1:4))) {
      stop("`trim` debe identificar trimestres entre 1 y 4.", call. = FALSE)
    }
    argumentos$periodo <- paste0(as.character(datos$anio), "-T", trimestre)
  }
  do.call(.armonizar_scian_hogares_core,
          c(list(datos = datos), argumentos))
}

#' Armonizar ocupacion hacia SINCO 2011 con un contrato auditable
#'
#' Produce el contrato de salida SINCO y agrega los campos
#' `sinco2011_*` que consumen las clasificaciones existentes. Una seleccion
#' por panel es `preferred`; una condicion ENOE es `conditional`, nunca
#' una equivalencia oficial `unique` a cuatro digitos.
#' La clave 9999 puede corresponder oficialmente a 9999 entre catalogos,
#' pero `sinco2011_codigo_especial = TRUE` y
#' `sinco2011_comparable = FALSE` impiden tratarla como ocupacion
#' sustantivamente identificada en clasificaciones derivadas.
#' @param datos Microdatos ENOE con `anio`, `trim` y `p3coe`.
#' @param escenario Contrato de decision: `official_strict` conserva solo
#'   equivalencias oficiales; `integrated_accepted` agrega reglas aceptadas de
#'   panel y condiciones ENOE; `analysis_legacy` habilita ademas rescates
#'   historicos no transportables.
#' @param legacy Compatibilidad explicita. `TRUE` selecciona
#'   `analysis_legacy`; `FALSE` impide combinar ese escenario.
#' @param capas Interfaz de bajo nivel. Si se proporciona se respeta, pero no
#'   habilita por si sola rescates historicos.
#' @param ... Argumentos adicionales para el motor SINCO interno.
#' @export
armonizar_sinco <- function(
    datos,
    escenario = c("integrated_accepted", "official_strict", "analysis_legacy"),
    legacy = NULL,
    capas = NULL,
    ...) {
  contrato <- .normalizar_escenario_clasificador(escenario, legacy)
  if (!is.null(capas)) contrato$capas <- capas
  do.call(
    .armonizar_sinco_enoe_core,
    c(list(
      data = datos,
      capas = contrato$capas,
      permitir_manual_1d = contrato$legacy,
      escenario = contrato$escenario
    ), list(...))
  )
}

#' Alias historico de armonizacion SINCO
#'
#' Conserva la API publicada antes de 0.3.0, emite una advertencia de
#' deprecacion y usa exactamente la misma ruta canonica.
#' @param data Microdatos ENOE.
#' @param ... Argumentos de [armonizar_sinco()].
#' @export
armoniza_sinco <- function(data, ...) {
  .Deprecated("armonizar_sinco", package = "renoe")
  armonizar_sinco(data, ...)
}

.contrato_sinco <- function(resultado, argumentos) {
  n <- nrow(resultado)
  tipo <- as.character(resultado$tipo_regla_cmo_sinco)
  origen <- as.character(resultado$version_sinco_origen)
  panel <- !is.na(tipo) & tipo == "panel_global"
  auxiliar <- !is.na(tipo) & tipo %in%
    c("scian_sector", "p4a_scian2", "p4f", "p4a_p4f",
      "manual_cmo_to_sinco1d")
  convergencia_3d <- !is.na(tipo) & tipo == "official_convergence_3d"
  observado <- origen == "SINCO 2011" & !is.na(resultado$sinco4d)
  nivel <- as.character(resultado$nivel_maximo_sinco)
  granularidad <- suppressWarnings(as.integer(sub("d$", "", nivel)))
  consenso_2019_3d <- origen == "SINCO 2019" &
    is.na(resultado$sinco4d) & !is.na(resultado$sinco3d)
  codigo <- ifelse(
    is.na(resultado$codigo_ocupacion_original), NA_character_,
    stringr::str_pad(as.character(resultado$codigo_ocupacion_original),
                     width = 4L, side = "left", pad = "0"))

  # Recuperar las alternativas catalogales, no el numero de destinos que
  # queda despues de escoger uno mediante panel o una condicion.
  alternativas <- rep(NA_character_, n)
  ruta_cmo <- system.file("extdata", "puente_cmo_sinco2011_oficial.csv",
                          package = "renoe")
  perfil_cmo <- if (is.null(argumentos$codigos) && nzchar(ruta_cmo)) {
    utils::read.csv(ruta_cmo, colClasses = "character", check.names = FALSE)
  } else {
    argumentos$codigos
  }
  if (!is.null(perfil_cmo) &&
      all(c("source_code", "destinations_4d") %in% names(perfil_cmo))) {
    clave <- stringr::str_pad(as.character(perfil_cmo$source_code),
                              width = 4L, side = "left", pad = "0")
    i <- match(codigo, clave)
    es_cmo <- !is.na(origen) & origen == "CMO"
    alternativas[es_cmo] <- as.character(perfil_cmo$destinations_4d[i[es_cmo]])
  }
  ruta_2019 <- system.file("extdata", "puente_sinco2019_sinco2011.csv",
                           package = "renoe")
  perfil_2019 <- if (is.null(argumentos$correspondencia_2019) &&
                     nzchar(ruta_2019)) {
    utils::read.csv(ruta_2019, colClasses = "character", check.names = FALSE)
  } else {
    argumentos$correspondencia_2019
  }
  if (!is.null(perfil_2019) &&
      all(c("sinco2019", "sinco2011") %in% names(perfil_2019))) {
    por_origen <- split(as.character(perfil_2019$sinco2011),
                        as.character(perfil_2019$sinco2019))
    por_origen <- lapply(por_origen, function(x) {
      x <- unique(x[!is.na(x) & nzchar(x)])
      if (length(x)) paste(x, collapse = "|") else NA_character_
    })
    es_2019 <- !is.na(origen) & origen == "SINCO 2019"
    alternativas[es_2019] <- vapply(codigo[es_2019], function(x) {
      z <- por_origen[[x]]
      if (is.null(z)) NA_character_ else z
    }, character(1L))
  }
  alternativas[observado] <- codigo[observado]
  alternativas[is.na(alternativas) | !nzchar(alternativas)] <- NA_character_
  n_oficial <- ifelse(
    is.na(alternativas), NA_integer_,
    lengths(strsplit(ifelse(is.na(alternativas), "", alternativas), "|",
                     fixed = TRUE)))
  elegido <- ifelse(
    is.na(resultado$sinco4d), NA_character_,
    stringr::str_pad(as.character(resultado$sinco4d),
                     width = 4L, side = "left", pad = "0"))
  en_puente <- rep(NA, n)
  comparar <- !is.na(elegido) & !is.na(alternativas)
  en_puente[comparar] <- vapply(which(comparar), function(i) {
    elegido[i] %in% strsplit(alternativas[i], "|", fixed = TRUE)[[1L]]
  }, logical(1L))
  oficial_4d <- !is.na(elegido) & !panel & !auxiliar &
    !is.na(n_oficial) & n_oficial == 1L & en_puente %in% TRUE
  analitico_4d <- !is.na(elegido) & !panel & !auxiliar &
    !oficial_4d & !observado

  estado <- as.character(resultado$sinco_decision_status)
  estado[panel] <- "preferred"
  estado[auxiliar & !is.na(granularidad)] <- "conditional"
  estado[convergencia_3d] <- "unique"
  estado[consenso_2019_3d] <- "unique"
  estado[analitico_4d] <- "preferred"
  sin_destino <- !is.na(codigo) & is.na(alternativas) &
    is.na(granularidad)
  estado[sin_destino] <- "no_official_destination"
  resultado$sinco_decision_status <- estado
  resultado$sinco2011_decision_status <- estado
  resultado$version_sinco_destino <- rep("SINCO 2011", n)
  resultado$sinco2011_granularidad <- granularidad
  resultado$sinco2011_nivel_sustentado <- nivel
  codigo_especial <- codigo == "9999" |
    (!is.na(resultado$sinco4d) &
                       resultado$sinco4d == 9999L) |
    (!is.na(resultado$sinco3d) & resultado$sinco3d == 999L)
  resultado$sinco2011_codigo_especial <- codigo_especial
  resultado$sinco2011_comparable <-
    !codigo_especial & !is.na(resultado$sinco1d)
  resultado$sinco2011_apto_4d <- !is.na(granularidad) & granularidad >= 4L
  resultado$sinco2011_apto_3d <- !is.na(granularidad) & granularidad >= 3L
  resultado$sinco2011_apto_1d <- !is.na(granularidad) & granularidad >= 1L
  resultado$sinco2011_prioridad <- ifelse(
    observado, 0L,
    ifelse(panel | auxiliar, 2L,
      ifelse(oficial_4d, 1L,
        ifelse(convergencia_3d | consenso_2019_3d, 3L, 4L))))
  fase <- ifelse(is.na(granularidad), "not_applicable", "candidate")
  fase[observado | oficial_4d | convergencia_3d |
       consenso_2019_3d] <- "integrated"
  fase[panel | auxiliar] <- "accepted"
  resultado$sinco_decision_phase <- fase
  resultado$sinco2011_decision_phase <- fase
  evidencia <- as.character(resultado$sinco_evidence_level)
  evidencia[observado] <- "observed"
  evidencia[oficial_4d] <- "official_exact"
  evidencia[convergencia_3d | consenso_2019_3d] <- "aggregation"
  evidencia[panel] <- "panel"
  evidencia[auxiliar] <- "auxiliary"
  evidencia[analitico_4d] <- "manual"
  evidencia[is.na(granularidad) & !is.na(n_oficial) &
             n_oficial > 1L] <- "official_multiple"
  resultado$sinco_evidence_level <- evidencia
  resultado$sinco2011_evidence_level <- evidencia
  resultado$sinco2011_regla_id <- ifelse(
    !is.na(resultado$regla_cmo_sinco),
    as.character(resultado$regla_cmo_sinco),
    ifelse(observado, "SINCO2011_OBSERVED",
      ifelse(origen == "SINCO 2019" & oficial_4d,
             "SINCO2019_OFFICIAL_UNIQUE_4D",
        ifelse(consenso_2019_3d,
               "SINCO2019_OFFICIAL_CONSENSUS_3D", NA_character_))))
  resultado$sinco2011_condicion_auxiliar <- ifelse(
    auxiliar, as.character(resultado$detalle_regla_cmo_sinco), NA_character_)
  adaptador <- rep(NA_character_, n)
  adaptador[auxiliar & tipo == "p4a_scian2"] <-
    "p4a_observado_SCIANHogares2007_prefijo2d"
  adaptador[auxiliar & tipo == "scian_sector"] <-
    "scian_agregado_ENOE_observado_version_pendiente"
  adaptador[auxiliar & tipo %in% c("p4f", "p4a_p4f")] <-
    "ENOE_observado_version_pendiente"
  resultado$sinco2011_adaptador_auxiliar <- adaptador
  resultado$sinco_escenario <- rep(
    if (is.null(argumentos$escenario)) "integrated_accepted" else
      argumentos$escenario,
    n
  )
  resultado$sinco_transportable <- !auxiliar |
    tipo != "manual_cmo_to_sinco1d"
  resultado$sinco_razon_no_clasificacion <- dplyr::case_when(
    codigo_especial ~ "codigo_especial_no_comparable",
    is.na(resultado$sinco1d) ~ as.character(resultado$sinco_motivo_pendiente),
    TRUE ~ NA_character_
  )
  resultado$codigo_ocupacion_original_txt <- codigo
  resultado$sinco2011_destinos_oficiales_4d <- alternativas
  resultado$sinco2011_destino_en_puente_oficial <- en_puente
  resultado$n_destinos_sinco <- ifelse(
    is.na(n_oficial), as.integer(resultado$n_destinos_sinco), n_oficial
  )
  resultado
}
