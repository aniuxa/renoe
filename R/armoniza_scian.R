#' Armoniza SCIAN-Hogares observado hacia SCIAN-Hogares 2018
#'
#' Usa el regimen documentado de la ENOE: SCIAN-Hogares 2007 hasta 2021-T2
#' y SCIAN-Hogares 2018 desde 2021-T3. Conserva codigos especiales ENOE y
#' explicita los resultados preferidos, agregados y pendientes.
#'
#' @param datos data.frame con el codigo observado.
#' @param codigo nombre de la columna de codigo (por defecto, `p4a`).
#' @param periodo columna, vector o escalar `YYYY-Tn`.
#' @param version columna, vector o escalar `2007`/`2018`. Si se omite, se
#'   infiere de `periodo`.
#' @param adapter_path Ruta opcional al adaptador. Por defecto usa la copia
#'   versionada incluida en el paquete.
#' @param catalog_path ruta al catalogo oficial verificado. Si es NULL, se
#'   localiza dentro del mismo producto que `adapter_path`.
#' @param estricto detenerse si quedan codigos no resueltos.
#' @param detalle Si es `TRUE`, conserva toda la trazabilidad. La salida
#'   compacta tambien conserva calidad, regla y motivo de pendiente.
#' @return `datos` con codigo original, version, resultado y trazabilidad.
#' @keywords internal
.armonizar_scian_hogares_core <- function(
  datos,
  codigo = "p4a",
  periodo = NULL,
  version = NULL,
  adapter_path = NULL,
  catalog_path = NULL,
  estricto = FALSE,
  detalle = TRUE
) {
  stopifnot(is.data.frame(datos), length(codigo) == 1L, codigo %in% names(datos))

  resolver <- function(x, nombre) {
    if (is.null(x)) return(NULL)
    if (length(x) == 1L && is.character(x) && x %in% names(datos)) x <- datos[[x]]
    if (length(x) == 1L) x <- rep(x, nrow(datos))
    if (length(x) != nrow(datos)) {
      stop(nombre, " debe ser escalar, columna de datos o vector de nrow(datos).")
    }
    as.character(x)
  }

  observado <- trimws(as.character(datos[[codigo]]))
  observado <- sub("[.]0$", "", observado)
  periodo_observado <- resolver(periodo, "periodo")
  version_observada <- resolver(version, "version")

  if (is.null(version_observada)) {
    if (is.null(periodo_observado)) stop("Se requiere `version` o `periodo`.")
    valido <- grepl("^[0-9]{4}-T[1-4]$", periodo_observado)
    if (!all(valido | is.na(periodo_observado))) stop("`periodo` debe usar YYYY-Tn.")
    indice <- suppressWarnings(
      as.integer(substr(periodo_observado, 1L, 4L)) * 10L +
        as.integer(substr(periodo_observado, 7L, 7L))
    )
    version_observada <- ifelse(indice <= 20212L, "2007", "2018")
  }
  version_observada <- sub(
    "^SCIAN-Hogares[[:space:]]+", "", trimws(version_observada)
  )
  if (!all(version_observada %in% c("2007", "2018") | is.na(version_observada))) {
    stop("Las versiones admitidas son 2007 y 2018.")
  }

  if (is.null(adapter_path)) {
    adapter_path <- system.file("extdata", "metodologia_scian",
      "adapter_stable_runtime.csv", package = "renoe")
  }
  if (!nzchar(adapter_path)) stop("No se encontro el adaptador SCIAN del paquete.")
  puente <- utils::read.csv(
    adapter_path,
    colClasses = "character",
    check.names = FALSE,
    fileEncoding = "UTF-8-BOM"
  )
  if (anyDuplicated(puente$source_code)) {
    stop("El adaptador estable debe tener una fila por source_code.")
  }
  if (is.null(catalog_path)) {
    catalog_path <- system.file("extdata", "metodologia_scian",
      "scian2018_codes_runtime.csv", package = "renoe")
  }
  catalogo <- utils::read.csv(
    catalog_path,
    colClasses = "character",
    check.names = FALSE,
    fileEncoding = "UTF-8-BOM"
  )
  codigos_2018 <- catalogo$codigo
  if (!length(codigos_2018)) stop("No se encontraron c\u00F3digos SCIAN-Hogares 2018.")

  i <- match(observado, puente$source_code)
  es_2007 <- !is.na(version_observada) & version_observada == "2007"
  es_2018 <- !is.na(version_observada) & version_observada == "2018"

  armonizado <- rep(NA_character_, nrow(datos))
  valido_2018 <- es_2018 & observado %in% codigos_2018
  armonizado[valido_2018] <- observado[valido_2018]
  armonizado[es_2007] <- puente$target_code[i[es_2007]]

  estado <- rep("unresolved", nrow(datos))
  evidencia <- rep("manual", nrow(datos))
  nivel <- rep(NA_character_, nrow(datos))
  estado[valido_2018] <- "unique"
  evidencia[valido_2018] <- "official_exact"
  nivel[valido_2018] <- ifelse(nchar(observado[valido_2018]) == 4L, "4d", "observado")
  valido_2007 <- es_2007 & !is.na(i)
  estado[valido_2007] <- puente$decision_status[i[valido_2007]]
  evidencia[valido_2007] <- puente$evidence_level[i[valido_2007]]
  nivel[valido_2007] <- puente$max_supported_level[i[valido_2007]]

  catalog_status <- rep("not_in_catalog", nrow(datos))
  catalog_status[is.na(observado) | !nzchar(observado)] <- "missing"
  catalog_status[es_2007 & !is.na(i)] <- "valid_operational_2007"
  catalog_status[valido_2018] <- "valid_official_2018"

  sin_resolver <- is.na(armonizado) & !is.na(observado) & nzchar(observado)
  if (estricto && any(sin_resolver)) {
    stop("C\u00F3digos SCIAN sin resolver: ", paste(unique(observado[sin_resolver]), collapse = ", "))
  }

  datos$scian_codigo_observado <- observado
  datos$scian_version_observada <- version_observada
  datos$scian_codigo_2018 <- armonizado
  datos$scian_catalogo_origen <- ifelse(
    is.na(version_observada), NA_character_,
    paste("SCIAN-Hogares", version_observada)
  )
  datos$scian_catalogo_destino <- "SCIAN-Hogares 2018"
  datos$scian_codigo_armonizado <- armonizado
  datos$scian_catalog_status <- catalog_status
  datos$scian_nivel_sustentado <- nivel
  datos$scian_decision_status <- estado
  datos$scian_evidence_level <- evidencia
  datos$scian_decision_phase <- ifelse(
    is.na(armonizado), "not_applicable", "integrated"
  )
  datos$scian_decision_layer <- ifelse(
    valido_2018, "official",
    ifelse(valido_2007 & !is.na(armonizado), "adapter", "unresolved")
  )
  datos$scian_regla_id <- ifelse(
    es_2018, "SCIAN2018_OBSERVADO",
    ifelse(!is.na(i), paste0("SCIAN2007_", observado), NA_character_)
  )
  datos$scian_n_destinos <- ifelse(
    observado == "5620" & es_2007, 2L,
    ifelse(!is.na(armonizado), 1L, NA_integer_)
  )
  datos$scian_destinos_plausibles <- ifelse(
    observado == "5620" & es_2007, "5621|5622", armonizado
  )
  datos$scian_auxiliares_usados <- NA_character_
  datos$scian_motivo_pendiente <- ifelse(
    is.na(observado) | !nzchar(observado), "codigo_faltante",
    ifelse(is.na(armonizado), "codigo_no_en_catalogo_o_adaptador", NA_character_)
  )
  datos$scian_capas_activas <- "oficial+adaptador"
  if (!isTRUE(detalle)) {
    quitar <- c("scian_catalog_status", "scian_n_destinos",
      "scian_destinos_plausibles", "scian_auxiliares_usados",
      "scian_decision_layer", "scian_decision_phase")
    datos <- datos[setdiff(names(datos), quitar)]
  }
  datos
}
