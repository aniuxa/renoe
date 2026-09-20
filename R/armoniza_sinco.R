#' Armonizar ocupaciones CMO, SINCO 2011 y SINCO 2019
#'
#' Construye codigos comparables en SINCO 2011 a partir de CMO entre 2005-I y
#' 2012-II, SINCO 2011 observado entre 2012-III y 2021-II, y SINCO 2019 desde
#' 2021-III. Para el ultimo periodo utiliza la tabla de equivalencia oficial
#' SINCO 2011-2019 y conserva sin resolver las correspondencias multiples.
#'
#' @param data Data frame con `anio`, `trim` y `p3coe`.
#' @param codigos Tabla opcional de correspondencia CMO-SINCO usada antes de
#'   2012-III.
#' @param correspondencia_2019 Tabla opcional, en formato largo, del puente
#'   SINCO 2019-SINCO 2011.
#' @param usar_reglas_enoe Compatibilidad: TRUE activa todas las capas;
#'   FALSE selecciona solo oficial. NULL utiliza `capas`.
#' @param capas Capas habilitadas: `oficial`, `panel`, `enoe` y `consenso`, todas por
#'   defecto. Oficial siempre se incluye. Panel identifica reglas validadas
#'   longitudinalmente; enoe habilita reglas que requieren otras preguntas
#'   ENOE, incluido el puente historico a un digito.
#'
#' La capa consenso actua posteriormente en
#' [procesar_clasificaciones_reproducibles()];
#' no aumenta la desagregacion SINCO identificada por esta funcion.
#' @param detalle Si es `TRUE`, devuelve trazabilidad auditable. La salida
#'   compacta conserva calidad, regla aplicada y motivo de pendiente.
#'
#' @return El data frame con `sinco4d`, `sinco3d`, `sinco2d` y `sinco1d`
#'   armonizados, ademas de variables de procedencia y calidad.
#' @keywords internal
#' @encoding UTF-8
#' @family procesamiento_enoe
#' @references
#' INEGI (2020). *Sistema Nacional de Clasificacion de Ocupaciones 2019*.
#' Anexo: Tabla de equivalencia SINCO 2011-2019.
#'
#' Escoto Castillo, A. y Sanchez Pena, L. (2024). *El riesgo de automatizacion
#' en Mexico: diferencias temporales y generacionales entre las distintas
#' ocupaciones*. CEPAL. \url{https://hdl.handle.net/11362/69015}
#'
#' @examples
#' datos <- data.frame(
#'   anio = c(2012, 2021, 2021),
#'   trim = c(3, 2, 3),
#'   p3coe = c(2436, 2423, 2433),
#'   pos_ocu = 1,
#'   tue2 = 1
#' )
#' armonizar_sinco(datos)
.armonizar_sinco_enoe_core <- function(
    data, codigos = NULL, correspondencia_2019 = NULL,
    usar_reglas_enoe = NULL,
    capas = c("oficial", "panel", "enoe", "consenso"), detalle = TRUE) {

  capas <- .normalizar_capas_cmo_sinco(capas, usar_reglas_enoe)

  requeridas <- c("anio", "trim", "p3coe")
  faltantes <- setdiff(requeridas, names(data))
  if (length(faltantes) > 0L) {
    stop("Faltan variables: ", paste(faltantes, collapse = ", "), call. = FALSE)
  }

  trimestre_n <- suppressWarnings(as.integer(
    stringr::str_remove(tolower(as.character(data$trim)), "^t")
  ))
  if (any(is.na(trimestre_n) | !trimestre_n %in% 1:4)) {
    stop("`trim` debe identificar trimestres entre 1 y 4.", call. = FALSE)
  }

  periodo_cmo <- data$anio < 2012L |
    (data$anio == 2012L & trimestre_n <= 2L)
  periodo_sinco2011 <- !periodo_cmo & (
    data$anio < 2021L | (data$anio == 2021L & trimestre_n <= 2L)
  )
  periodo_sinco2019 <- data$anio > 2021L |
    (data$anio == 2021L & trimestre_n >= 3L)
  codigo_original <- suppressWarnings(as.integer(as.character(data$p3coe)))
  # La tabla base y las reglas ENOE se aplican solo al periodo CMO.
  data <- renoe::cmo_to_sinco(
    data, codigos = codigos, usar_reglas_enoe = NULL, capas = capas
  )
  sinco_base2011 <- suppressWarnings(as.integer(as.character(data$sinco4d)))
  sinco3_base2011 <- suppressWarnings(as.integer(as.character(data$sinco3d)))
  regla_revision <- periodo_cmo &
    data$tipo_regla_cmo_sinco %in% c(
      "panel_global", "scian_sector", "p4a_scian2", "p4f", "p4a_p4f"
    ) &
    !is.na(sinco_base2011)
  regla_convergencia_3d <- periodo_cmo &
    data$tipo_regla_cmo_sinco == "official_convergence_3d" &
    is.na(sinco_base2011) & !is.na(sinco3_base2011)

  data$regla_cmo_sinco[!periodo_cmo] <- NA_character_
  data$tipo_regla_cmo_sinco[!periodo_cmo] <- NA_character_
  data$alcance_regla_cmo_sinco[!periodo_cmo] <- NA_character_
  data$detalle_regla_cmo_sinco[!periodo_cmo] <- NA_character_
  data$n_destinos_regla_cmo_sinco[!periodo_cmo] <- NA_integer_
  # SINCO 2011 observado se conserva directamente.
  sinco_base2011[periodo_sinco2011] <- codigo_original[periodo_sinco2011]

  # SINCO 2019 se cruza con la tabla oficial. Los casos multiples no se
  # resuelven mediante una seleccion arbitraria.
  puente_2019 <- renoe::sinco2019_to_sinco2011(
    data.frame(codigo = codigo_original),
    variable_sinco = "codigo",
    correspondencia = correspondencia_2019,
    resolver_multiples = "na"
  )
  sinco_base2011[periodo_sinco2019] <-
    puente_2019$sinco2011[periodo_sinco2019]
  # El remanente multiple a 4d puede converger oficialmente a 3d.
  # No escoger un destino 4d para conseguir esa agregacion.
  ruta_2019 <- system.file(
    "extdata", "puente_sinco2019_sinco2011.csv", package = "renoe"
  )
  tabla_2019 <- if (is.null(correspondencia_2019)) {
    utils::read.csv(ruta_2019, colClasses = "character", check.names = FALSE)
  } else {
    correspondencia_2019
  }
  fuente <- as.character(tabla_2019$sinco2019)
  destino <- as.character(tabla_2019$sinco2011)
  destino_4d <- ifelse(
    is.na(destino) | !nzchar(destino), NA_character_,
    stringr::str_pad(destino, 4L, "left", "0")
  )
  consenso_nivel <- function(digitos) {
    destino_nivel <- ifelse(
      is.na(destino_4d), NA_character_, substr(destino_4d, 1L, digitos)
    )
    por_fuente <- split(destino_nivel, fuente)
    vapply(por_fuente, function(x) {
      x <- unique(x[!is.na(x)])
      if (length(x) == 1L) as.integer(x) else NA_integer_
    }, integer(1L))
  }
  consenso_3d <- consenso_nivel(3L)
  consenso_2d <- consenso_nivel(2L)
  consenso_1d <- consenso_nivel(1L)
  codigo_txt <- ifelse(
    is.na(codigo_original), NA_character_,
    stringr::str_pad(as.character(codigo_original), 4L, "left", "0")
  )
  consenso_3d_fila <- unname(consenso_3d[codigo_txt])
  consenso_2d_fila <- unname(consenso_2d[codigo_txt])
  consenso_1d_fila <- unname(consenso_1d[codigo_txt])

  data$codigo_ocupacion_original <- codigo_original
  data$version_sinco_origen <- dplyr::case_when(
    periodo_cmo ~ "CMO",
    periodo_sinco2011 ~ "SINCO 2011",
    periodo_sinco2019 ~ "SINCO 2019",
    TRUE ~ NA_character_
  )
  data$sinco4d_base2011 <- sinco_base2011
  data$sinco4d <- sinco_base2011
  data$n_destinos_sinco <- dplyr::case_when(
    regla_revision ~ 1L,
    regla_convergencia_3d ~ data$n_destinos_regla_cmo_sinco,
    periodo_cmo & !is.na(sinco_base2011) ~
      dplyr::coalesce(data$n_destinos_regla_cmo_sinco, 1L),
    periodo_cmo ~ NA_integer_,
    periodo_sinco2011 & !is.na(codigo_original) ~ 1L,
    periodo_sinco2019 ~ puente_2019$sinco2011_n_destinos,
    TRUE ~ NA_integer_
  )
  data$calidad_puente_sinco <- dplyr::case_when(
    regla_revision ~ paste0(
      "Regla validada con panel ENOE: ", data$detalle_regla_cmo_sinco
    ),
    regla_convergencia_3d ~ paste0(
      "Convergencia oficial CMO-SINCO: ", data$detalle_regla_cmo_sinco
    ),
    periodo_cmo & !is.na(sinco_base2011) ~
      "Puente anal\u00EDtico CMO-SINCO 2011",
    periodo_cmo ~ "CMO sin equivalencia de cuatro d\u00EDgitos",
    periodo_sinco2011 & !is.na(codigo_original) ~ "SINCO 2011 observado",
    periodo_sinco2011 ~ "SINCO 2011 faltante",
    periodo_sinco2019 ~ puente_2019$sinco2011_calidad,
    TRUE ~ NA_character_
  )
  sinco4d_str <- stringr::str_pad(
    as.character(data$sinco4d), width = 4, side = "left", pad = "0"
  )
  sinco3d_nuevo <- suppressWarnings(as.integer(
    stringr::str_sub(sinco4d_str, 1, 3)
  ))
  data$sinco3d <- dplyr::if_else(
    periodo_cmo,
    dplyr::coalesce(data$sinco3d, sinco3d_nuevo),
    sinco3d_nuevo
  )
  consenso_2019_3d <- periodo_sinco2019 & is.na(data$sinco4d) &
    !is.na(consenso_3d_fila)
  data$sinco3d[consenso_2019_3d] <-
    consenso_3d_fila[consenso_2019_3d]
  data$calidad_puente_sinco[consenso_2019_3d] <-
    "Convergencia oficial SINCO 2019-SINCO 2011 a tres digitos"
  sinco3d_str <- stringr::str_pad(
    as.character(data$sinco3d), width = 3, side = "left", pad = "0"
  )
  data$sinco2d <- suppressWarnings(as.integer(
    stringr::str_sub(sinco3d_str, 1, 2)
  ))
  consenso_2019_2d <- periodo_sinco2019 & is.na(data$sinco2d) &
    !is.na(consenso_2d_fila)
  data$sinco2d[consenso_2019_2d] <-
    consenso_2d_fila[consenso_2019_2d]
  data$calidad_puente_sinco[consenso_2019_2d] <-
    "Convergencia oficial SINCO 2019-SINCO 2011 a dos digitos"
  sinco2d_str <- stringr::str_pad(
    as.character(data$sinco2d), width = 2, side = "left", pad = "0"
  )
  data$sinco1d <- suppressWarnings(as.integer(
    stringr::str_sub(sinco2d_str, 1, 1)
  ))
  consenso_2019_1d <- periodo_sinco2019 & is.na(data$sinco1d) &
    !is.na(consenso_1d_fila)
  data$sinco1d[consenso_2019_1d] <-
    consenso_1d_fila[consenso_2019_1d]
  data$calidad_puente_sinco[consenso_2019_1d] <-
    "Convergencia oficial SINCO 2019-SINCO 2011 a un digito"

  data$needs_manual_1d <- is.na(data$sinco1d) & periodo_cmo
  if ("enoe" %in% capas && any(data$needs_manual_1d, na.rm = TRUE)) {
    pendiente_manual <- data$needs_manual_1d
    data <- renoe::cmo_to_sinco1d(data)
    resuelto_manual <- pendiente_manual & !is.na(data$sinco1d)
    data$regla_cmo_sinco[resuelto_manual] <- "CMO_SINCO_MANUAL_1D"
    data$tipo_regla_cmo_sinco[resuelto_manual] <-
      "manual_cmo_to_sinco1d"
    data$alcance_regla_cmo_sinco[resuelto_manual] <-
      "puente_historico_ENOE"
    data$detalle_regla_cmo_sinco[resuelto_manual] <-
      "Puente historico manual CMO-SINCO a un digito"
  }

  data$nivel_maximo_sinco <- dplyr::case_when(
    !is.na(data$sinco4d) ~ "4d",
    !is.na(data$sinco3d) ~ "3d",
    !is.na(data$sinco2d) ~ "2d",
    !is.na(data$sinco1d) ~ "1d",
    TRUE ~ NA_character_
  )
  data$capas_sinco_activas <- rep(paste(capas, collapse = "+"), nrow(data))
  data$sinco_catalogo_destino <- rep("SINCO 2011", nrow(data))
  data$sinco_decision_status <- dplyr::case_when(
    !is.na(data$sinco4d) & data$n_destinos_sinco <= 1L ~ "unique",
    is.na(data$sinco4d) & !is.na(data$sinco1d) ~ "unique",
    data$n_destinos_sinco > 1L ~ "unresolved",
    TRUE ~ "unresolved"
  )
  data$sinco_decision_phase <- rep("integrated", nrow(data))
  data$sinco_evidence_level <- dplyr::case_when(
    data$tipo_regla_cmo_sinco == "panel_global" ~ "panel",
    data$tipo_regla_cmo_sinco %in% c("scian_sector", "p4a_scian2", "p4f", "p4a_p4f") ~ "panel_semantico",
    data$tipo_regla_cmo_sinco == "manual_cmo_to_sinco1d" ~ "enoe_historico",
    periodo_sinco2011 ~ "observed",
    periodo_sinco2019 ~ "official_crosswalk",
    !is.na(data$sinco1d) ~ "analytical_crosswalk",
    TRUE ~ NA_character_
  )
  data$sinco_decision_layer <- dplyr::case_when(
    data$tipo_regla_cmo_sinco == "panel_global" ~ "panel",
    data$tipo_regla_cmo_sinco %in% c("scian_sector", "p4a_scian2", "p4f", "p4a_p4f", "manual_cmo_to_sinco1d") ~ "enoe",
    TRUE ~ "official"
  )
  data$sinco_auxiliares_usados <- dplyr::case_when(
    data$tipo_regla_cmo_sinco == "scian_sector" ~ "scian",
    data$tipo_regla_cmo_sinco == "p4a_scian2" ~ "p4a",
    data$tipo_regla_cmo_sinco == "p4f" ~ "p4f",
    data$tipo_regla_cmo_sinco == "p4a_p4f" ~ "p4a+p4f",
    TRUE ~ NA_character_
  )
  data$sinco_motivo_pendiente <- dplyr::case_when(
    is.na(codigo_original) ~ "codigo_faltante",
    !is.na(data$sinco1d) ~ NA_character_,
    periodo_sinco2019 & data$n_destinos_sinco > 1L ~ "correspondencia_oficial_multiple",
    periodo_sinco2019 ~ "sin_equivalencia_oficial",
    TRUE ~ "sin_regla_al_nivel_solicitado"
  )

  if (!isTRUE(detalle)) {
    data <- data |> dplyr::select(-dplyr::any_of(c(
      "sinco_auxiliares_usados", "sinco_decision_layer",
      "sinco_decision_phase"
    )))
  }

  salida <- data |>
    dplyr::select(-needs_manual_1d) |>
    sjlabelled::var_labels(
      codigo_ocupacion_original = "C\u00F3digo ocupacional original del trimestre",
      version_sinco_origen = "Clasificador ocupacional de origen",
      sinco4d_base2011 = "C\u00F3digo ocupacional armonizado a SINCO 2011, cuatro d\u00EDgitos",
      sinco4d = "C\u00F3digo ocupacional armonizado a SINCO 2011, cuatro d\u00EDgitos",
      sinco3d = "C\u00F3digo ocupacional armonizado a SINCO 2011, tres d\u00EDgitos",
      sinco2d = "C\u00F3digo ocupacional armonizado a SINCO 2011, dos d\u00EDgitos",
      sinco1d = "C\u00F3digo ocupacional armonizado a SINCO 2011, un d\u00EDgito",
      regla_cmo_sinco = "Identificador de la regla CMO-SINCO aplicada",
      tipo_regla_cmo_sinco = "Tipo de regla CMO-SINCO aplicada",
      alcance_regla_cmo_sinco = "Alcance de aplicacion o fuente de validacion",
      detalle_regla_cmo_sinco = "Descripcion legible de la regla CMO-SINCO",
      n_destinos_regla_cmo_sinco = "Numero de destinos asociado a la regla",
      nivel_maximo_sinco = "Maximo nivel SINCO efectivamente identificado",
      capas_sinco_activas = "Capas solicitadas para el puente CMO-SINCO",
      n_destinos_sinco = "N\u00FAmero de destinos en el puente hacia SINCO 2011",
      calidad_puente_sinco = "Procedencia y calidad de la armonizaci\u00F3n ocupacional"
    )
  .contrato_sinco(
    salida,
    list(codigos = codigos, correspondencia_2019 = correspondencia_2019)
  )
}
