#' Armonizar ocupaciones CMO, SINCO 2011 y SINCO 2019
#'
#' Construye códigos comparables en SINCO 2011 a partir de CMO entre 2005-I y
#' 2012-II, SINCO 2011 observado entre 2012-III y 2021-II, y SINCO 2019 desde
#' 2021-III. Para el último periodo utiliza la tabla de equivalencia oficial
#' SINCO 2011-2019 y conserva sin resolver las correspondencias múltiples.
#'
#' @param data Data frame con `anio`, `trim` y `p3coe`.
#' @param codigos Tabla opcional de correspondencia CMO-SINCO usada antes de
#'   2012-III.
#' @param correspondencia_2019 Tabla opcional, en formato largo, del puente
#'   SINCO 2019-SINCO 2011.
#'
#' @return El data frame con `sinco4d`, `sinco3d`, `sinco2d` y `sinco1d`
#'   armonizados, además de variables de procedencia y calidad.
#' @export
#' @encoding UTF-8
#' @family procesamiento_enoe
#' @references
#' INEGI (2020). *Sistema Nacional de Clasificación de Ocupaciones 2019*.
#' Anexo: Tabla de equivalencia SINCO 2011-2019.
#'
#' Escoto Castillo, A. y Sánchez Peña, L. (2024). *El riesgo de automatización
#' en México: diferencias temporales y generacionales entre las distintas
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
#' armoniza_sinco(datos)
armoniza_sinco <- function(
    data, codigos = NULL, correspondencia_2019 = NULL) {

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

  # El puente CMO-SINCO conserva las reglas históricas ya documentadas.
  data <- renoe::cmo_to_sinco(data, codigos = codigos)
  sinco_base2011 <- suppressWarnings(as.integer(as.character(data$sinco4d)))

  # SINCO 2011 observado se conserva directamente.
  sinco_base2011[periodo_sinco2011] <- codigo_original[periodo_sinco2011]

  # SINCO 2019 se cruza con la tabla oficial. Los casos múltiples no se
  # resuelven mediante una selección arbitraria.
  puente_2019 <- renoe::sinco2019_to_sinco2011(
    data.frame(codigo = codigo_original),
    variable_sinco = "codigo",
    correspondencia = correspondencia_2019,
    resolver_multiples = "na"
  )
  sinco_base2011[periodo_sinco2019] <-
    puente_2019$sinco2011[periodo_sinco2019]

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
    periodo_cmo ~ NA_integer_,
    periodo_sinco2011 & !is.na(codigo_original) ~ 1L,
    periodo_sinco2019 ~ puente_2019$sinco2011_n_destinos,
    TRUE ~ NA_integer_
  )
  data$calidad_puente_sinco <- dplyr::case_when(
    periodo_cmo & !is.na(sinco_base2011) ~ "Puente analítico CMO-SINCO 2011",
    periodo_cmo ~ "CMO sin equivalencia de cuatro dígitos",
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
  data$sinco2d <- suppressWarnings(as.integer(
    stringr::str_sub(sinco4d_str, 1, 2)
  ))
  data$sinco1d <- suppressWarnings(as.integer(
    stringr::str_sub(sinco4d_str, 1, 1)
  ))

  data$needs_manual_1d <- is.na(data$sinco1d) & periodo_cmo
  if (any(data$needs_manual_1d, na.rm = TRUE)) {
    data <- renoe::cmo_to_sinco1d(data)
  }

  data |>
    dplyr::select(-needs_manual_1d) |>
    sjlabelled::var_labels(
      codigo_ocupacion_original = "Código ocupacional original del trimestre",
      version_sinco_origen = "Clasificador ocupacional de origen",
      sinco4d_base2011 = "Código ocupacional armonizado a SINCO 2011, cuatro dígitos",
      sinco4d = "Código ocupacional armonizado a SINCO 2011, cuatro dígitos",
      sinco3d = "Código ocupacional armonizado a SINCO 2011, tres dígitos",
      sinco2d = "Código ocupacional armonizado a SINCO 2011, dos dígitos",
      sinco1d = "Código ocupacional armonizado a SINCO 2011, un dígito",
      n_destinos_sinco = "Número de destinos en el puente hacia SINCO 2011",
      calidad_puente_sinco = "Procedencia y calidad de la armonización ocupacional"
    )
}
