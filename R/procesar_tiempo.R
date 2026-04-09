#' Procesar variables de tiempo en actividades del hogar y cuidado
#'
#' Calcula el tiempo dedicado a diversas actividades del hogar y cuidado,
#' expresado en horas, a partir de las preguntas de uso del tiempo de la ENOE.
#' Soporta tanto el cuestionario ampliado como el básico, ajustando el mapeo
#' de variables según el año. Si no existen las variables sociodemográficas
#' requeridas (`anio`, `coe_tipo`), las crea con `procesar_vars_sociodemo()`.
#'
#' Las variables específicas de tiempo (`t_estudiar`, `t_cuidado`,
#' `t_construir`, `t_reparar`, `t_quehacer`, `t_comun`, `t_compras`,
#' `t_traslado`) se calculan inicialmente en minutos y posteriormente se
#' convierten a horas. En esta conversión, los valores faltantes se sustituyen
#' por `0`, con el fin de facilitar la construcción de agregados y su uso en
#' análisis descriptivos.
#'
#' @param data Un data frame fusionado por `fusion_enoe()` o cargado directamente,
#'   que contenga variables de tiempo (`p11_*` o `p9_*`).
#' @param anio Año del trimestre.
#' @param trimestre Trimestre numérico (1-4).
#'
#' @return Un data frame con variables de tiempo procesadas y etiquetadas.
#'   Las variables `t_estudiar`, `t_cuidado`, `t_construir`, `t_reparar`,
#'   `t_quehacer`, `t_comun`, `t_compras` y `t_traslado` se expresan en horas.
#'   Las variables `t_total` y `t_total0` se expresan en minutos, mientras que
#'   `t_total_hrs` y `t_total_hrs0` se expresan en horas.
#' @export
#' @family procesamiento_enoe

procesar_tiempo <- function(data, anio, trimestre) {

  if (!all(c("anio", "coe_tipo") %in% names(data))) {
    message("Variables 'anio' y/o 'coe_tipo' no encontradas. Se procesan con `procesar_vars_sociodemo()`...")
    data <- procesar_vars_sociodemo(data, anio = anio, trimestre = trimestre)
  }

  tiene_ampliado <- any(data$coe_tipo == "ampliado", na.rm = TRUE)
  tiene_basico   <- any(data$coe_tipo == "basico", na.rm = TRUE)

  vars_tiempo_raw <- intersect(
    names(data),
    c(
      "p11_h1", "p11_h2", "p11_h3", "p11_h4", "p11_h5", "p11_h6", "p11_h7", "p11_h8",
      "p11_m1", "p11_m2", "p11_m3", "p11_m4", "p11_m5", "p11_m6", "p11_m7", "p11_m8",
      "p9_h1", "p9_h2", "p9_h3", "p9_h4", "p9_h5", "p9_h6", "p9_h7", "p9_h8",
      "p9_m1", "p9_m2", "p9_m3", "p9_m4", "p9_m5", "p9_m6", "p9_m7", "p9_m8"
    )
  )

  if (length(vars_tiempo_raw) > 0) {
    data <- data %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(vars_tiempo_raw),
          ~ suppressWarnings(as.numeric(as.character(.x)))
        )
      )
  }

  vars_tiempo <- c(
    "t_estudiar", "t_cuidado", "t_construir", "t_reparar",
    "t_quehacer", "t_comun", "t_compras", "t_traslado"
  )

  for (v in vars_tiempo) {
    if (!v %in% names(data)) data[[v]] <- NA_real_
  }

  if (tiene_ampliado) {
    data <- data %>%
      dplyr::mutate(
        t_estudiar = dplyr::case_when(
          coe_tipo == "ampliado" & "p11_h1" %in% names(data) & "p11_m1" %in% names(data) & p11_h1 < 98 ~ p11_h1 * 60 + p11_m1,
          TRUE ~ t_estudiar
        ),
        t_cuidado = dplyr::case_when(
          coe_tipo == "ampliado" & "p11_h2" %in% names(data) & "p11_m2" %in% names(data) & p11_h2 < 98 ~ p11_h2 * 60 + p11_m2,
          TRUE ~ t_cuidado
        ),
        t_compras = dplyr::case_when(
          coe_tipo == "ampliado" & anio >= 2013 & "p11_h3" %in% names(data) & "p11_m3" %in% names(data) & p11_h3 < 98 ~ p11_h3 * 60 + p11_m3,
          TRUE ~ t_compras
        ),
        t_traslado = dplyr::case_when(
          coe_tipo == "ampliado" & anio >= 2013 & "p11_h4" %in% names(data) & "p11_m4" %in% names(data) & p11_h4 < 98 ~ p11_h4 * 60 + p11_m4,
          TRUE ~ t_traslado
        ),
        t_construir = dplyr::case_when(
          coe_tipo == "ampliado" & anio < 2013 & "p11_h3" %in% names(data) & "p11_m3" %in% names(data) & p11_h3 < 98 ~ p11_h3 * 60 + p11_m3,
          coe_tipo == "ampliado" & anio >= 2013 & "p11_h5" %in% names(data) & "p11_m5" %in% names(data) & p11_h5 < 98 ~ p11_h5 * 60 + p11_m5,
          TRUE ~ t_construir
        ),
        t_reparar = dplyr::case_when(
          coe_tipo == "ampliado" & anio < 2013 & "p11_h4" %in% names(data) & "p11_m4" %in% names(data) & p11_h4 < 98 ~ p11_h4 * 60 + p11_m4,
          coe_tipo == "ampliado" & anio >= 2013 & "p11_h6" %in% names(data) & "p11_m6" %in% names(data) & p11_h6 < 98 ~ p11_h6 * 60 + p11_m6,
          TRUE ~ t_reparar
        ),
        t_quehacer = dplyr::case_when(
          coe_tipo == "ampliado" & anio < 2013 & "p11_h5" %in% names(data) & "p11_m5" %in% names(data) & p11_h5 < 98 ~ p11_h5 * 60 + p11_m5,
          coe_tipo == "ampliado" & anio >= 2013 & "p11_h7" %in% names(data) & "p11_m7" %in% names(data) & p11_h7 < 98 ~ p11_h7 * 60 + p11_m7,
          TRUE ~ t_quehacer
        ),
        t_comun = dplyr::case_when(
          coe_tipo == "ampliado" & anio < 2013 & "p11_h6" %in% names(data) & "p11_m6" %in% names(data) & p11_h6 < 98 ~ p11_h6 * 60 + p11_m6,
          coe_tipo == "ampliado" & anio >= 2013 & "p11_h8" %in% names(data) & "p11_m8" %in% names(data) & p11_h8 < 98 ~ p11_h8 * 60 + p11_m8,
          TRUE ~ t_comun
        )
      )
  }

  if (tiene_basico) {
    data <- data %>%
      dplyr::mutate(
        t_estudiar = dplyr::case_when(
          coe_tipo == "basico" & "p9_h1" %in% names(data) & "p9_m1" %in% names(data) & p9_h1 < 98 ~ p9_h1 * 60 + p9_m1,
          TRUE ~ t_estudiar
        ),
        t_cuidado = dplyr::case_when(
          coe_tipo == "basico" & "p9_h2" %in% names(data) & "p9_m2" %in% names(data) & p9_h2 < 98 ~ p9_h2 * 60 + p9_m2,
          TRUE ~ t_cuidado
        ),
        t_compras = dplyr::case_when(
          coe_tipo == "basico" & anio >= 2013 & "p9_h3" %in% names(data) & "p9_m3" %in% names(data) & p9_h3 < 98 ~ p9_h3 * 60 + p9_m3,
          TRUE ~ t_compras
        ),
        t_traslado = dplyr::case_when(
          coe_tipo == "basico" & anio >= 2013 & "p9_h4" %in% names(data) & "p9_m4" %in% names(data) & p9_h4 < 98 ~ p9_h4 * 60 + p9_m4,
          TRUE ~ t_traslado
        ),
        t_construir = dplyr::case_when(
          coe_tipo == "basico" & anio < 2013 & "p9_h3" %in% names(data) & "p9_m3" %in% names(data) & p9_h3 < 98 ~ p9_h3 * 60 + p9_m3,
          coe_tipo == "basico" & anio >= 2013 & "p9_h5" %in% names(data) & "p9_m5" %in% names(data) & p9_h5 < 98 ~ p9_h5 * 60 + p9_m5,
          TRUE ~ t_construir
        ),
        t_reparar = dplyr::case_when(
          coe_tipo == "basico" & anio < 2013 & "p9_h4" %in% names(data) & "p9_m4" %in% names(data) & p9_h4 < 98 ~ p9_h4 * 60 + p9_m4,
          coe_tipo == "basico" & anio >= 2013 & "p9_h6" %in% names(data) & "p9_m6" %in% names(data) & p9_h6 < 98 ~ p9_h6 * 60 + p9_m6,
          TRUE ~ t_reparar
        ),
        t_quehacer = dplyr::case_when(
          coe_tipo == "basico" & anio < 2013 & "p9_h5" %in% names(data) & "p9_m5" %in% names(data) & p9_h5 < 98 ~ p9_h5 * 60 + p9_m5,
          coe_tipo == "basico" & anio >= 2013 & "p9_h7" %in% names(data) & "p9_m7" %in% names(data) & p9_h7 < 98 ~ p9_h7 * 60 + p9_m7,
          TRUE ~ t_quehacer
        ),
        t_comun = dplyr::case_when(
          coe_tipo == "basico" & anio < 2013 & "p9_h6" %in% names(data) & "p9_m6" %in% names(data) & p9_h6 < 98 ~ p9_h6 * 60 + p9_m6,
          coe_tipo == "basico" & anio >= 2013 & "p9_h8" %in% names(data) & "p9_m8" %in% names(data) & p9_h8 < 98 ~ p9_h8 * 60 + p9_m8,
          TRUE ~ t_comun
        )
      )
  }

  vars_total <- intersect(
    names(data),
    c("t_cuidado", "t_construir", "t_reparar", "t_quehacer", "t_comun", "t_traslado", "t_compras")
  )

  vars_total0 <- intersect(
    names(data),
    c("t_cuidado", "t_construir", "t_reparar", "t_quehacer", "t_comun")
  )

  data <- data %>%
    dplyr::mutate(
      t_total      = rowSums(dplyr::across(dplyr::all_of(vars_total)), na.rm = TRUE),
      t_total0     = rowSums(dplyr::across(dplyr::all_of(vars_total0)), na.rm = TRUE),
      t_total_hrs  = t_total / 60,
      t_total_hrs0 = t_total0 / 60
    ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(intersect(
          names(data),
          c("t_estudiar", "t_cuidado", "t_construir", "t_reparar",
            "t_quehacer", "t_comun", "t_compras", "t_traslado")
        )),
        .fns = ~ dplyr::if_else(is.na(.x), 0, .x / 60)
      )
    ) %>%
    sjlabelled::var_labels(
      t_estudiar   = "Tiempo dedicado a estudiar o tomar cursos (horas)",
      t_cuidado    = "Tiempo dedicado al cuidado exclusivo sin pago (horas)",
      t_construir  = "Tiempo dedicado a construir o ampliar la vivienda (horas)",
      t_reparar    = "Tiempo dedicado a reparar o dar mantenimiento (horas)",
      t_quehacer   = "Tiempo dedicado a los quehaceres del hogar (horas)",
      t_comun      = "Tiempo dedicado a prestar servicios gratuitos a la comunidad (horas)",
      t_compras    = "Tiempo dedicado a compras, trámites y seguridad del hogar (horas)",
      t_traslado   = "Tiempo dedicado a traslados de integrantes del hogar (horas)",
      t_total      = "Suma total de actividades del hogar y cuidado (minutos)",
      t_total0     = "Suma de actividades del hogar y cuidado sin traslados ni compras (minutos)",
      t_total_hrs  = "Suma total de actividades del hogar y cuidado (horas)",
      t_total_hrs0 = "Suma de actividades del hogar y cuidado sin traslados ni compras (horas)"
    )

  return(data)
}
