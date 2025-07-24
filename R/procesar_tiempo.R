#' Procesar variables de tiempo en actividades del hogar y cuidado
#'
#' Calcula el tiempo (en minutos y horas) dedicado a diversas actividades como estudiar, cuidar, quehaceres, construcción, reparación, comunidad, compras y traslados.
#' Soporta tanto el cuestionario ampliado como el básico, ajustando el mapeo de variables por año. Si no existen las variables sociodemográficas requeridas (`anio`, `coe_tipo`), las crea con `procesar_vars_sociodemo()`.
#'
#' @param data Un data frame fusionado por `fusion_enoe()` o cargado directamente, que contenga variables de tiempo (`p11_*` o `p9_*`).
#' @param anio Año del trimestre (numérico).
#' @param trimestre Trimestre numérico (1–4).
#'
#' @return Un data frame con variables de tiempo en minutos y horas, y etiquetas descriptivas.
#' @export
#'
#' @examples
#' \dontrun{
#' datos <- fusion_enoe(2022, 1)
#' datos <- procesar_tiempo(datos, anio = 2022, trimestre = 1)
#' summary(datos$t_cuidado)
#' }
#' @family procesamiento_enoe
procesar_tiempo <- function(data, anio, trimestre) {
  if (!all(c("anio", "coe_tipo") %in% names(data))) {
    message("Variables 'anio' y/o 'coe_tipo' no encontradas. Se procesan con `procesar_vars_sociodemo()`...")
    data <- procesar_vars_sociodemo(data, anio = anio, trimestre = trimestre)
  }

  tiene_ampliado <- any(data$coe_tipo == "ampliado", na.rm = TRUE)
  tiene_basico   <- any(data$coe_tipo == "basico", na.rm = TRUE)
  anio_base      <- unique(data$anio)

  # --- BLOQUE AMPLIADO ---
  if (tiene_ampliado &&
      all(c("p11_h1", "p11_m1", "p11_h2", "p11_m2",
            "p11_h3", "p11_m3", "p11_h4", "p11_m4",
            "p11_h5", "p11_m5", "p11_h6", "p11_m6",
            "p11_h7", "p11_m7", "p11_h8", "p11_m8") %in% names(data))) {

    data <- data %>%
      dplyr::mutate(
        t_estudiar = dplyr::if_else(coe_tipo == "ampliado" & p11_h1 < 98, p11_h1 * 60 + p11_m1, NA_real_),
        t_cuidado  = dplyr::if_else(coe_tipo == "ampliado" & p11_h2 < 98, p11_h2 * 60 + p11_m2, NA_real_),
        t_construir = dplyr::case_when(
          coe_tipo == "ampliado" & anio < 2013 & p11_h3 < 98 ~ p11_h3 * 60 + p11_m3,
          coe_tipo == "ampliado" & anio >= 2013 & p11_h5 < 98 ~ p11_h5 * 60 + p11_m5,
          TRUE ~ NA_real_
        ),
        t_reparar = dplyr::case_when(
          coe_tipo == "ampliado" & anio < 2013 & p11_h4 < 98 ~ p11_h4 * 60 + p11_m4,
          coe_tipo == "ampliado" & anio >= 2013 & p11_h6 < 98 ~ p11_h6 * 60 + p11_m6,
          TRUE ~ NA_real_
        ),
        t_quehacer = dplyr::case_when(
          coe_tipo == "ampliado" & anio < 2013 & p11_h5 < 98 ~ p11_h5 * 60 + p11_m5,
          coe_tipo == "ampliado" & anio >= 2013 & p11_h7 < 98 ~ p11_h7 * 60 + p11_m7,
          TRUE ~ NA_real_
        ),
        t_comun = dplyr::case_when(
          coe_tipo == "ampliado" & anio < 2013 & p11_h6 < 98 ~ p11_h6 * 60 + p11_m6,
          coe_tipo == "ampliado" & anio >= 2013 & p11_h8 < 98 ~ p11_h8 * 60 + p11_m8,
          TRUE ~ NA_real_
        ),
        t_compras = if (anio_base >= 2013 && "p11_h3" %in% names(data)) {
          dplyr::if_else(coe_tipo == "ampliado" & p11_h3 < 98, p11_h3 * 60 + p11_m3, NA_real_)
        } else { NA_real_ },
        t_traslado = if (anio_base >= 2013 && "p11_h4" %in% names(data)) {
          dplyr::if_else(coe_tipo == "ampliado" & p11_h4 < 98, p11_h4 * 60 + p11_m4, NA_real_)
        } else { NA_real_ }
      )
  }

  # --- BLOQUE BÁSICO ---
  if (tiene_basico &&
      all(c("p9_h1", "p9_m1", "p9_h2", "p9_m2",
            "p9_h3", "p9_m3", "p9_h4", "p9_m4",
            "p9_h5", "p9_m5", "p9_h6", "p9_m6",
            "p9_h7", "p9_m7", "p9_h8", "p9_m8") %in% names(data))) {

    data <- data %>%
      dplyr::mutate(
        t_estudiar = dplyr::if_else(coe_tipo == "basico" & p9_h1 < 98, p9_h1 * 60 + p9_m1, NA_real_),
        t_cuidado  = dplyr::if_else(coe_tipo == "basico" & p9_h2 < 98, p9_h2 * 60 + p9_m2, NA_real_),
        t_construir = dplyr::case_when(
          coe_tipo == "basico" & anio < 2013 & p9_h3 < 98 ~ p9_h3 * 60 + p9_m3,
          coe_tipo == "basico" & anio >= 2013 & p9_h5 < 98 ~ p9_h5 * 60 + p9_m5,
          TRUE ~ NA_real_
        ),
        t_reparar = dplyr::case_when(
          coe_tipo == "basico" & anio < 2013 & p9_h4 < 98 ~ p9_h4 * 60 + p9_m4,
          coe_tipo == "basico" & anio >= 2013 & p9_h6 < 98 ~ p9_h6 * 60 + p9_m6,
          TRUE ~ NA_real_
        ),
        t_quehacer = dplyr::case_when(
          coe_tipo == "basico" & anio < 2013 & p9_h5 < 98 ~ p9_h5 * 60 + p9_m5,
          coe_tipo == "basico" & anio >= 2013 & p9_h7 < 98 ~ p9_h7 * 60 + p9_m7,
          TRUE ~ NA_real_
        ),
        t_comun = dplyr::case_when(
          coe_tipo == "basico" & anio < 2013 & p9_h6 < 98 ~ p9_h6 * 60 + p9_m6,
          coe_tipo == "basico" & anio >= 2013 & p9_h8 < 98 ~ p9_h8 * 60 + p9_m8,
          TRUE ~ NA_real_
        ),
        t_compras = if (anio_base >= 2013 && "p9_h3" %in% names(data)) {
          dplyr::if_else(coe_tipo == "basico" & p9_h3 < 98, p9_h3 * 60 + p9_m3, NA_real_)
        } else { NA_real_ },
        t_traslado = if (anio_base >= 2013 && "p9_h4" %in% names(data)) {
          dplyr::if_else(coe_tipo == "basico" & p9_h4 < 98, p9_h4 * 60 + p9_m4, NA_real_)
        } else { NA_real_ }
      )
  }

  # --- CÁLCULOS COMUNES ---
  if (any(grepl("^t_", names(data)))) {
    vars_total  <- intersect(names(data), c("t_cuidado", "t_construir", "t_reparar", "t_quehacer", "t_comun", "t_traslado", "t_compras"))
    vars_total0 <- intersect(names(data), c("t_cuidado", "t_construir", "t_reparar", "t_quehacer", "t_comun"))

    data <- data %>%
      dplyr::mutate(
        t_total      = rowSums(dplyr::across(all_of(vars_total)), na.rm = TRUE),
        t_total0     = rowSums(dplyr::across(all_of(vars_total0)), na.rm = TRUE),
        t_total_hrs  = t_total / 60,
        t_total_hrs0 = t_total0 / 60
      ) %>%
      dplyr::mutate(dplyr::across(
        .cols = intersect(names(data), c(
          "t_estudiar", "t_cuidado", "t_construir", "t_reparar",
          "t_quehacer", "t_comun", "t_compras", "t_traslado"
        )),
        .fns = ~ dplyr::if_else(is.na(.x), 0, .x / 60),
        .names = "{.col}"
      )) %>%
      dplyr::mutate(
        t_estudiar   = sjlabelled::set_label(t_estudiar,   label = "Tiempo dedicado a estudiar o tomar cursos (horas)"),
        t_cuidado    = sjlabelled::set_label(t_cuidado,    label = "Tiempo dedicado al cuidado exclusivo sin pago (horas)"),
        t_construir  = sjlabelled::set_label(t_construir,  label = "Tiempo dedicado a construir o ampliar la vivienda (horas)"),
        t_reparar    = sjlabelled::set_label(t_reparar,    label = "Tiempo dedicado a reparar o dar mantenimiento (horas)"),
        t_quehacer   = sjlabelled::set_label(t_quehacer,   label = "Tiempo dedicado a los quehaceres del hogar (horas)"),
        t_comun      = sjlabelled::set_label(t_comun,      label = "Tiempo dedicado a prestar servicios gratuitos a la comunidad (horas)"),
        t_compras    = sjlabelled::set_label(t_compras,    label = "Tiempo dedicado a compras, trámites y seguridad del hogar (horas)"),
        t_traslado   = sjlabelled::set_label(t_traslado,   label = "Tiempo dedicado a traslados de integrantes del hogar (horas)"),
        t_total      = sjlabelled::set_label(t_total,      label = "Suma total de actividades (minutos)"),
        t_total0     = sjlabelled::set_label(t_total0,     label = "Suma de actividades sin traslados ni compras (minutos)"),
        t_total_hrs  = sjlabelled::set_label(t_total_hrs,  label = "Total de actividades (horas)"),
        t_total_hrs0 = sjlabelled::set_label(t_total_hrs0, label = "Total sin traslados ni compras (horas)")
      )
  }


  return(data)
}
