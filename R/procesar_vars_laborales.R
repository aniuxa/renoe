#' Procesar variables de análisis laboral y desajuste educativo
#'
#' Esta función genera variables clasificatorias relacionadas con la ocupación, nivel educativo
#' y desajuste entre ambos, a partir de los códigos de ocupación (`p3coe`) y nivel educativo alcanzado (`cs_p13_1`).
#' Internamente armoniza los códigos SINCO (1, 2, 3 y 4 dígitos) usando correspondencias con códigos CMO
#' y reglas auxiliares. También clasifica el nivel de habilidad de la ocupación, el nivel educativo alcanzado
#' y construye indicadores de sobrecalificación, ajuste o subcalificación laboral.
#'
#' Además, genera variables relacionadas con la experiencia previa (`nunca_trabajo`), estatus laboral combinado (`status_seq`)
#' y características contractuales (`contrato0`, `contrato1`, `temporal`, `temporal_seq`) según el tipo de cuestionario.
#'
#' @param data Un data.frame con variables como:
#' - `anio`, `trimestre`: año y trimestre de la entrevista
#' - `coe_tipo`: tipo de cuestionario (`"ampliado"` o `"basico"`)
#' - `p3coe`: código ocupacional
#' - `cs_p13_1`, `anios_es`: nivel educativo alcanzado en categorías o años
#' - `clase2`: clase de actividad económica
#' - `pos_ocu`, `tue2`: posición en la ocupación y tipo de unidad económica
#' - `p2_4`: experiencia laboral previa
#' - `p3i`, `p3j`, `p3j1`, `p3k1`: variables sobre tipo de contrato
#'
#' @return Un data.frame con las variables originales y nuevas columnas:
#' - `sinco1d`, `sinco2d`, `sinco3d`, `sinco4d`
#' - `skill_level`, `skill_actual`
#' - `mismatch`, `mismatch2`
#' - `nunca_trabajo`, `status_seq`
#' - `contrato0`, `contrato1`, `temporal`, `temporal_seq`
#'
#' @export
#' @family procesamiento_enoe
procesar_vars_laborales <- function(data) {
  data <- renoe::armoniza_sinco(data)

  data <- data %>%
    dplyr::mutate(
      skill_level = dplyr::case_when(
        sinco1d %in% 1:3 ~ 3,
        sinco1d %in% 4:7 ~ 2,
        sinco1d %in% 8:9 ~ 1,
        TRUE ~ NA_real_
      ),
      skill_actual = dplyr::case_when(
        cs_p13_1 %in% 0:1 ~ 0,
        cs_p13_1 == 2 ~ 1,
        cs_p13_1 %in% 3:5 ~ 2,
        cs_p13_1 %in% 6:9 ~ 3,
        TRUE ~ NA_real_
      ),
      mismatch_raw = dplyr::case_when(
        !is.na(skill_level) & !is.na(skill_actual) & clase2 == 1 ~ skill_level - skill_actual,
        TRUE ~ NA_real_
      ),
      mismatch = dplyr::case_when(
        mismatch_raw < 0 ~ -1,
        mismatch_raw > 0 ~ 1,
        mismatch_raw == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::group_by(anio, sinco1d) %>%
    dplyr::mutate(esco_norm = mean(anios_es, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      mismatch2 = dplyr::case_when(
        !is.na(anios_es) & !is.na(esco_norm) & clase2 == 1 ~ anios_es - esco_norm,
        TRUE ~ NA_real_
      ),
      mismatch2 = dplyr::case_when(
        mismatch2 < -1 ~ -1,
        mismatch2 > 1 ~ 1,
        dplyr::between(mismatch2, -1, 1) ~ 0,
        TRUE ~ NA_real_
      ),
      nunca_trabajo = dplyr::case_when(
        p2_4 == 4 ~ 1,
        is.na(p2_4) & is.na(clase2) ~ NA_real_,
        is.na(p2_4) & !is.na(clase2) ~ 0,
        TRUE ~ 0
      ),
      status_seq = dplyr::case_when(
        nunca_trabajo == 1 & clase2 == 2 ~ 5,
        nunca_trabajo == 1 & clase2 == 3 ~ 6,
        nunca_trabajo == 1 & clase2 == 4 ~ 7,
        nunca_trabajo == 0 & clase2 == 1 ~ 1,
        nunca_trabajo == 0 & clase2 == 2 ~ 2,
        nunca_trabajo == 0 & clase2 == 3 ~ 3,
        nunca_trabajo == 0 & clase2 == 4 ~ 4,
        TRUE ~ NA_real_
      )
    )

  # Inicializar
  data$contrato0 <- NA_real_
  data$contrato1 <- NA_real_

  # Lógica condicional para contrato según tipo de cuestionario
  tiene_ampliado <- any(data$coe_tipo == "ampliado", na.rm = TRUE)
  tiene_basico   <- any(data$coe_tipo == "basico", na.rm = TRUE)

  # Bloque ampliado
  if (tiene_ampliado && all(c("p3j", "p3k1") %in% names(data))) {
    data <- data %>%
      dplyr::mutate(
        contrato0 = dplyr::if_else(coe_tipo == "ampliado" & p3j == 1 & pos_ocu == 1, 1, contrato0),
        contrato0 = dplyr::if_else(coe_tipo == "ampliado" & p3j == 2 & pos_ocu == 1, 0, contrato0),
        contrato1 = dplyr::if_else(coe_tipo == "ampliado" & p3k1 == 1 & pos_ocu == 1, 1, contrato1),
        contrato1 = dplyr::if_else(coe_tipo == "ampliado" & p3k1 == 2 & pos_ocu == 1, 0, contrato1)
      )
  }

  # Bloque básico
  if (tiene_basico && all(c("p3i", "p3j1") %in% names(data))) {
    data <- data %>%
      dplyr::mutate(
        contrato0 = dplyr::if_else(coe_tipo == "basico" & p3i == 1 & pos_ocu == 1, 1, contrato0),
        contrato0 = dplyr::if_else(coe_tipo == "basico" & p3i == 2 & pos_ocu == 1, 0, contrato0),
        contrato1 = dplyr::if_else(coe_tipo == "basico" & p3j1 == 1 & pos_ocu == 1, 1, contrato1),
        contrato1 = dplyr::if_else(coe_tipo == "basico" & p3j1 == 2 & pos_ocu == 1, 0, contrato1)
      )
  }

  data <- data %>%
    dplyr::mutate(
      temporal = dplyr::case_when(
        contrato1 == 1 & pos_ocu == 1 ~ 1,
        contrato1 == 0 & pos_ocu == 1 ~ 2,
        contrato0 == 0 & pos_ocu == 1 ~ 3,
        clase2 == 1 & pos_ocu != 1 ~ 4,
        TRUE ~ NA_real_
      ),
      temporal_seq = dplyr::case_when(
        clase2 > 1 ~ 0,
        TRUE ~ temporal
      ),
      skill_level = sjlabelled::set_label(skill_level, "Nivel de habilidad requerido por la ocupación"),
      skill_level = sjlabelled::set_labels(skill_level, labels = c(
        `1` = "Primaria", `2` = "Secundaria", `3` = "Terciaria")),
      skill_actual = sjlabelled::set_label(skill_actual, "Nivel educativo alcanzado"),
      skill_actual = sjlabelled::set_labels(skill_actual, labels = c(
        `0` = "Ninguna", `1` = "Primaria", `2` = "Secundaria", `3` = "Terciaria")),
      mismatch = sjlabelled::set_label(mismatch, "Desajuste educativo"),
      mismatch = sjlabelled::set_labels(mismatch, labels = c(
        `-1` = "Sobrecualificada/o", `0` = "Ajustada/o", `1` = "Infracualificada/o")),
      mismatch2 = sjlabelled::set_label(mismatch2, "Desajuste educativo (años escolares)"),
      mismatch2 = sjlabelled::set_labels(mismatch2, labels = c(
        `-1` = "Sobrecualificada/o", `0` = "Ajustada/o", `1` = "Infracualificada/o")),
      status_seq = sjlabelled::set_label(status_seq, "Condición laboral y experiencia previa"),
      status_seq = sjlabelled::set_labels(status_seq, labels = c(
        `1` = "Ocupado", `2` = "Desempleado con experiencia", `3` = "Disponible con experiencia",
        `4` = "No disponible con experiencia", `5` = "Desempleado sin experiencia",
        `6` = "Disponible sin experiencia", `7` = "No disponible sin experiencia")),
      temporal = sjlabelled::set_label(temporal, "Clasificación ocupación temporal"),
      temporal = sjlabelled::set_labels(temporal, labels = c(
        `1` = "Asalariado con contrato temporal", `2` = "Asalariado con contrato indefinido",
        `3` = "Asalariado sin contrato", `4` = "No asalariado")),
      temporal_seq = sjlabelled::set_label(temporal_seq, "Secuencia ocupación temporal"),
      temporal_seq = sjlabelled::set_labels(temporal_seq, labels = c(
        `0` = "Fuera de la Población Ocupada", `1` = "Asalariado con contrato temporal",
        `2` = "Asalariado con contrato indefinido", `3` = "Asalariado sin contrato",
        `4` = "No asalariado"))
    ) %>%
    dplyr::select(-mismatch_raw)

  return(data)
}
