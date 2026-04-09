#' Procesar variables de análisis laboral y desajuste educativo
#'
#' Esta función genera variables clasificatorias relacionadas con la ocupación,
#' el nivel educativo y el desajuste entre ambos, a partir de los códigos de
#' ocupación (`p3coe`) y del nivel educativo alcanzado (`cs_p13_1`).
#' Internamente armoniza los códigos SINCO (1, 2, 3 y 4 dígitos) usando
#' correspondencias con códigos CMO y reglas auxiliares. También clasifica el
#' nivel de habilidad de la ocupación, el nivel educativo alcanzado y construye
#' indicadores de sobrecalificación, ajuste o subcalificación laboral.
#'
#' Además, genera variables relacionadas con la experiencia previa
#' (`nunca_trabajo`), el estatus laboral combinado (`status_seq`) y las
#' características contractuales (`contrato0`, `contrato1`, `temporal`,
#' `temporal_seq`) según el tipo de cuestionario.
#'
#' @param data Un data.frame con variables como:
#' - `anio`, `trimestre`: año y trimestre de la entrevista
#' - `coe_tipo`: tipo de cuestionario (`"ampliado"` o `"basico"`)
#' - `p3coe`: código ocupacional
#' - `cs_p13_1`, `anios_es`: nivel educativo alcanzado en categorías o años
#' - `clase2`: clase de actividad económica
#' - `pos_ocu`, `tue2`: posición en la ocupación y tipo de unidad económica
#' - `p2h4`: experiencia laboral previa
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

  if ("p2h4" %in% names(data)) {
    data$var_exp_previa <- data$p2h4
  } else if ("p2_4" %in% names(data)) {
    data$var_exp_previa <- data$p2_4
  } else {
    data$var_exp_previa <- NA_real_
  }

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
    dplyr::mutate(
      esco_norm = mean(anios_es, na.rm = TRUE)
    ) %>%
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
        var_exp_previa == 4 ~ 1,
        is.na(var_exp_previa) & is.na(clase2) ~ NA_real_,
        is.na(var_exp_previa) & !is.na(clase2) ~ 0,
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

  data$contrato0 <- NA_real_
  data$contrato1 <- NA_real_

  tiene_ampliado <- any(data$coe_tipo == "ampliado", na.rm = TRUE)
  tiene_basico   <- any(data$coe_tipo == "basico", na.rm = TRUE)

  if (tiene_ampliado && all(c("p3j", "p3k1") %in% names(data))) {
    data <- data %>%
      dplyr::mutate(
        contrato0 = dplyr::if_else(coe_tipo == "ampliado" & p3j == 1 & pos_ocu == 1, 1, contrato0),
        contrato0 = dplyr::if_else(coe_tipo == "ampliado" & p3j == 2 & pos_ocu == 1, 0, contrato0),
        contrato1 = dplyr::if_else(coe_tipo == "ampliado" & p3k1 == 1 & pos_ocu == 1, 1, contrato1),
        contrato1 = dplyr::if_else(coe_tipo == "ampliado" & p3k1 == 2 & pos_ocu == 1, 0, contrato1)
      )
  }

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
      )
    ) %>%
    sjlabelled::var_labels(
      skill_level   = "Nivel de habilidad requerido por la ocupación",
      skill_actual  = "Nivel educativo alcanzado",
      mismatch      = "Desajuste educativo",
      mismatch2     = "Desajuste educativo (años escolares)",
      nunca_trabajo = "Indicador de nunca haber trabajado antes",
      status_seq    = "Condición laboral y experiencia previa",
      contrato0     = "Indicador de existencia de contrato laboral",
      contrato1     = "Indicador de contrato temporal o indefinido",
      temporal      = "Clasificación de temporalidad laboral",
      temporal_seq  = "Secuencia de temporalidad laboral"
    ) %>%
    sjlabelled::val_labels(
      skill_level = c(
        "Primaria" = 1,
        "Secundaria" = 2,
        "Terciaria" = 3
      ),
      skill_actual = c(
        "Ninguna" = 0,
        "Primaria" = 1,
        "Secundaria" = 2,
        "Terciaria" = 3
      ),
      mismatch = c(
        "Sobrecualificada/o" = -1,
        "Ajustada/o" = 0,
        "Infracualificada/o" = 1
      ),
      mismatch2 = c(
        "Sobrecualificada/o" = -1,
        "Ajustada/o" = 0,
        "Infracualificada/o" = 1
      ),
      nunca_trabajo = c(
        "Ya había trabajado" = 0,
        "Nunca había trabajado" = 1
      ),
      status_seq = c(
        "Ocupado" = 1,
        "Desempleado con experiencia" = 2,
        "Disponible con experiencia" = 3,
        "No disponible con experiencia" = 4,
        "Desempleado sin experiencia" = 5,
        "Disponible sin experiencia" = 6,
        "No disponible sin experiencia" = 7
      ),
      contrato0 = c(
        "Sin contrato" = 0,
        "Con contrato" = 1
      ),
      contrato1 = c(
        "Contrato indefinido" = 0,
        "Contrato temporal" = 1
      ),
      temporal = c(
        "Asalariado con contrato temporal" = 1,
        "Asalariado con contrato indefinido" = 2,
        "Asalariado sin contrato" = 3,
        "No asalariado" = 4
      ),
      temporal_seq = c(
        "Fuera de la población ocupada" = 0,
        "Asalariado con contrato temporal" = 1,
        "Asalariado con contrato indefinido" = 2,
        "Asalariado sin contrato" = 3,
        "No asalariado" = 4
      )
    ) %>%
    dplyr::select(-mismatch_raw, -var_exp_previa)

  return(data)
}
