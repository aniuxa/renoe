#' Procesar variables de analisis laboral y desajuste educativo
#'
#' Esta funcion genera variables clasificatorias relacionadas con la ocupacion,
#' el nivel educativo y el desajuste entre ambos, a partir de los codigos de
#' ocupacion (`p3coe`) y del nivel educativo alcanzado (`cs_p13_1`).
#' Internamente armoniza los codigos SINCO (1, 2, 3 y 4 digitos) usando
#' correspondencias con codigos CMO y reglas auxiliares. Tambien clasifica el
#' nivel agregado de competencia de la ocupacion, un proxy basado en escolaridad
#' y el desajuste entre ambos. La agrupacion de competencia requerida es una
#' aproximacion a un digito de SINCO y puede ocultar excepciones dentro de cada
#' division.
#'
#' La referencia estadistica en anos de escolaridad se calcula por separado con
#' [calcular_desajuste_estadistico()]. Esta separacion evita construir una
#' referencia aparentemente anual cuando la entrada contiene un solo trimestre.
#' Por compatibilidad, si la entrada ya contiene `esco_norm` o `mismatch2`, esas
#' columnas historicas se conservan sin recalcularlas.
#'
#' Ademas, genera variables relacionadas con la experiencia previa
#' (`nunca_trabajo`), el estatus laboral combinado (`status_seq`) y las
#' caracteristicas contractuales (`contrato0`, `contrato1`, `temporal`,
#' `temporal_seq`) segun el tipo de cuestionario.
#'
#' @param data Un data.frame con variables como:
#' - `anio`, `trimestre`: ano y trimestre de la entrevista
#' - `coe_tipo`: tipo de cuestionario (`"ampliado"` o `"basico"`)
#' - `p3coe`: codigo ocupacional
#' - `cs_p13_1`, `cs_p15`: nivel educativo y antecedente escolar
#' - `clase2`: clase de actividad economica
#' - `pos_ocu`, `tue2`: posicion en la ocupacion y tipo de unidad economica
#' - `p2h4`: experiencia laboral previa
#' - `p3i`, `p3j`, `p3j1`, `p3k1`: variables sobre tipo de contrato
#'
#' @param escenario Contrato de armonizacion ocupacional. Se conserva cuando
#'   la entrada ya fue armonizada por la ruta canonica.
#' @return Un data.frame con las variables originales y nuevas columnas:
#' - `sinco1d`, `sinco2d`, `sinco3d`, `sinco4d`
#' - `skill_level`, `skill_actual`
#' - `mismatch`
#' - `nunca_trabajo`, `status_seq`
#' - `contrato0`, `contrato1`, `temporal`, `temporal_seq`
#'
#' @export
#' @family procesamiento_enoe

procesar_vars_laborales <- function(
    data,
    escenario = c("integrated_accepted", "official_strict", "analysis_legacy")) {
  escenario <- match.arg(escenario)
  if ("p4a" %in% names(data) &&
      !"scian_version_observada" %in% names(data)) {
    data <- renoe::armonizar_scian(data)
  }
  escenario_actual <- if ("sinco_escenario" %in% names(data)) {
    unique(as.character(data$sinco_escenario))
  } else {
    character()
  }
  if (!"sinco2011_comparable" %in% names(data) ||
      length(escenario_actual) != 1L || escenario_actual != escenario) {
    data <- renoe::armonizar_sinco(data, escenario = escenario)
  }

  if (!"cs_p13_1" %in% names(data)) {
    stop("Falta la variable `cs_p13_1`.", call. = FALSE)
  }
  if (!"clase2" %in% names(data)) {
    stop("Falta la variable `clase2`.", call. = FALSE)
  }

  cs_p13_codigo <- .normalizar_codigo_educativo(data$cs_p13_1, 0:9)
  cs_p15_codigo <- if ("cs_p15" %in% names(data)) {
    .normalizar_codigo_educativo(data$cs_p15, 1:3)
  } else {
    rep(NA_integer_, nrow(data))
  }

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
        sinco2011_comparable & sinco1d %in% 1:2 ~ 3,
        sinco2011_comparable & sinco1d %in% 3:8 ~ 2,
        sinco2011_comparable & sinco1d == 9 ~ 1,
        TRUE ~ NA_real_
      ),
      skill_actual = dplyr::case_when(
        cs_p13_codigo %in% 0:1 ~ 0,
        cs_p13_codigo == 2 ~ 1,
        cs_p13_codigo %in% 3:4 ~ 2,
        cs_p13_codigo %in% 5:6 & cs_p15_codigo %in% 1:2 ~ 2,
        cs_p13_codigo %in% 5:6 & cs_p15_codigo == 3 ~ 3,
        cs_p13_codigo %in% 7:9 ~ 3,
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
    dplyr::mutate(
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

  data$contrato0 <- rep(NA_real_, nrow(data))
  data$contrato1 <- rep(NA_real_, nrow(data))

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
      skill_level   = "Nivel agregado de competencia requerido por la ocupaci\u00F3n",
      skill_actual  = "Proxy de competencia basado en escolaridad",
      mismatch      = "Desajuste educativo",
      nunca_trabajo = "Indicador de nunca haber trabajado antes",
      status_seq    = "Condici\u00F3n laboral y experiencia previa",
      contrato0     = "Indicador de existencia de contrato laboral",
      contrato1     = "Indicador de contrato temporal o indefinido",
      temporal      = "Clasificaci\u00F3n de temporalidad laboral",
      temporal_seq  = "Secuencia de temporalidad laboral"
    ) %>%
    sjlabelled::val_labels(
      skill_level = c(
        "Competencia b\u00E1sica" = 1,
        "Competencia media" = 2,
        "Competencia alta" = 3
      ),
      skill_actual = c(
        "Ninguna" = 0,
        "Primaria" = 1,
        "Secundaria" = 2,
        "Terciaria" = 3
      ),
      mismatch = c(
        "Sobreeducaci\u00F3n" = -1,
        "Ajuste" = 0,
        "Subeducaci\u00F3n" = 1
      ),
      nunca_trabajo = c(
        "Ya hab\u00EDa trabajado" = 0,
        "Nunca hab\u00EDa trabajado" = 1
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
        "Fuera de la poblaci\u00F3n ocupada" = 0,
        "Asalariado con contrato temporal" = 1,
        "Asalariado con contrato indefinido" = 2,
        "Asalariado sin contrato" = 3,
        "No asalariado" = 4
      )
    ) %>%
    dplyr::select(-mismatch_raw, -var_exp_previa)

  return(data)
}

.normalizar_codigo_educativo <- function(x, validos) {
  salida <- rep(NA_integer_, length(x))

  if (is.numeric(x)) {
    numero <- suppressWarnings(as.numeric(x))
    candidato <- is.finite(numero) & numero == floor(numero)
  } else {
    texto <- trimws(as.character(x))
    candidato <- !is.na(texto) & grepl("^[0-9]+$", texto)
    numero <- rep(NA_real_, length(texto))
    numero[candidato] <- suppressWarnings(as.numeric(texto[candidato]))
  }

  reconocido <- candidato & numero %in% validos
  salida[reconocido] <- as.integer(numero[reconocido])
  salida
}
