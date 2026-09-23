#' Procesar variables sociodemograficas basicas y extendidas
#'
#' Anade sexo, edad, grupos etarios, metadatos como ano, trimestre y tipo de
#' cuestionario COE, asi como variables extendidas de asistencia escolar,
#' estado conyugal, escolaridad, zona rural, tamano de localidad y zona
#' economica regional.
#' `zona_econ` sigue una regionalizacion analitica de ocho zonas documentada en
#' `clasificacion_region_socioeconomica.csv`; no representa una clasificacion
#' territorial oficial unica.
#'
#' @param data Un data frame tipo sdem, con variables como `sex`, `eda`,
#'   `cs_p17`, `e_con`, `anios_esc`, `par_c`, `t_loc` y `ent`.
#' @param anio Ano del trimestre.
#' @param trimestre Trimestre numerico (1-4).
#'
#' @return Un data frame con variables sociodemograficas procesadas y
#'   etiquetadas.
#' @export
#'
#' @examples
#' \dontrun{
#' datos_sdem_proc <- procesar_vars_sociodemo(sdem, anio = 2023, trimestre = 1)
#' }
#' @family procesamiento_enoe

procesar_vars_sociodemo <- function(data, anio, trimestre) {
  info <- info_trimestre(anio, trimestre)
  catalogo_parentesco_antiguo <- anio < 2012L ||
    (anio == 2012L && trimestre <= 2L)

  archivo_zonas <- system.file(
    "extdata/clasificacion_region_socioeconomica.csv", package = "renoe"
  )
  if (!nzchar(archivo_zonas)) {
    archivo_zonas <- file.path(
      "package", "renoe", "inst", "extdata",
      "clasificacion_region_socioeconomica.csv"
    )
  }
  tabla_zonas <- readr::read_csv(
    archivo_zonas, show_col_types = FALSE
  ) |>
    dplyr::select(ent, zona_econ)
  if (nrow(tabla_zonas) != 32L || anyDuplicated(tabla_zonas$ent) ||
      !setequal(tabla_zonas$ent, 1:32) ||
      !all(tabla_zonas$zona_econ %in% 1:8)) {
    stop("La tabla de regiones socioeconomicas no contiene 32 entidades validas.")
  }

  data <- data %>%
    dplyr::mutate(
      sexo = sex,
      edad = dplyr::case_when(
        as.numeric(eda) %in% c(98, 99) ~ NA_real_,
        TRUE ~ as.numeric(eda)
      ),
      edad5 = cut(edad, breaks = seq(0, 100, 5), right = FALSE),
      adm = dplyr::if_else(edad >= 65, 1, 0, missing = 0),
      i_00_05 = dplyr::if_else(edad < 6, 1, 0, missing = 0),
      i_06_12 = dplyr::if_else(dplyr::between(edad, 6, 12), 1, 0, missing = 0),
      i_13_17 = dplyr::if_else(dplyr::between(edad, 13, 17), 1, 0, missing = 0),
      i_18m = dplyr::if_else(edad >= 18, 1, 0, missing = 0),
      i_joven1 = dplyr::if_else(dplyr::between(edad, 15, 24), 1, 0, missing = 0),
      i_joven2 = dplyr::if_else(dplyr::between(edad, 15, 29), 1, 0, missing = 0),
      anio = anio,
      trim = paste0("t", trimestre),
      coe_tipo = info$coe_tipo,
      asiste = dplyr::case_when(
        cs_p17 == 1 ~ 1,
        cs_p17 == 2 ~ 0,
        is.na(cs_p17) | cs_p17 == 9 ~ NA_real_
      ),
      unido = dplyr::if_else(e_con %in% c(1, 5), 1, 0, missing = NA_real_),
      anios_es = dplyr::if_else(anios_esc == 99, NA_real_, anios_esc),
      parentesco = dplyr::case_when(
        par_c == 101 ~ 1,
        catalogo_parentesco_antiguo & par_c %in% 201:205 ~ 2,
        !catalogo_parentesco_antiguo & par_c %in% 201:204 ~ 2,
        catalogo_parentesco_antiguo & par_c %in% 301:305 ~ 3,
        !catalogo_parentesco_antiguo & par_c %in% 301:304 ~ 3,
        TRUE ~ 4
      ),
      par_dic = dplyr::if_else(par_c == 101, 1, 0),
      t_loc = as.numeric(t_loc),
      urb_rur = dplyr::case_when(
        t_loc %in% 1:3 ~ 1,
        t_loc == 4 ~ 2
      ),
      rural = dplyr::if_else(urb_rur == 2, TRUE, FALSE, missing = NA)
    ) %>%
    dplyr::select(-dplyr::any_of("zona_econ")) %>%
    dplyr::left_join(tabla_zonas, by = "ent") %>%
    sjlabelled::var_labels(
      sexo       = "Sexo",
      edad       = "Edad en a\u00F1os",
      edad5      = "Edad en grupos quinquenales",
      adm        = "Adulto mayor (65 a\u00F1os o m\u00E1s)",
      i_00_05    = "Persona de 0 a 5 a\u00F1os",
      i_06_12    = "Persona de 6 a 12 a\u00F1os",
      i_13_17    = "Persona de 13 a 17 a\u00F1os",
      i_18m      = "Persona de 18 a\u00F1os o m\u00E1s",
      i_joven1   = "Persona de 15 a 24 a\u00F1os",
      i_joven2   = "Persona de 15 a 29 a\u00F1os",
      anio       = "A\u00F1o del trimestre",
      trim       = "N\u00FAmero de trimestre (t1-t4)",
      coe_tipo   = "Tipo de cuestionario COE (b\u00E1sico o ampliado)",
      asiste     = "Asistencia escolar",
      unido      = "Estado conyugal: vive en uni\u00F3n",
      anios_es   = "A\u00F1os de escolaridad",
      parentesco = "Parentesco resumido con la jefatura",
      par_dic    = "Indicador de jefatura del hogar",
      t_loc      = "Tama\u00F1o de localidad",
      urb_rur    = "\u00C1rea urbana o rural",
      rural      = "Indicador dicot\u00F3mico de ruralidad",
      zona_econ  = "Regi\u00F3n socioecon\u00F3mica anal\u00EDtica de ocho zonas"
    ) %>%
    sjlabelled::val_labels(
      asiste = c(
        "No asiste a la escuela" = 0,
        "Asiste a la escuela" = 1
      ),
      unido = c(
        "No vive en uni\u00F3n" = 0,
        "Vive en uni\u00F3n" = 1
      ),
      parentesco = c(
        "Jefa/e" = 1,
        "C\u00F3nyuge o pareja" = 2,
        "Hija/o" = 3,
        "Otro parentesco" = 4
      ),
      t_loc = c(
        "100 000 o m\u00E1s habitantes" = 1,
        "15 000 a 99 999 habitantes" = 2,
        "2 500 a 14 999 habitantes" = 3,
        "Menos de 2 500 habitantes" = 4
      ),
      urb_rur = c(
        "Urbana" = 1,
        "Rural" = 2
      ),
      zona_econ = c(
        "Noreste" = 1,
        "Noroeste" = 2,
        "Occidente" = 3,
        "Oriente" = 4,
        "Centro norte" = 5,
        "Centro sur" = 6,
        "Suroeste" = 7,
        "Sureste" = 8
      )
    )

  return(data)
}
