#' Procesar variables sociodemográficas básicas y extendidas
#'
#' Añade sexo, edad, grupos etarios, metadatos como año, trimestre y tipo de
#' cuestionario COE, así como variables extendidas de asistencia escolar,
#' estado conyugal, escolaridad, zona rural, tamaño de localidad y zona
#' económica regional.
#'
#' @param data Un data frame tipo sdem, con variables como `sex`, `eda`,
#'   `cs_p17`, `e_con`, `anios_esc`, `par_c`, `t_loc` y `ent`.
#' @param anio Año del trimestre.
#' @param trimestre Trimestre numérico (1-4).
#'
#' @return Un data frame con variables sociodemográficas procesadas y
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
        par_c %in% 201:205 ~ 2,
        par_c %in% 301:304 ~ 3,
        TRUE ~ 4
      ),
      par_dic = dplyr::if_else(par_c == 101, 1, 0),
      t_loc = as.numeric(t_loc),
      urb_rur = dplyr::case_when(
        t_loc %in% 1:3 ~ 1,
        t_loc == 4 ~ 2
      ),
      rural = dplyr::if_else(urb_rur == 2, TRUE, FALSE, missing = NA),
      zona_econ = dplyr::case_when(
        ent %in% c(5, 19, 28) ~ 1,
        ent %in% c(2, 3, 8, 10, 25, 26) ~ 2,
        ent %in% c(6, 18, 16, 14) ~ 3,
        ent %in% c(13, 21, 29, 30) ~ 4,
        ent %in% c(1, 11, 22, 24, 32) ~ 5,
        ent %in% c(9, 15, 17) ~ 6,
        ent %in% c(4, 23, 27, 31) ~ 7,
        ent %in% c(7, 12, 20) ~ 8
      )
    ) %>%
    sjlabelled::var_labels(
      sexo       = "Sexo",
      edad       = "Edad en años",
      edad5      = "Edad en grupos quinquenales",
      adm        = "Adulto mayor (65 años o más)",
      i_00_05    = "Persona de 0 a 5 años",
      i_06_12    = "Persona de 6 a 12 años",
      i_13_17    = "Persona de 13 a 17 años",
      i_18m      = "Persona de 18 años o más",
      i_joven1   = "Persona de 15 a 24 años",
      i_joven2   = "Persona de 15 a 29 años",
      anio       = "Año del trimestre",
      trim       = "Número de trimestre (t1-t4)",
      coe_tipo   = "Tipo de cuestionario COE (básico o ampliado)",
      asiste     = "Asistencia escolar",
      unido      = "Estado conyugal: vive en unión",
      anios_es   = "Años de escolaridad",
      parentesco = "Parentesco resumido con la jefatura",
      par_dic    = "Indicador de jefatura del hogar",
      t_loc      = "Tamaño de localidad",
      urb_rur    = "Área urbana o rural",
      rural      = "Indicador dicotómico de ruralidad",
      zona_econ  = "Zona económica regional"
    ) %>%
    sjlabelled::val_labels(
      asiste = c(
        "No asiste a la escuela" = 0,
        "Asiste a la escuela" = 1
      ),
      unido = c(
        "No vive en unión" = 0,
        "Vive en unión" = 1
      ),
      parentesco = c(
        "Jefa/e" = 1,
        "Cónyuge o pareja" = 2,
        "Hija/o" = 3,
        "Otro parentesco" = 4
      ),
      t_loc = c(
        "100 000 o más habitantes" = 1,
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
