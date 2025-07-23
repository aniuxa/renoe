#' Procesar variables de estructura del hogar en la ENOE
#'
#' Calcula el tipo de parentesco, clasificaciones de hogares (familiares, extensos, compuestos, etc.)
#' y tasas de dependencia. Esta función requiere que las variables sociodemográficas ya hayan sido creadas.
#'
#' @encoding UTF-8
#' @param data Un data frame con variables como `par_c`, `edad`, `sexo`, `folio2`, previamente procesadas por `procesar_vars_sociodemo()`.
#'
#' @return Un data frame con variables nuevas:
#' \describe{
#'   \item{relative}{Clasificación del parentesco con respecto al jefe}
#'   \item{family, familyt, tipo_hog, tipo_hog_lab}{Clasificaciones del tipo de hogar}
#'   \item{tam_hog, t_dep1, t_dep2, t_dep3}{Tamaño del hogar y tasas de dependencia}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' datos <- fusion_enoe(2021, 1)
#' datos <- procesar_vars_sociodemo(datos)
#' datos <- procesar_vars_hogar(datos)
#' table(datos$tipo_hog_lab, useNA = "always")
#' }
procesar_vars_hogar <- function(data) {
  data %>%
    dplyr::mutate(
      relative = dplyr::case_when(
        .data$par_c >= 100 & .data$par_c < 200 ~ 1,  # Jefe
        .data$par_c >= 200 & .data$par_c < 300 ~ 2,  # Cónyuge
        .data$par_c >= 300 & .data$par_c < 400 ~ 3,  # Hijo/a
        .data$par_c >= 400 & .data$par_c < 403 ~ 4,  # Padre/madre
        .data$par_c >= 404 & .data$par_c < 424 ~ 5,  # Otros parientes
        .data$par_c == 601 | .data$par_c == 612 ~ 7, # Servicio doméstico
        TRUE ~ 6                                     # No parientes
      )
    ) %>%
    dplyr::group_by(folio2) %>%
    dplyr::mutate(
      rela1 = sum(relative == 1, na.rm = TRUE),
      rela2 = sum(relative == 2, na.rm = TRUE),
      rela3 = sum(relative == 3, na.rm = TRUE),
      rela4 = sum(relative == 4, na.rm = TRUE),
      rela5 = sum(relative == 5, na.rm = TRUE),
      rela6 = sum(relative == 6, na.rm = TRUE),
      jefa_mujer = sum(relative == 1 & sexo == 2, na.rm = TRUE),
      jefe_hombre = sum(relative == 1 & sexo == 1, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      family = dplyr::case_when(
        rela1 == 1 & rela2 == 0 & rela3 == 0 & rela4 == 0 & rela5 == 0 & rela6 == 0 ~ 1,
        rela1 == 1 & rela2 >= 1 & rela3 == 0 & rela4 == 0 & rela5 == 0 & rela6 == 0 ~ 2,
        rela1 == 1 & rela2 == 0 & rela3 >= 1 & rela4 == 0 & rela5 == 0 & rela6 == 0 ~ 3,
        rela1 == 1 & rela2 == 0 & rela3 == 0 & rela4 >= 1 & rela5 == 0 & rela6 == 0 ~ 4,
        rela1 == 1 & rela2 == 0 & rela3 == 0 & rela4 == 0 & rela5 >= 1 & rela6 == 0 ~ 5,
        rela1 == 1 & rela2 == 0 & rela3 == 0 & rela4 == 0 & rela5 == 0 & rela6 >= 1 ~ 6,
        rela1 == 1 & rela2 >= 1 & rela3 >= 1 & rela4 == 0 & rela5 == 0 & rela6 == 0 ~ 7,
        TRUE ~ 99
      ),
      familyt = dplyr::case_when(
        family == 1 ~ 1,
        family == 6 ~ 2,
        family == 2 ~ 3,
        family == 7 ~ 4,
        family == 3 ~ 5,
        family %in% c(4, 5, 8, 9, 11, 12, 14, 17, 18, 20, 23, 31) ~ 6,
        family %in% c(10, 13, 15, 16, 19, 21, 22, 24, 25, 26, 27, 28, 29, 30, 32) ~ 7
      ),
      familyt_lab = factor(familyt, levels = 1:7, labels = c(
        "Unipersonal", "Corresidentes", "Parejas sin hijos",
        "Parejas con hijos", "Jefe con hijos", "Hogares extensos", "Compuestos"
      )),
      tipo_hog = dplyr::case_when(
        familyt == 1 ~ 1,
        familyt == 2 ~ 2,
        familyt %in% c(3, 4, 5) ~ 3,
        familyt == 6 ~ 6,
        familyt == 7 ~ 7
      ),
      tipo_hog_lab = factor(tipo_hog, levels = c(1, 2, 3, 6, 7), labels = c(
        "Unipersonal", "Corresidentes", "Nuclear", "Hogares extensos", "Compuestos"
      )),
      tipo_hog2 = dplyr::recode(tipo_hog, `1` = 1, `2` = 1, `3` = 3, `6` = 6, `7` = 6),
      tipo_hog2_lab = factor(tipo_hog2, levels = c(1, 3, 6), labels = c(
        "No-familiar", "Nuclear", "Extensos"
      ))
    ) %>%
    dplyr::group_by(folio2) %>%
    dplyr::mutate(
      tam_hog = sum(relative != 7, na.rm = TRUE),
      men = sum(edad < 15, na.rm = TRUE),
      may = sum(edad >= 65, na.rm = TRUE),
      nondep = sum(edad >= 15 & edad < 65, na.rm = TRUE),
      dep = men + may,
      t_dep1 = dplyr::if_else(nondep > 0, men / nondep, tam_hog),
      t_dep2 = dplyr::if_else(nondep > 0, may / nondep, tam_hog),
      t_dep3 = dplyr::if_else(nondep > 0, dep / nondep, tam_hog)
    ) %>%
    dplyr::ungroup()
}
