#' Procesar variables de estructura del hogar en la ENOE
#'
#' Calcula variables derivadas sobre la composición y estructura de los hogares a partir de los microdatos de la ENOE.
#' Esta función requiere que previamente se hayan generado variables sociodemográficas mediante `procesar_vars_sociodemo()`.
#'
#' Incluye:
#' - Clasificación de parentesco (`relative`), ajustada al catálogo correspondiente según el periodo (antes o después de 2013 trimestre 3)
#' - Tipologías de hogares (familiares, extensos, compuestos, etc.)
#' - Tamaño del hogar y tasas de dependencia (menores, mayores y total)
#' - Conteo de integrantes por grupo etario
#' - Indicadores dicotómicos de presencia de grupos clave (niñez, juventud, adultez mayor)
#'
#' Las variables generadas permiten construir tipologías familiares, caracterizar hogares según su composición y
#' analizar necesidades de cuidado o dependencia demográfica.
#'
#' @encoding UTF-8
#' @param data Un data frame con variables como `par_c`, `edad`, `sexo`, `folio2`, previamente procesadas por `procesar_vars_sociodemo()`.
#' @param anio Año de referencia de los datos (numérico).
#' @param trimestre Trimestre de referencia de los datos (número del 1 al 4).
#'
#' @return Un data frame con las siguientes variables nuevas:
#' \describe{
#'   \item{relative}{Clasificación del parentesco con respecto al jefe del hogar, con base en el catálogo correspondiente al año y trimestre}
#'   \item{family, familyt}{Codificaciones intermedias de tipo de familia}
#'   \item{tipo_hog, tipo_hog_lab}{Tipología sintética del hogar: unipersonal, nuclear, extensos, etc.}
#'   \item{tam_hog}{Tamaño del hogar (sin servicio doméstico)}
#'   \item{t_dep1, t_dep2, t_dep3}{Tasas de dependencia menores, mayores y total}
#'   \item{h_00_05, h_06_12, h_13_17, h_18m, h_joven1, h_joven2, h_adm}{Conteo de personas por grupo etario y adultos mayores}
#'   \item{d_00_05, d_06_12, d_13_17, d_18m, d_joven1, d_joven2, d_adm}{Indicadores dicotómicos de presencia de cada grupo en el hogar}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' datos <- fusion_enoe(2021, 1)
#' datos <- procesar_vars_sociodemo(datos)
#' datos <- procesar_vars_hogar(datos, anio = 2021, trimestre = 1)
#' table(datos$tipo_hog_lab, useNA = "always")
#' }
#' @family procesamiento_enoe

procesar_vars_hogar <- function(data, anio, trimestre) {

  if (!"folio2" %in% names(data)) {
    message("Variable 'folio2' no encontrada. Se crea con `crear_folios()`...")
    data <- crear_folios(data)
  }

  # Bloque 1: Clasificación de parentesco usando catálogo externo
  version_parc <- ifelse(anio < 2012 | (anio == 2012 & trimestre <= 2), "par_c1", "par_c2")
  archivo_parc <- system.file(paste0("extdata/", version_parc, ".csv"), package = "renoe")

  cat_parc <- readr::read_csv(archivo_parc, show_col_types = FALSE)

  # Validar que 'par_c' está presente
  if (!"par_c" %in% names(data)) stop("La variable 'par_c' no está presente en el objeto de entrada.")

  # Unir con catálogo de clasificación de parentesco
  data <- data %>%
    dplyr::left_join(cat_parc, by = "par_c") %>%
    dplyr::mutate(
      relative = dplyr::if_else(is.na(relative), 6L, relative),
      relative = sjlabelled::set_label(relative,
                                       label = "Clasificación del parentesco respecto a la jefatura del hogar")
    )
  # Bloque 2: Conteo por tipo de parentesco dentro del hogar
  data <- data %>%
    dplyr::group_by(folio2) %>%
    dplyr::mutate(
      rela1 = sum(relative == 1, na.rm = TRUE), # jefes
      rela2 = sum(relative == 2, na.rm = TRUE), # cónyuge
      rela3 = sum(relative == 3, na.rm = TRUE), # hijos/as
      rela4 = sum(relative == 4, na.rm = TRUE), # madre/padre
      rela5 = sum(relative == 5, na.rm = TRUE), # otros parientes
      rela6 = sum(relative == 6, na.rm = TRUE), # no parientes
      jefa_mujer = sum(relative == 1 & sexo == 2, na.rm = TRUE),
      jefe_hombre = sum(relative == 1 & sexo == 1, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Bloque 3: Tipología del hogar con lógica completa
  data <- data %>%
    dplyr::mutate(
      family = dplyr::case_when(
        # Jefe solo
        rela1 == 1 & rela2 == 0 & rela3 == 0 & rela4 == 0 & rela5 == 0 & rela6 == 0 ~ 1,
        # Jefe con uno
        rela1 == 1 & rela2 >= 1 & rela3 == 0 & rela4 == 0 & rela5 == 0 & rela6 == 0 ~ 2,
        rela1 == 1 & rela2 == 0 & rela3 >= 1 & rela4 == 0 & rela5 == 0 & rela6 == 0 ~ 3,
        rela1 == 1 & rela2 == 0 & rela3 == 0 & rela4 >= 1 & rela5 == 0 & rela6 == 0 ~ 4,
        rela1 == 1 & rela2 == 0 & rela3 == 0 & rela4 == 0 & rela5 >= 1 & rela6 == 0 ~ 5,
        rela1 == 1 & rela2 == 0 & rela3 == 0 & rela4 == 0 & rela5 == 0 & rela6 >= 1 ~ 6,
        # Jefe con dos
        rela1 == 1 & rela2 >= 1 & rela3 >= 1 & rela4 == 0 & rela5 == 0 & rela6 == 0 ~ 7,
        rela1 == 1 & rela2 >= 1 & rela3 == 0 & rela4 >= 1 & rela5 == 0 & rela6 == 0 ~ 8,
        rela1 == 1 & rela2 >= 1 & rela3 == 0 & rela4 == 0 & rela5 >= 1 & rela6 == 0 ~ 9,
        rela1 == 1 & rela2 >= 1 & rela3 == 0 & rela4 == 0 & rela5 == 0 & rela6 >= 1 ~ 10,
        rela1 == 1 & rela2 == 0 & rela3 >= 1 & rela4 >= 1 & rela5 == 0 & rela6 == 0 ~ 11,
        rela1 == 1 & rela2 == 0 & rela3 >= 1 & rela4 == 0 & rela5 >= 1 & rela6 == 0 ~ 12,
        rela1 == 1 & rela2 == 0 & rela3 >= 1 & rela4 == 0 & rela5 == 0 & rela6 >= 1 ~ 13,
        rela1 == 1 & rela2 == 0 & rela3 == 0 & rela4 >= 1 & rela5 >= 1 & rela6 == 0 ~ 14,
        rela1 == 1 & rela2 == 0 & rela3 == 0 & rela4 >= 1 & rela5 == 0 & rela6 >= 1 ~ 15,
        rela1 == 1 & rela2 == 0 & rela3 == 0 & rela4 == 0 & rela5 >= 1 & rela6 >= 1 ~ 16,
        # Jefe con tres
        rela1 == 1 & rela2 >= 1 & rela3 >= 1 & rela4 >= 1 & rela5 == 0 & rela6 == 0 ~ 17,
        rela1 == 1 & rela2 >= 1 & rela3 >= 1 & rela4 == 0 & rela5 >= 1 & rela6 == 0 ~ 18,
        rela1 == 1 & rela2 >= 1 & rela3 >= 1 & rela4 == 0 & rela5 == 0 & rela6 >= 1 ~ 19,
        rela1 == 1 & rela2 >= 1 & rela3 == 0 & rela4 >= 1 & rela5 >= 1 & rela6 == 0 ~ 20,
        rela1 == 1 & rela2 >= 1 & rela3 == 0 & rela4 >= 1 & rela5 == 0 & rela6 >= 1 ~ 21,
        rela1 == 1 & rela2 >= 1 & rela3 == 0 & rela4 == 0 & rela5 >= 1 & rela6 >= 1 ~ 22,
        rela1 == 1 & rela2 == 0 & rela3 >= 1 & rela4 >= 1 & rela5 >= 1 & rela6 == 0 ~ 23,
        rela1 == 1 & rela2 == 0 & rela3 >= 1 & rela4 >= 1 & rela5 == 0 & rela6 >= 1 ~ 24,
        rela1 == 1 & rela2 == 0 & rela3 >= 1 & rela4 == 0 & rela5 >= 1 & rela6 >= 1 ~ 25,
        rela1 == 1 & rela2 == 0 & rela3 == 0 & rela4 >= 1 & rela5 >= 1 & rela6 >= 1 ~ 26,
        # Jefe con cuatro
        rela1 == 1 & rela2 == 0 & rela3 >= 1 & rela4 >= 1 & rela5 >= 1 & rela6 >= 1 ~ 27,
        rela1 == 1 & rela2 >= 1 & rela3 == 0 & rela4 >= 1 & rela5 >= 1 & rela6 >= 1 ~ 28,
        rela1 == 1 & rela2 >= 1 & rela3 >= 1 & rela4 == 0 & rela5 >= 1 & rela6 >= 1 ~ 29,
        rela1 == 1 & rela2 >= 1 & rela3 >= 1 & rela4 >= 1 & rela5 == 0 & rela6 >= 1 ~ 30,
        rela1 == 1 & rela2 >= 1 & rela3 >= 1 & rela4 >= 1 & rela5 >= 1 & rela6 == 0 ~ 31,
        # Jefe con cinco
        rela1 == 1 & rela2 >= 1 & rela3 >= 1 & rela4 >= 1 & rela5 >= 1 & rela6 >= 1 ~ 32,
        TRUE ~ 99
      ),

      # Tipología sintética basada en family
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
    )
  # Bloque 4: Tamaño del hogar y tasas de dependencia
  data <- data %>%
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

  # Bloque 5: Conteo de grupos etarios por hogar
  data <- data %>%
    dplyr::group_by(folio2) %>%
    dplyr::mutate(
      h_00_05 = sum(i_00_05 == 1, na.rm = TRUE),
      h_06_12 = sum(i_06_12 == 1, na.rm = TRUE),
      h_13_17 = sum(i_13_17 == 1, na.rm = TRUE),
      h_18m = sum(i_18m == 1, na.rm = TRUE),
      h_joven1 = sum(i_joven1 == 1, na.rm = TRUE),
      h_joven2 = sum(i_joven2 == 1, na.rm = TRUE),
      h_adm = sum(adm == 1, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Bloque 6: Indicadores dicotómicos por hogar
  data <- data %>%
    dplyr::group_by(folio2) %>%
    dplyr::mutate(
      d_00_05 = as.integer(any(i_00_05 == 1, na.rm = TRUE)),
      d_06_12 = as.integer(any(i_06_12 == 1, na.rm = TRUE)),
      d_13_17 = as.integer(any(i_13_17 == 1, na.rm = TRUE)),
      d_18m = as.integer(any(i_18m == 1, na.rm = TRUE)),
      d_joven1 = as.integer(any(i_joven1 == 1, na.rm = TRUE)),
      d_joven2 = as.integer(any(i_joven2 == 1, na.rm = TRUE)),
      d_adm = as.integer(any(adm == 1, na.rm = TRUE))
    ) %>%
    dplyr::ungroup()

  # Bloque 7: Ocupación en el hogar
  data <- data %>%
    dplyr::group_by(folio2) %>%
    dplyr::mutate(
      p_lab = sum(clase2 == 1, na.rm = TRUE),
      p_lab_ratio = dplyr::if_else(tam_hog > 0, p_lab / tam_hog, NA_real_)
    ) %>%
    dplyr::ungroup()


  # Bloque 8: Etiquetas
  data <- data %>%
    dplyr::mutate(
      h_00_05     = sjlabelled::set_label(h_00_05, label = "Número de integrantes de 0 a 5 años"),
      h_06_12     = sjlabelled::set_label(h_06_12, label = "Número de integrantes de 6 a 12 años"),
      h_13_17     = sjlabelled::set_label(h_13_17, label = "Número de integrantes de 13 a 17 años"),
      h_18m     = sjlabelled::set_label(h_18m, label = "Número de integrantes mayores de edad (18+)"),
      h_joven1    = sjlabelled::set_label(h_joven1, label = "Número de integrantes de 15 a 24 años"),
      h_joven2    = sjlabelled::set_label(h_joven2, label = "Número de integrantes de 15 a 29 años"),
      h_adm       = sjlabelled::set_label(h_adm, label = "Número de integrantes de 65 años o más (adultos mayores)"),

      d_00_05     = sjlabelled::set_label(d_00_05, label = "Hogar con al menos un integrante de 0 a 5 años"),
      d_06_12     = sjlabelled::set_label(d_06_12, label = "Hogar con al menos un integrante de 6 a 12 años"),
      d_13_17     = sjlabelled::set_label(d_13_17, label = "Hogar con al menos un integrante de 13 a 17 años"),
      d_18m     = sjlabelled::set_label(d_18m, label = "Hogar con al menos un integrante mayor de edad (18+)"),
      d_joven1    = sjlabelled::set_label(d_joven1, label = "Hogar con al menos un integrante de 15 a 24 años"),
      d_joven2    = sjlabelled::set_label(d_joven2, label = "Hogar con al menos un integrante de 15 a 29 años"),
      d_adm       = sjlabelled::set_label(d_adm, label = "Hogar con al menos un integrante adulto mayor (65+)"),
      p_lab       = sjlabelled::set_label(p_lab, label = "Ocupados en el hogar (clase2 == 1)"),
      p_lab_ratio = sjlabelled::set_label(p_lab_ratio, label = "Proporción de ocupados respecto al tamaño del hogar")
    )

  data <- data %>%
    dplyr::mutate(
      tam_hog       = sjlabelled::set_label(tam_hog, label = "Tamaño del hogar (sin servicio doméstico)"),
      t_dep1        = sjlabelled::set_label(t_dep1, label = "Tasa de dependencia juvenil"),
      t_dep2        = sjlabelled::set_label(t_dep2, label = "Tasa de dependencia senil"),
      t_dep3        = sjlabelled::set_label(t_dep3, label = "Tasa de dependencia total"),
      tipo_hog_lab  = sjlabelled::set_label(tipo_hog_lab, label = "Tipología del hogar"),
      tipo_hog2_lab = sjlabelled::set_label(tipo_hog2_lab, label = "Tipología sintética del hogar")
    )

  return(data)



}
