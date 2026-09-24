#' Procesar variables de estructura del hogar en la ENOE
#'
#' Calcula variables derivadas sobre la composicion y estructura de los hogares
#' a partir de los microdatos de la ENOE. Esta funcion requiere que previamente
#' se hayan generado variables sociodemograficas mediante
#' `procesar_vars_sociodemo()`.
#'
#' Incluye:
#' - Clasificacion de parentesco (`relative`), ajustada al catalogo correspondiente
#'   segun el periodo
#' - Tipologias de hogares (familiares, extensos, compuestos, etc.)
#' - Tamano del hogar y tasas de dependencia (menores, mayores y total)
#' - Conteo de integrantes por grupo etario
#' - Indicadores dicotomicos de presencia de grupos clave (ninez, juventud,
#'   adultez mayor)
#'
#' Las variables generadas permiten construir tipologias familiares,
#' caracterizar hogares segun su composicion y analizar necesidades de cuidado
#' o dependencia demografica.
#'
#' La variable `tam_hog` se calcula excluyendo al servicio domestico y a sus
#' familiares, identificados en el catalogo de `par_c` mediante `relative == 7`.
#'
#' @encoding UTF-8
#' @param data Un data frame con variables como `par_c`, `edad`, `sexo`,
#'   `folio2`, previamente procesadas por `procesar_vars_sociodemo()`.
#' @param anio Año de referencia de los datos.
#' @param trimestre Trimestre de referencia de los datos (numero del 1 al 4).
#'
#' @return Un data frame con variables derivadas de estructura del hogar y
#'   composicion demografica, etiquetadas.
#' @export
#'
#' @examples
#' \dontrun{
#' datos <- fusion_enoe(2021, 1)
#' datos <- procesar_vars_sociodemo(datos, anio = 2021, trimestre = 1)
#' datos <- procesar_vars_hogar(datos, anio = 2021, trimestre = 1)
#' table(datos$tipo_hog_lab, useNA = "always")
#' }
#' @family procesamiento_enoe

procesar_vars_hogar <- function(data, anio, trimestre) {

  if (!"folio2" %in% names(data)) {
    message("Variable 'folio2' no encontrada. Se crea con `crear_folios()`...")
    data <- crear_folios(data)
  }

  if (!"par_c" %in% names(data)) {
    stop("La variable 'par_c' no est\u00E1 presente en el objeto de entrada.")
  }

  # En bases apiladas, separar hogares tambien por periodo. Si anio o trim no
  # existen, se conserva el comportamiento previo para un solo trimestre.
  claves_hogar <- c(intersect(c("anio", "trim"), names(data)), "folio2")

  version_parc <- ifelse(
    anio < 2012 | (anio == 2012 & trimestre <= 2), "par_c1", "par_c2"
  )
  archivo_parc <- system.file(
    paste0("extdata/", version_parc, ".csv"), package = "renoe"
  )
  cat_parc <- readr::read_csv(archivo_parc, show_col_types = FALSE) |>
    dplyr::select(par_c, relative)

  data <- data %>%
    dplyr::left_join(cat_parc, by = "par_c") %>%
    dplyr::mutate(
      relative = dplyr::if_else(is.na(relative), 6L, relative)
    )

  # Calcular todos los agregados una sola vez por hogar y unirlos de vuelta es
  # sustancialmente mas rapido que ejecutar dos group_by() + mutate() sobre
  # cada integrante. La llave incluye el periodo cuando procesa bases apiladas.
  agregados_hogar <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(claves_hogar))) %>%
    dplyr::summarise(
      rela1 = sum(relative == 1, na.rm = TRUE),
      rela2 = sum(relative == 2, na.rm = TRUE),
      rela3 = sum(relative == 3, na.rm = TRUE),
      rela4 = sum(relative == 4, na.rm = TRUE),
      rela5 = sum(relative == 5, na.rm = TRUE),
      rela6 = sum(relative == 6, na.rm = TRUE),
      jefa_mujer = sum(relative == 1 & sexo == 2, na.rm = TRUE),
      jefe_hombre = sum(relative == 1 & sexo == 1, na.rm = TRUE),
      tam_hog = sum(relative != 7, na.rm = TRUE),
      men = sum(edad < 15 & relative != 7, na.rm = TRUE),
      may = sum(edad >= 65 & relative != 7, na.rm = TRUE),
      nondep = sum(edad >= 15 & edad < 65 & relative != 7, na.rm = TRUE),
      h_00_05 = sum(i_00_05 == 1 & relative != 7, na.rm = TRUE),
      h_06_12 = sum(i_06_12 == 1 & relative != 7, na.rm = TRUE),
      h_13_17 = sum(i_13_17 == 1 & relative != 7, na.rm = TRUE),
      h_18m = sum(i_18m == 1 & relative != 7, na.rm = TRUE),
      h_joven1 = sum(i_joven1 == 1 & relative != 7, na.rm = TRUE),
      h_joven2 = sum(i_joven2 == 1 & relative != 7, na.rm = TRUE),
      h_adm = sum(adm == 1 & relative != 7, na.rm = TRUE),
      d_00_05 = as.integer(any(i_00_05 == 1 & relative != 7, na.rm = TRUE)),
      d_06_12 = as.integer(any(i_06_12 == 1 & relative != 7, na.rm = TRUE)),
      d_13_17 = as.integer(any(i_13_17 == 1 & relative != 7, na.rm = TRUE)),
      d_18m = as.integer(any(i_18m == 1 & relative != 7, na.rm = TRUE)),
      d_joven1 = as.integer(any(i_joven1 == 1 & relative != 7, na.rm = TRUE)),
      d_joven2 = as.integer(any(i_joven2 == 1 & relative != 7, na.rm = TRUE)),
      d_adm = as.integer(any(adm == 1 & relative != 7, na.rm = TRUE)),
      p_lab = sum(clase2 == 1 & relative != 7, na.rm = TRUE),
      .groups = "drop"
    )

  archivo_tipologia <- system.file(
    "extdata/clasificacion_tipologia_hogar.csv",
    package = "renoe"
  )
  if (!nzchar(archivo_tipologia)) {
    archivo_tipologia <- file.path(
      "package", "renoe", "inst", "extdata",
      "clasificacion_tipologia_hogar.csv"
    )
  }
  tabla_tipologia <- readr::read_csv(
    archivo_tipologia,
    show_col_types = FALSE
  ) %>%
    dplyr::select(
      tiene_conyuge, tiene_hijos, tiene_ascendientes,
      tiene_otros_parientes, tiene_no_parientes,
      family, familyt, tipo_hog, tipo_hog2,
      tipologia_hogar_capa, tipologia_hogar_regla_id
    )

  claves_tipologia <- c(
    "tiene_conyuge", "tiene_hijos", "tiene_ascendientes",
    "tiene_otros_parientes", "tiene_no_parientes"
  )
  if (nrow(tabla_tipologia) != 32L ||
      anyDuplicated(tabla_tipologia[claves_tipologia])) {
    stop("La tabla canonica de tipologia del hogar no contiene 32 llaves unicas.")
  }

  agregados_hogar <- agregados_hogar %>%
    dplyr::mutate(
      tiene_conyuge = as.integer(rela2 >= 1),
      tiene_hijos = as.integer(rela3 >= 1),
      tiene_ascendientes = as.integer(rela4 >= 1),
      tiene_otros_parientes = as.integer(rela5 >= 1),
      tiene_no_parientes = as.integer(rela6 >= 1)
    ) %>%
    dplyr::left_join(tabla_tipologia, by = claves_tipologia) %>%
    dplyr::mutate(
      family = dplyr::if_else(rela1 == 1, family, 99L),
      familyt = dplyr::if_else(rela1 == 1, familyt, NA_integer_),
      tipo_hog = dplyr::if_else(rela1 == 1, tipo_hog, NA_integer_),
      tipo_hog2 = dplyr::if_else(rela1 == 1, tipo_hog2, NA_integer_),
      tipologia_hogar_capa = dplyr::if_else(
        rela1 == 1, tipologia_hogar_capa, NA_character_
      ),
      tipologia_hogar_regla_id = dplyr::if_else(
        rela1 == 1, tipologia_hogar_regla_id, NA_character_
      )
    ) %>%
    dplyr::select(-dplyr::all_of(claves_tipologia))

  data <- data %>%
    dplyr::left_join(agregados_hogar, by = claves_hogar) %>%
    dplyr::mutate(
      familyt_lab = factor(
        familyt,
        levels = 1:7,
        labels = c(
          "Unipersonal", "Corresidentes", "Parejas sin hijos",
          "Parejas con hijos", "Jefa/e con hijos",
          "Hogares extensos", "Compuestos"
        )
      ),
      tipo_hog_lab = factor(
        tipo_hog,
        levels = c(1, 2, 3, 6, 7),
        labels = c("Unipersonal", "Corresidentes", "Nuclear", "Hogares extensos", "Compuestos")
      ),
      tipo_hog2_lab = factor(
        tipo_hog2,
        levels = c(1, 3, 6),
        labels = c("No familiar", "Nuclear", "Extensos")
      ),
      dep = men + may,
      t_dep1 = dplyr::if_else(nondep > 0, men / nondep, tam_hog),
      t_dep2 = dplyr::if_else(nondep > 0, may / nondep, tam_hog),
      t_dep3 = dplyr::if_else(nondep > 0, dep / nondep, tam_hog),
      p_lab_ratio = dplyr::if_else(tam_hog > 0, p_lab / tam_hog, NA_real_)
    ) %>%
    sjlabelled::var_labels(
      relative      = "Clasificaci\u00F3n del parentesco respecto a la jefatura del hogar",
      rela1         = "N\u00FAmero de jefas o jefes en el hogar",
      rela2         = "N\u00FAmero de c\u00F3nyuges o parejas en el hogar",
      rela3         = "N\u00FAmero de hijas o hijos en el hogar",
      rela4         = "N\u00FAmero de madres o padres de la jefatura en el hogar",
      rela5         = "N\u00FAmero de otros parientes en el hogar",
      rela6         = "N\u00FAmero de personas no parientes u otras en el hogar",
      family        = "Tipolog\u00EDa detallada del hogar",
      familyt       = "Tipolog\u00EDa resumida del hogar",
      familyt_lab   = "Tipolog\u00EDa resumida del hogar",
      tipologia_hogar_capa = "Capa que asigna la tipolog\u00EDa del hogar",
      tipologia_hogar_regla_id = "Regla que asigna la tipolog\u00EDa del hogar",
      tipo_hog      = "Tipolog\u00EDa sint\u00E9tica del hogar",
      tipo_hog_lab  = "Tipolog\u00EDa sint\u00E9tica del hogar",
      tipo_hog2     = "Tipolog\u00EDa agregada del hogar",
      tipo_hog2_lab = "Tipolog\u00EDa agregada del hogar",
      tam_hog       = "Tama\u00F1o del hogar (sin servicio dom\u00E9stico)",
      men           = "N\u00FAmero de integrantes menores de 15 a\u00F1os",
      may           = "N\u00FAmero de integrantes de 65 a\u00F1os o m\u00E1s",
      nondep        = "N\u00FAmero de integrantes de 15 a 64 a\u00F1os",
      dep           = "N\u00FAmero de integrantes dependientes: menores de 15 y personas de 65 a\u00F1os o m\u00E1s",
      t_dep1        = "Tasa de dependencia juvenil",
      t_dep2        = "Tasa de dependencia senil",
      t_dep3        = "Tasa de dependencia total",
      h_00_05       = "N\u00FAmero de integrantes de 0 a 5 a\u00F1os",
      h_06_12       = "N\u00FAmero de integrantes de 6 a 12 a\u00F1os",
      h_13_17       = "N\u00FAmero de integrantes de 13 a 17 a\u00F1os",
      h_18m         = "N\u00FAmero de integrantes de 18 a\u00F1os o m\u00E1s",
      h_joven1      = "N\u00FAmero de integrantes de 15 a 24 a\u00F1os",
      h_joven2      = "N\u00FAmero de integrantes de 15 a 29 a\u00F1os",
      h_adm         = "N\u00FAmero de integrantes de 65 a\u00F1os o m\u00E1s",
      d_00_05       = "Hogar con al menos una persona de 0 a 5 a\u00F1os",
      d_06_12       = "Hogar con al menos una persona de 6 a 12 a\u00F1os",
      d_13_17       = "Hogar con al menos una persona de 13 a 17 a\u00F1os",
      d_18m         = "Hogar con al menos una persona de 18 a\u00F1os o m\u00E1s",
      d_joven1      = "Hogar con al menos una persona de 15 a 24 a\u00F1os",
      d_joven2      = "Hogar con al menos una persona de 15 a 29 a\u00F1os",
      d_adm         = "Hogar con al menos una persona de 65 a\u00F1os o m\u00E1s",
      p_lab         = "Personas ocupadas en el hogar (clase2 == 1)",
      p_lab_ratio   = "Proporci\u00F3n de personas ocupadas respecto al tama\u00F1o del hogar"
    ) %>%
    sjlabelled::val_labels(
      relative = c(
        "Jefa/e" = 1,
        "C\u00F3nyuge o pareja" = 2,
        "Hija/o" = 3,
        "Madre o padre" = 4,
        "Otros parientes" = 5,
        "No parientes u otros" = 6,
        "Servicio dom\u00E9stico y familiares" = 7
      ),
      familyt = c(
        "Unipersonal" = 1,
        "Corresidentes" = 2,
        "Parejas sin hijos" = 3,
        "Parejas con hijos" = 4,
        "Jefa/e con hijos" = 5,
        "Hogares extensos" = 6,
        "Compuestos" = 7
      ),
      tipo_hog = c(
        "Unipersonal" = 1,
        "Corresidentes" = 2,
        "Nuclear" = 3,
        "Hogares extensos" = 6,
        "Compuestos" = 7
      ),
      tipo_hog2 = c(
        "No familiar" = 1,
        "Nuclear" = 3,
        "Extensos" = 6
      ),
      d_00_05 = c("No" = 0, "S\u00ED" = 1),
      d_06_12 = c("No" = 0, "S\u00ED" = 1),
      d_13_17 = c("No" = 0, "S\u00ED" = 1),
      d_18m = c("No" = 0, "S\u00ED" = 1),
      d_joven1 = c("No" = 0, "S\u00ED" = 1),
      d_joven2 = c("No" = 0, "S\u00ED" = 1),
      d_adm = c("No" = 0, "S\u00ED" = 1)
    )

  return(data)
}
