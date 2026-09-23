#' Procesar contribucion economica y de trabajo no remunerado al hogar
#'
#' A partir de variables laborales previamente construidas, genera el ingreso
#' ocupacional individual deflactado, agregados del hogar, indicadores per capita
#' y quintiles ponderados de ingreso y trabajo no remunerado.
#'
#' @param data Un data frame que contenga, al menos, las variables `ingocup_imp`,
#'   `ipc`, `hrsocup`, `t_trabajo_hogar_armonizado`, `t_total_instrumento`, `folio2`,
#'   `tam_hog` y
#'   `fac`.
#'
#' @return Un data frame con variables derivadas sobre contribucion economica y
#'   trabajo no remunerado en el hogar, junto con quintiles ponderados etiquetados.
#' @export
#'
#' @examples
#' \dontrun{
#' datos <- datos |>
#'   renoe::procesar_vars_laborales() |>
#'   renoe::procesar_contribucion_hogar()
#' }
#' @family procesamiento_enoe

procesar_contribucion_hogar <- function(data) {

  vars_requeridas <- c(
    "ingocup_imp", "ipc", "hrsocup", "t_trabajo_hogar_armonizado", "t_total_instrumento",
    "folio2", "tam_hog", "fac"
  )

  faltantes <- setdiff(vars_requeridas, names(data))

  if (length(faltantes) > 0) {
    stop(
      "Faltan variables requeridas en `data`: ",
      paste(faltantes, collapse = ", ")
    )
  }

  # En bases apiladas, folio2 solo no identifica de manera unica al hogar.
  # Conservamos compatibilidad con bases de un trimestre sin anio/trim.
  claves_periodo <- intersect(c("anio", "trim"), names(data))
  claves_hogar <- c(claves_periodo, "folio2")

  factores_hogar <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(claves_hogar))) %>%
    dplyr::summarise(
      n_fac = dplyr::n_distinct(fac, na.rm = FALSE),
      .groups = "drop"
    )
  if (any(factores_hogar$n_fac != 1L)) {
    stop("`fac` debe ser constante dentro de cada hogar.", call. = FALSE)
  }

  sumar_observados <- function(x) {
    if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  }

  data <- data %>%
    dplyr::mutate(
      ing_ipc = dplyr::if_else(
        !is.na(ipc) & ipc > 0 & !is.na(hrsocup) & hrsocup > 0,
        (ingocup_imp / ipc * 100) / (hrsocup * 4.33),
        NA_real_
      ),
      ing_mensual_ipc = dplyr::if_else(
        !is.na(ipc) & ipc != 0,
        ingocup_imp / ipc * 100,
        NA_real_
      )
    )

  hogares <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(claves_hogar))) %>%
    dplyr::summarise(
      fac_hog = dplyr::first(fac),
      tam_hog_hogar = dplyr::first(tam_hog),
      ing_hog = sumar_observados(ing_mensual_ipc),
      norem_hog_armonizado = sumar_observados(t_trabajo_hogar_armonizado),
      norem_hog_instrumento = sumar_observados(t_total_instrumento),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      ing_hog_pc = dplyr::if_else(
        !is.na(tam_hog_hogar) & tam_hog_hogar > 0,
        ing_hog / tam_hog_hogar, NA_real_
      ),
      norem_pc_armonizado = dplyr::if_else(
        !is.na(tam_hog_hogar) & tam_hog_hogar > 0,
        norem_hog_armonizado / tam_hog_hogar, NA_real_
      )
    )

  # Las distribuciones del hogar usan una sola fila y un solo factor por hogar.
  if (length(claves_periodo) > 0) {
    hogares <- hogares %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(claves_periodo)))
  }
  hogares <- hogares %>%
    dplyr::mutate(
      quintil_ing_hog_pc = dineq::ntiles.wtd(
        ing_hog_pc, n = 5, weights = fac_hog
      ),
      quintil_norem_pc_armonizado = dineq::ntiles.wtd(
        norem_pc_armonizado, n = 5, weights = fac_hog
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-fac_hog, -tam_hog_hogar)

  data <- data %>%
    dplyr::left_join(hogares, by = claves_hogar) %>%
    dplyr::mutate(
      ing_hog_pc_sego = dplyr::if_else(
        !is.na(tam_hog) & tam_hog > 0 & !is.na(ing_hog),
        (ing_hog - ing_mensual_ipc) / tam_hog, NA_real_
      )
    )
  if (length(claves_periodo) > 0) {
    data <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(claves_periodo)))
  }
  data <- data %>%
    dplyr::mutate(
      quintil_ing_ind = dineq::ntiles.wtd(
        ing_mensual_ipc, n = 5, weights = fac
      ),
      quintil_ing_hog_pc_sego = dineq::ntiles.wtd(
        ing_hog_pc_sego, n = 5, weights = fac
      )
    ) %>%
    dplyr::ungroup() %>%
    sjlabelled::var_labels(
      ing_ipc                 = "Ingreso ocupacional por hora a precios constantes",
      ing_mensual_ipc         = "Ingreso ocupacional mensual imputado a precios constantes (IPC base 100)",
      ing_hog                 = "Ingreso total del hogar deflactado",
      norem_hog_armonizado    = "Horas armonizadas de trabajo no remunerado del hogar; ruptura de medicion en 2013",
      norem_hog_instrumento   = "Horas captadas de trabajo no remunerado del hogar",
      ing_hog_pc              = "Ingreso per c\u00E1pita del hogar deflactado",
      norem_pc_armonizado     = "Horas armonizadas no remuneradas per c\u00E1pita del hogar; ruptura de medicion en 2013",
      ing_hog_pc_sego         = "Ingreso per c\u00E1pita del hogar sin la persona ego",
      quintil_ing_ind         = "Quintil ponderado de ingreso ocupacional individual deflactado",
      quintil_ing_hog_pc      = "Quintil ponderado de ingreso per c\u00E1pita del hogar",
      quintil_norem_pc_armonizado = "Quintil ponderado de trabajo no remunerado armonizado per c\u00E1pita",
      quintil_ing_hog_pc_sego = "Quintil individual ponderado de ingreso per c\u00E1pita del hogar sin ego"
    ) %>%
    sjlabelled::val_labels(
      quintil_ing_ind = c(
        "Quintil 1 (m\u00E1s bajo)" = 1,
        "Quintil 2" = 2,
        "Quintil 3" = 3,
        "Quintil 4" = 4,
        "Quintil 5 (m\u00E1s alto)" = 5
      ),
      quintil_ing_hog_pc = c(
        "Quintil 1 (m\u00E1s bajo)" = 1,
        "Quintil 2" = 2,
        "Quintil 3" = 3,
        "Quintil 4" = 4,
        "Quintil 5 (m\u00E1s alto)" = 5
      ),
      quintil_norem_pc_armonizado = c(
        "Quintil 1 (m\u00E1s bajo)" = 1,
        "Quintil 2" = 2,
        "Quintil 3" = 3,
        "Quintil 4" = 4,
        "Quintil 5 (m\u00E1s alto)" = 5
      ),
      quintil_ing_hog_pc_sego = c(
        "Quintil 1 (m\u00E1s bajo)" = 1,
        "Quintil 2" = 2,
        "Quintil 3" = 3,
        "Quintil 4" = 4,
        "Quintil 5 (m\u00E1s alto)" = 5
      )
    )

  return(data)
}
