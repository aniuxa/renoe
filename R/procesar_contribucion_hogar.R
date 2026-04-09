#' Procesar contribución económica y de trabajo no remunerado al hogar
#'
#' A partir de variables laborales previamente construidas, genera el ingreso
#' ocupacional individual deflactado, agregados del hogar, indicadores per cápita
#' y quintiles ponderados de ingreso y trabajo no remunerado.
#'
#' @param data Un data frame que contenga, al menos, las variables `ingocup_imp`,
#'   `ipc`, `t_total_hrs0`, `t_total_hrs`, `folio2`, `tam_hog` y `fac`.
#'
#' @return Un data frame con variables derivadas sobre contribución económica y
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
    "ingocup_imp", "ipc", "t_total_hrs0", "t_total_hrs",
    "folio2", "tam_hog", "fac"
  )

  faltantes <- setdiff(vars_requeridas, names(data))

  if (length(faltantes) > 0) {
    stop(
      "Faltan variables requeridas en `data`: ",
      paste(faltantes, collapse = ", ")
    )
  }

  data <- data %>%
    dplyr::mutate(
      ing_ipc = dplyr::if_else(
        !is.na(ipc) & ipc != 0,
        ingocup_imp / ipc,
        NA_real_
      )
    ) %>%
    dplyr::mutate(
      ing_hog    = sum(ing_ipc, na.rm = TRUE),
      norem_hog0 = sum(t_total_hrs0, na.rm = TRUE),
      norem_hog  = sum(t_total_hrs, na.rm = TRUE),
      .by = folio2
    ) %>%
    dplyr::mutate(
      ing_hog_pc      = dplyr::if_else(!is.na(tam_hog) & tam_hog > 0, ing_hog / tam_hog, NA_real_),
      norem_pc        = dplyr::if_else(!is.na(tam_hog) & tam_hog > 0, norem_hog0 / tam_hog, NA_real_),
      ing_hog_pc_sego = dplyr::if_else(!is.na(tam_hog) & tam_hog > 0, (ing_hog - ing_ipc) / tam_hog, NA_real_)
    ) %>%
    dplyr::mutate(
      quintil_ing_ind          = dineq::ntiles.wtd(ing_ipc, n = 5, weights = fac),
      quintil_ing_hog_pc       = dineq::ntiles.wtd(ing_hog_pc, n = 5, weights = fac),
      quintil_norem_pc         = dineq::ntiles.wtd(norem_pc, n = 5, weights = fac),
      quintil_ing_hog_pc_sego  = dineq::ntiles.wtd(ing_hog_pc_sego, n = 5, weights = fac)
    ) %>%
    sjlabelled::var_labels(
      ing_ipc                 = "Ingreso ocupacional individual deflactado",
      ing_hog                 = "Ingreso total del hogar deflactado",
      norem_hog0              = "Horas totales de trabajo no remunerado del hogar",
      norem_hog               = "Horas totales de trabajo no remunerado del hogar",
      ing_hog_pc              = "Ingreso per cápita del hogar deflactado",
      norem_pc                = "Horas no remuneradas per cápita del hogar",
      ing_hog_pc_sego         = "Ingreso per cápita del hogar sin la persona ego",
      quintil_ing_ind         = "Quintil ponderado de ingreso ocupacional individual deflactado",
      quintil_ing_hog_pc      = "Quintil ponderado de ingreso per cápita del hogar",
      quintil_norem_pc        = "Quintil ponderado de trabajo no remunerado per cápita",
      quintil_ing_hog_pc_sego = "Quintil ponderado de ingreso per cápita del hogar sin ego"
    ) %>%
    sjlabelled::val_labels(
      quintil_ing_ind = c(
        "Quintil 1 (más bajo)" = 1,
        "Quintil 2" = 2,
        "Quintil 3" = 3,
        "Quintil 4" = 4,
        "Quintil 5 (más alto)" = 5
      ),
      quintil_ing_hog_pc = c(
        "Quintil 1 (más bajo)" = 1,
        "Quintil 2" = 2,
        "Quintil 3" = 3,
        "Quintil 4" = 4,
        "Quintil 5 (más alto)" = 5
      ),
      quintil_norem_pc = c(
        "Quintil 1 (más bajo)" = 1,
        "Quintil 2" = 2,
        "Quintil 3" = 3,
        "Quintil 4" = 4,
        "Quintil 5 (más alto)" = 5
      ),
      quintil_ing_hog_pc_sego = c(
        "Quintil 1 (más bajo)" = 1,
        "Quintil 2" = 2,
        "Quintil 3" = 3,
        "Quintil 4" = 4,
        "Quintil 5 (más alto)" = 5
      )
    )

  return(data)
}
