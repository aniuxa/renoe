#' Crear identificadores unicos para vivienda, hogar y persona
#'
#' Esta funcion genera los identificadores `folio`, `folio2` y `folio3`
#' basados en las variables clave del diseno de la ENOE.
#' @encoding UTF-8
#' @param data Un data.frame o tibble con las variables base (`cd_a`, `ent`, `con`, `v_sel`).
#' @return Un data.frame con las columnas `folio`, `folio2` y opcionalmente `folio3`.
#' @export
#' @family procesamiento_enoe
crear_folios <- function(data) {
  vars_base <- c("cd_a", "ent", "con", "v_sel")
  vars_opt <- c("ca", "tipo", "mes_cal")
  vars_hogar <- c("n_hog", "h_mud")
  var_persona <- "n_ren"

  faltantes <- setdiff(vars_base, names(data))
  if (length(faltantes)) {
    stop(
      "Faltan variables base para construir los folios: ",
      paste(faltantes, collapse = ", "),
      call. = FALSE
    )
  }

  vars_opt_incluidas <- vars_opt[vars_opt %in% names(data)]
  vars_hogar_incluidas <- vars_hogar[vars_hogar %in% names(data)]

  pegar_claves <- function(variables) {
    if (!nrow(data)) return(character())
    valores <- lapply(data[variables], as.character)
    do.call(paste, c(valores, sep = "_"))
  }

  data <- data %>%
    mutate(
      folio = sjlabelled::set_label(
        pegar_claves(vars_base),
        "Identificador de vivienda"
      ),
      folio2 = sjlabelled::set_label(
        pegar_claves(c(vars_base, vars_opt_incluidas, vars_hogar_incluidas)),
        "Identificador de hogar"
      )
    )

  if (var_persona %in% names(data)) {
    data <- data %>%
      mutate(
        folio3 = sjlabelled::set_label(
          paste(folio2, .data[[var_persona]], sep = "_"),
          "Identificador de individuo"
        )
      )
  } else {
    warning("No se encontr\u00F3 la variable 'n_ren'. No se gener\u00F3 'folio3'.")
  }
  return(data)
}
