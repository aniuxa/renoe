#' Procesar variables sociodemográficas básicas en la ENOE
#'
#' Crea variables derivadas a partir de la edad, sexo y tipo de cuestionario. Esta función es útil
#' como primer paso en el procesamiento de los datos fusionados de la ENOE.
#'
#' @encoding UTF-8
#' @param data Un data frame con las variables `sex`, `eda`, `anio` y `t`, típicamente generado por `fusion_enoe()`.
#'
#' @return El mismo data frame con variables nuevas:
#' \describe{
#'   \item{sexo}{Recodificación de `sex`}
#'   \item{edad}{Edad en años, valores 98 y 99 recodificados como NA}
#'   \item{edad5}{Edad en grupos quinquenales}
#'   \item{old}{Indicador de persona mayor (65 o más años)}
#'   \item{coe_tipo}{Tipo de cuestionario: ampliado o básico}
#'   \item{anio, trim}{Año y trimestre en formato útil}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' datos <- fusion_enoe(2022, 4)
#' datos <- procesar_vars_sociodemo(datos)
#' table(datos$edad5, useNA = "always")
#' }
procesar_vars_sociodemo <- function(data) {
  data %>%
    dplyr::mutate(
      sexo = .data$sex,
      edad = as.numeric(.data$eda),
      edad = dplyr::case_when(edad %in% c(98, 99) ~ NA_real_, TRUE ~ edad),
      edad5 = cut(edad, breaks = seq(0, 100, 5), right = FALSE),
      old = dplyr::if_else(edad >= 65, 1, 0, missing = 0),
      anio = .data$anio,
      trim = paste0("t", .data$t),
      coe_tipo = .data$coe_tipo
    )
}
