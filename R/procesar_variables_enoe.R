#' Procesar variables sociodemográficas y de estructura del hogar
#'
#' Esta función aplica en cadena `procesar_vars_sociodemo()` y `procesar_vars_hogar()` para
#' crear variables derivadas clave en el análisis de datos de la ENOE. Incluye transformación
#' de edad, identificación de parentescos, clasificación de hogares y cálculo de tasas de dependencia.
#'
#' @encoding UTF-8
#' @param data Un data frame con las tablas fusionadas de la ENOE (por ejemplo, salida de `fusion_enoe()`).
#'
#' @return Un data frame con variables sociodemográficas generales y de estructura del hogar agregadas.
#' @export
#'
#' @seealso [procesar_vars_sociodemo()], [procesar_vars_hogar()]
#'
#' @examples
#' \dontrun{
#' datos <- fusion_enoe(2022, 1)
#' datos_proc <- procesar_variables_enoe(datos)
#'
#' dplyr::glimpse(datos_proc)
#' table(datos_proc$tipo_hog_lab, useNA = "always")
#' }
procesar_variables_enoe <- function(data) {
  data %>%
    procesar_vars_sociodemo() %>%
    procesar_vars_hogar()
}
