#' Clasificar ocupaciones susceptibles de teletrabajo
#'
#' Identifica ocupaciones cuyo contenido permite potencialmente realizar
#' teletrabajo. No mide si la persona teletrabajo efectivamente. La funcion usa
#' la clasificacion de Gabriela Cervantes para SINCO 2011 entre 2012-III y
#' 2021-II, y su actualizacion a SINCO 2019 desde 2021-III.
#'
#' La actualizacion conserva la equivalencia sustantiva de Nutriologos:
#' SINCO 2011 `2423` pasa a SINCO 2019 `2433`. El codigo `2423` de SINCO 2019
#' corresponde a Ginecologos y obstetras y no se clasifica como susceptible.
#'
#' @param data Data frame con el codigo SINCO, ano y trimestre.
#' @param variable_sinco Nombre de la variable SINCO a cuatro digitos.
#' @param variable_anio Nombre de la variable de ano.
#' @param variable_trim Nombre de la variable de trimestre.
#' @param nombre_salida Nombre del indicador binario creado.
#' @param sobrescribir Si es `TRUE`, permite reemplazar variables existentes.
#'
#' @return El mismo data frame con `nombre_salida` y
#'   `version_sinco_teletrabajo`. El indicador vale 1 para ocupaciones
#'   susceptibles, 0 para codigos observados no susceptibles y `NA` cuando
#'   falta el codigo o el periodo queda fuera de la ventana comparable.
#'
#' @details
#' Periodo comparable: desde 2012-III. El corte entre clasificadores se fija en
#' 2021-III. Para estudiar poblacion ocupada, filtre `clase2 == 1` antes o
#' despues de ejecutar la funcion.
#'
#' @examples
#' datos <- data.frame(
#'   sinco4d = c("2423", "2433", "6111"),
#'   anio = c(2021, 2021, 2022),
#'   trim = c(2, 3, 1)
#' )
#' clasificar_susceptibilidad_teletrabajo(
#'   datos,
#'   variable_sinco = "sinco4d",
#'   variable_anio = "anio",
#'   variable_trim = "trim"
#' )
#'
#' @export
clasificar_susceptibilidad_teletrabajo <- function(
    data,
    variable_sinco = "sinco4d",
    variable_anio = "anio",
    variable_trim = "trim",
    nombre_salida = "susceptible_teletrabajo",
    sobrescribir = FALSE) {

  requeridas <- c(variable_sinco, variable_anio, variable_trim)
  faltantes <- setdiff(requeridas, names(data))
  if (length(faltantes) > 0L) {
    stop("Faltan variables: ", paste(faltantes, collapse = ", "), call. = FALSE)
  }

  nuevas <- c(nombre_salida, "version_sinco_teletrabajo")
  conflictos <- intersect(nuevas, names(data))
  if (length(conflictos) > 0L && !isTRUE(sobrescribir)) {
    stop(
      "Ya existen variables de salida: ", paste(conflictos, collapse = ", "),
      ". Use sobrescribir = TRUE para reemplazarlas.",
      call. = FALSE
    )
  }

  sinco2011 <- c(
    "1111", "1112", "1113", "1121", "1122", "1129", "1131", "1132",
    "1133", "1135", "1211", "1212", "1222", "1223", "1224", "1313",
    "1314", "1315", "1321", "1322", "1323", "1324", "1511", "1512",
    "1522", "1621", "1622", "1623", "1624", "1629", "1999", "2111",
    "2112", "2113", "2121", "2122", "2123", "2131", "2132", "2133",
    "2134", "2135", "2141", "2142", "2151", "2152", "2153", "2161",
    "2162", "2163", "2171", "2211", "2212", "2253", "2263", "2271",
    "2272", "2281", "2423", "2531", "2532", "2541", "2542", "2543",
    "2625", "2711", "2713", "2714", "3112", "3113", "3201", "3212",
    "3213", "3221", "3232", "4213", "4221", "4222", "4223", "4224"
  )

  sinco2019 <- c(
    "1111", "1112", "1113", "1121", "1122", "1129", "1131", "1132",
    "1133", "1135", "1211", "1212", "1222", "1223", "1224", "1313",
    "1314", "1315", "1319", "1321", "1322", "1323", "1324", "1329",
    "1511", "1512", "1522", "1621", "1622", "1623", "1624", "1629",
    "1999", "2111", "2112", "2113", "2121", "2122", "2123", "2131",
    "2132", "2133", "2134", "2135", "2136", "2141", "2142", "2151",
    "2152", "2153", "2161", "2162", "2163", "2171", "2211", "2212",
    "2253", "2263", "2271", "2272", "2281", "2433", "2531", "2532",
    "2541", "2542", "2543", "2625", "2711", "2713", "2714", "3112",
    "3113", "3201", "3212", "3213", "3221", "3232", "4213", "4221",
    "4222", "4223", "4224"
  )

  codigo <- trimws(as.character(data[[variable_sinco]]))
  codigo[codigo %in% c("", "NA", "NaN")] <- NA_character_
  anio <- suppressWarnings(as.integer(as.character(data[[variable_anio]])))
  trim_texto <- trimws(tolower(as.character(data[[variable_trim]])))
  trim_texto <- sub("^t", "", trim_texto)
  trim <- suppressWarnings(as.integer(trim_texto))
  trim[!trim %in% 1:4] <- NA_integer_

  periodo <- anio * 10L + trim
  usa_2011 <- !is.na(periodo) & periodo >= 20123L & periodo <= 20212L
  usa_2019 <- !is.na(periodo) & periodo >= 20213L
  comparable <- usa_2011 | usa_2019

  resultado <- rep(NA_integer_, nrow(data))
  observado <- comparable & !is.na(codigo)
  resultado[observado] <- 0L
  resultado[usa_2011 & !is.na(codigo) & codigo %in% sinco2011] <- 1L
  resultado[usa_2019 & !is.na(codigo) & codigo %in% sinco2019] <- 1L

  version <- rep(NA_character_, nrow(data))
  version[usa_2011] <- "SINCO 2011"
  version[usa_2019] <- "SINCO 2019"

  data[[nombre_salida]] <- resultado
  data[["version_sinco_teletrabajo"]] <- version
  data
}
