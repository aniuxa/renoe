#' renoe: Herramientas para trabajar con la ENOE desde 2005
#'
#' Este paquete proporciona funciones para descargar, cargar, fusionar y procesar datos de la Encuesta Nacional de Ocupación y Empleo (ENOE) de México desde 2005.
#'
#' Incluye utilidades para imputación de ingresos ocupacionales, procesamiento de estructura del hogar, uso del tiempo, codificación ocupacional y desajuste educativo, así como funciones para manejar archivos ZIP, metadatos del INEGI y formatos como `.parquet`, `.dta` y `.rds`.
#'
#' @keywords internal
#' @import dplyr
#' @import ggplot2
#' @import mice
#' @importFrom sjlabelled  as_label set_label set_labels get_label get_labels
#' @importFrom arrow write_parquet read_parquet
#' @importFrom haven write_dta read_dta
#' @importFrom httr GET http_error write_disk
#' @importFrom readr read_csv locale
#' @importFrom stats setNames
#' @importFrom stringr str_remove str_detect str_replace str_pad
#' @importFrom tools file_path_sans_ext
#' @importFrom utils download.file read.csv unzip
"_PACKAGE"
