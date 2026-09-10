#' renoe: Herramientas para trabajar con la ENOE desde 2005
#'
#' Este paquete proporciona funciones para descargar, cargar, fusionar y procesar
#' datos de la Encuesta Nacional de Ocupacion y Empleo (ENOE) de Mexico desde 2005.
#'
#' Incluye utilidades para imputacion de ingresos ocupacionales, procesamiento de
#' estructura del hogar, uso del tiempo, codificacion ocupacional y desajuste
#' educativo, asi como funciones para manejar archivos ZIP, metadatos del INEGI y
#' formatos como `.parquet`, `.dta` y `.rds`.
#'
#' @keywords internal
#' @import dplyr
#' @import mice
#' @importFrom magrittr %>% %<>%
#' @importFrom sjlabelled set_label set_labels get_label get_labels var_labels val_labels
#' @importFrom arrow write_parquet read_parquet
#' @importFrom haven write_dta read_dta
#' @importFrom httr GET http_error write_disk
#' @importFrom readr read_csv locale problems
#' @importFrom stats setNames
#' @importFrom stringr str_remove str_detect str_replace str_pad
#' @importFrom tools file_path_sans_ext
#' @importFrom utils download.file read.csv unzip
#' @importFrom dineq ntiles.wtd
"_PACKAGE"
