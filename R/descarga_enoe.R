#' Descargar archivos de microdatos de la ENOE
#'
#' Descarga los archivos comprimidos de microdatos de la ENOE desde el sitio del INEGI,
#' los descomprime y guarda las tablas en el formato especificado.
#' @encoding UTF-8
#' @param anio Ano del trimestre (2005-2026). Debe ser un valor numerico.
#' @param trimestre Numero del trimestre (1-4). Donde:
#'   \itemize{
#'     \item 1 = Enero-Marzo
#'     \item 2 = Abril-Junio
#'     \item 3 = Julio-Septiembre
#'     \item 4 = Octubre-Diciembre
#'   }
#' @param formato Formato de salida para los archivos. Puede ser:
#'   \itemize{
#'     \item "parquet" (recomendado para eficiencia)
#'     \item "rds" (formato nativo de R)
#'     \item "dta" (compatible con Stata)
#'   }
#' @param intentos Numero maximo de intentos de descarga si falla la conexion (por defecto 3).
#' @param timeout_sec Tiempo maximo de espera para la descarga en segundos (por defecto 300).
#' @param verificar_url Logico. Si TRUE (por defecto), verifica multiples formatos de URL
#'   para encontrar la correcta. Util cuando INEGI cambia la estructura de archivos.
#' @param cache Logico. Si TRUE (por defecto), usa archivos en cache si existen.
#'
#' @export
#' @examples
#' \dontrun{
#' descarga_enoe(2023, 1)
#' descarga_enoe(2022, 4, formato = "dta", intentos = 5)
#' }
#' @family descarga_documenta_enoe

descarga_enoe <- function(anio, trimestre, formato = "parquet", intentos = 3,
                          timeout_sec = 300, verificar_url = TRUE, cache = TRUE) {
  # 1. Validaciones iniciales
  if (!is.numeric(anio)) stop("El a\u00F1o debe ser num\u00E9rico")
  if (!is.numeric(trimestre)) stop("El trimestre debe ser num\u00E9rico")
  if (!trimestre %in% 1:4) stop("El trimestre debe ser un valor entre 1 y 4")
  if (anio < 2005 || anio > 2026) stop("La ENOE cubre de 2005 en adelante y hasta ahora hasta 2026")
  if (anio == 2020 && trimestre == 2) stop("No existe el trimestre 2 de 2020 en la ENOE debido a la pandemia de COVID-19")

  # 2. Configuracion inicial
  tablas <- c("viv", "hog", "sdem", "coe1", "coe2")
  url_info <- .construir_url_enoe(anio, trimestre)
  unzip_dir <- paste0("zip/enoe_", anio, "_", trimestre, "t")
  dir.create("datos", showWarnings = FALSE, recursive = TRUE)
  dir.create("zip", showWarnings = FALSE, recursive = TRUE)

  # 3. Verificar si ya existen los archivos en el formato solicitado
  archivos_salida <- file.path("datos", paste0(tablas, anio, "_", trimestre, "t.", formato))
  if (cache && all(file.exists(archivos_salida))) {
    message("Los archivos ya existen en el directorio 'datos'. Usando cach\u00E9.")
    return(invisible(TRUE))
  }

  # 4. Descargar y extraer datos
  if (!.descargar_zip_enoe(url_info$url, url_info$zip_file, intentos, timeout_sec)) {
    stop("No se pudo descargar el archivo ZIP despu\u00E9s de ", intentos, " intentos")
  }

  if (!.extraer_zip_enoe(url_info$zip_file, unzip_dir)) {
    stop("No se pudo extraer los archivos del ZIP descargado")
  }

  # 5. Procesar y guardar cada tabla
  resultados <- sapply(tablas, function(tabla) {
    tryCatch({
      datos <- .leer_datos_enoe(tabla, unzip_dir, url_info$prefijo, anio, trimestre)
      if (is.null(datos)) return(FALSE)
      datos <- .estandarizar_ids(datos, anio, trimestre)
      archivo_salida <- file.path("datos", paste0(tabla, anio, "_", trimestre, "t.", formato))

      switch(formato,
             "parquet" = arrow::write_parquet(datos, archivo_salida, compression = "snappy"),
             "rds" = saveRDS(datos, archivo_salida),
             "dta" = haven::write_dta(datos, archivo_salida))

      TRUE
    }, error = function(e) {
      warning("Error procesando tabla ", tabla, ": ", e$message)
      FALSE
    })
  })

  # 6. Limpieza final
  if (file.exists(url_info$zip_file)) unlink(url_info$zip_file)

  # 7. Reportar resultados
  if (!all(resultados)) {
    tablas_fallidas <- tablas[!resultados]
    warning("No se pudieron procesar las siguientes tablas: ",
            paste(tablas_fallidas, collapse = ", "))
  }

  invisible(all(resultados))
}
