#' Descargar archivos de microdatos de la ENOE
#'
#' Descarga los archivos comprimidos de microdatos de la ENOE desde el sitio del INEGI,
#' los descomprime y guarda las tablas en el formato especificado.
#' @encoding UTF-8
#' @param anio Año del trimestre (2005-2026). Debe ser un valor numerico.
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
#' @param verificar_url Logico. Si TRUE, permite la URL alternativa conocida de 2018-T2 a partir del segundo intento.
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
  .validar_periodo_enoe(anio, trimestre)
  formato <- match.arg(formato, c('parquet', 'rds', 'dta'))
  .validar_opciones_descarga(intentos, timeout_sec, verificar_url, cache)

  # 2. Configuracion inicial
  tablas <- c("viv", "hog", "sdem", "coe1", "coe2")
  url_info <- .construir_url_enoe(anio, trimestre)
  unzip_dir <- paste0("zip/enoe_", anio, "_", trimestre, "t")
  dir.create("datos", showWarnings = FALSE, recursive = TRUE)
  dir.create("zip", showWarnings = FALSE, recursive = TRUE)

  # 3. Verificar si ya existen los archivos en el formato solicitado
  archivos_salida <- file.path("datos", paste0(tablas, anio, "_", trimestre, "t.", formato))
  if (cache && .cache_archivos_enoe_valida(archivos_salida, formato)) {
    message("Los archivos ya existen en el directorio 'datos'. Usando cach\u00E9.")
    return(invisible(TRUE))
  }

  # 4. Descargar y extraer datos
  if (!.descargar_zip_enoe(url_info$url, url_info$zip_file, intentos, timeout_sec, verificar_url)) {
    stop("No se pudo descargar el archivo ZIP despu\u00E9s de ", intentos, " intentos")
  }

  .preparar_extraccion_enoe(url_info$zip_file, unzip_dir, tablas, url_info$prefijo, anio, trimestre)

  # Preparar las cinco tablas en temporales antes de reemplazar la cache.
  temporales <- vapply(archivos_salida, function(x) tempfile(tmpdir = dirname(x), fileext = paste0('.', formato)), character(1))
  on.exit(unlink(temporales), add = TRUE)
  resultados <- vapply(seq_along(tablas), function(i) {
    tryCatch({
      datos <- .leer_datos_enoe(tablas[i], unzip_dir, url_info$prefijo, anio, trimestre)
      if (is.null(datos) || nrow(datos) == 0L || ncol(datos) == 0L) return(FALSE)
      datos <- .estandarizar_ids(datos, anio, trimestre)
      switch(formato,
        parquet = arrow::write_parquet(datos, temporales[i], compression = 'snappy'),
        rds = saveRDS(datos, temporales[i]),
        dta = haven::write_dta(datos, temporales[i])
      )
      file.exists(temporales[i]) && file.size(temporales[i]) > 0
    }, error = function(e) {
      warning('Error procesando tabla ', tablas[i], ': ', e$message, call. = FALSE)
      FALSE
    })
  }, logical(1))
  if (all(resultados)) {
    resultados <- vapply(seq_along(tablas), function(i) {
      isTRUE(file.copy(temporales[i], archivos_salida[i], overwrite = TRUE))
    }, logical(1))
  }

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
