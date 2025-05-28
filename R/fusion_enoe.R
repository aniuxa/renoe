#' Fusionar tablas de la ENOE
#'
#' Une las tablas de vivienda, hogar, sociodemográfico y componentes COE en un único data frame
#' @encoding UTF-8
#' @param anio Año del trimestre (2005-2024).
#' @param trimestre Número del trimestre (1-4).
#' @param rapida Lógico. Si TRUE, omite el etiquetado de variables.
#' @param formato Formato de salida ("parquet", "rds" o "dta"). Opcional.
#' @param guardar Lógico. Si TRUE y se especifica formato, guarda el archivo fusionado.
#' @param intentos Número de intentos para cargar datos (por defecto 3).
#' @param ... Otros parámetros para pasar a carga_enoe()
#' @return Un data frame con las tablas fusionadas. Si se especifica formato y guardar = TRUE,
#'   guarda el archivo en el subdirectorio "datos" con el nombre:
#'   "enoe_fusion_\[AÑO\]_\[TRIMESTRE\]t.\[FORMATO\]"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Fusionar tablas para el T3 de 2020
#' datos2020 <- fusion_enoe(2020, 3)
#'
#' # Fusionar y guardar como Parquet
#' fusion_enoe(2019, 2, formato = "parquet", guardar = TRUE)
#' }
fusion_enoe <- function(anio, trimestre, rapida = FALSE, formato = NULL,
                        guardar = !is.null(formato), intentos = 3, ...) {

  tablas <- c("viv", "hog", "sdem", "coe1", "coe2")
  url_info <- .construir_url_enoe(anio, trimestre)
  unzip_dir <- paste0("zip/enoe_", anio, "_", trimestre, "t")
  prefijo <- url_info$prefijo

  if (!dir.exists(unzip_dir)) {
    message("Archivos no encontrados localmente. Usando carga_enoe() para descargar y procesar.")
    datos <- carga_enoe(anio = anio, trimestre = trimestre, list = TRUE,
                        rapida = rapida, intentos = intentos, ...)
  } else {
    message("Cargando archivos desde ", unzip_dir)
    datos <- lapply(tablas, function(tabla) {
      df <- .leer_datos_enoe(tabla, unzip_dir, prefijo, anio, trimestre)
      if (!is.null(df)) {
        df <- .estandarizar_ids(df, anio, trimestre)
        if (!rapida) {
          df <- .procesar_etiquetas_enoe(df, tabla, anio, trimestre, unzip_dir, prefijo)
        }
      }
      df
    }) |> stats::setNames(tablas)
  }

  if (!all(tablas %in% names(datos))) {
    stop("Faltan tablas requeridas: ", paste(setdiff(tablas, names(datos)), collapse = ", "))
  }

  message("\nFusionando tablas para ", anio, " trimestre ", trimestre, "...")
  columnas_comunes <- intersect(names(datos$coe1), names(datos$coe2))
  coe_fusionado <- dplyr::left_join(datos$coe1, datos$coe2, by = columnas_comunes) %>%
    dplyr::rename_at(dplyr::vars(dplyr::one_of("p1", "p3", "p4_1", "p4_2")), ~ paste0(.x, "coe"))

  enoe_fusionado <- datos$viv %>%
    dplyr::left_join(datos$hog, by = intersect(names(datos$viv), names(datos$hog))) %>%
    dplyr::left_join(datos$sdem, by = intersect(names(datos$hog), names(datos$sdem))) %>%
    dplyr::filter(r_def == 0, c_res != 2) %>%
    dplyr::left_join(coe_fusionado, by = intersect(names(datos$sdem), names(coe_fusionado)))

  if (!is.null(formato)) {
    ruta_salida <- file.path("datos", paste0("enoe_fusion_", anio, "_", trimestre, "t.", formato))
    dir.create(dirname(ruta_salida), recursive = TRUE, showWarnings = FALSE)
    message("Guardando como: ", ruta_salida)
    switch(formato,
           "parquet" = arrow::write_parquet(enoe_fusionado, ruta_salida),
           "rds" = saveRDS(enoe_fusionado, ruta_salida),
           "dta" = haven::write_dta(enoe_fusionado, ruta_salida),
           stop("Formato no soportado: ", formato))
  }

  return(enoe_fusionado)
}
