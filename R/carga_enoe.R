#' Cargar microdatos de la ENOE
#' @encoding UTF-8
#' @param anio Año del trimestre (2005-2024)
#' @param trimestre Número del trimestre (1-4)
#' @param list Lógico. Si TRUE, devuelve lista con data frames
#' @param rapida Lógico. Si TRUE, omite etiquetado de variables
#' @param intentos Número de intentos de descarga (default 3)
#' @param timeout_sec Tiempo máximo de espera (default 300)
#' @param verificar_url Lógico. Si TRUE, verifica formatos de URL alternativos
#' @param cache Lógico. Si TRUE, usa archivos en caché
#' @param prefijo Texto. Prefijo para nombres de archivos ("enoe" o "enoen")
#' @export
carga_enoe <- function(anio, trimestre, list = FALSE, rapida = FALSE,
                       intentos = 3, timeout_sec = 300, verificar_url = TRUE,
                       cache = TRUE, prefijo = NULL) {

  # Validaciones iniciales
  if (!is.numeric(anio)) stop("El año debe ser numérico")
  if (!is.numeric(trimestre)) stop("El trimestre debe ser numérico")
  if (!trimestre %in% 1:4) stop("El trimestre debe ser entre 1 y 4")
  if (anio < 2005 || anio > 2024) stop("Año fuera del rango permitido (2005-2024)")

  # Determinar prefijo automáticamente si no se especificó
  if (is.null(prefijo)) {
    if ((anio == 2020 && trimestre >= 3) || (anio >= 2021 && anio <= 2022)) {
      prefijo <- "enoen"
    } else {
      prefijo <- "enoe"
    }
  }


  # 2. Configuración inicial
  tablas <- c("viv", "hog", "sdem", "coe1", "coe2")
  url_info <- .construir_url_enoe(anio, trimestre)
  unzip_dir <- paste0("zip/enoe_", anio, "_", trimestre, "t")
  dir.create("zip", showWarnings = FALSE, recursive = TRUE)
  dir.create(unzip_dir, showWarnings = FALSE, recursive = TRUE)

  # 3. Verificar caché
  if (cache && .verificar_cache(unzip_dir, tablas, url_info$prefijo, anio, trimestre)) {
    message("Usando datos en caché de ", unzip_dir)

    datos_lista <- lapply(tablas, function(tabla) {
      datos <- .leer_datos_enoe(tabla, unzip_dir, url_info$prefijo, anio, trimestre)

      if (!is.null(datos) && ncol(datos) > 0) {
        datos <- .estandarizar_ids(datos, anio, trimestre)
        if (!rapida) {
          datos <- .procesar_etiquetas_enoe(datos, tabla, anio, trimestre, unzip_dir, url_info$prefijo)
        }
      }

      datos
    }) |> stats::setNames(tablas)

  } else {
    # 4. Descargar y extraer datos
    if (!.descargar_zip_enoe(url_info$url, url_info$zip_file, intentos, timeout_sec)) {
      stop("No se pudo descargar los datos después de ", intentos, " intentos")
    }

    if (!.extraer_zip_enoe(url_info$zip_file, unzip_dir)) {
      stop("No se pudo extraer los archivos descargados")
    }

    # 5. Procesar cada tabla
    datos_lista <- lapply(tablas, function(tabla) {
      datos <- .leer_datos_enoe(tabla, unzip_dir, url_info$prefijo, anio, trimestre)

      if (!is.null(datos) && ncol(datos) > 0) {
        datos <- .estandarizar_ids(datos, anio, trimestre)
        if (!rapida) {
          datos <- .procesar_etiquetas_enoe(datos, tabla, anio, trimestre, unzip_dir, url_info$prefijo)
        }
      }
      datos
    }) |> stats::setNames(tablas)
  }

  # 6. Filtrar tablas nulas
  datos_lista <- datos_lista[!sapply(datos_lista, is.null)]
  if (length(datos_lista) == 0) {
    stop("No se pudo cargar ninguna tabla para el trimestre ", anio, "-T", trimestre)
  }

  # 7. Retornar resultados
  if (list) {
    return(datos_lista)
  } else {
    nombres_objetos <- paste0(names(datos_lista), anio, "_t", trimestre)
    list2env(stats::setNames(datos_lista, nombres_objetos), envir = .GlobalEnv)
    invisible(NULL)
  }
}
