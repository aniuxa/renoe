#' Cargar microdatos de la ENOE
#'
#' Descarga, extrae y carga las tablas de microdatos de la Encuesta Nacional de Ocupación y Empleo (ENOE)
#' para un trimestre específico. Incluye la corrección automática del archivo defectuoso de hogares
#' del primer trimestre de 2022. Las tablas disponibles son: vivienda (viv), hogar (hog), sociodemográfica (sdem)
#' y los dos componentes del cuestionario ampliado (coe1 y coe2).
#'
#' @encoding UTF-8
#' @param anio Año del trimestre (2005–2024)
#' @param trimestre Número del trimestre (1–4), donde:
#'   \itemize{
#'     \item 1 = Enero–Marzo
#'     \item 2 = Abril–Junio
#'     \item 3 = Julio–Septiembre
#'     \item 4 = Octubre–Diciembre
#'   }
#' @param list Lógico. Si TRUE, devuelve una lista con los data frames. Si FALSE (por defecto), los objetos se cargan al entorno global.
#' @param rapida Lógico. Si TRUE, omite el etiquetado de variables (más rápido).
#' @param intentos Número máximo de intentos de descarga (por defecto = 3).
#' @param timeout_sec Tiempo máximo de espera por intento en segundos (por defecto = 300).
#' @param verificar_url Lógico. Si TRUE (por defecto), intenta formatos alternativos de URL si falla la descarga.
#' @param cache Lógico. Si TRUE (por defecto), reutiliza datos descargados previamente si existen.
#' @param prefijo Cadena opcional para el prefijo de los archivos ("enoe" o "enoen"). Si no se indica, se detecta automáticamente.
#'
#' @return Si \code{list = TRUE}, devuelve una lista con las cinco tablas. Si \code{list = FALSE}, las tablas se cargan
#' al entorno global con nombres como \code{viv2023_t1}, \code{hog2023_t1}, etc.
#'
#' @details
#' Esta función combina varias operaciones comunes al trabajar con microdatos de la ENOE:
#' descarga, extracción del ZIP, lectura, estandarización de identificadores y aplicación de etiquetas.
#' La función maneja de forma especial el primer trimestre de 2022, sustituyendo automáticamente el archivo
#' por la versión que se descarga en microdatos de INEGI y no en datos abiertos..
#'
#' @seealso \code{\link{fusion_enoe}}, \code{\link{descarga_enoe}}, \code{\link{procesar_vars_sociodemo}}
#'
#' @examples
#' \dontrun{
#' # Cargar datos al entorno global para el segundo trimestre de 2023
#' carga_enoe(2023, 2)
#'
#' # Cargar datos como lista sin etiquetas
#' datos <- carga_enoe(2022, 4, list = TRUE, rapida = TRUE)
#'
#' # Cargar el trimestre corregido 2022T1 desde caché o desde extdata si es necesario
#' carga_enoe(2022, 1)
#' }
#'
#' @export
#' @family descarga_documenta_enoe

carga_enoe <- function(anio, trimestre, list = FALSE, rapida = FALSE,
                       intentos = 3, timeout_sec = 300, verificar_url = TRUE,
                       cache = TRUE, prefijo = NULL) {

  # 1. Validaciones
  if (!is.numeric(anio) || anio < 2005 || anio > 2025)
    stop("El año debe estar entre 2005 y 2025.")
  if (!is.numeric(trimestre) || !trimestre %in% 1:4)
    stop("El trimestre debe ser 1, 2, 3 o 4.")

  # 2. Determinar prefijo si no se indicó
  if (is.null(prefijo)) {
    prefijo <- if ((anio == 2020 && trimestre >= 3) || (anio %in% 2021:2022)) "enoen" else "enoe"
  }

  # 3. Configurar carpetas y tablas
  tablas <- c("viv", "hog", "sdem", "coe1", "coe2")
  url_info <- .construir_url_enoe(anio, trimestre)
  unzip_dir <- paste0("zip/enoe_", anio, "_", trimestre, "t")
  dir.create("zip", recursive = TRUE, showWarnings = FALSE)
  dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)

  usar_cache <- cache && .verificar_cache(unzip_dir, tablas, url_info$prefijo, anio, trimestre)

  if (usar_cache) {
    message("Usando datos en caché de ", unzip_dir)
  } else {
    if (!.descargar_zip_enoe(url_info$url, url_info$zip_file, intentos, timeout_sec)) {
      stop("No se pudo descargar el archivo ZIP después de ", intentos, " intentos.")
    }
    if (!.extraer_zip_enoe(url_info$zip_file, unzip_dir)) {
      stop("No se pudo extraer el ZIP descargado.")
    }
  }

  # Sustitución especial completa para todas las tablas 2022 T1
  if (anio == 2022 && trimestre == 1) {
    .sustituir_todo_enoe_2022t1(unzip_dir)
  }


  # 5. Cargar y procesar las tablas
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

  # 6. Eliminar tablas no válidas
  datos_lista <- datos_lista[!sapply(datos_lista, is.null)]
  if (length(datos_lista) == 0) {
    stop("No se pudo cargar ninguna tabla para el trimestre ", anio, "-T", trimestre)
  }

  # 7. Retornar
  if (list) {
    return(datos_lista)
  } else {
    nombres_objetos <- paste0(names(datos_lista), anio, "_t", trimestre)
    list2env(stats::setNames(datos_lista, nombres_objetos), envir = .GlobalEnv)
    invisible(NULL)
  }
}
