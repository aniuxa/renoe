#' Obtener metadatos de versiones de cuestionarios ENOE por trimestre
#'
#' Consulta la informacion sobre que versiones de cuestionarios (COE, SDEM, FD)
#' corresponden a un trimestre especifico de la ENOE.
#' @encoding UTF-8
#' @param anio Año del trimestre (2005-2026). Debe ser un valor numerico entre 2005 y 2026.
#' @param trimestre Numero del trimestre (1-4). Donde 1 = ENE-MAR, 2 = ABR-JUN,
#'   3 = JUL-SEP, 4 = OCT-DIC.
#'
#' @return Un data.frame con 6 columnas:
#' \describe{
#'   \item{trimestre}{Codigo del trimestre (ej. "t105")}
#'   \item{coe_tipo}{Tipo de cuestionario COE ("ampliado" o "basico")}
#'   \item{coe_v}{Version del cuestionario COE (ej. "v1", "v2")}
#'   \item{sdem_v}{Version del cuestionario SDEM}
#'   \item{fd}{Version del file descriptor}
#'   \item{encoding}{Encoding recomendado para los archivos}
#' }
#' Retorna NULL si no se encuentra informacion para el trimestre especificado.
#'
#' @export
#'
#' @examples
#' # Consultar informacion para el primer trimestre de 2020
#' info_trimestre(2020, 1)
#'
#' # Consultar informacion para el tercer trimestre de 2015
#' info_trimestre(2015, 3)
#' @family descarga_documenta_enoe

info_trimestre <- function(anio, trimestre) {
  if (!.validar_periodo_enoe(anio, trimestre, exigir_disponible = FALSE)) {
    warning(sprintf("No se encontro informacion para t%d%02d: periodo no habilitado", trimestre, anio %% 100), call. = FALSE)
    return(NULL)
  }

  # Generar codigo de trimestre
  trim_code <- sprintf("t%d%02d", trimestre, anio %% 100)

  # Obtener ruta al archivo interno
  csv_path <- system.file("extdata", "trimestre_info.csv", package = "renoe")

  # Verificar que el archivo existe
  if (!file.exists(csv_path)) {
    stop("Archivo trimestre_info.csv no encontrado en el paquete. ",
         "Debe estar en inst/extdata/")
  }

  # Leer el archivo con encoding
  cuestionarios_data <- utils::read.csv(
    file = csv_path,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )

  # Filtrar por trimestre
  result <- cuestionarios_data[cuestionarios_data$trimestre == trim_code, ]

  if (nrow(result) == 0) {
    warning("No se encontr\u00F3 informaci\u00F3n para ", trim_code)
    return(NULL)
  }

  return(result)
}
