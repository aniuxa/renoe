#' Obtener metadatos de versiones de cuestionarios ENOE por trimestre
#'
#' Consulta la informacipib sobre qué versiones de cuestionarios (COE, SDEM, FD)
#' corresponden a un trimestre específico de la ENOE.
#' @encoding UTF-8
#' @param anio Año del trimestre (2005-2024). Debe ser un valor numérico entre 2005 y 2024.
#' @param trimestre Núemero del trimestre (1-4). Donde 1 = ENE-MAR, 2 = ABR-JUN,
#'   3 = JUL-SEP, 4 = OCT-DIC.
#'
#' @return Un data.frame con 6 columnas:
#' \describe{
#'   \item{trimestre}{Código del trimestre (ej. "t105")}
#'   \item{coe_tipo}{Tipo de cuestionario COE ("ampliado" o "basico")}
#'   \item{coe_v}{Versión del cuestionario COE (ej. "v1", "v2")}
#'   \item{sdem_v}{Versión del cuestionario SDEM}
#'   \item{fd}{Versión del file descriptor}
#'   \item{encoding}{Encoding recomendado para los archivos}
#' }
#' Retorna NULL si no se encuentra información para el trimestre especificado.
#'
#' @export
#'
#' @examples
#' # Consultar información para el primer trimestre de 2020
#' info_trimestre(2020, 1)
#'
#' # Consultar información para el tercer trimestre de 2015
#' info_trimestre(2015, 3)

info_trimestre <- function(anio, trimestre) {
  # Validaciones
  if (!trimestre %in% 1:4) stop("Trimestre debe ser 1-4")
  if (!anio %in% 2005:2024) stop("Au00f1o debe estar entre 2005-2024")
  if (anio == 2020 && trimestre == 2) stop("No existe 2020-T2 por COVID")

  # Generar cou00f3digo de trimestre
  trim_code <- sprintf("t%d%02d", trimestre, anio %% 100)

  # Obtener ruta al archivo interno
  csv_path <- system.file("extdata", "trimestre_info.csv", package = "renoe")

  # Verificar que el archivo existe
  if (!file.exists(csv_path)) {
    stop("Archivo trimestre_info.csv no encontrado en el paquete. ",
         "Debe estar en inst/extdata/")
  }

  # Leer el archivo con encoding expliu00edcito
  cuestionarios_data <- utils::read.csv(
    file = csv_path,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )

  # Filtrar por trimestre
  result <- cuestionarios_data[cuestionarios_data$trimestre == trim_code, ]

  if (nrow(result) == 0) {
    warning("No se encontró información para ", trim_code)
    return(NULL)
  }

  return(result)
}
