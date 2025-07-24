#' Descargar cuestionarios técnicos de la ENOE
#'
#' Descarga automáticamente los cuestionarios en PDF y los descriptores de archivos (FD) correspondientes
#' a un trimestre específico de la ENOE, basándose en la información de versiones. Los archivos se almacenan
#' en una carpeta única con nombre `docs_{anio}_{trim}` dentro del directorio especificado.
#'
#' @encoding UTF-8
#' @param anio Año del trimestre (2005-2024). Debe ser un valor numérico entre 2005 y 2024.
#' @param trimestre Número del trimestre (1-4). Donde 1 = ENE-MAR, 2 = ABR-JUN,
#'   3 = JUL-SEP, 4 = OCT-DIC.
#' @param destino Directorio base donde se guardarán los archivos. Por defecto, "cuestionarios".
#' @param sobrescribir Lógico. ¿Deben sobrescribirse los archivos existentes?
#'   (FALSE por defecto).
#'
#' @return Vector invisible con las rutas de los archivos descargados.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' descargar_cuestionarios(2020, 1)
#' descargar_cuestionarios(2019, 2, sobrescribir = TRUE)
#' descargar_cuestionarios(2018, 3, destino = "documentacion_enoe")
#' }
#' @family descarga_documenta_enoe
descargar_cuestionarios <- function(anio, trimestre,
                                    destino = "cuestionarios",
                                    sobrescribir = FALSE) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Paquete 'httr' requerido. Instala con: install.packages('httr')")
  }

  meta <- info_trimestre(anio, trimestre)
  if (is.null(meta)) stop("Trimestre no disponible")

  # Directorio único para el trimestre
  carpeta_destino <- file.path(destino, paste0("docs_", anio, "_", trimestre))
  dir.create(carpeta_destino, recursive = TRUE, showWarnings = FALSE)

  # Mapeo de archivos
  map_files <- list(
    coe = list(
      ampliado = c(v1 = "c_amp_v1.pdf", v2 = "c_amp_v2.pdf", v3 = "c_amp_v3.pdf",
                   v4 = "c_amp_v4.pdf", v5 = "c_amp_v5.pdf", v5a = "c_amp_v5a.pdf",
                   v6 = "c_amp_v6.pdf", v6a = "c_amp_v6a.pdf"),
      basico = c(v1 = "c_bas_v1.pdf", v2 = "c_bas_v2.pdf", v3 = "c_bas_v3.pdf",
                 v4 = "c_bas_v4.pdf", v5 = "c_bas_v5.pdf", v6 = "c_bas_v6.pdf",
                 v7 = "c_bas_v7.pdf")
    ),
    sdem = c(v1 = "c_sdem_v1.pdf", v2 = "c_sdem_v2.pdf", v3 = "c_sdem_v3.pdf",
             v4 = "c_sdem_v4.pdf", v5a = "c_sdem_v5a.pdf"),
    fd = c(
      v1 = "fd_c_bas_amp_15ymas.pdf",
      v2 = "enoe_n_fd_c_bas_amp.pdf",
      v3 = "enoe_n_321_fd_c_bas_amp.pdf",
      v4 = "enoe_123_fd_c_bas_amp.pdf"
    )
  )

  # Función interna de descarga
  descargar <- function(url, dest) {
    if (file.exists(dest)) {
      if (sobrescribir) file.remove(dest)
      else {
        message("Archivo ya existe: ", dest)
        return(dest)
      }
    }
    message("Descargando: ", url)
    resp <- httr::GET(url, httr::write_disk(dest, overwrite = TRUE))
    if (httr::http_error(resp)) {
      warning("Error al descargar: ", url)
      return(NULL)
    }
    return(dest)
  }

  resultados <- list()
  base_url <- "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/doc/"

  # COE
  tipo <- tolower(meta$coe_tipo)
  if (tipo %in% names(map_files$coe) && meta$coe_v %in% names(map_files$coe[[tipo]])) {
    archivo <- map_files$coe[[tipo]][[meta$coe_v]]
    resultados$coe <- descargar(paste0(base_url, archivo), file.path(carpeta_destino, archivo))
  }

  # SDEM
  if (meta$sdem_v %in% names(map_files$sdem)) {
    archivo <- map_files$sdem[[meta$sdem_v]]
    resultados$sdem <- descargar(paste0(base_url, archivo), file.path(carpeta_destino, archivo))
  }

  # FD
  if (meta$fd %in% names(map_files$fd)) {
    archivo <- map_files$fd[[meta$fd]]
    resultados$fd <- descargar(paste0(base_url, archivo), file.path(carpeta_destino, archivo))
  }

  invisible(unlist(resultados))
}
