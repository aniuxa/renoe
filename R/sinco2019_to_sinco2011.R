#' Convertir SINCO 2019 a SINCO 2011
#'
#' Aplica la tabla de equivalencia oficial incluida en el anexo del
#' [SINCO 2019](https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=702825198411).
#' La tabla conserva todas las relaciones uno-a-varios. Por defecto, la
#' funcion no elige arbitrariamente un destino cuando el codigo SINCO 2019
#' tiene mas de una equivalencia en SINCO 2011.
#'
#' @param data Data frame que contiene el codigo SINCO 2019.
#' @param variable_sinco Nombre de la variable con el codigo SINCO 2019.
#' @param correspondencia Tabla opcional en formato largo con las columnas
#'   `sinco2019` y `sinco2011`. Si se omite, se utiliza la tabla oficial
#'   distribuida con el paquete.
#' @param resolver_multiples Tratamiento de correspondencias uno-a-varios:
#'   `"na"` (recomendado) las conserva como ambiguas y deja `sinco2011` en
#'   `NA`; `"primero"` selecciona el primer destino unicamente para reproducir
#'   un analisis que documente expresamente esa decision.
#' @param sobrescribir Si es `FALSE`, detiene la ejecucion cuando alguna
#'   variable de salida ya existe.
#'
#' @return El mismo data frame con `sinco2019_original`, `sinco2011`,
#'   `sinco2011_n_destinos` y `sinco2011_calidad`.
#' @export
#' @encoding UTF-8
#' @family procesamiento_enoe
#'
#' @references
#' INEGI (2020). *Sistema Nacional de Clasificacion de Ocupaciones 2019*.
#' Anexo: Tabla de equivalencia SINCO 2011-2019.
#'
#' @examples
#' datos <- data.frame(ocupacion = c(2433, 2423, 2429, NA))
#' sinco2019_to_sinco2011(datos, variable_sinco = "ocupacion")
sinco2019_to_sinco2011 <- function(
    data,
    variable_sinco = "sinco4d",
    correspondencia = NULL,
    resolver_multiples = c("na", "primero"),
    sobrescribir = TRUE) {

  resolver_multiples <- match.arg(resolver_multiples)
  if (!variable_sinco %in% names(data)) {
    stop("No existe la variable SINCO 2019 `", variable_sinco, "`.", call. = FALSE)
  }

  salidas <- c(
    "sinco2019_original", "sinco2011", "sinco2011_n_destinos",
    "sinco2011_calidad"
  )
  existentes <- intersect(salidas, names(data))
  if (length(existentes) > 0L && !sobrescribir) {
    stop(
      "Ya existen variables de salida: ", paste(existentes, collapse = ", "),
      ".", call. = FALSE
    )
  }

  if (is.null(correspondencia)) {
    ruta <- system.file(
      "extdata", "puente_sinco2019_sinco2011.csv", package = "renoe"
    )
    if (!nzchar(ruta)) {
      stop("No se encontr\u00F3 el puente SINCO 2019-SINCO 2011.", call. = FALSE)
    }
    correspondencia <- utils::read.csv(
      ruta, stringsAsFactors = FALSE, na.strings = c("", "NA")
    )
  }

  requeridas <- c("sinco2019", "sinco2011")
  faltantes <- setdiff(requeridas, names(correspondencia))
  if (length(faltantes) > 0L) {
    stop(
      "Faltan columnas en `correspondencia`: ",
      paste(faltantes, collapse = ", "), call. = FALSE
    )
  }

  puente <- correspondencia |>
    dplyr::transmute(
      sinco2019 = suppressWarnings(as.integer(as.character(sinco2019))),
      sinco2011 = suppressWarnings(as.integer(as.character(sinco2011)))
    ) |>
    dplyr::filter(!is.na(sinco2019)) |>
    dplyr::distinct()

  perfil <- puente |>
    dplyr::summarise(
      n_destinos = sum(!is.na(sinco2011)),
      destino_primero = {
        disponibles <- sinco2011[!is.na(sinco2011)]
        if (length(disponibles)) disponibles[[1L]] else NA_integer_
      },
      .by = sinco2019
    )

  original <- suppressWarnings(as.integer(as.character(data[[variable_sinco]])))
  posicion <- match(original, perfil$sinco2019)
  n_destinos <- perfil$n_destinos[posicion]
  destino <- perfil$destino_primero[posicion]
  if (resolver_multiples == "na") {
    destino[n_destinos > 1L] <- NA_integer_
  }

  data$sinco2019_original <- original
  data$sinco2011 <- as.integer(destino)
  data$sinco2011_n_destinos <- as.integer(n_destinos)
  data$sinco2011_calidad <- dplyr::case_when(
    is.na(original) ~ "SINCO 2019 faltante",
    is.na(n_destinos) ~ "C\u00F3digo ausente de la tabla oficial",
    n_destinos == 0L ~ "Sin equivalencia SINCO 2011",
    n_destinos == 1L ~ "Equivalencia oficial directa",
    n_destinos > 1L & resolver_multiples == "na" ~
      "Equivalencia oficial m\u00FAltiple: sin resolver",
    n_destinos > 1L ~ "Equivalencia oficial m\u00FAltiple: primer destino",
    TRUE ~ NA_character_
  )

  data |>
    sjlabelled::var_labels(
      sinco2019_original = "C\u00F3digo ocupacional original SINCO 2019",
      sinco2011 = "C\u00F3digo equivalente en SINCO 2011",
      sinco2011_n_destinos = "N\u00FAmero de destinos SINCO 2011 en la tabla oficial",
      sinco2011_calidad = "Calidad del puente oficial SINCO 2019-SINCO 2011"
    )
}
