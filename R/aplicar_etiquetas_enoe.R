#' Restaurar etiquetas de variables y valores de la ENOE
#'
#' Restaura metadatos que pueden perderse al guardar y volver a leer archivos
#' Parquet. Las descripciones de variables se toman de
#' `diccionario_variables.csv` y las etiquetas de codigos de
#' `diccionario_etiquetas_valores.csv`. La funcion conserva los codigos
#' numericos y usa la clase `haven_labelled`, por lo que el resultado puede
#' guardarse como RDS o exportarse a Stata sin convertir las variables en
#' factores.
#'
#' Esta funcion esta pensada para la etapa de distribucion o exportacion. No
#' es necesario aplicarla antes de cada transformacion analitica.
#'
#' @param data Data frame o tibble con variables ENOE procesadas.
#' @param diccionario_variables Ruta a un CSV o data frame con las columnas
#'   `variable_nombre` y `descripcion`. Si es `NULL`, usa el diccionario
#'   incluido en `renoe`.
#' @param diccionario_valores Ruta a un CSV o data frame con las columnas
#'   `variable_nombre`, `codigo` y `etiqueta`. Si es `NULL`, usa el catalogo
#'   incluido en `renoe`.
#' @param sobrescribir Si es `TRUE`, sustituye etiquetas existentes. Por
#'   defecto solo completa etiquetas ausentes.
#' @param informar Si es `TRUE`, informa cuantas etiquetas fueron aplicadas.
#'
#' @return El mismo objeto con atributos `label` y, para variables numericas
#'   catalogadas, clase `haven_labelled` y etiquetas de valores.
#' @export
#'
#' @examples
#' datos_etiquetados <- aplicar_etiquetas_enoe(
#'   data.frame(sexo = c(1, 2), clase2 = c(1, 4))
#' )
aplicar_etiquetas_enoe <- function(
    data,
    diccionario_variables = NULL,
    diccionario_valores = NULL,
    sobrescribir = FALSE,
    informar = interactive()
) {
  stopifnot(is.data.frame(data))

  leer_diccionario <- function(x, archivo) {
    if (is.data.frame(x)) return(x)
    ruta <- if (is.null(x)) system.file("extdata", archivo, package = "renoe") else x
    if (!nzchar(ruta) || !file.exists(ruta)) {
      stop("No se encontr\u00F3 el diccionario `", archivo, "`.", call. = FALSE)
    }
    readr::read_csv(ruta, show_col_types = FALSE)
  }

  dic_vars <- leer_diccionario(
    diccionario_variables,
    "diccionario_variables.csv"
  )
  dic_vals <- leer_diccionario(
    diccionario_valores,
    "diccionario_etiquetas_valores.csv"
  )

  requeridas_vars <- c("variable_nombre", "descripcion")
  requeridas_vals <- c("variable_nombre", "codigo", "etiqueta")
  if (!all(requeridas_vars %in% names(dic_vars))) {
    stop("El diccionario de variables no tiene las columnas requeridas.", call. = FALSE)
  }
  if (!all(requeridas_vals %in% names(dic_vals))) {
    stop("El diccionario de valores no tiene las columnas requeridas.", call. = FALSE)
  }

  n_variables <- 0L
  aplicables <- dic_vars[dic_vars$variable_nombre %in% names(data), , drop = FALSE]
  for (i in seq_len(nrow(aplicables))) {
    variable <- aplicables$variable_nombre[[i]]
    descripcion <- aplicables$descripcion[[i]]
    sin_etiqueta <- is.null(attr(data[[variable]], "label", exact = TRUE))
    if ((sobrescribir || sin_etiqueta) && !is.na(descripcion) && nzchar(descripcion)) {
      attr(data[[variable]], "label") <- descripcion
      n_variables <- n_variables + 1L
    }
  }

  n_valores <- 0L
  por_variable <- split(dic_vals, dic_vals$variable_nombre)
  por_variable <- por_variable[intersect(names(por_variable), names(data))]
  for (variable in names(por_variable)) {
    x <- data[[variable]]
    if (!is.numeric(x)) next
    ya_etiquetada <- !is.null(attr(x, "labels", exact = TRUE))
    if (!sobrescribir && ya_etiquetada) next

    catalogo <- por_variable[[variable]]
    codigos <- suppressWarnings(as.numeric(catalogo$codigo))
    validos <- !is.na(codigos) & !is.na(catalogo$etiqueta) & nzchar(catalogo$etiqueta)
    if (!any(validos)) next

    etiquetas <- stats::setNames(codigos[validos], catalogo$etiqueta[validos])
    etiqueta_variable <- attr(x, "label", exact = TRUE)
    data[[variable]] <- haven::labelled(
      x,
      labels = etiquetas,
      label = etiqueta_variable
    )
    n_valores <- n_valores + 1L
  }

  if (isTRUE(informar)) {
    message(
      "Etiquetas restauradas: ", n_variables,
      " variables y ", n_valores, " cat\u00E1logos de valores."
    )
  }
  data
}
