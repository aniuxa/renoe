#' Convertir códigos CMO a SINCO (3 y 4 dígitos)
#'
#' Esta función toma códigos CMO de 4 dígitos —ya sea proporcionados directamente en `cmo_4d`
#' o a través de `p3coe`— y devuelve su correspondencia con códigos SINCO 2011 (3 y 4 dígitos),
#' usando una tabla de equivalencias incluida en el paquete. Funciona para el 82% de las ocupaciones.
#'
#' @param data Un data.frame que contenga la variable `cmo_4d` o `p3coe`.
#' @param codigos Opcional: data.frame de equivalencias. Si se omite, se usa una tabla interna del paquete.
#' @param var_origen Nombre de la variable que contiene el código CMO (por defecto `cmo_4d`).
#' @param keep_labels Lógico. Si TRUE, mantiene las etiquetas si existen.
#'
#' @return El `data.frame` original con columnas adicionales: `cmo_4d`, `sinco4d` y `sinco3d`.
#' @export
#'
#' @examples
#' datos <- data.frame(p3coe = c(1101, 1102, 1167))
#' cmo_to_sinco(datos)
cmo_to_sinco <- function(data, codigos = NULL, var_origen = "cmo_4d", keep_labels = FALSE) {
  # Crear cmo_4d desde p3coe si no está presente
  if (!"cmo_4d" %in% names(data)) {
    if ("p3coe" %in% names(data)) {
      data <- data %>%
        dplyr::mutate(cmo_4d = .data[["p3coe"]])
      var_origen <- "cmo_4d"
    } else {
      stop("El data frame debe contener 'cmo_4d' o 'p3coe'.")
    }
  }

  # Cargar tabla de correspondencias si no se proporcionó
  if (is.null(codigos)) {
    codigos <- readr::read_csv(
      system.file("extdata", "cmo_sinco_total.csv", package = "renoe"),
      show_col_types = FALSE
    )
  }

  # Filtrar y unir correspondencias
  codigos <- codigos %>%
    dplyr::select(cmo_4d, sinco4d, sinco3d) %>%
    dplyr::distinct()

  data <- data %>%
    dplyr::left_join(codigos, by = setNames("cmo_4d", var_origen))

  return(data)
}
