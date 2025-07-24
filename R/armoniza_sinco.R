#' Armonizar códigos SINCO desde CMO o SINCO 2011
#'
#' Esta función construye variables `sinco1d`, `sinco2d`, `sinco3d` y `sinco4d` a partir de códigos CMO (anteriores a 2013,
#' o hasta el segundo trimestre de 2013) o directamente desde SINCO 2011 (desde el tercer trimestre de 2013 en adelante),
#' utilizando una correspondencia incluida en el paquete y reglas manuales de clasificación cuando no hay equivalencia directa.
#'
#' @param data Un data.frame con al menos `anio`, `trimestre` (como "t1", "t2", etc.) y `p3coe`.
#'             También puede contener `sinco4d`, `pos_ocu`, `tue2`.
#' @param codigos (Opcional) data.frame de correspondencia CMO–SINCO. Si se omite, se usa la tabla interna del paquete.
#'
#' @return Un data.frame con variables armonizadas: `cmo_4d`, `sinco4d`, `sinco3d`, `sinco2d`, `sinco1d`.
#' @export
#' @encoding UTF-8
#'
#' @examples
#' datos <- data.frame(
#'   anio = c(2011, 2012, 2013),
#'   trim = c("t1", "t2", "t3"),
#'   p3coe = c(7121, 4190, 1110),
#'   pos_ocu = c(1, 2, 1),
#'   tue2 = c(1, 2, 1)
#' )
#' armoniza_sinco(datos)
#' @family procesamiento_enoe
armoniza_sinco <- function(data, codigos = NULL) {
  stopifnot(all(c("anio", "trim") %in% names(data)))

  # Convertir trimestre a número (t1 → 1, t2 → 2, etc.)
  data <- data %>%
    dplyr::mutate(trimestre_n = as.numeric(stringr::str_remove(trim, "^t")))

  # Aplicar correspondencia directa (sinco4d, sinco3d) a partir de cmo_4d o p3coe
  data <- renoe::cmo_to_sinco(data, codigos = codigos)

  # Para año >= 2013 y trimestre >= 3: usar p3coe directamente como sinco4d si está faltante
  data <- data %>%
    dplyr::mutate(
      sinco4d = dplyr::if_else(
        anio > 2013 | (anio == 2013 & trimestre_n >= 3),
        dplyr::coalesce(sinco4d, p3coe),
        sinco4d
      )
    )

  # Derivar sinco3d, sinco2d, sinco1d con substrings
  data <- data %>%
    dplyr::mutate(
      sinco4d_str = stringr::str_pad(as.character(sinco4d), width = 4, side = "left", pad = "0"),
      sinco3d     = dplyr::coalesce(sinco3d, as.numeric(stringr::str_sub(sinco4d_str, 1, 3))),
      sinco2d     = as.numeric(stringr::str_sub(sinco4d_str, 1, 2)),
      sinco1d     = as.numeric(stringr::str_sub(sinco4d_str, 1, 1))
    ) %>%
    dplyr::select(-sinco4d_str)

  # Imputar sinco1d manualmente solo para anio < 2013 o trimestre < 3 en 2013
  data <- data %>%
    dplyr::mutate(needs_manual_1d = is.na(sinco1d) & (anio < 2013 | (anio == 2013 & trimestre_n < 3)))

  if (any(data$needs_manual_1d, na.rm = TRUE)) {
    data <- renoe::cmo_to_sinco1d(data)
  }

  data <- data %>%
    dplyr::select(-needs_manual_1d, -trimestre_n)

  return(data)
}
