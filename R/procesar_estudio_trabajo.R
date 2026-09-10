#' Clasificar la combinacion de estudio y trabajo
#'
#' Construye una clasificacion general de asistencia escolar y condicion de
#' ocupacion, ademas de indicadores para tres situaciones dentro de la poblacion
#' que no estudia ni trabaja: busqueda de trabajo, dedicacion a los quehaceres
#' del hogar y disponibilidad laboral. Los indicadores pueden superponerse; la
#' variable `tipo_neet` ofrece una version mutuamente excluyente.
#'
#' La disponibilidad (`neet_disponible`) se conserva con ese nombre descriptivo.
#' Su interpretacion como proxy de desaliento requiere justificacion en cada
#' analisis y no es impuesta por esta funcion.
#'
#' @param data Data frame con `clase2`, `cs_p17` y `p2e`.
#'
#' @return El mismo data frame con la clasificacion de estudio y trabajo, el
#'   indicador general de no estudio y no trabajo, tres indicadores de grupo y
#'   una tipologia exclusiva.
#' @export
#' @family procesamiento_enoe
#'
#' @examples
#' datos <- data.frame(
#'   clase2 = c(1, 1, 3, 3, 2, 4),
#'   cs_p17 = c(1, 2, 1, 2, 2, 2),
#'   p2e = c(NA, NA, 3, 4, NA, 6)
#' )
#' procesar_estudio_trabajo(datos)
procesar_estudio_trabajo <- function(data) {
  requeridas <- c("clase2", "cs_p17", "p2e")
  faltantes <- setdiff(requeridas, names(data))
  if (length(faltantes) > 0L) {
    stop(
      "Faltan variables requeridas en `data`: ",
      paste(faltantes, collapse = ", "),
      call. = FALSE
    )
  }

  data |>
    dplyr::mutate(
      situacion_estudio_trabajo = dplyr::case_when(
        clase2 %in% 2:4 & cs_p17 == 1 ~ 1,
        clase2 == 1 & cs_p17 == 2 ~ 2,
        clase2 == 1 & cs_p17 == 1 ~ 3,
        clase2 %in% 2:4 & cs_p17 == 2 ~ 4,
        TRUE ~ NA_real_
      ),
      no_estudia_no_trabaja = dplyr::case_when(
        situacion_estudio_trabajo == 4 ~ 1,
        situacion_estudio_trabajo %in% 1:3 ~ 0,
        TRUE ~ NA_real_
      ),
      neet_buscador = dplyr::case_when(
        no_estudia_no_trabaja == 1 & clase2 == 2 ~ 1,
        no_estudia_no_trabaja == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      neet_cuidador = dplyr::case_when(
        no_estudia_no_trabaja == 1 & p2e == 4 ~ 1,
        no_estudia_no_trabaja == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      neet_disponible = dplyr::case_when(
        no_estudia_no_trabaja == 1 & clase2 == 3 ~ 1,
        no_estudia_no_trabaja == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      tipo_neet = dplyr::case_when(
        no_estudia_no_trabaja != 1 | is.na(no_estudia_no_trabaja) ~ NA_real_,
        clase2 == 2 ~ 1,
        p2e == 4 ~ 2,
        clase2 == 3 ~ 3,
        TRUE ~ 4
      )
    ) |>
    sjlabelled::var_labels(
      situacion_estudio_trabajo = "Situaci\u00F3n combinada de asistencia escolar y ocupaci\u00F3n",
      no_estudia_no_trabaja = "Persona que no estudia ni trabaja",
      neet_buscador = "Persona que no estudia ni trabaja y busca trabajo",
      neet_cuidador = "Persona que no estudia ni trabaja y se dedica a los quehaceres del hogar",
      neet_disponible = "Persona que no estudia ni trabaja y est\u00E1 disponible para trabajar",
      tipo_neet = "Tipo principal de situaci\u00F3n entre quienes no estudian ni trabajan"
    ) |>
    sjlabelled::val_labels(
      situacion_estudio_trabajo = c(
        "Estudia" = 1,
        "Trabaja" = 2,
        "Estudia y trabaja" = 3,
        "No estudia ni trabaja" = 4
      ),
      no_estudia_no_trabaja = c("No" = 0, "S\u00ED" = 1),
      neet_buscador = c("No" = 0, "S\u00ED" = 1),
      neet_cuidador = c("No" = 0, "S\u00ED" = 1),
      neet_disponible = c("No" = 0, "S\u00ED" = 1),
      tipo_neet = c(
        "Buscador" = 1,
        "Cuidador" = 2,
        "Disponible no cuidador" = 3,
        "No disponible y no cuidador" = 4
      )
    )
}
