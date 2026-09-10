#' Convertir códigos CMO a clasificación SINCO 1d (nivel agregado)
#'
#' Aplica mapeo manual desde 2005-I hasta el segundo trimestre de 2012,
#' con base en `p3_coe`, usando reglas por `cmo2d`, `cmo3d` y códigos completos.
#'
#' @param data Un data frame con variables `p3_coe`, `anio`, `trim` y preferentemente `pos_ocu`, `tue2`.
#' @return El data frame original con la variable `sinco1d` agregada.
#' @export
#' @encoding UTF-8
cmo_to_sinco1d <- function(data) {
  # Si la función recibe una clasificación SINCO ya calculada, las reglas CMO
  # sólo completan los casos históricos y no deben sobrescribirla.
  if (!"sinco1d" %in% names(data)) data$sinco1d <- NA_real_

  data <- data %>%
    dplyr::mutate(
      .trim_n = as.numeric(stringr::str_remove(as.character(trim), "^t")),
      aplicar_reglas = anio < 2012 | (anio == 2012 & .trim_n <= 2),
      cmo = dplyr::if_else(aplicar_reglas, p3coe, NA_real_),
      str_cmo = stringr::str_pad(as.character(cmo), width = 4, side = "left", pad = "0"),
      cmo2d = as.numeric(substr(str_cmo, 1, 2)),
      cmo3d = as.numeric(substr(str_cmo, 1, 3))
    )

  reglas_cmo2d <- c(
    `11` = 2, `12` = 2, `13` = 2, `14` = 2,
    `21` = 1,
    `41` = 6,
    `51` = 1,
    `52` = 7,
    `53` = 8,
    `54` = 9,
    `55` = 8,
    `61` = 5,
    `62` = 3,
    `71` = 4,
    `72` = 9,
    `81` = 5,
    `82` = 9,
    `83` = 9,
    `99` = 9
  )

  data <- data %>%
    dplyr::mutate(
      sinco1d = dplyr::case_when(
        aplicar_reglas & !is.na(cmo2d) & as.character(cmo2d) %in% names(reglas_cmo2d) ~
          as.numeric(reglas_cmo2d[as.character(cmo2d)]),
        TRUE ~ sinco1d
      ),
      sinco1d = dplyr::case_when(
        aplicar_reglas & cmo == 7121 ~ 8,
        aplicar_reglas & cmo3d == 720 ~ 9,
        aplicar_reglas & cmo %in% c(4109, 4119, 4129, 4139, 4149, 4159, 4169) ~ 9,
        aplicar_reglas & cmo3d == 419 ~ 9,
        aplicar_reglas & cmo2d == 41 & pos_ocu == 4 ~ 9,
        aplicar_reglas & cmo2d == 41 & tue2 == 7 ~ 9,
        aplicar_reglas & cmo2d == 52 & pos_ocu == 4 ~ 9,
        TRUE ~ sinco1d
      ),
      # Los códigos SINCO son categóricos enteros. Esta conversión explícita
      # evita que los trimestres CMO queden como double y entren en conflicto
      # con los trimestres SINCO al construir paneles longitudinales.
      sinco1d = as.integer(sinco1d),
      sinco1d = sjlabelled::set_label(sinco1d, "Clasificación SINCO 1d (ocupación agregada)")
    )

  # Estas columnas solo sirven para aplicar las reglas de correspondencia.
  # No forman parte de la salida analitica de la funcion.
  data %>%
    dplyr::select(
      -.trim_n, -aplicar_reglas, -cmo, -str_cmo, -cmo2d, -cmo3d
    )
}
