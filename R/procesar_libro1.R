#' Procesar indicadores individuales para el proyecto del libro
#'
#' Crea indicadores de origen extranjero, no ocupacion por cuidados, mujeres
#' con educacion universitaria y afiliacion sindical. La afiliacion solo es medible
#' en cuestionarios ampliados; esta funcion no calcula estimaciones survey.
#'
#' @param data Data frame individual previamente procesado por el paquete renoe.
#'
#' @return El data frame con los nuevos indicadores individuales.
#' @export
#' @family procesamiento_enoe
#'
#' @examples
#' \dontrun{
#' datos <- datos |>
#'   procesar_libro1()
#' }
procesar_libro1 <- function(data) {
  requeridas <- c(
    "anio", "trim", "l_nac_c", "p2g2", "sexo", "cs_p13_1",
    "clase2", "pos_ocu"
  )
  faltantes <- setdiff(requeridas, names(data))
  if (length(faltantes) > 0) {
    stop(
      "Faltan variables requeridas en `data`: ",
      paste(faltantes, collapse = ", ")
    )
  }

  # p3i pertenece al cuestionario ampliado. Si la columna no existe en un
  # trimestre basico, se crea como NA estructural en vez de detener la tuberia.
  tiene_p3i <- "p3i" %in% names(data)
  if (!tiene_p3i) data$p3i <- NA_real_

  if (!"coe_tipo" %in% names(data)) data$coe_tipo <- NA_character_

  # Antes de crear columnas estructurales se registra si las baterias realmente
  # venian en el archivo. Es el mismo principio usado en
  # procesar_vars_laborales(): solo se calcula una variable del ampliado cuando
  # el trimestre es ampliado y existen sus insumos originales.
  vars_prestaciones <- paste0("p3m", 1:9)
  tiene_bateria_p3m <- all(vars_prestaciones %in% names(data))
  tiene_p3r_anio <- "p3r_anio" %in% names(data)
  tiene_p3r_mes <- "p3r_mes" %in% names(data)
  tiene_d_mes <- "d_mes" %in% names(data)

  # Variables exclusivas del cuestionario ampliado. Se crean como NA
  # estructural cuando no existen para que la funcion opere en todos los
  # trimestres sin confundir ausencia del instrumento con respuesta negativa.
  vars_ampliado <- c(
    paste0("p3m", 1:9), "p3r_anio", "p3r_mes", "p3r", "d_mes"
  )
  for (v in setdiff(vars_ampliado, names(data))) data[[v]] <- NA_real_

  nombres_prestaciones <- c(
    p3m1 = "prest_credito_vivienda",
    p3m2 = "prest_guarderia",
    p3m3 = "prest_tiempo_cuidados_parentales",
    p3m4 = "prest_fondo_retiro",
    p3m5 = "prest_seguro_vida",
    p3m6 = "prest_seguro_medico_privado",
    p3m7 = "prest_prestamos_caja_ahorro"
  )

  elegible_prestaciones <- dplyr::coalesce(
    data$coe_tipo == "ampliado" & tiene_bateria_p3m & data$pos_ocu == 1,
    FALSE
  )
  elegible_antiguedad <- dplyr::coalesce(
    data$coe_tipo == "ampliado" & tiene_p3r_anio & data$pos_ocu == 1,
    FALSE
  )

  # La clave de Estados Unidos se identifica por trimestre. En el clasificador
  # moderno 201 es Anguila y 221 es Estados Unidos; 201 solo se interpreta como
  # Estados Unidos en los periodos históricos donde 221 no está presente.
  data$origen_codigo <- suppressWarnings(as.numeric(data$l_nac_c))
  data <- data |>
    dplyr::mutate(
      codigo_eeuu_usado = dplyr::case_when(
        any(origen_codigo == 221, na.rm = TRUE) ~ 221,
        any(origen_codigo == 201, na.rm = TRUE) ~ 201,
        TRUE ~ NA_real_
      ),
      .by = c(anio, trim)
    )
  no_sabe_prestaciones <- elegible_prestaciones & data$p3m9 == 9

  for (i in seq_along(nombres_prestaciones)) {
    origen <- names(nombres_prestaciones)[[i]]
    destino <- unname(nombres_prestaciones[[i]])
    codigo <- i
    data[[destino]] <- dplyr::case_when(
      !elegible_prestaciones ~ NA_real_,
      no_sabe_prestaciones ~ NA_real_,
      data[[origen]] == codigo ~ 1,
      TRUE ~ 0
    )
  }

  data |>
    dplyr::mutate(
      # Clasificacion comparable entre el catalogo agregado de 2005 y el
      # clasificador detallado posterior. Importante: 260 es Mexico en el
      # catalogo moderno y no debe clasificarse como extranjero.
      region_origen_long = dplyr::case_when(
        origen_codigo %in% c(1:33, 260) ~ 0,
        !is.na(codigo_eeuu_usado) &
          origen_codigo == codigo_eeuu_usado ~ 1,
        origen_codigo == 225 ~ 2,
        origen_codigo == 200 |
          dplyr::between(origen_codigo, 201, 252) ~ 3,
        origen_codigo == 400 |
          dplyr::between(origen_codigo, 401, 453) ~ 4,
        origen_codigo %in% c(100, 300, 500) |
          dplyr::between(origen_codigo, 101, 172) |
          dplyr::between(origen_codigo, 301, 358) |
          dplyr::between(origen_codigo, 501, 535) ~ 5,
        origen_codigo == 600 ~ 6,
        TRUE ~ NA_real_
      ),
      extr = dplyr::case_when(
        region_origen_long == 0 ~ 0,
        region_origen_long %in% 1:6 ~ 1,
        TRUE ~ NA_real_
      ),
      extr_especificado = dplyr::case_when(
        region_origen_long == 0 ~ NA_real_,
        region_origen_long %in% 1:5 ~ 1,
        region_origen_long == 6 ~ 0,
        TRUE ~ NA_real_
      ),
      origen_nivel_detalle = dplyr::case_when(
        origen_codigo %in% 1:32 ~ "Entidad mexicana identificada",
        origen_codigo %in% c(33, 260) ~ "Mexico sin entidad comparable",
        (!is.na(codigo_eeuu_usado) &
          origen_codigo == codigo_eeuu_usado) |
          origen_codigo %in% c(225, 415) ~
          "Pais extranjero identificado",
        origen_codigo %in% c(100, 200, 300, 400, 500) ~
          "Region extranjera agregada",
        region_origen_long %in% 1:5 ~ "Pais extranjero identificado",
        region_origen_long == 6 ~ "Pais extranjero no especificado",
        TRUE ~ NA_character_
      ),
      no_ocupacion_cuidados = dplyr::case_when(
        clase2 %in% 2:4 & p2g2 == 9 ~ 1,
        clase2 %in% 2:4 & !is.na(p2g2) ~ 0,
        TRUE ~ NA_real_
      ),
      nivel_educativo_codigo = suppressWarnings(
        as.integer(as.character(cs_p13_1))
      ),
      educacion_universitaria = dplyr::case_when(
        nivel_educativo_codigo %in% 7:9 ~ 1,
        nivel_educativo_codigo %in% 0:6 ~ 0,
        TRUE ~ NA_real_
      ),
      # Profesional, maestria o doctorado. No incluye normal ni carrera
      # tecnica, aunque ambas formen parte del nivel terciario agregado usado
      # para medir desajuste vertical.
      mujer_universitaria = dplyr::case_when(
        is.na(sexo) | is.na(educacion_universitaria) ~ NA_real_,
        sexo == 2 & educacion_universitaria == 1 ~ 1,
        sexo %in% 1:2 ~ 0,
        TRUE ~ NA_real_
      ),
      sindicato_medible = coe_tipo == "ampliado" & tiene_p3i,
      sindicato = dplyr::case_when(
        sindicato_medible & pos_ocu == 1 & p3i == 1 ~ 1,
        sindicato_medible & pos_ocu == 1 & p3i == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      prestaciones_medibles = elegible_prestaciones,
      prestaciones_no_sabe = dplyr::case_when(
        !elegible_prestaciones ~ NA_real_,
        p3m9 == 9 ~ 1,
        TRUE ~ 0
      ),
      prestaciones_ninguna = dplyr::case_when(
        !elegible_prestaciones | p3m9 == 9 ~ NA_real_,
        p3m8 == 8 ~ 1,
        TRUE ~ 0
      ),
      n_prestaciones_ampliado = dplyr::if_else(
        elegible_prestaciones & (is.na(p3m9) | p3m9 != 9),
        prest_credito_vivienda + prest_guarderia +
          prest_tiempo_cuidados_parentales + prest_fondo_retiro +
          prest_seguro_vida + prest_seguro_medico_privado +
          prest_prestamos_caja_ahorro,
        NA_real_
      ),
      anio_inici = dplyr::case_when(
        elegible_antiguedad &
          dplyr::between(as.numeric(p3r_anio), 1900, as.numeric(anio)) ~
          as.numeric(p3r_anio),
        TRUE ~ NA_real_
      ),
      mes_inici = dplyr::case_when(
        tiene_p3r_mes & !is.na(anio_inici) &
          dplyr::between(as.numeric(p3r_mes), 1, 12) ~
          as.numeric(p3r_mes),
        TRUE ~ NA_real_
      ),
      mes_entrevista = dplyr::case_when(
        tiene_d_mes & dplyr::between(as.numeric(d_mes), 1, 12) ~
          as.numeric(d_mes),
        TRUE ~ NA_real_
      ),
      antiguedad_medible = elegible_antiguedad,
      duracion_precision = dplyr::case_when(
        is.na(anio_inici) ~ NA_character_,
        !is.na(mes_inici) & !is.na(mes_entrevista) ~ "mensual",
        TRUE ~ "anual"
      ),
      duracion_meses = dplyr::if_else(
        duracion_precision == "mensual",
        12 * (as.numeric(anio) - anio_inici) + mes_entrevista - mes_inici,
        NA_real_
      ),
      # Estimacion puntual: exacta cuando hay meses; diferencia de anos cuando
      # el mes inicial no fue captado.
      duracion_anios = dplyr::case_when(
        duracion_precision == "mensual" ~ duracion_meses / 12,
        duracion_precision == "anual" ~ as.numeric(anio) - anio_inici,
        TRUE ~ NA_real_
      ),
      # Intervalo compatible con el dato cuando no conocemos el mes inicial.
      duracion_min_meses = dplyr::case_when(
        duracion_precision == "mensual" ~ duracion_meses,
        duracion_precision == "anual" & !is.na(mes_entrevista) ~ pmax(
          0, 12 * (as.numeric(anio) - anio_inici) + mes_entrevista - 12
        ),
        duracion_precision == "anual" ~ pmax(
          0, 12 * (as.numeric(anio) - anio_inici - 1)
        ),
        TRUE ~ NA_real_
      ),
      duracion_max_meses = dplyr::case_when(
        duracion_precision == "mensual" ~ duracion_meses,
        duracion_precision == "anual" & !is.na(mes_entrevista) ~
          12 * (as.numeric(anio) - anio_inici) + mes_entrevista - 1,
        duracion_precision == "anual" ~
          12 * (as.numeric(anio) - anio_inici + 1) - 1,
        TRUE ~ NA_real_
      )
    ) |>
    sjlabelled::var_labels(
      origen_codigo = "Clave original del lugar de nacimiento",
      codigo_eeuu_usado = "Clave identificada como Estados Unidos en el trimestre",
      region_origen_long = "Region de nacimiento armonizada longitudinalmente",
      extr = "Persona nacida en el extranjero",
      extr_especificado = "Persona extranjera con region de origen identificada",
      origen_nivel_detalle = "Nivel de detalle del lugar de nacimiento disponible",
      no_ocupacion_cuidados = "No ocupacion por razones de cuidado",
      nivel_educativo_codigo = "Nivel educativo de CS_P13_1 normalizado como entero",
      educacion_universitaria = "Educacion profesional, maestria o doctorado",
      mujer_universitaria = "Mujer con educacion profesional, maestria o doctorado",
      sindicato_medible = "Afiliacion sindical medible en el trimestre",
      sindicato = "Afiliacion sindical de personas asalariadas",
      prest_credito_vivienda = "Prestacion: credito para vivienda",
      prest_guarderia = "Prestacion: guarderia",
      prest_tiempo_cuidados_parentales = "Prestacion: tiempo para cuidados maternos o paternos",
      prest_fondo_retiro = "Prestacion: fondo de retiro (SAR o Afore)",
      prest_seguro_vida = "Prestacion: seguro de vida",
      prest_seguro_medico_privado = "Prestacion: seguro privado para gastos medicos",
      prest_prestamos_caja_ahorro = "Prestacion: prestamos personales o caja de ahorro",
      prestaciones_medibles = "Prestaciones del cuestionario ampliado medibles",
      prestaciones_no_sabe = "No sabe que prestaciones recibe",
      prestaciones_ninguna = "No recibe ninguna prestacion de la bateria P3M",
      n_prestaciones_ampliado = "Numero de prestaciones recibidas de P3M1 a P3M7",
      anio_inici = "Ano de inicio en el trabajo actual",
      mes_inici = "Mes de inicio en el trabajo actual",
      mes_entrevista = "Mes del resultado definitivo de la entrevista",
      antiguedad_medible = "Antiguedad en el trabajo medible en el trimestre",
      duracion_precision = "Precision de la antiguedad en el trabajo",
      duracion_meses = "Antiguedad exacta en meses cuando se conoce el mes inicial",
      duracion_anios = "Antiguedad en el trabajo expresada en anos",
      duracion_min_meses = "Limite inferior de antiguedad compatible con la fecha",
      duracion_max_meses = "Limite superior de antiguedad compatible con la fecha"
    ) |>
    sjlabelled::val_labels(
      region_origen_long = c(
        "Mexico" = 0,
        "Estados Unidos" = 1,
        "Guatemala" = 2,
        "Resto de America" = 3,
        "Europa" = 4,
        "Resto del mundo" = 5,
        "Pais extranjero no especificado" = 6
      ),
      extr = c("Nacida en Mexico" = 0, "Nacida en el extranjero" = 1),
      extr_especificado = c(
        "Pais extranjero no especificado" = 0,
        "Region de origen identificada" = 1
      ),
      no_ocupacion_cuidados = c("No" = 0, "Si" = 1),
      educacion_universitaria = c("No" = 0, "Si" = 1),
      mujer_universitaria = c("No" = 0, "Si" = 1),
      sindicato_medible = c("No" = 0, "Si" = 1),
      sindicato = c("No afiliada/o" = 0, "Afiliada/o" = 1)
    )
}
