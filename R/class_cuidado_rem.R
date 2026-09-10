#' Clasificar el trabajo de cuidado de mercado
#'
#' Reproduce la clasificacion utilizada en el articulo a partir de la
#' ocupacion armonizada a tres digitos y de la rama de actividad. Distingue
#' ocupaciones de cuidado, industria del cuidado y la posicion resultante del
#' trabajador en la economia del cuidado. La pertenencia a este conjunto no
#' implica por si sola que se observe remuneracion positiva.
#'
#' La funcion detecta automaticamente el clasificador por periodo. Entre
#' 2005-I y 2012-II aplica el puente analitico `cmo_to_sinco11_care()`; desde
#' 2012-III utiliza SINCO. La concordancia CMO-SINCO no es biunivoca, por lo
#' que conserva sus banderas de calidad y no debe interpretarse como una
#' conversion oficial exacta para otros usos.
#'
#' @param data Data frame individual de ENOE ya procesado.
#' @param variable_ocupacion Nombre de la variable SINCO a tres digitos.
#' @param variable_actividad Nombre de la variable SCIAN-Hogares (`p4a`).
#' @param variable_ocupado Nombre de la variable de condicion de ocupacion.
#' @param valor_ocupado Valor que identifica a la poblacion ocupada.
#' @param variable_cmo Nombre de la ocupacion CMO en los periodos hasta
#'   2012-II; normalmente `p3coe`.
#' @param aplicar_puente_cmo Si es TRUE, aplica automaticamente el puente de
#'   cuidado a las observaciones de 2005-I a 2012-II.
#'
#' @return El mismo data frame con `class_ocu`, `isco_care`, `care_industry`,
#'   `care_w`, `cuida_total`, `trabajo_cuidado_mercado`, el alias deprecado
#'   `trabajo_cuidado_rem`, `cuida_1d` y banderas de medicion.
#' @export
#' @family cuidado_remunerado
#'
#' @examples
#' \dontrun{
#' datos <- datos |>
#'   class_cuidado_rem()
#' }
class_cuidado_rem <- function(
    data,
    variable_ocupacion = "sinco3d",
    variable_actividad = "p4a",
    variable_ocupado = "clase2",
    valor_ocupado = 1,
    variable_cmo = "p3coe",
    aplicar_puente_cmo = TRUE
) {
  requeridas <- c("anio", "trim", variable_actividad, variable_ocupado)
  faltantes <- setdiff(requeridas, names(data))
  if (length(faltantes) > 0) {
    stop(
      "Faltan variables requeridas en `data`: ",
      paste(faltantes, collapse = ", ")
    )
  }

  # `procesar_vars_sociodemo()` conserva el trimestre como "t1"-"t4".
  # Tambien se aceptan valores numericos para bases creadas fuera del flujo.
  anio_obs <- suppressWarnings(
    as.integer(as.character(data$anio))
  )
  trim_obs <- suppressWarnings(
    as.integer(sub("^t", "", tolower(as.character(data$trim))))
  )
  periodo_invalido <- is.na(anio_obs) | is.na(trim_obs) |
    trim_obs < 1L | trim_obs > 4L
  if (any(periodo_invalido)) {
    stop(
      "`anio` debe ser num\u00E9rico y `trim` debe ser 1-4 o t1-t4."
    )
  }
  periodo_obs <- anio_obs * 10L + trim_obs
  periodo_cmo <- !is.na(periodo_obs) & periodo_obs <= 20122L

  # Estas banderas permiten comprobar despues de la ejecucion si el puente
  # realmente se intento y si produjo una correspondencia.
  data$puente_cmo_requerido <- periodo_cmo
  data$puente_cmo_aplicado <- rep(FALSE, nrow(data))

  if (any(periodo_cmo) && aplicar_puente_cmo) {
    if (!variable_cmo %in% names(data)) {
      stop(
        "Hay observaciones de 2005-I a 2012-II, pero falta la variable CMO `",
        variable_cmo, "`."
      )
    }
    # Para ahorrar memoria no se copia el data frame completo: el puente recibe
    # unicamente el vector CMO de las filas historicas.
    indice_cmo <- which(periodo_cmo)
    datos_cmo <- data.frame(
      .cmo = data[[variable_cmo]][indice_cmo]
    )
    salidas_puente <- c(
      "cmo_original", "sinco11", "sinco3d",
      "sinco11_n_destinos", "sinco11_calidad"
    )
    datos_cmo <- cmo_to_sinco11_care(
      datos_cmo,
      variable_cmo = ".cmo"
    )

    n_cmo <- nrow(datos_cmo)
    n_cmo_no_faltante <- sum(!is.na(datos_cmo$cmo_original))
    n_convertidos <- sum(!is.na(datos_cmo$sinco3d))
    n_sin_correspondencia <- sum(
      !is.na(datos_cmo$cmo_original) & is.na(datos_cmo$sinco3d)
    )

    message(
      "Puente CMO-SINCO cuidado aplicado: ", n_convertidos, " de ",
      n_cmo_no_faltante, " c\u00F3digos CMO no faltantes convertidos (",
      n_cmo, " observaciones del periodo CMO; ",
      n_sin_correspondencia, " sin correspondencia)."
    )

    for (v in salidas_puente) {
      if (!v %in% names(data)) data[[v]] <- datos_cmo[[v]][NA_integer_]
      data[[v]][indice_cmo] <- datos_cmo[[v]]
    }
    if (variable_ocupacion != "sinco3d") {
      if (!variable_ocupacion %in% names(data)) data[[variable_ocupacion]] <- rep(NA_integer_, nrow(data))
      data[[variable_ocupacion]][indice_cmo] <- datos_cmo$sinco3d
    }
    data$puente_cmo_aplicado[indice_cmo] <- TRUE
  } else if (any(periodo_cmo) && !aplicar_puente_cmo) {
    warning(
      "Hay observaciones de 2005-I a 2012-II, pero el puente CMO fue ",
      "desactivado con `aplicar_puente_cmo = FALSE`.",
      call. = FALSE
    )
  }

  if (!variable_ocupacion %in% names(data)) {
    stop(
      "No existe la ocupacion armonizada `", variable_ocupacion,
      "` despues de seleccionar el clasificador."
    )
  }

  # Codigos no nulos de la hoja SINCO de homologa.xlsx. Los restantes codigos
  # validos reciben 0 (ocupacion no vinculada al cuidado).
  ocupacion_directa <- c(233, 234, 281, 282, 520, 522, 960, 961, 962, 964)
  # El grupo 243 (otros especialistas en salud) aparece con SINCO 2019,
  # aplicado por ENOE desde 2021-III.
  ocupacion_directa_ampliada <- c(232, 239, 241, 242, 243, 253, 256, 271, 521)
  ocupacion_indirecta <- c(122, 152, 214, 231)

  isco_13 <- c(122, 152, 231)
  isco_26 <- 214
  isco_22 <- c(241, 242, 243)
  isco_23 <- c(232, 233, 234, 239, 271)
  isco_34 <- c(253, 256, 521)
  isco_32 <- c(281, 282)
  isco_53 <- c(520, 522)
  isco_91 <- c(960, 961, 962, 964)

  # Ramas identificadas en la hoja SCIAN de homologa.xlsx.
  industria_cuidado <- c(
    6111, 6112, 6119, 6121, 6122, 6129, 6131, 6132, 6139,
    6141, 6142, 6149, 6150, 6199,
    6211, 6212, 6219, 6221, 6222, 6229,
    6231, 6232, 6239, 6241, 6242, 6249, 6251, 6252, 6259, 6299
  )
  hogares_servicio_domestico <- 8140
  servicios_veterinarios <- 5413

  # Las claves de cuidado se conservan entre SCIAN-Hogares 2007 y 2018,
  # aunque algunas descripciones cambian. La ruptura documentada de ENOE-N
  # ocurre en 2021-III. La reconstruccion oficial identifica SCIAN-Hogares 2007
  # desde 2005-I hasta 2021-II, aunque varios ZIP contengan catalogos
  # retrospectivos o sin encabezado.
  # CMO 8200 tiene tres destinos SINCO vinculados al trabajo domestico
  # (cocina, trabajo domestico y lavado). La primera regla de la concordancia
  # no basta para identificar su contenido de cuidado, por eso se recupera
  # explicitamente cuando se conservo `cmo_original`.
  cmo_8200_domestico <- if ("cmo_original" %in% names(data)) {
    suppressWarnings(as.integer(as.character(data$cmo_original))) == 8200L
  } else {
    rep(FALSE, nrow(data))
  }

  cmo_8200_domestico <- periodo_cmo & !is.na(cmo_8200_domestico) & cmo_8200_domestico

  ocupado <- !is.na(data[[variable_ocupado]]) & data[[variable_ocupado]] == valor_ocupado
  ocu <- .cuidado_codigo(data[[variable_ocupacion]], 3L)
  act <- .cuidado_codigo(data[[variable_actividad]], 4L)
  ocupacion_medible <- ocupado & !is.na(ocu)
  actividad_medible <- ocupado & !is.na(act)

  data |>
    dplyr::mutate(
      cuidado_ocupacion_medible = ocupacion_medible,
      cuidado_actividad_medible = actividad_medible,
      cuidado_cmo_8200_domestico = ocupado & cmo_8200_domestico,
      clasificador_ocupacion_cuidado = dplyr::case_when(
        periodo_cmo ~ "CMO armonizada con puente de cuidado",
        periodo_obs >= 20213L ~ "SINCO 2019 observado",
        periodo_obs >= 20123L ~ "SINCO 2011 observado",
        TRUE ~ NA_character_
      ),
      sinco_version_cuidado = dplyr::case_when(
        is.na(periodo_obs) ~ NA_character_,
        periodo_obs >= 20213L ~ "SINCO 2019",
        TRUE ~ "SINCO 2011 o puente CMO-SINCO 2011"
      ),
      scian_version_cuidado = dplyr::case_when(
        is.na(periodo_obs) ~ NA_character_,
        periodo_obs >= 20213L ~ "SCIAN-Hogares 2018",
        TRUE ~ "SCIAN-Hogares 2007"
      ),
      scian_catalogo_alerta = dplyr::case_when(
        is.na(periodo_obs) ~ NA,
        periodo_obs %in% c(20231L, 20241L, 20251L) ~ TRUE,
        TRUE ~ FALSE
      ),
      class_ocu = dplyr::case_when(
        !ocupado | is.na(ocu) ~ NA_real_,
        ocu %in% ocupacion_directa ~ 11,
        ocu %in% ocupacion_directa_ampliada ~ 12,
        ocu %in% ocupacion_indirecta | cmo_8200_domestico ~ 13,
        TRUE ~ 0
      ),
      isco_care = dplyr::case_when(
        !ocupado | is.na(ocu) ~ NA_real_,
        ocu %in% isco_13 ~ 13,
        ocu %in% isco_26 ~ 26,
        ocu %in% isco_22 ~ 22,
        ocu %in% isco_23 ~ 23,
        ocu %in% isco_34 ~ 34,
        ocu %in% isco_32 ~ 32,
        ocu %in% isco_53 ~ 53,
        ocu %in% isco_91 | cmo_8200_domestico ~ 91,
        TRUE ~ 0
      ),
      care_industry_detalle = dplyr::case_when(
        !ocupado | is.na(act) ~ NA_character_,
        act %in% c(6111, 6112, 6119, 6121, 6122, 6129, 6131, 6132,
                   6139, 6141, 6142, 6149, 6150, 6199) ~
          "Servicios educativos",
        act %in% c(6211, 6212, 6219) ~ "Consulta externa y servicios medicos",
        act %in% c(6221, 6222, 6229) ~ "Hospitales",
        act %in% c(6231, 6232, 6239) ~ "Residencias con cuidados de salud",
        act %in% c(6241, 6242, 6249) ~ "Otros servicios de asistencia social",
        act %in% c(6251, 6252, 6259) ~ "Guarderias",
        act == 6299 ~ "Salud y asistencia social no especificada",
        act == hogares_servicio_domestico ~ "Hogares con empleados domesticos",
        act == servicios_veterinarios ~ "Servicios veterinarios (excluir)",
        TRUE ~ "Fuera de la industria del cuidado"
      ),
      care_industry = dplyr::case_when(
        !ocupado | is.na(act) ~ NA_real_,
        act %in% industria_cuidado ~ 1,
        act %in% hogares_servicio_domestico ~ 2,
        act %in% servicios_veterinarios ~ 3,
        TRUE ~ 0
      ),
      # Replica la tipologia del articulo. El orden es deliberado: el trabajo
      # contratado directamente por hogares se identifica antes de evaluar la
      # ocupacion, y posteriormente `cuida_total` excluye ocupaciones no cuidadoras.
      care_w = dplyr::case_when(
        is.na(class_ocu) | is.na(care_industry) ~ NA_real_,
        class_ocu != 0 & care_industry == 1 ~ 1,
        care_industry == 2 ~ 2,
        isco_care %in% c(22, 32) & care_industry == 0 ~ 3,
        isco_care %in% c(23, 53) & !care_industry %in% c(1, 2) ~ 3,
        class_ocu == 0 & care_industry == 1 ~ 4,
        TRUE ~ 0
      ),
      cuida_total = dplyr::case_when(
        is.na(care_w) ~ NA_real_,
        care_w == 2 & class_ocu == 0 ~ 0,
        care_w != 0 ~ 1,
        TRUE ~ 0
      ),
      trabajo_cuidado_mercado = cuida_total,
      trabajo_cuidado_rem = cuida_total,
      cuida_1d = dplyr::case_when(
        is.na(class_ocu) | is.na(care_w) ~ NA_real_,
        class_ocu == 11 ~ 1,
        class_ocu == 12 ~ 2,
        class_ocu == 13 ~ 3,
        class_ocu == 0 & care_w == 4 ~ 4,
        TRUE ~ 0
      )
    ) |>
    sjlabelled::var_labels(
      cuidado_ocupacion_medible = "Ocupacion armonizada disponible para clasificar cuidados",
      cuidado_actividad_medible = "Rama de actividad disponible para clasificar cuidados",
      cuidado_cmo_8200_domestico = "CMO 8200 recuperado como ocupacion domestica indirecta",
      puente_cmo_requerido = "Observacion perteneciente al periodo CMO 2005-I a 2012-II",
      puente_cmo_aplicado = "Puente CMO-SINCO de cuidado ejecutado para la observacion",
      clasificador_ocupacion_cuidado = "Ruta utilizada para clasificar la ocupacion de cuidado",
      sinco_version_cuidado = "Version ocupacional usada para clasificar cuidados",
      scian_version_cuidado = "Versi\u00F3n SCIAN-Hogares documentada",
      scian_catalogo_alerta = "Catalogo P4A empaquetado inconsistente con el periodo documentado",
      class_ocu = "Tipo de ocupacion vinculada al cuidado",
      isco_care = "Gran grupo ISCO de la ocupacion de cuidado",
      care_industry = "Tipo de rama vinculada al cuidado",
      care_industry_detalle = "Rama detallada de la industria del cuidado",
      care_w = "Posicion del trabajador en la economia del cuidado",
      cuida_total = "Trabajadora/or de cuidado de mercado",
      trabajo_cuidado_mercado = "Trabajadora/or de cuidado de mercado",
      trabajo_cuidado_rem = "Alias deprecado de trabajo_cuidado_mercado",
      cuida_1d = "Grupo agregado de trabajo de cuidado de mercado"
    ) |>
    sjlabelled::val_labels(
      class_ocu = c(
        "No cuidado" = 0,
        "Cuidado directo" = 11,
        "Cuidado directo ampliado" = 12,
        "Ocupaciones indirectas" = 13
      ),
      care_industry = c(
        "No cuidado" = 0,
        "Industria del cuidado" = 1,
        "Hogares con servicio domestico" = 2,
        "Excluir veterinaria" = 3
      ),
      care_w = c(
        "Resto de trabajadores" = 0,
        "Trabajadores de cuidado en el sector de cuidado" = 1,
        "Trabajadores contratados por hogares" = 2,
        "Trabajadores de cuidado en otros sectores" = 3,
        "Otros trabajadores del sector de cuidado" = 4
      ),
      cuida_total = c("No" = 0, "S\u00ED" = 1),
      trabajo_cuidado_mercado = c("No" = 0, "S\u00ED" = 1),
      trabajo_cuidado_rem = c("No" = 0, "S\u00ED" = 1),
      cuida_1d = c(
        "No cuidado" = 0,
        "Cuidado directo" = 1,
        "Cuidado directo ampliado" = 2,
        "Ocupaciones indirectas" = 3,
        "Otros trabajadores del sector de cuidado" = 4
      )
    )
}
