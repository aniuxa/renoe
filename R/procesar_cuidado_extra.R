# ==============================================================================
# VARIABLES ADICIONALES PARA EL PROYECTO DEL LIBRO
# Funciones candidatas para incorporarse posteriormente al paquete {renoe}
# ==============================================================================


# Helpers internos -------------------------------------------------------------

# Devuelve NA cuando todos los valores elegibles son desconocidos; devuelve 0
# cuando no hay integrantes elegibles y suma normalmente en los demas casos.
.sum_condicional <- function(x, elegible) {
  elegible <- !is.na(elegible) & elegible
  if (!any(elegible)) return(0)
  valores <- x[elegible]
  if (all(is.na(valores))) return(NA_real_)
  sum(valores, na.rm = TRUE)
}

.any_condicional <- function(x, elegible) {
  elegible <- !is.na(elegible) & elegible
  if (!any(elegible)) return(FALSE)
  valores <- x[elegible]
  if (all(is.na(valores))) return(NA)
  any(valores, na.rm = TRUE)
}


#' Procesar variables adicionales de cuidado y capacidad del hogar
#'
#' Construye indicadores individuales auxiliares, los agrega por hogar y une
#' los resultados nuevamente a cada integrante. Debe ejecutarse despues de
#' `procesar_tiempo()`, `imputa_ingocup()` y
#' `procesar_contribucion_hogar()`.
#'
#' @param data Data frame individual de ENOE ya procesado.
#' @param edad_adulta Edad minima para considerar a una persona adulta.
#' @param edad_adolescente_min Edad minima del grupo adolescente.
#' @param edad_adolescente_max Edad maxima del grupo adolescente.
#' @param umbral_jornada_alta Horas semanales que definen jornada mayor a 40.
#' @param umbral_jornada_muy_alta Horas semanales que definen jornada mayor a 48.
#'
#' @return El mismo data frame individual con variables del hogar anadidas.
#' @export
#' @family procesamiento_enoe
#' @references
#' Escoto, Ana (2026, 4 de junio). *Transversalidad del derecho al cuidado:
#' tensiones y desafios* (ponencia). Mesa 1 del conversatorio *?Una cancha
#' pareja? Escuela, cuidado y fragmentacion de derechos*, El Colegio de Mexico.
#' Transmision oficial: \url{https://www.youtube.com/watch?v=fdpzAe6IYBc}
#'
#' @examples
#' \dontrun{
#' datos <- datos |>
#'   procesar_cuidado_extra()
#' }
procesar_cuidado_extra <- function(
    data,
    edad_adulta = 18,
    edad_adolescente_min = 13,
    edad_adolescente_max = 17,
    umbral_jornada_alta = 40,
    umbral_jornada_muy_alta = 48
) {
  requeridas <- c(
    "anio", "trim", "folio2", "fac", "ent", "edad", "sexo", "clase2",
    "hrsocup", "t_cuidado_directo", "t_cuidado_amplio",
    "jefa_mujer", "jefe_hombre", "h_00_05", "h_06_12", "h_13_17",
    "d_00_05", "d_06_12", "d_13_17"
  )
  faltantes <- setdiff(requeridas, names(data))
  if (length(faltantes) > 0) {
    stop(
      "Faltan variables requeridas en `data`: ",
      paste(faltantes, collapse = ", ")
    )
  }

  # entidad puede existir como etiqueta adicional, pero no es indispensable.
  tiene_entidad <- "entidad" %in% names(data)

  trabajo <- data |>
    dplyr::mutate(
      .adulto = !is.na(edad) & edad >= edad_adulta,
      .mujer = !is.na(sexo) & sexo == 2,
      .hombre = !is.na(sexo) & sexo == 1,
      .adulto_ocupado = .adulto & !is.na(clase2) & clase2 == 1,
      .adulto_no_ocupado = .adulto & !is.na(clase2) & clase2 %in% 2:4,
      .jornada_mas_40 = .adulto_ocupado & !is.na(hrsocup) &
        hrsocup > umbral_jornada_alta,
      .jornada_mas_48 = .adulto_ocupado & !is.na(hrsocup) &
        hrsocup > umbral_jornada_muy_alta,
      .adolescente = !is.na(edad) & dplyr::between(
        edad, edad_adolescente_min, edad_adolescente_max
      ),
      .adolescente_cuidado_amplio_medible = .adolescente &
        !is.na(t_cuidado_amplio),
      .adolescente_cuidado_directo_medible = .adolescente &
        !is.na(t_cuidado_directo),
      .adolescente_cuidado_amplio = .adolescente_cuidado_amplio_medible &
        t_cuidado_amplio > 0,
      .adolescente_cuidado_directo = .adolescente_cuidado_directo_medible &
        t_cuidado_directo > 0
    )

  hogares <- trabajo |>
    dplyr::group_by(anio, trim, folio2) |>
    dplyr::summarise(
      fac_hog = dplyr::first(fac),
      n_personas = dplyr::n(),
      n_adultos = sum(.adulto),
      n_adultos_ocupados = sum(.adulto_ocupado),
      n_adultos_no_ocupados = sum(.adulto_no_ocupado),
      n_mujeres_adultas = sum(.adulto & .mujer),
      n_hombres_adultos = sum(.adulto & .hombre),
      n_mujeres_adultas_ocupadas = sum(.adulto_ocupado & .mujer),
      n_hombres_adultos_ocupados = sum(.adulto_ocupado & .hombre),
      horas_laborales_hogar = .sum_condicional(hrsocup, .adulto_ocupado),
      horas_mujeres_adultas = .sum_condicional(
        hrsocup, .adulto_ocupado & .mujer
      ),
      horas_hombres_adultos = .sum_condicional(
        hrsocup, .adulto_ocupado & .hombre
      ),
      algun_adulto_mas_40 = .any_condicional(
        hrsocup > umbral_jornada_alta, .adulto_ocupado
      ),
      algun_adulto_mas_48 = .any_condicional(
        hrsocup > umbral_jornada_muy_alta, .adulto_ocupado
      ),
      alguna_mujer_mas_40 = .any_condicional(
        hrsocup > umbral_jornada_alta, .adulto_ocupado & .mujer
      ),
      alguna_mujer_mas_48 = .any_condicional(
        hrsocup > umbral_jornada_muy_alta, .adulto_ocupado & .mujer
      ),
      algun_hombre_mas_40 = .any_condicional(
        hrsocup > umbral_jornada_alta, .adulto_ocupado & .hombre
      ),
      algun_hombre_mas_48 = .any_condicional(
        hrsocup > umbral_jornada_muy_alta, .adulto_ocupado & .hombre
      ),
      n_adolescentes = sum(.adolescente),
      n_adolescentes_cuidado_amplio = dplyr::if_else(
        any(.adolescente) & !any(.adolescente_cuidado_amplio_medible),
        NA_integer_, sum(.adolescente_cuidado_amplio)
      ),
      hay_adolescente_cuidado_amplio = dplyr::if_else(
        any(.adolescente) & !any(.adolescente_cuidado_amplio_medible),
        NA, any(.adolescente_cuidado_amplio)
      ),
      horas_cuidado_amplio_adolescentes = .sum_condicional(
        t_cuidado_amplio, .adolescente_cuidado_amplio
      ),
      n_adolescentes_cuidado_directo = dplyr::if_else(
        any(.adolescente) & !any(.adolescente_cuidado_directo_medible),
        NA_integer_, sum(.adolescente_cuidado_directo)
      ),
      hay_adolescente_cuidado_directo = dplyr::if_else(
        any(.adolescente) & !any(.adolescente_cuidado_directo_medible),
        NA, any(.adolescente_cuidado_directo)
      ),
      horas_cuidado_directo_adolescentes = .sum_condicional(
        t_cuidado_directo, .adolescente_cuidado_directo
      ),
      .groups = "drop"
    )

  # Las siguientes variables ya son constantes dentro del hogar porque fueron
  # creadas por procesar_vars_hogar() y procesar_contribucion_hogar(). No se
  # vuelven a unir para evitar sufijos .x/.y.
  hogares <- hogares |>
    dplyr::left_join(
      trabajo |>
        dplyr::summarise(
          h_00_05 = dplyr::first(h_00_05),
          h_06_12 = dplyr::first(h_06_12),
          h_13_17 = dplyr::first(h_13_17),
          d_00_05 = dplyr::first(d_00_05),
          d_06_12 = dplyr::first(d_06_12),
          d_13_17 = dplyr::first(d_13_17),
          jefa_mujer = dplyr::first(jefa_mujer),
          jefe_hombre = dplyr::first(jefe_hombre),
          .by = c(anio, trim, folio2)
        ),
      by = c("anio", "trim", "folio2")
    ) |>
    dplyr::mutate(
      h_escolar = h_06_12 + h_13_17,
      d_escolar = d_06_12 == 1 | d_13_17 == 1,
      todos_adultos_ocupados = n_adultos > 0 &
        n_adultos_ocupados == n_adultos,
      algun_adulto_no_ocupado = n_adultos_no_ocupados > 0,
      hogar_un_adulto = n_adultos == 1,
      hogar_un_adulto_ocupado = n_adultos == 1 & n_adultos_ocupados == 1,
      tipo_presencia_menores = dplyr::case_when(
        d_00_05 == 1 & d_06_12 == 1 & d_13_17 == 1 ~
          "Con 0 a 5, 6 a 12 y 13 a 17",
        d_00_05 == 1 & d_06_12 == 1 ~ "Con 0 a 5 y 6 a 12",
        d_00_05 == 1 & d_13_17 == 1 ~ "Con 0 a 5 y 13 a 17",
        d_06_12 == 1 & d_13_17 == 1 ~ "Con 6 a 12 y 13 a 17",
        d_00_05 == 1 ~ "Solo con 0 a 5",
        d_06_12 == 1 ~ "Solo con 6 a 12",
        d_13_17 == 1 ~ "Solo con 13 a 17",
        TRUE ~ "Sin menores de 0 a 17"
      ),
      baja_capacidad_absorcion = d_escolar &
        todos_adultos_ocupados & algun_adulto_mas_40,
      muy_baja_capacidad_absorcion = d_escolar &
        todos_adultos_ocupados & algun_adulto_mas_48,
      presion_alta_un_adulto = d_escolar & hogar_un_adulto_ocupado,
      jefatura = dplyr::case_when(
        jefa_mujer == 1 ~ "Jefatura femenina",
        jefe_hombre == 1 ~ "Jefatura masculina",
        TRUE ~ "Sin clasificar"
      )
    ) |>
    dplyr::select(-jefa_mujer, -jefe_hombre, -dplyr::starts_with("h_0"),
                  -h_13_17, -dplyr::starts_with("d_0"), -d_13_17)

  resultado <- trabajo |>
    dplyr::select(-dplyr::starts_with(".")) |>
    dplyr::left_join(hogares, by = c("anio", "trim", "folio2"))

  resultado <- resultado |>
    sjlabelled::var_labels(
      fac_hog = "Factor de expansion del hogar",
      n_personas = "Numero de personas registradas en el hogar",
      n_adultos = "Numero de personas adultas en el hogar",
      n_adultos_ocupados = "Numero de personas adultas ocupadas",
      n_adultos_no_ocupados = "Numero de personas adultas no ocupadas",
      n_mujeres_adultas = "N\u00FAmero de mujeres adultas en el hogar",
      n_hombres_adultos = "N\u00FAmero de hombres adultos en el hogar",
      n_mujeres_adultas_ocupadas = "N\u00FAmero de mujeres adultas ocupadas en el hogar",
      n_hombres_adultos_ocupados = "N\u00FAmero de hombres adultos ocupados en el hogar",
      horas_laborales_hogar = "Horas semanales de trabajo remunerado de adultos del hogar",
      horas_mujeres_adultas = "Horas semanales de trabajo remunerado de mujeres adultas ocupadas",
      horas_hombres_adultos = "Horas semanales de trabajo remunerado de hombres adultos ocupados",
      algun_adulto_mas_40 = "Hogar con al menos una persona adulta ocupada que trabaja m\u00E1s de 40 horas semanales",
      algun_adulto_mas_48 = "Hogar con al menos una persona adulta ocupada que trabaja m\u00E1s de 48 horas semanales",
      alguna_mujer_mas_40 = "Hogar con al menos una mujer adulta ocupada que trabaja m\u00E1s de 40 horas semanales",
      alguna_mujer_mas_48 = "Hogar con al menos una mujer adulta ocupada que trabaja m\u00E1s de 48 horas semanales",
      algun_hombre_mas_40 = "Hogar con al menos un hombre adulto ocupado que trabaja m\u00E1s de 40 horas semanales",
      algun_hombre_mas_48 = "Hogar con al menos un hombre adulto ocupado que trabaja m\u00E1s de 48 horas semanales",
      n_adolescentes = "N\u00FAmero de adolescentes de 13 a 17 a\u00F1os en el hogar",
      n_adolescentes_cuidado_amplio = "N\u00FAmero de adolescentes de 13 a 17 a\u00F1os con cuidado amplio sin pago",
      hay_adolescente_cuidado_amplio = "Hogar con al menos una persona adolescente con cuidado amplio sin pago",
      horas_cuidado_amplio_adolescentes = "Horas de cuidado amplio sin pago realizadas por adolescentes",
      n_adolescentes_cuidado_directo = "N\u00FAmero de adolescentes de 13 a 17 a\u00F1os con cuidado directo sin pago (desde 2013)",
      hay_adolescente_cuidado_directo = "Hogar con al menos una persona adolescente con cuidado directo sin pago (desde 2013)",
      horas_cuidado_directo_adolescentes = "Horas de cuidado directo sin pago realizadas por adolescentes (desde 2013)",
      h_00_05 = "N\u00FAmero de integrantes de 0 a 5 a\u00F1os en el hogar",
      h_06_12 = "N\u00FAmero de integrantes de 6 a 12 a\u00F1os en el hogar",
      h_13_17 = "N\u00FAmero de integrantes de 13 a 17 a\u00F1os en el hogar",
      d_00_05 = "Hogar con al menos una persona de 0 a 5 a\u00F1os",
      d_06_12 = "Hogar con al menos una persona de 6 a 12 a\u00F1os",
      d_13_17 = "Hogar con al menos una persona de 13 a 17 a\u00F1os",
      h_escolar = "N\u00FAmero de integrantes de 6 a 17 a\u00F1os en el hogar",
      d_escolar = "Hogar con poblaci\u00F3n de 6 a 17 a\u00F1os",
      todos_adultos_ocupados = "Todas las personas adultas del hogar est\u00E1n ocupadas",
      algun_adulto_no_ocupado = "Hogar con al menos una persona adulta no ocupada",
      hogar_un_adulto = "Hogar integrado por una sola persona adulta",
      hogar_un_adulto_ocupado = "Hogar con una sola persona adulta y esta se encuentra ocupada",
      tipo_presencia_menores = "Combinaci\u00F3n de grupos de edad de 0 a 17 a\u00F1os presentes en el hogar",
      baja_capacidad_absorcion = "Hogar con poblaci\u00F3n de 6 a 17 a\u00F1os, todas las personas adultas ocupadas y al menos una con jornada mayor de 40 horas semanales (Escoto, 2026)",
      muy_baja_capacidad_absorcion = "Hogar con poblaci\u00F3n de 6 a 17 a\u00F1os, todas las personas adultas ocupadas y al menos una con jornada mayor de 48 horas semanales (Escoto, 2026)",
      presion_alta_un_adulto = "Presion de cuidado en hogar con un solo adulto ocupado",
      jefa_mujer = "Hogar con jefatura femenina",
      jefe_hombre = "Hogar con jefatura masculina",
      jefatura = "Sexo de la jefatura del hogar"
    )

  if (tiene_entidad) {
    # entidad permanece intacta en la base individual.
    resultado$entidad <- data$entidad
  }

  resultado
}
