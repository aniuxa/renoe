###############################################################################
# Armonización longitudinal de carreras en la ENOE
#
# Normaliza y armoniza CS_P14_C en los tres regímenes observados:
#
#   2005-I  a 2012-II : Catálogo de Codificación de Carreras 2005
#   2012-III a 2021-II: CMPE 2011
#   2021-III en adelante: CMPE 2016
#
# Las correspondencias conservan los casos ambiguos en vez de forzar una
# precisión inexistente en el clasificador de origen.
###############################################################################

# Auxiliares -------------------------------------------------------------------

normalizar_trim <- function(x) {
  trim <- suppressWarnings(as.integer(stringr::str_extract(as.character(x), "[1-4]")))

  if (any(is.na(trim))) {
    stop("`trim` contiene valores que no se pueden convertir a 1, 2, 3 o 4.")
  }

  trim
}

identificar_clasificador_carrera <- function(anio, trim) {
  periodo <- as.integer(anio) * 10L + normalizar_trim(trim)

  dplyr::case_when(
    periodo <= 20122L ~ "Carreras 2005",
    periodo <= 20212L ~ "CMPE 2011",
    TRUE              ~ "CMPE 2016"
  )
}

normalizar_codigo_carrera <- function(codigo, clasificador) {
  codigo_chr <- as.character(codigo)
  codigo_chr <- stringr::str_trim(codigo_chr)
  codigo_chr[codigo_chr %in% c("", "NA", "NaN", "NULL")] <- NA_character_
  codigo_chr <- stringr::str_replace(codigo_chr, "\\.0+$", "")

  ancho <- dplyr::if_else(clasificador == "CMPE 2016", 6L, 4L)

  dplyr::if_else(
    is.na(codigo_chr),
    NA_character_,
    stringr::str_pad(codigo_chr, width = ancho, side = "left", pad = "0")
  )
}

etiquetas_arm8 <- c(
  "1" = "Educación",
  "2" = "Artes y humanidades",
  "3" = "Ciencias sociales, administración y derecho",
  "4" = "Ciencias naturales, matemáticas, estadística y TIC",
  "5" = "Ingeniería, manufactura y construcción",
  "6" = "Agronomía y veterinaria",
  "7" = "Salud",
  "8" = "Servicios"
)

etiquetas_arm10 <- c(
  "01" = "Educación",
  "02" = "Artes y humanidades",
  "03" = "Ciencias sociales y derecho",
  "04" = "Administración y negocios",
  "05" = "Ciencias naturales, matemáticas y estadística",
  "06" = "Tecnologías de la información y la comunicación",
  "07" = "Ingeniería, manufactura y construcción",
  "08" = "Agronomía y veterinaria",
  "09" = "Salud",
  "10" = "Servicios"
)

# Correspondencia conservadora del grupo del Catálogo 2005 --------------------
#
# Los grupos ambiguos combinan campos que las clasificaciones posteriores
# separan. No se les fuerza una categoría: quedan con calidad "Ambigua".

mapa_2005 <- tibble::tribble(
  ~grupo_2005, ~campo_arm8, ~campo_arm10, ~calidad,
  "00", "2", "02", "Agregada",
  "01", NA,  NA,   "Ambigua",
  "02", "7", "09", "Agregada",
  "03", NA,  NA,   "Ambigua",
  "04", "2", "02", "Agregada",
  "05", "3", "04", "Agregada",
  "06", "1", "01", "Agregada",
  "07", NA,  NA,   "Ambigua",
  "08", "8", "10", "Agregada",
  "11", "2", "02", "Agregada",
  "12", "4", "05", "Agregada",
  "13", "6", "08", "Agregada",
  "14", "7", "09", "Agregada",
  "15", NA,  NA,   "Ambigua",
  "16", NA,  NA,   "Ambigua",
  "17", "2", "02", "Agregada",
  "18", "3", "04", "Agregada",
  "19", "1", "01", "Agregada",
  "20", NA,  NA,   "Ambigua",
  "21", "8", "10", "Agregada",
  "22", "8", "10", "Agregada",
  "31", NA,  NA,   "Ambigua",
  "32", "4", "05", "Agregada",
  "33", "6", "08", "Agregada",
  "34", "7", "09", "Agregada",
  "35", "2", "02", "Agregada",
  "36", "4", "05", "Agregada",
  "37", "3", "03", "Agregada",
  "38", "2", "02", "Agregada",
  "39", "3", "04", "Agregada",
  "40", "1", "01", "Agregada",
  "41", "5", "07", "Agregada",
  "42", "4", "05", "Agregada",
  "51", NA,  NA,   "Ambigua",
  "52", "4", "05", "Agregada",
  "53", "6", "08", "Agregada",
  "54", "7", "09", "Agregada",
  "55", "2", "02", "Agregada",
  "56", "4", "05", "Agregada",
  "57", "3", "03", "Agregada",
  "58", "2", "02", "Agregada",
  "59", "3", "04", "Agregada",
  "60", "1", "01", "Agregada",
  "61", "5", "07", "Agregada",
  "62", "4", "05", "Agregada",
  # Clave especial de normal básica en el catálogo anterior.
  "71", "1", "01", "Agregada",
  # Grupos especiales: nombre o campo de carrera no especificado.
  "09", NA,  NA,   "No especificada",
  "29", NA,  NA,   "No especificada",
  "49", NA,  NA,   "No especificada",
  "69", NA,  NA,   "No especificada"
)

# Correspondencias por clave completa del Catálogo 2005 -----------------------
#
# Estas reglas tienen prioridad sobre el grupo. Se basan en la descripción de
# cada clave completa del Catálogo de Codificación de Carreras de la ENOE.
# Los códigos realmente mixtos no se incluyen y conservan calidad "Ambigua".

mapa_2005_detalle <- tibble::tribble(
  ~cs_p14_c_canonica, ~campo_arm8_det, ~campo_arm10_det, ~descripcion_2005_arm,
  "3111", "5", "07", "Arquitectura",
  "3112", "5", "07", "Urbanismo y planeación territorial",
  "3119", "5", "07", "Otros estudios de arquitectura y urbanismo",
  "5110", "5", "07", "Arquitectura, urbanismo y diseño en posgrado",
  "2012", "5", "07", "Construcción",
  "2013", "5", "07", "Construcción técnica",
  "2032", "4", "06", "Computación e informática",
  "2033", "4", "06", "Computación e informática",
  "0721", "4", "06", "Computación e informática",
  "3131", "2", "02", "Artes gráficas y diseño gráfico",
  "3121", "2", "02", "Diseño",
  "3122", "2", "02", "Diseño industrial y artesanal",
  "3123", "2", "02", "Diseño de interiores",
  "3124", "2", "02", "Diseño textil y de moda",
  "3129", "2", "02", "Otros estudios de diseño",
  "5120", "2", "02", "Diseño en posgrado",
  "2042", "5", "07", "Electricidad y electrónica",
  "2043", "5", "07", "Electricidad y electrónica",
  "0731", "5", "07", "Electricidad y electrónica",
  "2052", "5", "07", "Mecánica y electromecánica",
  "2053", "5", "07", "Mecánica y mantenimiento",
  "0741", "5", "07", "Mecánica y mantenimiento",
  "2062", "5", "07", "Producción industrial y manufactura",
  "2063", "5", "07", "Producción industrial y manufactura",
  "0751", "5", "07", "Producción industrial y manufactura",
  "0761", "5", "07", "Industria textil y confección",
  "0121", "5", "07", "Industria de la madera",
  "2022", "5", "07", "Minería y extracción",
  "2023", "5", "07", "Minería y metalurgia",
  "2072", "8", "10", "Servicios de transporte",
  "2073", "8", "10", "Servicios de transporte",
  "1612", "3", "03", "Comunicación",
  "1613", "3", "03", "Comunicación y periodismo",
  "1622", "2", "02", "Idiomas",
  "1623", "2", "02", "Idiomas y traducción",
  "0311", "4", "06", "Telecomunicaciones"
)

#' Armonizar carreras de la ENOE entre 2005 y la actualidad
#'
#' Identifica automáticamente el clasificador utilizado en cada observación,
#' normaliza la clave de carrera conservando ceros iniciales y genera campos de
#' formación comparables. Reconoce el Catálogo de Carreras 2005 hasta 2012-II,
#' la CMPE 2011 entre 2012-III y 2021-II y la CMPE 2016 desde 2021-III.
#'
#' La función conserva `cs_p14_c` y añade su versión original y canónica. Las
#' equivalencias del catálogo 2005 se aplican primero por clave completa y sólo
#' después por grupo. Las claves que mezclan campos incompatibles permanecen
#' explícitamente como ambiguas.
#'
#' @param data Data frame con `anio`, `trim`, `cs_p13_1` y `cs_p14_c`.
#'
#' @return El mismo data frame, sin cambiar el número de filas, con:
#'   `clasificador_carrera`, `cs_p14_c_original`, `cs_p14_c_canonica`,
#'   `nivel_carrera`, `grupo_2005`, `campo_cmpe2011`, `campo_cmpe2016`,
#'   `campo_arm8`, `campo_arm8_desc`, `campo_arm10`, `campo_arm10_desc`,
#'   `descripcion_2005_arm`, `calidad_armonizacion`, `elegible_carrera`,
#'   `tiene_codigo_carrera` y `cobertura_arm8`.
#'
#' @details
#' `campo_arm8` es la clasificación recomendada para comparaciones de toda la
#' serie. `campo_arm10` conserva mayor cercanía con los diez campos amplios de
#' la CMPE 2016. Los códigos se manejan como texto para preservar sus ceros.
#'
#' `campo_arm8` puede emplearse posteriormente para estudiar desajuste
#' horizontal, comparándolo con la ocupación. Esta función no calcula ese
#' indicador. El archivo `correspondencia_arm8_isco08_montt.csv` reproduce la
#' tabla normativa internacional de Montt (2015). El archivo
#' `correspondencia_campo_arm8_sinco3d.csv` contiene la adaptación mexicana
#' propuesta y versionada. Sus filas con `estado_revision = "propuesta"`
#' requieren validación sustantiva antes de considerarse definitivas.
#'
#' @references
#' Montt, G. (2015). The causes and consequences of field-of-study mismatch:
#' An analysis using PIAAC. OECD Social, Employment and Migration Working
#' Papers, No. 167. \doi{10.1787/5jrxm4dhv9r2-en}
#'
#' Wolbers, M. H. J. (2003). Job mismatches and their labour-market effects
#' among school-leavers in Europe. European Sociological Review, 19(3),
#' 249-266. \doi{10.1093/esr/19.3.249}
#'
#' International Labour Organization. Education and Mismatch Indicators.
#' \url{https://ilostat.ilo.org/methods/concepts-and-definitions/description-education-and-mismatch-indicators/}
#'
#' Somers, M. A., Cabus, S. J., Groot, W., and van den Brink, H. M. (2019).
#' Horizontal mismatch between employment and field of education: Evidence
#' from a systematic literature review. Journal of Economic Surveys, 33(2),
#' 567-603. \doi{10.1111/joes.12271}
#'
#' @export
#' @family procesamiento_enoe
#'
#' @examples
#' datos <- data.frame(
#'   anio = c(2012, 2012, 2021),
#'   trim = c("t2", "t3", "t3"),
#'   cs_p13_1 = c(7, 7, 7),
#'   cs_p14_c = c("3111", "5335", 41400)
#' )
#' armonizar_carreras_enoe(datos)
armonizar_carreras_enoe <- function(data) {
  str_sub <- stringr::str_sub

  requeridas <- c("anio", "trim", "cs_p13_1", "cs_p14_c")
  faltantes <- setdiff(requeridas, names(data))

  if (length(faltantes) > 0L) {
    stop("Faltan variables requeridas: ", paste(faltantes, collapse = ", "))
  }

  n_inicial <- nrow(data)

  resultado <- data |>
    mutate(
      clasificador_carrera = identificar_clasificador_carrera(anio, trim),
      cs_p14_c_original = as.character(cs_p14_c),
      cs_p14_c_canonica = normalizar_codigo_carrera(
        cs_p14_c,
        clasificador_carrera
      ),
      nivel_carrera = suppressWarnings(as.integer(cs_p13_1)),
      grupo_2005 = if_else(
        clasificador_carrera == "Carreras 2005",
        str_sub(cs_p14_c_canonica, 1L, 2L),
        NA_character_
      ),
      campo_cmpe2011 = if_else(
        clasificador_carrera == "CMPE 2011",
        str_sub(cs_p14_c_canonica, 2L, 4L),
        NA_character_
      ),
      campo_cmpe2016 = if_else(
        clasificador_carrera == "CMPE 2016",
        cs_p14_c_canonica,
        NA_character_
      )
    )

  # El emparejamiento es muchos-a-uno por grupo y no debe aumentar filas.
  resultado <- resultado |>
    left_join(mapa_2005, by = "grupo_2005") |>
    left_join(mapa_2005_detalle, by = "cs_p14_c_canonica") |>
    mutate(
      # CMPE 2011: campo amplio de 8 categorías.
      campo_arm8 = case_when(
        clasificador_carrera == "CMPE 2011" ~ str_sub(campo_cmpe2011, 1L, 1L),
        clasificador_carrera == "CMPE 2016" ~ case_when(
          str_sub(campo_cmpe2016, 1L, 2L) == "01" ~ "1",
          str_sub(campo_cmpe2016, 1L, 2L) == "02" ~ "2",
          str_sub(campo_cmpe2016, 1L, 2L) %in% c("03", "04") ~ "3",
          str_sub(campo_cmpe2016, 1L, 2L) %in% c("05", "06") ~ "4",
          str_sub(campo_cmpe2016, 1L, 2L) == "07" ~ "5",
          str_sub(campo_cmpe2016, 1L, 2L) == "08" ~ "6",
          str_sub(campo_cmpe2016, 1L, 2L) == "09" ~ "7",
          str_sub(campo_cmpe2016, 1L, 2L) == "10" ~ "8",
          TRUE ~ NA_character_
        ),
        clasificador_carrera == "Carreras 2005" & !is.na(campo_arm8_det) ~
          campo_arm8_det,
        TRUE ~ campo_arm8
      ),
      # CMPE 2011 puede llevarse a los 10 campos usando su campo específico.
      campo_arm10 = case_when(
        clasificador_carrera == "CMPE 2011" ~ case_when(
          str_sub(campo_cmpe2011, 1L, 1L) == "1" ~ "01",
          str_sub(campo_cmpe2011, 1L, 1L) == "2" ~ "02",
          str_sub(campo_cmpe2011, 1L, 2L) %in% c("31", "32", "34") ~ "03",
          str_sub(campo_cmpe2011, 1L, 2L) == "33" ~ "04",
          str_sub(campo_cmpe2011, 1L, 2L) %in% c("41", "42", "43") ~ "05",
          str_sub(campo_cmpe2011, 1L, 2L) == "44" ~ "06",
          str_sub(campo_cmpe2011, 1L, 1L) == "5" ~ "07",
          str_sub(campo_cmpe2011, 1L, 1L) == "6" ~ "08",
          str_sub(campo_cmpe2011, 1L, 1L) == "7" ~ "09",
          str_sub(campo_cmpe2011, 1L, 1L) == "8" ~ "10",
          TRUE ~ NA_character_
        ),
        clasificador_carrera == "CMPE 2016" ~ str_sub(campo_cmpe2016, 1L, 2L),
        clasificador_carrera == "Carreras 2005" & !is.na(campo_arm10_det) ~
          campo_arm10_det,
        TRUE ~ campo_arm10
      ),
      calidad_armonizacion = case_when(
        is.na(cs_p14_c_canonica) ~ "Sin información de carrera",
        cs_p14_c_canonica %in% c("9999", "999999") ~ "No especificada",
        clasificador_carrera == "Carreras 2005" & !is.na(campo_arm8_det) ~
          "Exacta por clave detallada",
        clasificador_carrera == "Carreras 2005" & !is.na(calidad) ~ calidad,
        clasificador_carrera == "Carreras 2005" ~ "Sin correspondencia",
        !is.na(campo_arm8) ~ "Exacta a nivel amplio",
        TRUE ~ "Sin correspondencia"
      ),
      campo_arm8_desc = unname(etiquetas_arm8[campo_arm8]),
      campo_arm10_desc = unname(etiquetas_arm10[campo_arm10]),
      elegible_carrera = nivel_carrera %in% 5:9,
      tiene_codigo_carrera = !is.na(cs_p14_c_canonica),
      cobertura_arm8 = elegible_carrera & !is.na(campo_arm8)
    ) |>
    select(-calidad, -campo_arm8_det, -campo_arm10_det)

  if (nrow(resultado) != n_inicial) {
    stop(
      "La armonización cambió el número de filas: ",
      n_inicial, " -> ", nrow(resultado), "."
    )
  }

  resultado |>
    sjlabelled::var_labels(
      clasificador_carrera = "Clasificador de carreras utilizado por la ENOE",
      cs_p14_c_original = "Clave de carrera tal como fue recibida",
      cs_p14_c_canonica = "Clave de carrera normalizada como texto",
      nivel_carrera = "Nivel educativo asociado a la carrera",
      grupo_2005 = "Grupo del Catálogo de Carreras 2005",
      campo_cmpe2011 = "Campo detallado de la CMPE 2011",
      campo_cmpe2016 = "Clave de campo de la CMPE 2016",
      campo_arm8 = "Campo de formación armonizado en ocho categorías",
      campo_arm8_desc = "Descripción del campo armonizado en ocho categorías",
      campo_arm10 = "Campo de formación armonizado en diez categorías",
      campo_arm10_desc = "Descripción del campo armonizado en diez categorías",
      descripcion_2005_arm = "Descripción resumida de la equivalencia del Catálogo 2005",
      calidad_armonizacion = "Calidad de la armonización de la carrera",
      elegible_carrera = "Nivel educativo elegible para registrar carrera",
      tiene_codigo_carrera = "Cuenta con código de carrera",
      cobertura_arm8 = "Cuenta con campo armonizado de ocho categorías"
    )
}
