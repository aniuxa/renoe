###############################################################################
# Armonizacion longitudinal de carreras en la ENOE
#
# Normaliza y armoniza CS_P14_C en los tres regimenes observados:
#
#   2005-I  a 2012-II : Catalogo de Codificacion de Carreras 2005
#   2012-III a 2021-II: CMPE 2011
#   2021-III en adelante: CMPE 2016
#
# Las correspondencias conservan los casos ambiguos en vez de forzar una
# precision inexistente en el clasificador de origen.
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

consenso_arm8_destinos <- function(x) {
  vapply(x, function(destinos) {
    if (is.na(destinos) || !nzchar(destinos)) {
      return(NA_character_)
    }
    codigos <- unlist(strsplit(destinos, "\\|", fixed = FALSE))
    codigos <- codigos[grepl("^[1-8][0-9]{2}$", codigos)]
    campos <- unique(substr(codigos, 1L, 1L))
    if (length(campos) == 1L) campos else NA_character_
  }, character(1L), USE.NAMES = FALSE)
}

calcular_cobertura_arm8_anual <- function(data) {
  peso <- if ("fac" %in% names(data)) {
    suppressWarnings(as.numeric(data$fac))
  } else {
    rep(1, nrow(data))
  }
  peso[is.na(peso) | peso < 0] <- 0

  horizontal_disponible <- all(c("clase2", "sinco3d") %in% names(data))
  ocupado <- if (horizontal_disponible) {
    suppressWarnings(as.integer(as.character(data$clase2))) == 1L
  } else {
    rep(FALSE, nrow(data))
  }
  sinco_txt <- if (horizontal_disponible) {
    stringr::str_pad(
      as.character(suppressWarnings(as.integer(as.character(data$sinco3d)))),
      3L,
      "left",
      "0"
    )
  } else {
    rep(NA_character_, nrow(data))
  }
  sinco_valido <- !is.na(sinco_txt) &
    stringr::str_detect(sinco_txt, "^[1-9][0-9]{2}$") & sinco_txt != "999"

  indice <- data$en_serie_historica_horizontal & data$elegible_carrera
  base <- tibble::tibble(
    anio = suppressWarnings(as.integer(as.character(data$anio[indice]))),
    peso = peso[indice],
    clasificado = data[[if (
      "campo_arm8_horizontal" %in% names(data)
    ) "campo_arm8_horizontal" else "campo_arm8_cmpe2011"]][indice] %in%
      as.character(1:8),
    universo_horizontal = ocupado[indice],
    sinco_valido = sinco_valido[indice]
  )

  if (!nrow(base)) {
    return(tibble::tibble(
      anio = integer(),
      registros_elegibles = integer(),
      cobertura_arm8 = numeric(),
      pct_no_clasificado_arm8 = numeric(),
      cobertura_horizontal = numeric(),
      pct_no_clasificado_horizontal = numeric(),
      tipo_estimacion = character()
    ))
  }

  base |>
    dplyr::group_by(anio) |>
    dplyr::summarise(
      registros_elegibles = dplyr::n(),
      poblacion_elegible = sum(peso),
      poblacion_clasificada = sum(peso[clasificado]),
      cobertura_arm8 = dplyr::if_else(
        poblacion_elegible > 0,
        100 * poblacion_clasificada / poblacion_elegible,
        NA_real_
      ),
      pct_no_clasificado_arm8 = 100 - cobertura_arm8,
      poblacion_universo_horizontal = sum(peso[universo_horizontal]),
      poblacion_horizontal_clasificable = sum(
        peso[universo_horizontal & clasificado & sinco_valido]
      ),
      cobertura_horizontal = dplyr::if_else(
        horizontal_disponible & poblacion_universo_horizontal > 0,
        100 * poblacion_horizontal_clasificable /
          poblacion_universo_horizontal,
        NA_real_
      ),
      pct_no_clasificado_horizontal = 100 - cobertura_horizontal,
      tipo_estimacion = if ("fac" %in% names(data)) "ponderada_fac" else "registros",
      .groups = "drop"
    )
}

emitir_advertencia_cobertura_arm8 <- function(cobertura, perfil) {
  if (!nrow(cobertura)) {
    warning(
      "La aplicaci\u00F3n no contiene observaciones dentro de la serie hist\u00F3rica ",
      "recomendada (2014-III en adelante).",
      call. = FALSE,
      immediate. = TRUE
    )
    return(invisible(NULL))
  }

  if (all(is.na(cobertura$cobertura_horizontal))) {
    detalle <- sprintf(
      "%s: ARM8 %.2f%% clasificado (%.2f%% sin clasificar)",
      cobertura$anio,
      cobertura$cobertura_arm8,
      cobertura$pct_no_clasificado_arm8
    )
  } else {
    detalle <- sprintf(
      paste0(
        "%s: ARM8 %.2f%% clasificado; horizontal %.2f%% clasificado ",
        "(%.2f%% sin clasificar)"
      ),
      cobertura$anio,
      cobertura$cobertura_arm8,
      cobertura$cobertura_horizontal,
      cobertura$pct_no_clasificado_horizontal
    )
  }
  detalle[cobertura$anio == 2014L] <- sub(
    "^2014:", "2014 (desde III):", detalle[cobertura$anio == 2014L]
  )
  warning(
    "Cobertura del perfil '", perfil,
    "' para la serie 2014-III+:\n", paste(detalle, collapse = "\n"),
    "\nAdvertencia de circularidad: no use SINCO observado o imputado ",
    "para completar carreras que despu\u00E9s se comparar\u00E1n con SINCO. El lookup ",
    "integrado de carreras no usa ocupaci\u00F3n ni SINCO.",
    call. = FALSE,
    immediate. = TRUE
  )
  invisible(NULL)
}

cargar_cobertura_carreras_cmpe2011 <- function() {
  archivo <- "correspondencia_carreras_cmpe2011_cobertura.csv"
  candidatas <- c(
    file.path("inst", "extdata", archivo),
    system.file("extdata", archivo, package = "renoe")
  )
  candidatas <- candidatas[nzchar(candidatas) & file.exists(candidatas)]

  if (!length(candidatas)) {
    stop("No se encontr\u00F3 `", archivo, "` en inst/extdata.", call. = FALSE)
  }

  tabla <- readr::read_csv(
    candidatas[[1L]],
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  numericas <- c(
    "soporte_codigo_n",
    "soporte_codigo_poblacion",
    "proporcion_modal_codigo_registros",
    "proporcion_modal_codigo_poblacion",
    "paneles_soporte_codigo"
  )
  tabla[numericas] <- lapply(tabla[numericas], as.numeric)

  if (anyDuplicated(paste(tabla$clasificador_carrera, tabla$codigo_fuente))) {
    stop("La tabla de cobertura contiene c\u00F3digos duplicados.", call. = FALSE)
  }

  tabla
}

etiquetas_arm8 <- c(
  "1" = "Educaci\u00F3n",
  "2" = "Artes y humanidades",
  "3" = "Ciencias sociales, administraci\u00F3n y derecho",
  "4" = "Ciencias naturales, matem\u00E1ticas, estad\u00EDstica y TIC",
  "5" = "Ingenier\u00EDa, manufactura y construcci\u00F3n",
  "6" = "Agronom\u00EDa y veterinaria",
  "7" = "Salud",
  "8" = "Servicios"
)

etiquetas_arm10 <- c(
  "01" = "Educaci\u00F3n",
  "02" = "Artes y humanidades",
  "03" = "Ciencias sociales y derecho",
  "04" = "Administraci\u00F3n y negocios",
  "05" = "Ciencias naturales, matem\u00E1ticas y estad\u00EDstica",
  "06" = "Tecnolog\u00EDas de la informaci\u00F3n y la comunicaci\u00F3n",
  "07" = "Ingenier\u00EDa, manufactura y construcci\u00F3n",
  "08" = "Agronom\u00EDa y veterinaria",
  "09" = "Salud",
  "10" = "Servicios"
)

# Correspondencia conservadora del grupo del Catalogo 2005 --------------------
#
# Los grupos ambiguos combinan campos que las clasificaciones posteriores
# separan. No se les fuerza una categoria: quedan con calidad "Ambigua".

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
  # Clave especial de normal basica en el catalogo anterior.
  "71", "1", "01", "Agregada",
  # Grupos especiales: nombre o campo de carrera no especificado.
  "09", NA,  NA,   "No especificada",
  "29", NA,  NA,   "No especificada",
  "49", NA,  NA,   "No especificada",
  "69", NA,  NA,   "No especificada"
)

# Correspondencias por clave completa del Catalogo 2005 -----------------------
#
# Estas reglas tienen prioridad sobre el grupo. Se basan en la descripcion de
# cada clave completa del Catalogo de Codificacion de Carreras de la ENOE.
# Los codigos realmente mixtos no se incluyen y conservan calidad "Ambigua".

mapa_2005_detalle <- tibble::tribble(
  ~cs_p14_c_canonica, ~campo_arm8_det, ~campo_arm10_det, ~descripcion_2005_arm,
  "3111", "5", "07", "Arquitectura",
  "3112", "5", "07", "Urbanismo y planeaci\u00F3n territorial",
  "3119", "5", "07", "Otros estudios de arquitectura y urbanismo",
  "5110", "5", "07", "Arquitectura, urbanismo y dise\u00F1o en posgrado",
  "2012", "5", "07", "Construcci\u00F3n",
  "2013", "5", "07", "Construcci\u00F3n t\u00E9cnica",
  "2032", "4", "06", "Computaci\u00F3n e inform\u00E1tica",
  "2033", "4", "06", "Computaci\u00F3n e inform\u00E1tica",
  "0721", "4", "06", "Computaci\u00F3n e inform\u00E1tica",
  "3131", "2", "02", "Artes gr\u00E1ficas y dise\u00F1o gr\u00E1fico",
  "3121", "2", "02", "Dise\u00F1o",
  "3122", "2", "02", "Dise\u00F1o industrial y artesanal",
  "3123", "2", "02", "Dise\u00F1o de interiores",
  "3124", "2", "02", "Dise\u00F1o textil y de moda",
  "3129", "2", "02", "Otros estudios de dise\u00F1o",
  "5120", "2", "02", "Dise\u00F1o en posgrado",
  "2042", "5", "07", "Electricidad y electr\u00F3nica",
  "2043", "5", "07", "Electricidad y electr\u00F3nica",
  "0731", "5", "07", "Electricidad y electr\u00F3nica",
  "2052", "5", "07", "Mec\u00E1nica y electromec\u00E1nica",
  "2053", "5", "07", "Mec\u00E1nica y mantenimiento",
  "0741", "5", "07", "Mec\u00E1nica y mantenimiento",
  "2062", "5", "07", "Producci\u00F3n industrial y manufactura",
  "2063", "5", "07", "Producci\u00F3n industrial y manufactura",
  "0751", "5", "07", "Producci\u00F3n industrial y manufactura",
  "0761", "5", "07", "Industria textil y confecci\u00F3n",
  "0121", "5", "07", "Industria de la madera",
  "2022", "5", "07", "Miner\u00EDa y extracci\u00F3n",
  "2023", "5", "07", "Miner\u00EDa y metalurgia",
  "2072", "8", "10", "Servicios de transporte",
  "2073", "8", "10", "Servicios de transporte",
  "1612", "3", "03", "Comunicaci\u00F3n",
  "1613", "3", "03", "Comunicaci\u00F3n y periodismo",
  "1622", "2", "02", "Idiomas",
  "1623", "2", "02", "Idiomas y traducci\u00F3n",
  "0311", "4", "06", "Telecomunicaciones"
)

#' Armonizar carreras de la ENOE entre 2005 y la actualidad
#'
#' Identifica automaticamente el clasificador utilizado en cada observacion,
#' normaliza la clave de carrera conservando ceros iniciales y genera campos de
#' formacion comparables. Reconoce el Catalogo de Carreras 2005 hasta 2012-II,
#' la CMPE 2011 entre 2012-III y 2021-II y la CMPE 2016 desde 2021-III.
#'
#' La funcion conserva `cs_p14_c` y anade su version original y canonica. Las
#' equivalencias del catalogo 2005 se aplican primero por clave completa y solo
#' despues por grupo. Las claves que mezclan campos incompatibles permanecen
#' explicitamente como ambiguas.
#'
#' @param data Data frame con `anio`, `trim`, `cs_p13_1` y `cs_p14_c`.
#' @param usar_puente_2005 Si es `TRUE`, aplica los destinos preferentes
#'   aceptados del puente Carreras 2005 -> CMPE 2011. El valor predeterminado
#'   es `FALSE`, porque para desajuste horizontal se recomienda comenzar en
#'   2012-III. Aun con `FALSE`, la funcion informa la evidencia disponible por
#'   codigo para facilitar futuras revisiones y desempates.
#' @param perfil Ruta de evidencia que puede integrarse: `"oficial"` conserva
#'   unicamente la CMPE 2011 observada; `"panel_validado"` anade los destinos
#'   aceptados por panel; `"experimental"` anade consensos a ARM8 cuando todos
#'   los destinos detallados plausibles pertenecen al mismo campo. El perfil
#'   experimental no inventa una carrera CMPE 2011 detallada.
#' @param advertir_cobertura Si es `TRUE`, valor predeterminado, emite en cada
#'   aplicacion una advertencia con la cobertura ARM8 por ano dentro de la
#'   serie historica recomendada desde 2014-III. Usa `fac` cuando esta
#'   disponible y registros en caso contrario.
#' @param salida `"auditable"` conserva todas las columnas de evidencia;
#'   `"compacta"` conserva las variables recibidas y el contrato minimo de
#'   calidad, regla, fase, evidencia, granularidad, auxiliares y pendientes.
#'
#' @return El mismo data frame, sin cambiar el numero de filas, con:
#'   `clasificador_carrera`, `cs_p14_c_original`, `cs_p14_c_canonica`,
#'   `nivel_carrera`, `grupo_2005`, `campo_cmpe2011`, `campo_cmpe2016`,
#'   `campo_arm8`, `campo_arm8_desc`, `campo_arm10`, `campo_arm10_desc`,
#'   `descripcion_2005_arm`, `calidad_armonizacion`, `elegible_carrera`,
#'   `tiene_codigo_carrera`, `cobertura_arm8`, `carrera_cmpe2011`,
#'   `campo_arm8_cmpe2011`, `campo_arm8_horizontal`,
#'   `fuente_arm8_horizontal`, `nivel_cobertura_codigo`, proporciones modales,
#'   soporte, destinos posibles, `perfil_armonizacion_carrera`, fuente y nivel
#'   del resultado, `carrera_asistida_por_ocupacion`,
#'   `apta_carrera_para_desajuste_horizontal` y `uso_desajuste_horizontal`.
#'
#' @details
#' `campo_arm8` conserva la salida historica de la funcion. Para el desajuste
#' horizontal se recomienda `campo_arm8_horizontal`: usa CMPE 2011 nativa y,
#' desde 2021-III, agrega directamente el prefijo oficial de CMPE 2016 a ARM8,
#' sin imputar una carrera detallada CMPE 2011 ni usar ocupacion. La variable
#' `campo_arm8_cmpe2011` se conserva para analisis que si requieren el puente
#' detallado entre clasificadores. `campo_arm10` conserva mayor
#' cercania con los diez campos amplios de la CMPE 2016. Los codigos se manejan
#' como texto para preservar sus ceros.
#'
#' `nivel_cobertura_codigo` distingue catalogo nativo, puente con proporciones
#' modales de 80% o mas, puente aceptado entre 65% y 79%, y codigos sin regla
#' aceptada. Las proporciones son evidencia empirica del destino modal por
#' codigo, no probabilidades individuales ni equivalencias oficiales.
#'
#' `campo_arm8` puede emplearse posteriormente para estudiar desajuste
#' horizontal, comparandolo con la ocupacion. Esta funcion no calcula ese
#' indicador. El archivo `correspondencia_arm8_isco08_montt.csv` reproduce la
#' tabla normativa internacional de Montt (2015). El archivo
#' `correspondencia_campo_arm8_sinco3d.csv` contiene la adaptacion mexicana
#' aceptada y versionada. La version 1.0.0 conserva 216 relaciones con
#' `estado_revision = "aceptada"`; el indicador rechaza matrices propuestas.
#'
#' La ruta integrada de carreras se construyo sin ocupacion, CMO ni SINCO. No
#' deben usarse codigos SINCO observados o imputados para completar una carrera
#' que despues se comparara con SINCO: ello introduce circularidad mecanica.
#' Aun una regla asistida por SINCO observado debera marcar
#' `carrera_asistida_por_ocupacion = TRUE` y excluirse del indicador principal;
#' solo podra presentarse como sensibilidad. El orden canonico del proceso es
#' armonizar carreras, armonizar SCIAN y finalmente armonizar SINCO. SCIAN puede
#' auxiliar la ultima etapa si se conserva la procedencia, pero la carrera no
#' puede escoger el SINCO usado por el indicador principal.
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
#' armonizar_carreras_enoe(datos, usar_puente_2005 = TRUE)
#' armonizar_carreras_enoe(datos, perfil = "experimental")
armonizar_carreras_enoe <- function(
    data,
    usar_puente_2005 = FALSE,
    perfil = c("panel_validado", "oficial", "experimental"),
    advertir_cobertura = TRUE,
    salida = c("auditable", "compacta")) {
  str_sub <- stringr::str_sub

  perfil <- match.arg(perfil)
  salida <- match.arg(salida)

  if (!is.logical(advertir_cobertura) || length(advertir_cobertura) != 1L ||
      is.na(advertir_cobertura)) {
    stop("`advertir_cobertura` debe ser TRUE o FALSE.", call. = FALSE)
  }

  if (!is.logical(usar_puente_2005) || length(usar_puente_2005) != 1L ||
      is.na(usar_puente_2005)) {
    stop("`usar_puente_2005` debe ser TRUE o FALSE.", call. = FALSE)
  }

  requeridas <- c("anio", "trim", "cs_p13_1", "cs_p14_c")
  faltantes <- setdiff(requeridas, names(data))

  if (length(faltantes) > 0L) {
    stop("Faltan variables requeridas: ", paste(faltantes, collapse = ", "))
  }

  n_inicial <- nrow(data)
  nombres_entrada <- names(data)

  resultado <- data |>
    mutate(
      periodo_carrera = as.integer(anio) * 10L + normalizar_trim(trim),
      clasificador_carrera = identificar_clasificador_carrera(anio, trim),
      cs_p14_c_original = as.character(cs_p14_c),
      codigo_carrera_sin_relleno = stringr::str_replace(
        stringr::str_trim(as.character(cs_p14_c)),
        "\\.0+$",
        ""
      ),
      codigo_carrera_sin_relleno = if_else(
        codigo_carrera_sin_relleno %in% c("", "NA", "NaN", "NULL"),
        NA_character_,
        codigo_carrera_sin_relleno
      ),
      cs_p14_c_canonica = normalizar_codigo_carrera(
        cs_p14_c,
        clasificador_carrera
      ),
      nivel_observacion_carrera = case_when(
        periodo_carrera >= 20131L & periodo_carrera <= 20142L ~
          "CMPE 2011 agregado: nivel + campo",
        clasificador_carrera == "CMPE 2011" ~ "CMPE 2011 detallado",
        clasificador_carrera == "Carreras 2005" ~ "Carreras 2005 detallado",
        TRUE ~ "CMPE 2016 detallado"
      ),
      nivel_carrera = suppressWarnings(as.integer(cs_p13_1)),
      grupo_2005 = if_else(
        clasificador_carrera == "Carreras 2005",
        str_sub(cs_p14_c_canonica, 1L, 2L),
        NA_character_
      ),
      campo_cmpe2011 = case_when(
        periodo_carrera >= 20131L & periodo_carrera <= 20142L &
          nchar(codigo_carrera_sin_relleno) == 2L ~
          str_sub(codigo_carrera_sin_relleno, 2L, 2L),
        clasificador_carrera == "CMPE 2011" ~
          str_sub(cs_p14_c_canonica, 2L, 4L),
        TRUE ~ NA_character_
      ),
      campo_cmpe2016 = if_else(
        clasificador_carrera == "CMPE 2016",
        cs_p14_c_canonica,
        NA_character_
      ),
      codigo_union_cmpe2011 = if_else(
        clasificador_carrera == "CMPE 2011" & nchar(campo_cmpe2011) == 3L,
        campo_cmpe2011,
        if_else(clasificador_carrera == "CMPE 2011", NA_character_, cs_p14_c_canonica)
      )
    )

  # El emparejamiento es muchos-a-uno por grupo y no debe aumentar filas.
  resultado <- resultado |>
    left_join(mapa_2005, by = "grupo_2005") |>
    left_join(mapa_2005_detalle, by = "cs_p14_c_canonica") |>
    left_join(
      cargar_cobertura_carreras_cmpe2011(),
      by = c(
        "clasificador_carrera",
        "codigo_union_cmpe2011" = "codigo_fuente"
      )
    ) |>
    mutate(
      # CMPE 2011: campo amplio de 8 categorias.
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
      # CMPE 2011 puede llevarse a los 10 campos usando su campo especifico.
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
      arm8_consenso_lookup = consenso_arm8_destinos(
        destinos_cmpe2011_posibles
      ),
      perfil_habilita_panel = perfil %in% c("panel_validado", "experimental"),
      perfil_habilita_experimental = perfil == "experimental",
      puente_2005_aplicado =
        clasificador_carrera == "Carreras 2005" &
        usar_puente_2005 &
        perfil_habilita_panel &
        fase_correspondencia_cmpe2011 == "accepted" &
        !is.na(carrera_cmpe2011_lookup),
      carrera_cmpe2011 = case_when(
        clasificador_carrera == "CMPE 2011" ~ campo_cmpe2011,
        clasificador_carrera == "CMPE 2016" &
          perfil_habilita_panel &
          fase_correspondencia_cmpe2011 == "accepted" ~
          carrera_cmpe2011_lookup,
        puente_2005_aplicado ~ carrera_cmpe2011_lookup,
        TRUE ~ NA_character_
      ),
      campo_arm8_cmpe2011 = case_when(
        clasificador_carrera == "CMPE 2011" &
          campo_arm8 %in% as.character(1:8) ~ campo_arm8,
        !is.na(carrera_cmpe2011) ~ arm8_cmpe2011_lookup,
        perfil_habilita_experimental & clasificador_carrera == "CMPE 2016" ~
          arm8_consenso_lookup,
        perfil_habilita_experimental & clasificador_carrera == "Carreras 2005" &
          usar_puente_2005 ~ arm8_consenso_lookup,
        TRUE ~ NA_character_
      ),
      # El consumidor horizontal requiere ARM8, no una carrera CMPE 2011
      # detallada. En CMPE 2016 el prefijo de dos posiciones permite una
      # agregacion documental directa a ARM8, sin imputacion ni uso de SINCO.
      campo_arm8_horizontal = case_when(
        clasificador_carrera == "CMPE 2011" &
          campo_arm8 %in% as.character(1:8) ~ campo_arm8,
        clasificador_carrera == "CMPE 2016" &
          campo_arm8 %in% as.character(1:8) ~ campo_arm8,
        clasificador_carrera == "Carreras 2005" & usar_puente_2005 ~
          campo_arm8_cmpe2011,
        TRUE ~ NA_character_
      ),
      fuente_arm8_horizontal = case_when(
        clasificador_carrera == "CMPE 2011" &
          !is.na(campo_arm8_horizontal) ~ "cmpe2011_nativo",
        clasificador_carrera == "CMPE 2016" &
          !is.na(campo_arm8_horizontal) ~ "cmpe2016_nativo_agregado",
        clasificador_carrera == "Carreras 2005" &
          !is.na(campo_arm8_horizontal) ~ "puente_2005_cmpe2011",
        TRUE ~ "sin_clasificar"
      ),
      perfil_armonizacion_carrera = perfil,
      # El lookup integrado se estimo sin ocupacion, CMO ni SINCO. Esta bandera
      # debe cambiar a TRUE si una ruta futura introduce asistencia ocupacional.
      carrera_asistida_por_ocupacion = FALSE,
      en_serie_historica_horizontal = periodo_carrera >= 20143L,
      segmento_serie_horizontal = case_when(
        periodo_carrera < 20143L ~ "fuera_serie_principal",
        periodo_carrera <= 20212L ~ "cmpe2011_detallado_2014III_2021II",
        TRUE ~ "cmpe2016_cambio_clasificador_2021III_mas"
      ),
      fuente_armonizacion_carrera = case_when(
        is.na(cs_p14_c_canonica) ~ "sin_informacion",
        clasificador_carrera == "CMPE 2011" & !is.na(campo_arm8_cmpe2011) ~
          "oficial_observada",
        !is.na(carrera_cmpe2011) ~ "panel_validado",
        perfil_habilita_experimental & !is.na(campo_arm8_cmpe2011) ~
          "consenso_arm8_experimental",
        TRUE ~ "sin_regla_integrada"
      ),
      nivel_resultado_carrera = case_when(
        !is.na(carrera_cmpe2011) & nchar(carrera_cmpe2011) == 3L ~
          "cmpe2011_detallado",
        !is.na(campo_arm8_horizontal) ~ "arm8",
        TRUE ~ "sin_clasificar"
      ),
      nivel_cobertura_codigo = case_when(
        is.na(cs_p14_c_canonica) ~ "sin_informacion",
        nivel_observacion_carrera == "CMPE 2011 agregado: nivel + campo" &
          !is.na(campo_arm8) ~ "nativa_cmpe2011_agregada_arm8",
        !is.na(nivel_cobertura_codigo) ~ nivel_cobertura_codigo,
        clasificador_carrera == "CMPE 2011" ~ "nativa_fuera_inventario",
        TRUE ~ "sin_evidencia_por_codigo"
      ),
      apta_carrera_para_desajuste_horizontal =
        en_serie_historica_horizontal &
        !carrera_asistida_por_ocupacion &
        campo_arm8_horizontal %in% as.character(1:8),
      uso_desajuste_horizontal = case_when(
        !en_serie_historica_horizontal ~
          "S\u00F3lo sensibilidad: fuera de la serie principal 2014-III+",
        clasificador_carrera == "CMPE 2016" &
          !is.na(campo_arm8_horizontal) ~
          paste0(
            "Continuidad ARM8 con marca de cambio CMPE 2016; ",
            "validar la comparabilidad temporal"
          ),
        fuente_armonizacion_carrera == "consenso_arm8_experimental" ~
          "S\u00F3lo sensibilidad: perfil experimental",
        !is.na(campo_arm8_horizontal) ~
          "Recomendado por periodo; validar matriz ARM8-SINCO",
        TRUE ~ "Pendiente de correspondencia de carrera"
      ),
      carrera_catalogo_origen = clasificador_carrera,
      carrera_catalogo_destino = "CMPE 2011",
      carrera_codigo_original = cs_p14_c_original,
      carrera_codigo_armonizado_3d = carrera_cmpe2011,
      carrera_codigo_armonizado_arm8 = campo_arm8_horizontal,
      carrera_nivel_maximo_sustentado = nivel_resultado_carrera,
      carrera_decision_status = case_when(
        fuente_armonizacion_carrera == "oficial_observada" ~ "unique",
        fuente_arm8_horizontal == "cmpe2016_nativo_agregado" &
          is.na(carrera_cmpe2011) ~
          "unique_at_arm8",
        fuente_armonizacion_carrera == "consenso_arm8_experimental" ~ "consensus",
        !is.na(estado_correspondencia_cmpe2011) ~ estado_correspondencia_cmpe2011,
        TRUE ~ "unresolved"
      ),
      carrera_decision_phase = case_when(
        fuente_armonizacion_carrera == "oficial_observada" ~ "integrated",
        fuente_arm8_horizontal == "cmpe2016_nativo_agregado" &
          is.na(carrera_cmpe2011) ~ "integrated",
        !is.na(fase_correspondencia_cmpe2011) ~ fase_correspondencia_cmpe2011,
        TRUE ~ "not_applicable"
      ),
      carrera_evidence_level = case_when(
        fuente_armonizacion_carrera == "oficial_observada" ~ "observed",
        fuente_arm8_horizontal == "cmpe2016_nativo_agregado" &
          is.na(carrera_cmpe2011) ~
          "official_native_aggregation",
        fuente_armonizacion_carrera == "panel_validado" ~ "panel",
        fuente_armonizacion_carrera == "consenso_arm8_experimental" ~
          "panel_consensus_arm8",
        !is.na(nivel_evidencia_cmpe2011) ~ nivel_evidencia_cmpe2011,
        TRUE ~ "none"
      ),
      carrera_regla_id = case_when(
        fuente_armonizacion_carrera == "oficial_observada" &
          nivel_observacion_carrera == "CMPE 2011 agregado: nivel + campo" ~
          "CMPE2011_OBSERVED_AGGREGATED_ARM8",
        fuente_armonizacion_carrera == "oficial_observada" ~
          "CMPE2011_OBSERVED",
        fuente_arm8_horizontal == "cmpe2016_nativo_agregado" &
          is.na(carrera_cmpe2011) ~
          "CMPE2016_NATIVE_ARM8",
        !is.na(regla_correspondencia_cmpe2011) ~
          regla_correspondencia_cmpe2011,
        TRUE ~ NA_character_
      ),
      carrera_decision_layer = case_when(
        fuente_armonizacion_carrera == "oficial_observada" ~ "native_observed",
        fuente_arm8_horizontal == "cmpe2016_nativo_agregado" &
          is.na(carrera_cmpe2011) ~
          "native_aggregation",
        fuente_armonizacion_carrera == "panel_validado" ~ "panel_crosswalk",
        fuente_armonizacion_carrera == "consenso_arm8_experimental" ~
          "experimental_arm8_consensus",
        TRUE ~ "unresolved"
      ),
      carrera_auxiliares_usados = "ninguno_en_aplicacion",
      carrera_destinos_plausibles = destinos_cmpe2011_posibles,
      carrera_motivo_pendiente = case_when(
        !nivel_carrera %in% 5:9 ~ "nivel_educativo_no_elegible",
        is.na(cs_p14_c_canonica) ~ "codigo_carrera_faltante",
        !is.na(campo_arm8_horizontal) & is.na(carrera_cmpe2011) ~
          "detalle_cmpe2011_sin_correspondencia; arm8_disponible",
        is.na(campo_arm8_cmpe2011) & !is.na(destinos_cmpe2011_posibles) ~
          "destinos_multiples_sin_regla_aceptada",
        is.na(campo_arm8_cmpe2011) ~ "sin_correspondencia_aceptada",
        TRUE ~ NA_character_
      ),
      calidad_armonizacion = case_when(
        is.na(cs_p14_c_canonica) ~ "Sin informaci\u00F3n de carrera",
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
    select(
      -calidad,
      -campo_arm8_det,
      -campo_arm10_det,
      -periodo_carrera,
      -codigo_carrera_sin_relleno,
      -codigo_union_cmpe2011,
      -carrera_cmpe2011_lookup,
      -arm8_cmpe2011_lookup,
      -arm8_consenso_lookup,
      -perfil_habilita_panel,
      -perfil_habilita_experimental
    )

  if (nrow(resultado) != n_inicial) {
    stop(
      "La armonizaci\u00F3n cambi\u00F3 el n\u00FAmero de filas: ",
      n_inicial, " -> ", nrow(resultado), "."
    )
  }

  resultado <- resultado |>
    sjlabelled::var_labels(
      clasificador_carrera = "Clasificador de carreras utilizado por la ENOE",
      cs_p14_c_original = "Clave de carrera tal como fue recibida",
      cs_p14_c_canonica = "Clave de carrera normalizada como texto",
      nivel_observacion_carrera = "Clasificador y granularidad observada de la carrera",
      nivel_carrera = "Nivel educativo asociado a la carrera",
      grupo_2005 = "Grupo del Cat\u00E1logo de Carreras 2005",
      campo_cmpe2011 = "Campo detallado de la CMPE 2011",
      campo_cmpe2016 = "Clave de campo de la CMPE 2016",
      campo_arm8 = "Campo de formaci\u00F3n armonizado en ocho categor\u00EDas",
      campo_arm8_desc = "Descripci\u00F3n del campo armonizado en ocho categor\u00EDas",
      campo_arm10 = "Campo de formaci\u00F3n armonizado en diez categor\u00EDas",
      campo_arm10_desc = "Descripci\u00F3n del campo armonizado en diez categor\u00EDas",
      descripcion_2005_arm = "Descripci\u00F3n resumida de la equivalencia del Cat\u00E1logo 2005",
      calidad_armonizacion = "Calidad de la armonizaci\u00F3n de la carrera",
      carrera_cmpe2011 = "C\u00F3digo de carrera armonizado a CMPE 2011",
      campo_arm8_cmpe2011 = "ARM8 derivado de CMPE 2011 nativa o correspondencia aceptada",
      campo_arm8_horizontal = "ARM8 recomendado para el consumidor de desajuste horizontal",
      fuente_arm8_horizontal = "Procedencia del ARM8 usado por el consumidor horizontal",
      perfil_armonizacion_carrera = "Perfil de evidencia solicitado para armonizar carreras",
      carrera_asistida_por_ocupacion = "La carrera fue completada con ocupaci\u00F3n, CMO o SINCO",
      fuente_armonizacion_carrera = "Fuente de la decisi\u00F3n finalmente integrada",
      nivel_resultado_carrera = "M\u00E1xima granularidad sustentada por el perfil solicitado",
      en_serie_historica_horizontal = "Pertenece a la serie hist\u00F3rica principal desde 2014-III",
      segmento_serie_horizontal = "Segmento de medici\u00F3n para el an\u00E1lisis horizontal",
      apta_carrera_para_desajuste_horizontal = "Carrera metodol\u00F3gicamente apta para el indicador horizontal principal",
      destinos_cmpe2011_posibles = "Destinos CMPE 2011 observados o plausibles para el c\u00F3digo",
      estado_correspondencia_cmpe2011 = "Estado de la correspondencia hacia CMPE 2011",
      nivel_evidencia_cmpe2011 = "Nivel de evidencia de la correspondencia hacia CMPE 2011",
      fase_correspondencia_cmpe2011 = "Fase candidate, accepted o integrated de la correspondencia",
      soporte_codigo_n = "Personas de soporte emp\u00EDrico para el c\u00F3digo",
      soporte_codigo_poblacion = "Poblaci\u00F3n de referencia de soporte para el c\u00F3digo",
      proporcion_modal_codigo_registros = "Proporci\u00F3n muestral del destino modal para el c\u00F3digo",
      proporcion_modal_codigo_poblacion = "Proporci\u00F3n ponderada del destino modal para el c\u00F3digo",
      paneles_soporte_codigo = "N\u00FAmero de paneles con soporte para el c\u00F3digo",
      nivel_cobertura_codigo = "Nivel de cobertura y decisi\u00F3n disponible para el c\u00F3digo",
      regla_correspondencia_cmpe2011 = "Identificador de la regla hacia CMPE 2011",
      version_regla = "Versi\u00F3n de la tabla de correspondencia de carreras",
      puente_2005_aplicado = "Se aplic\u00F3 el puente opcional Carreras 2005 a CMPE 2011",
      uso_desajuste_horizontal = "Recomendaci\u00F3n de uso para desajuste horizontal",
      carrera_catalogo_origen = "Cat\u00E1logo de carreras observado",
      carrera_catalogo_destino = "Cat\u00E1logo can\u00F3nico de destino para carreras",
      carrera_codigo_original = "C\u00F3digo original de carrera para auditor\u00EDa",
      carrera_codigo_armonizado_3d = "C\u00F3digo CMPE 2011 al m\u00E1ximo detalle sustentado",
      carrera_codigo_armonizado_arm8 = "Campo ARM8 armonizado",
      carrera_nivel_maximo_sustentado = "M\u00E1ximo nivel de carrera sustentado",
      carrera_decision_status = "Estado de decisi\u00F3n de la carrera",
      carrera_decision_phase = "Fase de la regla de carrera",
      carrera_evidence_level = "Evidencia que sustenta la carrera",
      carrera_regla_id = "Regla aplicada para armonizar la carrera",
      carrera_decision_layer = "Capa que resolvi\u00F3 la carrera",
      carrera_auxiliares_usados = "Auxiliares usados al aplicar la regla",
      carrera_destinos_plausibles = "Destinos plausibles conservados",
      carrera_motivo_pendiente = "Motivo por el que la carrera queda pendiente",
      elegible_carrera = "Nivel educativo elegible para registrar carrera",
      tiene_codigo_carrera = "Cuenta con c\u00F3digo de carrera",
      cobertura_arm8 = "Cuenta con campo armonizado de ocho categor\u00EDas"
    )

  cobertura_anual <- calcular_cobertura_arm8_anual(resultado)
  attr(resultado, "cobertura_arm8_anual") <- cobertura_anual
  attr(resultado, "orden_canonico_armonizacion") <- c(
    "armonizar_carreras", "armonizar_scian", "armonizar_sinco"
  )
  attr(resultado, "advertencia_circularidad_horizontal") <- paste(
    "No usar SINCO observado o imputado para completar carreras que despu\u00E9s",
    "se comparar\u00E1n con SINCO. Toda ruta asistida por ocupaci\u00F3n es s\u00F3lo",
    "sensibilidad y queda fuera del indicador principal."
  )
  if (advertir_cobertura) {
    emitir_advertencia_cobertura_arm8(cobertura_anual, perfil)
  }

  if (salida == "compacta") {
    atributos <- attributes(resultado)[c(
      "cobertura_arm8_anual", "orden_canonico_armonizacion",
      "advertencia_circularidad_horizontal"
    )]
    contrato_compacto <- c(
      "clasificador_carrera", "cs_p14_c_original", "carrera_cmpe2011",
      "campo_arm8_cmpe2011", "campo_arm8_horizontal",
      "fuente_arm8_horizontal", "calidad_armonizacion", "carrera_regla_id",
      "carrera_decision_status", "carrera_decision_phase",
      "carrera_evidence_level", "carrera_nivel_maximo_sustentado",
      "carrera_auxiliares_usados", "carrera_destinos_plausibles",
      "carrera_motivo_pendiente", "carrera_asistida_por_ocupacion",
      "apta_carrera_para_desajuste_horizontal", "segmento_serie_horizontal",
      "uso_desajuste_horizontal"
    )
    resultado <- resultado[, unique(c(nombres_entrada, contrato_compacto)), drop = FALSE]
    for (nombre in names(atributos)) attr(resultado, nombre) <- atributos[[nombre]]
  }

  resultado
}

#' @rdname armonizar_carreras_enoe
#' @export
armonizar_carreras <- function(
    data,
    usar_puente_2005 = FALSE,
    perfil = c("panel_validado", "oficial", "experimental"),
    advertir_cobertura = TRUE,
    salida = c("auditable", "compacta")) {
  armonizar_carreras_enoe(
    data = data,
    usar_puente_2005 = usar_puente_2005,
    perfil = perfil,
    advertir_cobertura = advertir_cobertura,
    salida = salida
  )
}
