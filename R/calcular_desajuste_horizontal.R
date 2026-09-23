#' Calcular desajuste horizontal entre campo de formacion y ocupacion
#'
#' Compara el campo ARM8 con SINCO a tres digitos mediante la matriz mexicana
#' aceptada. Separa la base ocupada, el universo al que aplica y el universo
#' clasificable. SINCO `999` permanece pendiente y nunca cuenta como desajuste.
#'
#' @param data Data frame con `anio`, `trim`, `sinco3d`, `clase2` y un campo
#'   ARM8 (`campo_arm8_horizontal` o `campo_arm8`). Debe contener ademas
#'   `elegible_carrera` o `cs_p13_1`.
#' @param inicio Primer trimestre admisible, como entero `AAAAT`.
#' @param matriz Ruta alternativa a la matriz, principalmente para pruebas.
#' @return El data frame con las variantes estricta y amplia, indicadores de
#'   universo y una razon trazable de no clasificacion.
#' @export
#' @family procesamiento_enoe
calcular_desajuste_horizontal <- function(data, inicio = 20143L, matriz = NULL) {
  requeridas <- c("anio", "trim", "sinco3d", "clase2")
  faltantes <- setdiff(requeridas, names(data))
  if (length(faltantes)) {
    stop("Faltan variables requeridas: ", paste(faltantes, collapse = ", "),
      call. = FALSE)
  }
  campo_variable <- if ("campo_arm8_horizontal" %in% names(data)) {
    "campo_arm8_horizontal"
  } else if ("campo_arm8" %in% names(data)) {
    "campo_arm8"
  } else {
    stop("Falta `campo_arm8_horizontal` o `campo_arm8`.", call. = FALSE)
  }
  if (!"elegible_carrera" %in% names(data) && !"cs_p13_1" %in% names(data)) {
    stop("Falta `elegible_carrera` o `cs_p13_1` para definir el universo.",
      call. = FALSE)
  }

  if (is.null(matriz)) {
    candidatas <- c(
      file.path("inst", "extdata", "correspondencia_campo_arm8_sinco3d.csv"),
      system.file("extdata", "correspondencia_campo_arm8_sinco3d.csv",
        package = "renoe")
    )
    candidatas <- candidatas[nzchar(candidatas) & file.exists(candidatas)]
    matriz <- if (length(candidatas)) candidatas[[1L]] else ""
  }
  if (!nzchar(matriz) || !file.exists(matriz)) {
    stop("No se encontr\u00F3 la matriz aceptada de desajuste horizontal.",
      call. = FALSE)
  }
  correspondencia <- readr::read_csv(
    matriz, show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  ) |> as.data.frame(stringsAsFactors = FALSE)
  columnas <- c("campo_arm8", "sinco3d", "ajuste_estricto",
    "ajuste_amplio", "estado_revision")
  if (!all(columnas %in% names(correspondencia))) {
    stop("La matriz de desajuste horizontal tiene un esquema incompleto.",
      call. = FALSE)
  }
  if (any(correspondencia$estado_revision != "aceptada")) {
    stop("La matriz contiene relaciones que a\u00FAn no fueron aceptadas.",
      call. = FALSE)
  }

  trimestre <- suppressWarnings(as.integer(stringr::str_extract(
    as.character(data$trim), "[1-4]"
  )))
  periodo <- suppressWarnings(as.integer(data$anio)) * 10L + trimestre
  ocupado <- !is.na(data$clase2) &
    suppressWarnings(as.integer(as.character(data$clase2))) == 1L
  elegible <- if ("elegible_carrera" %in% names(data)) {
    !is.na(data$elegible_carrera) & as.logical(data$elegible_carrera)
  } else {
    suppressWarnings(as.integer(as.character(data$cs_p13_1))) %in% 5:9
  }
  aplica <- ocupado & elegible & !is.na(periodo) & periodo >= inicio

  campo <- trimws(as.character(data[[campo_variable]]))
  campo[is.na(data[[campo_variable]]) | campo == ""] <- NA_character_
  arm8_valido <- !is.na(campo) & campo %in% as.character(1:8)
  codigo_variable <- intersect(
    c("cs_p14_c_original", "cs_p14_c", "carrera_codigo_original"),
    names(data)
  )
  if (length(codigo_variable)) {
    codigo <- trimws(as.character(data[[codigo_variable[[1L]]]]))
    codigo <- stringr::str_replace(codigo, "\\.0+$", "")
    codigo[codigo %in% c("", "NA", "NaN", "NULL")] <- NA_character_
    codigo_faltante <- is.na(codigo)
    codigo_invalido <- !codigo_faltante &
      !stringr::str_detect(codigo, "^([0-9]+|N[0-9]{3})$")
  } else {
    codigo_faltante <- is.na(campo)
    codigo_invalido <- rep(FALSE, nrow(data))
  }
  sinco <- stringr::str_pad(
    as.character(suppressWarnings(as.integer(as.character(data$sinco3d)))),
    3L, "left", "0"
  )
  sinco_valido <- !is.na(sinco) &
    stringr::str_detect(sinco, "^[1-9][0-9]{2}$") & sinco != "999"

  estado <- rep("fallo_fusion", nrow(data))
  estado[!ocupado | !elegible] <- "no_aplica"
  estado[ocupado & elegible & !is.na(periodo) & periodo < inicio] <-
    "fuera_periodo"
  estado[aplica & codigo_faltante] <- "codigo_carrera_faltante"
  estado[aplica & !codigo_faltante & codigo_invalido] <-
    "codigo_carrera_invalido"
  estado[aplica & !codigo_faltante & !codigo_invalido & !arm8_valido] <-
    "sin_arm8"
  estado[aplica & arm8_valido & !sinco_valido] <- "sin_sinco_comparable"
  estado[aplica & arm8_valido & sinco_valido] <- "clasificable"
  estado <- factor(estado, levels = c(
    "clasificable", "no_aplica", "fuera_periodo",
    "codigo_carrera_faltante", "codigo_carrera_invalido", "sin_arm8",
    "sin_sinco_comparable", "fallo_fusion"
  ))

  clave <- paste(campo, sinco, sep = "|")
  clave_matriz <- paste(
    correspondencia$campo_arm8,
    stringr::str_pad(as.character(suppressWarnings(as.integer(
      correspondencia$sinco3d
    ))), 3L, "left", "0"), sep = "|"
  )
  es_verdadero <- function(x) {
    toupper(trimws(x)) %in% c("TRUE", "T", "1", "SI", "S\u00CD")
  }
  claves_estrictas <- unique(
    clave_matriz[es_verdadero(correspondencia$ajuste_estricto)]
  )
  claves_amplias <- unique(
    clave_matriz[es_verdadero(correspondencia$ajuste_amplio)]
  )
  clasificable <- estado == "clasificable"
  estricto <- amplio <- rep(NA_integer_, nrow(data))
  estricto[clasificable] <- as.integer(
    !clave[clasificable] %in% claves_estrictas
  )
  amplio[clasificable] <- as.integer(
    !clave[clasificable] %in% claves_amplias
  )

  data$base_referencia_desajuste_horizontal <- ocupado
  data$aplica_desajuste_horizontal <- aplica
  data$clasificable_desajuste_horizontal <- clasificable
  data$estado_desajuste_horizontal <- estado
  data$calidad_desajuste_horizontal <- estado
  data$desajuste_horizontal_estricto <- estricto
  data$desajuste_horizontal_amplio <- amplio
  data <- sjlabelled::var_labels(data,
    base_referencia_desajuste_horizontal = "Base de referencia: poblaci\u00F3n ocupada",
    aplica_desajuste_horizontal =
      "Universo de aplicaci\u00F3n: ocupado, elegible y desde 2014-T3",
    clasificable_desajuste_horizontal =
      "Cuenta con ARM8 y SINCO comparable para desajuste horizontal",
    estado_desajuste_horizontal = "Estado trazable del desajuste horizontal",
    calidad_desajuste_horizontal = "Estado trazable del desajuste horizontal",
    desajuste_horizontal_estricto =
      "Desajuste horizontal con correspondencia estricta",
    desajuste_horizontal_amplio =
      "Desajuste horizontal con correspondencia amplia"
  )
  data <- sjlabelled::val_labels(data,
    desajuste_horizontal_estricto = c(
      "Ajuste horizontal" = 0, "Desajuste horizontal" = 1
    ),
    desajuste_horizontal_amplio = c(
      "Ajuste horizontal" = 0, "Desajuste horizontal" = 1
    )
  )
  data
}
