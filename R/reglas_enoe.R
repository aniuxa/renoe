#' Aplicar reglas declarativas y trazables a variables ENOE
#'
#' Aplica una tabla de reglas en formato largo a una sola variable de destino.
#' Las condiciones de una misma `regla_id` se combinan con AND y las reglas se
#' evaluan por prioridad ascendente. No evalua expresiones R arbitrarias.
#'
#' @param data Data frame que contiene las variables auxiliares.
#' @param reglas Data frame largo con las columnas `regla_id`,
#'   `variable_destino`, `valor_destino`, `prioridad`, `perfil_regla`,
#'   `fase_decision`, `nivel_evidencia`, `variable_auxiliar`, `operador`,
#'   `valor_condicion`, `rol_auxiliar` y `origen_auxiliar`.
#' @param consumidor Producto que usara el resultado. `"desajuste_horizontal"`
#'   activa el control especifico de circularidad entre carrera y ocupacion.
#' @param perfil Maximo nivel de evidencia habilitado: `"oficial"`,
#'   `"panel_validado"` o `"experimental"`.
#' @param modo `"principal"` excluye reglas de sensibilidad; `"sensibilidad"`
#'   las permite. Las reglas prohibidas nunca se aplican.
#' @param sobrescribir Si es `FALSE`, solo completa valores faltantes. Si es
#'   `TRUE`, la primera regla aplicable puede reemplazar el valor recibido.
#' @param advertir Si es `TRUE`, informa reglas omitidas por fase, perfil o
#'   circularidad.
#'
#' @return El mismo data frame, sin cambiar filas, con la variable de destino y
#'   las columnas `regla_enoe_id`, `regla_enoe_perfil`, `regla_enoe_fase`,
#'   `regla_enoe_evidencia`, `regla_enoe_auxiliares`,
#'   `regla_enoe_circularidad` y `regla_enoe_aplicada`.
#'
#' @details
#' Operadores disponibles: `==`, `!=`, `in`, `between`, `is_na` y `not_na`.
#' Para `in`, los valores se separan con `|`; para `between`, se proporcionan
#' dos limites numericos separados con `|`.
#'
#' En el consumidor `desajuste_horizontal`, una regla que complete una carrera
#' con ocupacion observada se clasifica como sensibilidad. Si la ocupacion fue
#' armonizada o imputada, la regla se prohibe. Un auxiliar con rol `resultado`
#' tambien se prohibe. Esta restriccion evita usar el propio resultado para
#' fabricar uno de sus componentes.
#'
#' @export
#' @family procesamiento_enoe
#' @examples
#' datos <- data.frame(nivel = c(7, 6), carrera = c(NA, NA))
#' reglas <- data.frame(
#'   regla_id = "NIVEL_7", variable_destino = "carrera",
#'   valor_destino = "A", prioridad = 1, perfil_regla = "panel_validado",
#'   fase_decision = "accepted", nivel_evidencia = "auxiliary",
#'   variable_auxiliar = "nivel", operador = "==", valor_condicion = "7",
#'   rol_auxiliar = "educacion", origen_auxiliar = "observado"
#' )
#' reglas_enoe(datos, reglas, perfil = "panel_validado")
reglas_enoe <- function(
    data,
    reglas,
    consumidor = "general",
    perfil = c("oficial", "panel_validado", "experimental"),
    modo = c("principal", "sensibilidad"),
    sobrescribir = FALSE,
    advertir = TRUE) {

  perfil <- match.arg(perfil)
  modo <- match.arg(modo)

  for (arg in c("sobrescribir", "advertir")) {
    valor <- get(arg)
    if (!is.logical(valor) || length(valor) != 1L || is.na(valor)) {
      stop("`", arg, "` debe ser TRUE o FALSE.", call. = FALSE)
    }
  }
  if (!is.data.frame(data) || !is.data.frame(reglas)) {
    stop("`data` y `reglas` deben ser data frames.", call. = FALSE)
  }

  requeridas <- c(
    "regla_id", "variable_destino", "valor_destino", "prioridad",
    "perfil_regla", "fase_decision", "nivel_evidencia",
    "variable_auxiliar", "operador", "valor_condicion",
    "rol_auxiliar", "origen_auxiliar"
  )
  faltantes <- setdiff(requeridas, names(reglas))
  if (length(faltantes)) {
    stop(
      "Faltan columnas en `reglas`: ", paste(faltantes, collapse = ", "),
      call. = FALSE
    )
  }
  if (!nrow(reglas)) {
    stop("`reglas` no puede estar vac\u00EDa.", call. = FALSE)
  }

  reglas <- as.data.frame(reglas, stringsAsFactors = FALSE)
  reglas[requeridas] <- lapply(reglas[requeridas], as.character)
  reglas$prioridad <- suppressWarnings(as.numeric(reglas$prioridad))
  if (anyNA(reglas$prioridad)) {
    stop("`prioridad` debe ser num\u00E9rica y no faltante.", call. = FALSE)
  }
  if (anyNA(reglas$regla_id) || any(!nzchar(reglas$regla_id))) {
    stop("Cada condici\u00F3n debe tener `regla_id`.", call. = FALSE)
  }

  destinos <- unique(reglas$variable_destino)
  if (length(destinos) != 1L || is.na(destinos)) {
    stop("Cada aplicaci\u00F3n admite una sola `variable_destino`.", call. = FALSE)
  }
  destino <- destinos[[1L]]

  operadores <- c("==", "!=", "in", "between", "is_na", "not_na")
  if (any(!reglas$operador %in% operadores)) {
    stop("`operador` contiene valores no admitidos.", call. = FALSE)
  }
  if (any(!reglas$variable_auxiliar %in% names(data))) {
    stop(
      "Faltan auxiliares en `data`: ",
      paste(setdiff(unique(reglas$variable_auxiliar), names(data)), collapse = ", "),
      call. = FALSE
    )
  }

  perfiles <- c(oficial = 1L, panel_validado = 2L, experimental = 3L)
  if (any(!reglas$perfil_regla %in% names(perfiles))) {
    stop("`perfil_regla` contiene perfiles desconocidos.", call. = FALSE)
  }
  fases <- c("candidate", "accepted", "integrated")
  if (any(!reglas$fase_decision %in% fases)) {
    stop("`fase_decision` contiene fases desconocidas.", call. = FALSE)
  }
  origenes <- c("observado", "armonizado", "imputado")
  if (any(!reglas$origen_auxiliar %in% origenes)) {
    stop("`origen_auxiliar` debe ser observado, armonizado o imputado.", call. = FALSE)
  }

  ids <- unique(reglas$regla_id)
  campos_constantes <- c(
    "variable_destino", "valor_destino", "prioridad", "perfil_regla",
    "fase_decision", "nivel_evidencia"
  )
  inconsistente <- vapply(ids, function(id) {
    bloque <- reglas[reglas$regla_id == id, campos_constantes, drop = FALSE]
    any(vapply(bloque, function(x) length(unique(x)) != 1L, logical(1)))
  }, logical(1))
  if (any(inconsistente)) {
    stop(
      "Metadatos inconsistentes dentro de: ",
      paste(ids[inconsistente], collapse = ", "), call. = FALSE
    )
  }

  resumen <- do.call(rbind, lapply(ids, function(id) {
    bloque <- reglas[reglas$regla_id == id, , drop = FALSE]
    roles <- unique(tolower(bloque$rol_auxiliar))
    origen <- unique(tolower(bloque$origen_auxiliar))
    es_carrera <- grepl("carrera|campo_arm|cmpe", destino, ignore.case = TRUE)
    horizontal <- identical(consumidor, "desajuste_horizontal")
    circularidad <- "principal"
    if (horizontal && es_carrera && any(roles %in% c("ocupacion", "sinco", "cmo"))) {
      circularidad <- if (any(origen != "observado")) "prohibida" else "sensibilidad"
    }
    if (horizontal && any(roles == "resultado")) {
      circularidad <- "prohibida"
    }
    data.frame(
      regla_id = id,
      valor_destino = bloque$valor_destino[[1L]],
      prioridad = bloque$prioridad[[1L]],
      perfil_regla = bloque$perfil_regla[[1L]],
      fase_decision = bloque$fase_decision[[1L]],
      nivel_evidencia = bloque$nivel_evidencia[[1L]],
      auxiliares = paste(unique(bloque$variable_auxiliar), collapse = "|"),
      circularidad = circularidad,
      stringsAsFactors = FALSE
    )
  }))

  fase_permitida <- switch(
    perfil,
    oficial = resumen$fase_decision == "integrated",
    panel_validado = resumen$fase_decision %in% c("accepted", "integrated"),
    experimental = resumen$fase_decision %in% fases
  )
  perfil_permitido <- perfiles[resumen$perfil_regla] <= perfiles[[perfil]]
  circularidad_permitida <- resumen$circularidad == "principal" |
    (modo == "sensibilidad" & resumen$circularidad == "sensibilidad")
  usar <- fase_permitida & perfil_permitido & circularidad_permitida

  if (advertir && any(!usar)) {
    warning(
      sum(!usar), " regla(s) omitida(s) por perfil, fase o circularidad. ",
      "Las reglas prohibidas nunca se aplican.", call. = FALSE
    )
  }

  if (!destino %in% names(data)) {
    data[[destino]] <- NA_character_
  }
  original_faltante <- is.na(data[[destino]])
  disponible <- if (sobrescribir) rep(TRUE, nrow(data)) else original_faltante

  data$regla_enoe_id <- NA_character_
  data$regla_enoe_perfil <- NA_character_
  data$regla_enoe_fase <- NA_character_
  data$regla_enoe_evidencia <- NA_character_
  data$regla_enoe_auxiliares <- NA_character_
  data$regla_enoe_circularidad <- NA_character_
  data$regla_enoe_aplicada <- FALSE

  orden <- order(resumen$prioridad, resumen$regla_id)
  resumen <- resumen[orden, , drop = FALSE]
  usar_ids <- resumen$regla_id[usar[orden]]

  cumple_condicion <- function(x, operador, valor) {
    x_chr <- as.character(x)
    switch(
      operador,
      "==" = !is.na(x_chr) & x_chr == valor,
      "!=" = !is.na(x_chr) & x_chr != valor,
      "in" = !is.na(x_chr) & x_chr %in% strsplit(valor, "|", fixed = TRUE)[[1L]],
      "between" = {
        limites <- suppressWarnings(as.numeric(strsplit(valor, "|", fixed = TRUE)[[1L]]))
        if (length(limites) != 2L || anyNA(limites)) {
          stop("`between` requiere dos l\u00EDmites num\u00E9ricos separados por |.", call. = FALSE)
        }
        x_num <- suppressWarnings(as.numeric(x_chr))
        !is.na(x_num) & x_num >= limites[[1L]] & x_num <= limites[[2L]]
      },
      "is_na" = is.na(x),
      "not_na" = !is.na(x)
    )
  }

  asignar_valor <- function(vector, indice, valor) {
    if (is.integer(vector)) valor <- suppressWarnings(as.integer(valor))
    else if (is.double(vector)) valor <- suppressWarnings(as.numeric(valor))
    else if (is.logical(vector)) valor <- as.logical(valor)
    else valor <- as.character(valor)
    vector[indice] <- valor
    vector
  }

  for (id in usar_ids) {
    bloque <- reglas[reglas$regla_id == id, , drop = FALSE]
    coincide <- rep(TRUE, nrow(data))
    for (j in seq_len(nrow(bloque))) {
      coincide <- coincide & cumple_condicion(
        data[[bloque$variable_auxiliar[[j]]]],
        bloque$operador[[j]],
        bloque$valor_condicion[[j]]
      )
    }
    aplicar <- coincide & disponible & !data$regla_enoe_aplicada
    if (!any(aplicar)) next
    meta <- resumen[resumen$regla_id == id, , drop = FALSE]
    data[[destino]] <- asignar_valor(data[[destino]], aplicar, meta$valor_destino)
    data$regla_enoe_id[aplicar] <- id
    data$regla_enoe_perfil[aplicar] <- meta$perfil_regla
    data$regla_enoe_fase[aplicar] <- meta$fase_decision
    data$regla_enoe_evidencia[aplicar] <- meta$nivel_evidencia
    data$regla_enoe_auxiliares[aplicar] <- meta$auxiliares
    data$regla_enoe_circularidad[aplicar] <- meta$circularidad
    data$regla_enoe_aplicada[aplicar] <- TRUE
  }

  attr(data, "reglas_enoe_resumen") <- resumen
  attr(data, "reglas_enoe_consumidor") <- consumidor
  attr(data, "reglas_enoe_modo") <- modo
  data
}
