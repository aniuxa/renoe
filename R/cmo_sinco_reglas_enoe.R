# Utilidades internas para aplicar las capas CMO-SINCO distribuidas con renoe.
.normalizar_capas_cmo_sinco <- function(
    capas = c("oficial", "panel", "enoe", "consenso"),
    usar_reglas_enoe = NULL) {
  permitidas <- c("oficial", "panel", "enoe", "consenso")
  if (!is.null(usar_reglas_enoe)) {
    if (length(usar_reglas_enoe) != 1L || is.na(usar_reglas_enoe) ||
        !is.logical(usar_reglas_enoe)) {
      stop("usar_reglas_enoe debe ser TRUE, FALSE o NULL.", call. = FALSE)
    }
    capas <- if (usar_reglas_enoe) permitidas else "oficial"
  }
  capas <- unique(as.character(capas))
  invalidas <- setdiff(capas, permitidas)
  if (!length(capas) || length(invalidas)) {
    stop(
      "capas debe contener oficial, panel, enoe y/o consenso.",
      call. = FALSE
    )
  }
  unique(c("oficial", capas))
}

.cargar_reglas_cmo_sinco_enoe <- function() {
  path <- system.file(
    "extdata", "metodologia_cmo_sinco", "reglas_enoe_aceptadas.csv",
    package = "renoe"
  )
  if (!nzchar(path)) {
    stop("No se encuentra la tabla de reglas ENOE CMO-SINCO.", call. = FALSE)
  }
  utils::read.csv(
    path, stringsAsFactors = FALSE, na.strings = c("", "NA"),
    check.names = FALSE
  )
}

.cargar_convergencias_cmo_sinco_3d <- function() {
  path <- system.file(
    "extdata", "metodologia_cmo_sinco", "convergencias_oficiales_3d.csv",
    package = "renoe"
  )
  if (!nzchar(path)) {
    stop("No se encuentra la tabla de convergencias oficiales a 3d.",
         call. = FALSE)
  }
  utils::read.csv(
    path, stringsAsFactors = FALSE, na.strings = c("", "NA"),
    check.names = FALSE
  )
}

.codigo_entero <- function(data, variable) {
  if (!variable %in% names(data)) {
    return(rep(NA_integer_, nrow(data)))
  }
  suppressWarnings(as.integer(as.character(data[[variable]])))
}

.aplicar_reglas_cmo_sinco_enoe <- function(data, capas) {
  rules <- .cargar_reglas_cmo_sinco_enoe()
  scopes <- character()
  if ("panel" %in% capas) scopes <- c(scopes, "validacion_panel_ENOE")
  if ("enoe" %in% capas) scopes <- c(scopes, "aplicacion_auxiliar_ENOE")
  rules <- rules[rules$scope %in% scopes, , drop = FALSE]
  rules <- rules[order(rules$priority), , drop = FALSE]

  cmo <- .codigo_entero(data, "cmo_4d")
  scian <- .codigo_entero(data, "scian")
  p4a <- .codigo_entero(data, "p4a")
  p4f <- .codigo_entero(data, "p4f")
  p4a_text <- ifelse(
    is.na(p4a), NA_character_,
    stringr::str_pad(as.character(p4a), width = 4L, pad = "0")
  )
  p4a_prefix <- suppressWarnings(as.integer(substr(p4a_text, 1L, 2L)))

  for (i in seq_len(nrow(rules))) {
    rule <- rules[i, , drop = FALSE]
    matches <- !is.na(cmo) & cmo == as.integer(rule$source_code)
    rule_type <- rule$rule_type[[1L]]

    if (rule_type == "scian_sector") {
      matches <- matches & !is.na(scian) &
        scian == as.integer(rule$scian_value)
    } else if (rule_type == "p4a_scian2") {
      matches <- matches & !is.na(p4a_prefix) &
        p4a_prefix == as.integer(rule$p4a_prefix)
    } else if (rule_type == "p4f") {
      matches <- matches & !is.na(p4f) &
        p4f == as.integer(rule$p4f_value)
    } else if (rule_type == "p4a_p4f") {
      matches <- matches & !is.na(p4a) & !is.na(p4f) &
        p4a == as.integer(rule$p4a_value) &
        p4f == as.integer(rule$p4f_value)
    }

    assign <- is.na(data$sinco4d) & matches
    if (!any(assign, na.rm = TRUE)) next

    data$sinco4d[assign] <- as.integer(rule$target_code)
    data$sinco3d[assign] <- as.integer(rule$target_3d)
    data$regla_cmo_sinco[assign] <- rule$rule_id[[1L]]
    data$tipo_regla_cmo_sinco[assign] <- rule_type
    data$alcance_regla_cmo_sinco[assign] <- rule$scope[[1L]]
    data$detalle_regla_cmo_sinco[assign] <- rule$rule_label[[1L]]
    data$n_destinos_regla_cmo_sinco[assign] <- 1L
  }
  data
}

.aplicar_convergencias_cmo_sinco_3d <- function(data) {
  convergence <- .cargar_convergencias_cmo_sinco_3d()
  cmo <- .codigo_entero(data, "cmo_4d")
  map <- stats::setNames(
    as.integer(convergence$target_3d),
    as.character(convergence$source_code)
  )
  target <- unname(map[as.character(cmo)])
  map_n <- stats::setNames(
    as.integer(convergence$n_destinations_4d),
    as.character(convergence$source_code)
  )
  target_n <- unname(map_n[as.character(cmo)])
  assign <- is.na(data$sinco4d) & !is.na(target) &
    (is.na(data$sinco3d) | data$sinco3d == target)
  data$sinco3d[assign] <- target[assign]
  data$regla_cmo_sinco[assign] <- paste0(
    "CMO_SINCO_OFICIAL_3D_", cmo[assign]
  )
  data$tipo_regla_cmo_sinco[assign] <- "official_convergence_3d"
  data$alcance_regla_cmo_sinco[assign] <- "general_clasificadores"
  data$detalle_regla_cmo_sinco[assign] <- paste0(
    "CMO ", cmo[assign], " -> SINCO ", target[assign],
    " a tres digitos; cuatro digitos permanecen ambiguos"
  )
  data$n_destinos_regla_cmo_sinco[assign] <- target_n[assign]

  data
}
