#' Calcular una referencia estadistica de escolaridad y su desajuste
#'
#' Calcula la escolaridad media observada por division SINCO y periodo entre la
#' poblacion ocupada. `esco_ref` es una referencia estadistica interna, no una
#' norma ocupacional externa. La referencia predeterminada es trimestral.
#'
#' La referencia anual se calcula unicamente sobre datos ya acumulados y su
#' unidad es persona-trimestre. La funcion no deduplica personas: la rotacion de
#' ENOE forma parte de los cortes transversales acumulados. Para publicar una
#' referencia anual se requieren cuatro trimestres por año; el tratamiento de
#' años incompletos puede cambiarse explicitamente con `anio_incompleto`.
#'
#' El ponderador se elige con `variable_ponderador`. Dividir por una constante
#' comun, como cuatro en un año completo, no cambia la media, aunque si importa
#' para estimar totales anuales.
#'
#' @param data Data frame con `anio`, `trim`, `clase2`, `sinco1d` y `anios_es`.
#' @param periodo_referencia `"trimestre"` o `"anio"`.
#' @param umbral_anios Umbral simetrico en años; por defecto 1.
#' @param ponderado Si es `TRUE`, usa el ponderador indicado.
#' @param variable_ponderador Nombre del ponderador; por defecto `fac`.
#' @param anio_incompleto Tratamiento de años con menos de cuatro trimestres:
#'   `"error"`, `"advertir"` o `"permitir"`.
#'
#' @return El mismo data frame, en el mismo orden, con `esco_ref`, `mismatch2`
#'   y metadatos explicitos del periodo, ponderador, numero de trimestres y
#'   unidad persona-trimestre.
#' @export
#' @family procesamiento_enoe
#'
#' @examples
#' datos <- data.frame(
#'   anio = c(2025, 2025), trim = c(1, 1), clase2 = c(1, 1),
#'   sinco1d = c(3, 3), anios_es = c(9, 11), fac = c(1, 1)
#' )
#' calcular_desajuste_estadistico(datos)
calcular_desajuste_estadistico <- function(
    data,
    periodo_referencia = c("trimestre", "anio"),
    umbral_anios = 1,
    ponderado = TRUE,
    variable_ponderador = "fac",
    anio_incompleto = c("error", "advertir", "permitir")) {
  if (!is.data.frame(data)) {
    stop("`data` debe ser un data frame.", call. = FALSE)
  }
  periodo_referencia <- match.arg(periodo_referencia)
  anio_incompleto <- match.arg(anio_incompleto)
  requeridas <- c("anio", "trim", "clase2", "sinco1d", "anios_es")
  faltantes <- setdiff(requeridas, names(data))
  if (length(faltantes) > 0L) {
    stop("Faltan variables: ", paste(faltantes, collapse = ", "), call. = FALSE)
  }
  if (length(umbral_anios) != 1L || !is.numeric(umbral_anios) ||
      is.na(umbral_anios) || !is.finite(umbral_anios) || umbral_anios < 0) {
    stop("`umbral_anios` debe ser un n\u00FAmero finito mayor o igual que cero.", call. = FALSE)
  }
  if (length(ponderado) != 1L || !is.logical(ponderado) || is.na(ponderado)) {
    stop("`ponderado` debe ser TRUE o FALSE.", call. = FALSE)
  }
  if (length(variable_ponderador) != 1L || !is.character(variable_ponderador) ||
      is.na(variable_ponderador) || !nzchar(variable_ponderador)) {
    stop("`variable_ponderador` debe contener un nombre de columna.", call. = FALSE)
  }
  if (ponderado && !variable_ponderador %in% names(data)) {
    stop(
      "Falta el ponderador `", variable_ponderador,
      "`. Agr\u00E9guelo o use `ponderado = FALSE`.",
      call. = FALSE
    )
  }

  n <- nrow(data)
  anio <- suppressWarnings(as.integer(as.character(data$anio)))
  trimestre <- suppressWarnings(as.integer(
    sub("^t", "", tolower(trimws(as.character(data$trim))))
  ))
  clase2 <- suppressWarnings(as.numeric(as.character(data$clase2)))
  sinco1d <- suppressWarnings(as.integer(as.character(data$sinco1d)))
  anios_es <- suppressWarnings(as.numeric(as.character(data$anios_es)))
  elegible <- clase2 == 1 & !is.na(sinco1d) & sinco1d %in% 1:9 &
    is.finite(anios_es) & !is.na(anio) & !is.na(trimestre) &
    trimestre %in% 1:4
  elegible[is.na(elegible)] <- FALSE

  periodos_validos <- unique(data.frame(
    anio = anio[elegible], trim = trimestre[elegible]
  ))
  conteos <- if (nrow(periodos_validos)) {
    table(periodos_validos$anio)
  } else integer()
  n_trimestres <- rep(NA_integer_, n)
  if (periodo_referencia == "trimestre") {
    n_trimestres[!is.na(anio) & !is.na(trimestre) & trimestre %in% 1:4] <- 1L
  } else if (length(conteos)) {
    n_trimestres <- as.integer(conteos[as.character(anio)])
  }

  if (periodo_referencia == "anio" && length(conteos)) {
    incompletos <- conteos[conteos < 4L]
    if (length(incompletos)) {
      detalle <- paste0(names(incompletos), " (", as.integer(incompletos), ")")
      mensaje <- paste0(
        "La referencia anual requiere cuatro trimestres por a\u00F1o; a\u00F1o incompleto: ",
        paste(detalle, collapse = ", "), "."
      )
      if (anio_incompleto == "error") stop(mensaje, call. = FALSE)
      if (anio_incompleto == "advertir") warning(mensaje, call. = FALSE)
    }
  }

  data$esco_ref <- rep(NA_real_, n)
  data$mismatch2 <- rep(NA_real_, n)
  data$periodo_referencia_mismatch2 <- rep(periodo_referencia, n)
  data$ponderador_mismatch2 <- rep(
    if (ponderado) variable_ponderador else "sin_ponderador", n
  )
  data$trimestres_referencia_mismatch2 <- n_trimestres
  data$unidad_referencia_mismatch2 <- rep("persona-trimestre", n)

  if (any(elegible)) {
    periodo <- if (periodo_referencia == "anio") {
      as.character(anio)
    } else {
      paste(anio, trimestre, sep = "-")
    }
    clave <- paste(periodo, sinco1d, sep = "|")
    grupos <- split(which(elegible), clave[elegible])
    pesos <- if (ponderado) {
      suppressWarnings(as.numeric(as.character(data[[variable_ponderador]])))
    } else {
      rep(1, n)
    }

    for (indices in grupos) {
      pesos_grupo <- pesos[indices]
      validos <- is.finite(pesos_grupo) & pesos_grupo > 0
      referencia <- if (ponderado) {
        if (any(validos)) {
          stats::weighted.mean(anios_es[indices][validos], pesos_grupo[validos])
        } else {
          NA_real_
        }
      } else {
        mean(anios_es[indices])
      }
      data$esco_ref[indices] <- referencia
    }

    diferencia <- anios_es - data$esco_ref
    medido <- elegible & is.finite(data$esco_ref)
    data$mismatch2[medido] <- dplyr::case_when(
      diferencia[medido] > umbral_anios ~ -1,
      diferencia[medido] < -umbral_anios ~ 1,
      TRUE ~ 0
    )
  }

  data <- data |>
    sjlabelled::var_labels(
      esco_ref = "Referencia estad\u00EDstica observada de a\u00F1os de escolaridad",
      mismatch2 = "Desajuste educativo respecto a la referencia estad\u00EDstica observada",
      periodo_referencia_mismatch2 = "Periodo usado para la referencia estad\u00EDstica de escolaridad",
      ponderador_mismatch2 = "Ponderador usado para la referencia estad\u00EDstica de escolaridad",
      trimestres_referencia_mismatch2 = "N\u00FAmero de trimestres acumulados en la referencia",
      unidad_referencia_mismatch2 = "Unidad anal\u00EDtica de la referencia estad\u00EDstica"
    ) |>
    sjlabelled::val_labels(
      mismatch2 = c(
        "Sobreeducaci\u00F3n" = -1,
        "Ajuste" = 0,
        "Subeducaci\u00F3n" = 1
      )
    )

  data
}
