#' Procesar el modulo de cuidado de mercado
#'
#' Interfaz del modulo desarrollado para el articulo sobre brechas de ingreso
#' mediante regresiones cuantilicas. Clasifica en memoria; no descarga, escribe
#' ni reconstruye microdatos. Los modelos pertenecen al proyecto del articulo.
#'
#' Cuando detecta la salida completa de [armonizar_sinco()], usa
#' `sinco4d_base2011` y `sinco3d` como insumos canonicos para todos los
#' periodos. Conserva `p3coe` como codigo original y no vuelve a decidir el
#' catalogo por su cuenta. Si la ruta canonica no esta presente, conserva el
#' comportamiento historico basado en el clasificador observado.
#'
#' Conserva las columnas originales, incluido `sinco3d`. Agrega las salidas de
#' `class_cuidado_rem()` y trazabilidad especifica. `codigo_ocupacion_armonizado`
#' tiene tres digitos y representa SINCO 2011 cuando la ruta canonica esta
#' disponible; en el modo heredado conserva la interpretacion anterior.
#' Para los remanentes CMO sin SINCO 3d canonico, el puente analitico de
#' cuidado recupera solo la clasificacion dependiente y registra su
#' multiplicidad; no rellena el SINCO general ni convierte 9999 en ocupacion.
#' `codigo_ocupacion_original_cuidado` conserva el insumo sin recodificar.
#' Las correspondencias multiples siguen la primera regla del material de
#' Damian y quedan identificadas en `calidad_armonizacion_cuidado`.
#'
#' `trabajo_cuidado_mercado` es el nombre de la tipologia. La funcion separa la
#' posicion remunerada, la posicion explicita sin pago y la evidencia de ingreso
#' observado o imputado. Un ingreso imputado positivo nunca se presenta como
#' remuneracion observada.
#'
#' @param data Data frame de personas; puede contener varios trimestres.
#' @param anio,trimestre Periodo opcional, escalar o vector de longitud
#'   `nrow(data)`. Si se omite, usa `anio` y `trim` en data. Rechaza conflictos.
#' @param variable_codigo Codigo ocupacional observado de cuatro digitos.
#' @param variable_ocupacion Respaldo SINCO observado de tres digitos.
#' @param variable_actividad Variable SCIAN-Hogares.
#' @param variable_ocupado Variable de condicion de ocupacion.
#' @param valor_ocupado Valor que identifica personas ocupadas.
#' @return El data frame con clasificacion y trazabilidad del cuidado. Recalcula
#'   las salidas propias del modulo si existen; preserva los insumos.
#' @export
#' @family cuidado_remunerado
#' @examples
#' x <- data.frame(p3coe = c(2331, 4111), p4a = c(6111, 6111), clase2 = 1)
#' procesar_cuidado_remunerado(x, anio = 2022, trimestre = 1)
procesar_cuidado_remunerado <- function(
    data, anio = NULL, trimestre = NULL,
    variable_codigo = "p3coe", variable_ocupacion = "sinco3d",
    variable_actividad = "p4a", variable_ocupado = "clase2",
    valor_ocupado = 1) {
  if (!is.data.frame(data)) stop("`data` debe ser un data frame.")
  n <- nrow(data)
  periodo <- function(valor, nombre, trimestre = FALSE) {
    existente <- if (nombre %in% names(data)) data[[nombre]] else NULL
    parsear <- function(x) {
      y <- as.character(x)
      if (trimestre) y <- sub("^t", "", tolower(y))
      suppressWarnings(as.numeric(y))
    }
    if (is.null(valor)) valor <- existente
    if (is.null(valor)) stop("Falta el periodo: ", nombre)
    if (!length(valor) %in% unique(c(1L, n))) stop("Longitud invalida: ", nombre)
    valor <- rep_len(parsear(valor), n)
    if (anyNA(valor) || any(!is.finite(valor)) ||
        any(valor != floor(valor)) ||
        (trimestre && any(!valor %in% 1:4)) ||
        (!trimestre && any(valor < 2005 | valor > 9999))) {
      stop("Periodo invalido: ", nombre)
    }
    if (!is.null(existente) && !identical(parsear(existente), valor)) {
      stop("Conflicto entre argumento y columna: ", nombre)
    }
    valor
  }
  a <- periodo(anio, "anio")
  t <- periodo(trimestre, "trim", TRUE)
  p <- a * 10 + t
  cmo <- p <= 20122
  vars <- c(variable_actividad, variable_ocupado)
  faltantes <- setdiff(vars, names(data))
  if (length(faltantes)) stop("Faltan variables: ", paste(faltantes, collapse=", "))
  if (length(valor_ocupado) != 1L || is.na(valor_ocupado)) {
    stop("`valor_ocupado` debe tener un valor no faltante.")
  }
  canonico <- all(c(
    "sinco4d_base2011", "sinco3d", "calidad_puente_sinco"
  ) %in% names(data))
  tiene_marca_canonica <- any(c(
    "sinco4d_base2011", "calidad_puente_sinco",
    "sinco2011_granularidad"
  ) %in% names(data))
  if (tiene_marca_canonica && !canonico) {
    stop(
      "SINCO previamente armonizado, pero la ruta canonica esta incompleta: ",
      "se requieren sinco4d_base2011, sinco3d y calidad_puente_sinco."
    )
  }
  raw <- variable_codigo %in% names(data)
  if (!raw && !canonico && any(cmo)) {
    stop("Falta la variable CMO: ", variable_codigo)
  }
  if (!raw && !canonico && !variable_ocupacion %in% names(data)) {
    stop("Falta ocupacion: ", variable_codigo, " o ", variable_ocupacion)
  }
  original <- if (raw) {
    data[[variable_codigo]]
  } else if ("codigo_ocupacion_original" %in% names(data)) {
    data$codigo_ocupacion_original
  } else {
    data[[variable_ocupacion]]
  }
  if (canonico) {
    codigo <- .cuidado_codigo(data$sinco4d_base2011, 4L)
    ocu <- .cuidado_codigo(data$sinco3d, 3L)
  } else {
    codigo <- .cuidado_codigo(original, if (raw) 4L else 3L)
    ocu <- if (raw) codigo %/% 10L else codigo
  }
  # Aislar insumos evita sobrescribir la armonizacion general y metadatos previos.
  trabajo <- data.frame(anio=a, trim=t, p3coe=codigo, sinco3d=ocu,
                        p4a=data[[variable_actividad]], clase2=data[[variable_ocupado]],
                        cmo_original=ifelse(cmo, original, NA))
  calidad_rescate <- rep(NA_character_, n)
  n_destinos_rescate <- rep(NA_integer_, n)
  rescate_cmo <- integer()
  if (canonico) {
    rescate_cmo <- which(cmo & is.na(ocu))
    if (length(rescate_cmo)) {
      puente <- cmo_to_sinco11_care(
        data.frame(p3coe = original[rescate_cmo]),
        variable_cmo = "p3coe"
      )
      destino <- suppressWarnings(as.integer(puente$sinco3d))
      destino[is.na(destino) | destino < 100L |
                destino >= 999L] <- NA_integer_
      trabajo$sinco3d[rescate_cmo] <- destino
      calidad_rescate[rescate_cmo] <- puente$sinco11_calidad
      n_destinos_rescate[rescate_cmo] <- puente$sinco11_n_destinos
    }
  }
  salida <- class_cuidado_rem(
    trabajo,
    valor_ocupado=valor_ocupado,
    aplicar_puente_cmo=!canonico,
    puente_cmo_precalculado=canonico
  )
  nuevas <- setdiff(names(salida), c(names(trabajo), "cmo_original", "sinco11",
                                    "sinco11_n_destinos", "sinco11_calidad"))
  for (v in nuevas) data[[v]] <- salida[[v]]
  if (length(rescate_cmo)) {
    data$puente_cmo_aplicado[rescate_cmo] <-
      !is.na(trabajo$sinco3d[rescate_cmo])
  }
  data$clasificador_ocupacion <- ifelse(cmo, "CMO",
                                       ifelse(p >= 20213, "SINCO 2019", "SINCO 2011"))
  data$version_scian <- salida$scian_version_cuidado
  data$codigo_ocupacion_original_cuidado <- original
  data$codigo_ocupacion_armonizado <- if (canonico) {
    trabajo$sinco3d
  } else {
    .cuidado_codigo(salida$sinco3d, 3L)
  }
  data$metodo_armonizacion_cuidado <- if (canonico) {
    rep("SINCO 2011 canonico de armonizar_sinco", n)
  } else {
    ifelse(
      cmo,
      "Puente analitico CMO: primera regla de Damian",
      if (raw) "SINCO observado: primeros tres digitos" else
        "SINCO observado a tres digitos"
    )
  }
  if (length(rescate_cmo)) {
    data$metodo_armonizacion_cuidado[rescate_cmo] <-
      "Puente analitico CMO de Damian en remanente; no equivalencia oficial"
  }
  calidad <- if (canonico) {
    as.character(data$calidad_puente_sinco)
  } else {
    ifelse(
      is.na(data$codigo_ocupacion_armonizado),
      "Codigo faltante o no clasificable",
      "SINCO observado"
    )
  }
  if (!canonico && any(cmo)) calidad[cmo] <- salida$sinco11_calidad[cmo]
  if (length(rescate_cmo)) calidad[rescate_cmo] <- calidad_rescate[rescate_cmo]
  data$calidad_armonizacion_cuidado <- calidad
  data$cuidado_n_destinos_puente_cmo <- n_destinos_rescate
  if (canonico) {
    data$clasificador_ocupacion_cuidado <- "SINCO 2011 armonizado"
    data$sinco_version_cuidado <- "SINCO 2011 armonizado"
  }
  mercado <- suppressWarnings(as.numeric(as.character(
    salida$trabajo_cuidado_mercado
  )))
  data$trabajo_cuidado_mercado <- mercado

  opcional_num <- function(nombre) {
    if (!nombre %in% names(data)) return(rep(NA_real_, n))
    suppressWarnings(as.numeric(as.character(data[[nombre]])))
  }
  posicion <- opcional_num("pos_ocu")
  ingreso_observado <- opcional_num("ingocup")
  ingreso_imputado <- opcional_num("ingocup_imp")
  fue_imputado <- opcional_num("imp_ingocup")
  sin_ingreso <- opcional_num("sin_pago")
  cuidado <- !is.na(mercado) & mercado == 1
  no_cuidado <- !is.na(mercado) & mercado == 0

  data$cuidado_posicion_remunerada <- dplyr::case_when(
    no_cuidado ~ 0,
    cuidado & posicion %in% 1:3 ~ 1,
    cuidado & posicion == 4 ~ 0,
    TRUE ~ NA_real_
  )
  data$cuidado_sin_pago <- dplyr::case_when(
    no_cuidado ~ 0,
    cuidado & posicion == 4 ~ 1,
    cuidado & posicion %in% 1:3 ~ 0,
    TRUE ~ NA_real_
  )

  observado_positivo <- cuidado & is.finite(ingreso_observado) &
    ingreso_observado > 0 & (is.na(fue_imputado) | fue_imputado != 1)
  imputado_positivo <- cuidado & fue_imputado == 1 &
    is.finite(ingreso_imputado) & ingreso_imputado > 0
  ingreso_cero <- cuidado & (
    posicion == 4 | sin_ingreso == 1 |
      (is.finite(ingreso_observado) & ingreso_observado == 0 &
         (is.na(fue_imputado) | fue_imputado != 1))
  )
  data$estado_ingreso_cuidado <- dplyr::case_when(
    observado_positivo ~ "observado_positivo",
    imputado_positivo ~ "imputado_positivo",
    ingreso_cero ~ "sin_ingreso_identificado",
    cuidado ~ "no_determinado",
    TRUE ~ NA_character_
  )

  data |>
    sjlabelled::var_labels(
      trabajo_cuidado_mercado = "Trabajadora/or de cuidado de mercado",
      cuidado_posicion_remunerada = "Cuidado de mercado en posici\u00F3n ocupacional remunerada",
      cuidado_sin_pago = "Cuidado de mercado en posici\u00F3n ocupacional sin pago",
      estado_ingreso_cuidado = "Estado de observaci\u00F3n o imputaci\u00F3n del ingreso en el cuidado de mercado"
    ) |>
    sjlabelled::val_labels(
      trabajo_cuidado_mercado = c("No" = 0, "S\u00ED" = 1),
      cuidado_posicion_remunerada = c("No" = 0, "S\u00ED" = 1),
      cuidado_sin_pago = c("No" = 0, "S\u00ED" = 1)
    )
}

# Validacion de formato; no pretende sustituir un catalogo exhaustivo.
.cuidado_codigo <- function(x, digitos) {
  y <- suppressWarnings(as.numeric(as.character(x)))
  valido <- is.finite(y) & y == floor(y) &
    y >= 10^(digitos - 1L) & y < 10^digitos - 1L
  y[!valido] <- NA_real_
  as.integer(y)
}
