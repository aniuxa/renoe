#' Procesar variables de tiempo en actividades del hogar y cuidado
#'
#' Calcula duraciones semanales a partir de las baterías `p11_*` y `p9_*` de
#' ENOE. Conserva sin cambios los campos fuente y distingue duración observada,
#' actividad realizada con duración desconocida (98), realización desconocida
#' (99), reactivo no seleccionado y batería no medible.
#'
#' El orden de las actividades cambió en 2011. Hasta 2010, los reactivos 3 a 6
#' corresponden a construcción, reparación, quehaceres y servicios a la
#' comunidad. Desde 2011, los reactivos 3 y 4 corresponden a compras y traslados,
#' y las cuatro actividades anteriores pasan a los reactivos 5 a 8.
#'
#' Las variables específicas `t_*` se expresan en horas. `t_total`, `t_total0`
#' y sus versiones parciales se expresan en minutos; los sufijos `_hrs` son sus
#' equivalentes en horas. Un total completo es `NA` cuando contiene una
#' actividad con duración desconocida, realización desconocida, información
#' incompleta o inválida. El total parcial suma sólo las duraciones observadas y
#' los ceros de reactivos no seleccionados.
#'
#' Las columnas `*_legacy` reproducen el contrato histórico, que convertía a
#' cero los códigos 98, 99 y todos los faltantes. El argumento
#' `tratamiento_faltantes = "historico_cero"` permite mantener temporalmente
#' ese resultado en los nombres principales.
#'
#' @param data Data frame fusionado por `fusion_enoe()` o cargado directamente.
#' @param anio Año del trimestre, usado si falta `anio` en `data`.
#' @param trimestre Trimestre 1–4, usado si faltan metadatos en `data`.
#' @param tratamiento_faltantes Contrato de las variables principales:
#'   `"distinguir"` conserva los estados y `NA`; `"historico_cero"` reproduce
#'   la conversión histórica a cero. En ambos casos se crean columnas legacy.
#'
#' @return El mismo data frame, en el mismo orden, con duraciones, estados de
#'   medición, totales completos y parciales, y resultados históricos.
#' @export
#' @family procesamiento_enoe
procesar_tiempo <- function(
    data, anio, trimestre,
    tratamiento_faltantes = c("distinguir", "historico_cero")) {

  tratamiento_faltantes <- match.arg(tratamiento_faltantes)
  if (!all(c("anio", "coe_tipo") %in% names(data))) {
    message("Variables 'anio' y/o 'coe_tipo' no encontradas. Se procesan con `procesar_vars_sociodemo()`...")
    data <- procesar_vars_sociodemo(data, anio = anio, trimestre = trimestre)
  }

  n <- nrow(data)
  anio_obs <- suppressWarnings(as.integer(as.character(data$anio)))
  tipo_obs <- tolower(trimws(as.character(data$coe_tipo)))
  actividades <- c(
    "estudiar", "cuidado", "construir", "reparar",
    "quehacer", "comun", "compras", "traslado"
  )

  raw_num <- function(nombre) {
    if (!nombre %in% names(data)) return(rep(NA_real_, n))
    suppressWarnings(as.numeric(as.character(data[[nombre]])))
  }
  raw_observado <- function(nombre) {
    if (!nombre %in% names(data)) return(rep(FALSE, n))
    x <- data[[nombre]]
    !is.na(x) & nzchar(trimws(as.character(x)))
  }

  bateria_medible <- rep(FALSE, n)
  for (prefijo in c("p11", "p9")) {
    corresponde <- tipo_obs == if (prefijo == "p11") "ampliado" else "basico"
    observados <- rep(FALSE, n)
    for (i in 1:8) {
      observados <- observados |
        raw_observado(paste0(prefijo, "_", i)) |
        raw_observado(paste0(prefijo, "_h", i)) |
        raw_observado(paste0(prefijo, "_m", i))
    }
    bateria_medible[corresponde & !is.na(corresponde)] <-
      observados[corresponde & !is.na(corresponde)]
  }

  minutos <- setNames(lapply(actividades, function(x) rep(NA_real_, n)), actividades)
  minutos_legacy <- setNames(lapply(actividades, function(x) rep(0, n)), actividades)
  estados <- setNames(lapply(actividades, function(x) rep("no_aplica_instrumento", n)), actividades)
  preguntada <- setNames(lapply(actividades, function(x) rep(FALSE, n)), actividades)

  asignar <- function(actividad, prefijo, item, indice) {
    if (!any(indice)) return(invisible(NULL))
    vh <- paste0(prefijo, "_h", item)
    vm <- paste0(prefijo, "_m", item)
    vo <- paste0(prefijo, "_", item)
    existe <- vh %in% names(data) && vm %in% names(data)
    preguntada[[actividad]][indice] <<- existe
    if (!existe) return(invisible(NULL))

    h <- raw_num(vh)
    m <- raw_num(vm)
    seleccionada <- raw_observado(vo) | !is.na(h) | !is.na(m)
    hora_valida <- is.finite(h) & h == floor(h) & h %in% 0:97
    minuto_valido <- is.finite(m) & m == floor(m) & m %in% 0:59
    duracion_valida <- hora_valida & minuto_valido
    invalido <- (!is.na(h) & !hora_valida & !h %in% c(98, 99)) |
      (!is.na(m) & !minuto_valido)

    estado <- rep(NA_character_, n)
    estado[!bateria_medible] <- "bateria_no_medible"
    estado[bateria_medible & !seleccionada] <- "no_seleccionada"
    estado[bateria_medible & seleccionada & !is.na(h) & h == 98] <-
      "realizada_duracion_desconocida"
    estado[bateria_medible & seleccionada & !is.na(h) & h == 99] <-
      "realizacion_desconocida"
    estado[bateria_medible & seleccionada & invalido] <- "valor_invalido"
    estado[bateria_medible & seleccionada & !invalido &
             !duracion_valida & !h %in% c(98, 99)] <- "duracion_incompleta"
    estado[bateria_medible & duracion_valida] <- "duracion_observada"

    valor <- rep(NA_real_, n)
    valor[estado == "no_seleccionada"] <- 0
    valor[duracion_valida] <- h[duracion_valida] * 60 + m[duracion_valida]
    legado <- ifelse(duracion_valida, h * 60 + m, 0)

    estados[[actividad]][indice] <<- estado[indice]
    minutos[[actividad]][indice] <<- valor[indice]
    minutos_legacy[[actividad]][indice] <<- legado[indice]
    invisible(NULL)
  }

  for (prefijo in c("p11", "p9")) {
    es_tipo <- tipo_obs == if (prefijo == "p11") "ampliado" else "basico"
    es_tipo[is.na(es_tipo)] <- FALSE
    pre <- es_tipo & !is.na(anio_obs) & anio_obs < 2011
    post <- es_tipo & !is.na(anio_obs) & anio_obs >= 2011
    asignar("estudiar", prefijo, 1, pre | post)
    asignar("cuidado", prefijo, 2, pre | post)
    asignar("construir", prefijo, 3, pre)
    asignar("reparar", prefijo, 4, pre)
    asignar("quehacer", prefijo, 5, pre)
    asignar("comun", prefijo, 6, pre)
    asignar("compras", prefijo, 3, post)
    asignar("traslado", prefijo, 4, post)
    asignar("construir", prefijo, 5, post)
    asignar("reparar", prefijo, 6, post)
    asignar("quehacer", prefijo, 7, post)
    asignar("comun", prefijo, 8, post)
  }

  # Actividades ausentes en una versión del instrumento conservan cero para
  # compatibilidad y quedan identificadas por su estado.
  for (actividad in actividades) {
    no_aplica <- !preguntada[[actividad]]
    minutos[[actividad]][no_aplica] <- 0
    estados[[actividad]][no_aplica] <- "no_aplica_instrumento"
  }

  matriz <- do.call(cbind, minutos)
  matriz_legacy <- do.call(cbind, minutos_legacy)
  matriz_estado <- do.call(cbind, estados)
  actividades_total <- setdiff(actividades, "estudiar")
  actividades_total0 <- setdiff(actividades_total, c("compras", "traslado"))
  incompleto <- apply(
    matriz_estado[, actividades_total, drop = FALSE],
    1,
    function(x) any(x %in% c(
      "realizada_duracion_desconocida", "realizacion_desconocida",
      "duracion_incompleta", "valor_invalido"
    ))
  )
  if (!n) incompleto <- logical()
  incompleto[!bateria_medible] <- NA
  incompleto0 <- apply(
    matriz_estado[, actividades_total0, drop = FALSE],
    1,
    function(x) any(x %in% c(
      "realizada_duracion_desconocida", "realizacion_desconocida",
      "duracion_incompleta", "valor_invalido"
    ))
  )
  if (!n) incompleto0 <- logical()
  incompleto0[!bateria_medible] <- NA

  sumar_parcial <- function(columnas) {
    z <- matriz[, columnas, drop = FALSE]
    z[is.na(z)] <- 0
    resultado <- rowSums(z)
    resultado[!bateria_medible] <- NA_real_
    resultado
  }
  total_parcial <- sumar_parcial(actividades_total)
  total0_parcial <- sumar_parcial(actividades_total0)
  total <- total_parcial
  total0 <- total0_parcial
  total[!is.na(incompleto) & incompleto] <- NA_real_
  total0[!is.na(incompleto0) & incompleto0] <- NA_real_
  total_legacy <- rowSums(matriz_legacy[, actividades_total, drop = FALSE])
  total0_legacy <- rowSums(
    matriz_legacy[, actividades_total0, drop = FALSE]
  )

  for (actividad in actividades) {
    principal <- minutos[[actividad]] / 60
    legado <- minutos_legacy[[actividad]] / 60
    if (tratamiento_faltantes == "historico_cero") principal <- legado
    data[[paste0("t_", actividad)]] <- principal
    data[[paste0("t_", actividad, "_estado")]] <- estados[[actividad]]
    data[[paste0("t_", actividad, "_legacy")]] <- legado
  }
  if (tratamiento_faltantes == "historico_cero") {
    total <- total_legacy
    total0 <- total0_legacy
  }
  data$tiempo_medible <- bateria_medible
  data$t_total_incompleto <- incompleto
  data$t_total0_incompleto <- incompleto0
  data$t_total_parcial <- total_parcial
  data$t_total0_parcial <- total0_parcial
  data$t_total <- total
  data$t_total0 <- total0
  data$t_total_hrs <- total / 60
  data$t_total_hrs0 <- total0 / 60
  data$t_total_legacy <- total_legacy
  data$t_total0_legacy <- total0_legacy
  data$t_total_hrs_legacy <- total_legacy / 60
  data$t_total_hrs0_legacy <- total0_legacy / 60

  data |>
    sjlabelled::var_labels(
      t_estudiar = "Tiempo dedicado a estudiar o tomar cursos (horas)",
      t_cuidado = "Tiempo dedicado al cuidado exclusivo sin pago (horas)",
      t_construir = "Tiempo dedicado a construir o ampliar la vivienda (horas)",
      t_reparar = "Tiempo dedicado a reparar bienes del hogar (horas)",
      t_quehacer = "Tiempo dedicado a quehaceres del hogar (horas)",
      t_comun = "Tiempo dedicado a servicios gratuitos a la comunidad (horas)",
      t_compras = "Tiempo dedicado a compras, trámites y seguridad del hogar (horas)",
      t_traslado = "Tiempo dedicado a traslados de integrantes del hogar (horas)",
      tiempo_medible = "Batería de uso del tiempo con alguna respuesta observada",
      t_total_incompleto = "Total de tiempo afectado por duración o realización desconocida",
      t_total0_incompleto = "Total sin traslados ni compras afectado por duración o realización desconocida",
      t_total_parcial = "Suma parcial de actividades con duración observada (minutos)",
      t_total0_parcial = "Suma parcial sin traslados ni compras (minutos)",
      t_total = "Suma completa de actividades del hogar y cuidado (minutos)",
      t_total0 = "Suma completa sin traslados ni compras (minutos)",
      t_total_hrs = "Suma completa de actividades del hogar y cuidado (horas)",
      t_total_hrs0 = "Suma completa sin traslados ni compras (horas)",
      t_total_legacy = "Suma histórica que convierte faltantes y códigos especiales a cero (minutos)",
      t_total0_legacy = "Suma histórica sin compras ni traslados (minutos)"
    )
}
