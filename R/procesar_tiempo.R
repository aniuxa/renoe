#' Procesar variables de tiempo en actividades del hogar y cuidado
#'
#' Calcula duraciones semanales a partir de las baterias `p11_*` (cuestionario
#' ampliado) y `p9_*` (cuestionario basico) de la ENOE. Conserva los campos
#' fuente y distingue duracion observada, actividad realizada con duracion
#' desconocida (98), realizacion desconocida (99), reactivo no seleccionado y
#' reactivo que no existe en la version del instrumento.
#'
#' La bateria cambio en 2013. Hasta 2012 contiene seis actividades y el reactivo
#' de cuidado incluye los traslados. Desde 2013 contiene ocho: separa traslados
#' del cuidado y agrega compras, cuentas, tramites y seguridad del hogar.
#'
#' Todas las duraciones derivadas se expresan en horas. `t_cuidado_directo` solo
#' es identificable desde 2013. `t_cuidado_amplio` armoniza el contenido anterior
#' sumando cuidado y traslado desde 2013.
#' `t_trabajo_hogar_indirecto_armonizado` usa construccion, reparacion y
#' quehaceres. `t_trabajo_hogar_armonizado` suma cuidado amplio y trabajo
#' indirecto realizado para el propio hogar. Los servicios comunitarios se
#' conservan en `t_comun`, pero no integran estas sumas. La armonizacion no
#' elimina la ruptura de medicion observada en 2013;
#' `t_total_instrumento` suma todos los reactivos no educativos disponibles en
#' cada version y, por ello, no debe usarse como serie homogenea.
#'
#' @param data Data frame fusionado por `fusion_enoe()` o cargado directamente.
#' @param anio Ano del trimestre, usado si falta `anio` en `data`.
#' @param trimestre Trimestre 1-4, usado si faltan metadatos en `data`.
#'
#' @return El mismo data frame, en el mismo orden, con duraciones, estados de
#'   medicion, version del instrumento y agregados conceptuales.
#' @export
#' @family procesamiento_enoe
procesar_tiempo <- function(data, anio, trimestre) {
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
    usar <- corresponde & !is.na(corresponde)
    bateria_medible[usar] <- observados[usar]
  }

  minutos <- setNames(lapply(actividades, function(x) rep(NA_real_, n)), actividades)
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
    estados[[actividad]][indice] <<- estado[indice]
    minutos[[actividad]][indice] <<- valor[indice]
    invisible(NULL)
  }

  for (prefijo in c("p11", "p9")) {
    es_tipo <- tipo_obs == if (prefijo == "p11") "ampliado" else "basico"
    es_tipo[is.na(es_tipo)] <- FALSE
    version_6 <- es_tipo & !is.na(anio_obs) & anio_obs <= 2012
    version_8 <- es_tipo & !is.na(anio_obs) & anio_obs >= 2013
    asignar("estudiar", prefijo, 1, version_6 | version_8)
    asignar("cuidado", prefijo, 2, version_6 | version_8)
    asignar("construir", prefijo, 3, version_6)
    asignar("reparar", prefijo, 4, version_6)
    asignar("quehacer", prefijo, 5, version_6)
    asignar("comun", prefijo, 6, version_6)
    asignar("compras", prefijo, 3, version_8)
    asignar("traslado", prefijo, 4, version_8)
    asignar("construir", prefijo, 5, version_8)
    asignar("reparar", prefijo, 6, version_8)
    asignar("quehacer", prefijo, 7, version_8)
    asignar("comun", prefijo, 8, version_8)
  }

  for (actividad in actividades) {
    no_aplica <- !preguntada[[actividad]]
    minutos[[actividad]][no_aplica] <- NA_real_
    estados[[actividad]][no_aplica] <- "no_aplica_instrumento"
  }

  matriz <- do.call(cbind, minutos)
  matriz_estado <- do.call(cbind, estados)
  estados_invalidos <- c(
    "realizada_duracion_desconocida", "realizacion_desconocida",
    "duracion_incompleta", "valor_invalido"
  )
  version_6 <- !is.na(anio_obs) & anio_obs <= 2012
  version_8 <- !is.na(anio_obs) & anio_obs >= 2013

  sumar_completo <- function(columnas_6, columnas_8) {
    valor <- rep(NA_real_, n)
    incompleto <- rep(NA, n)
    asignar_grupo <- function(indice, columnas) {
      indice <- indice & bateria_medible
      if (!any(indice)) return(invisible(NULL))
      estado_grupo <- matriz_estado[indice, columnas, drop = FALSE]
      invalida <- matrix(
        estado_grupo %in% estados_invalidos,
        nrow = nrow(estado_grupo), ncol = ncol(estado_grupo)
      )
      inc <- rowSums(invalida) > 0
      suma <- rowSums(matriz[indice, columnas, drop = FALSE], na.rm = TRUE) / 60
      posiciones <- which(indice)
      incompleto[posiciones] <<- inc
      valor[posiciones[!inc]] <<- suma[!inc]
      invisible(NULL)
    }
    asignar_grupo(version_6, columnas_6)
    asignar_grupo(version_8, columnas_8)
    list(valor = valor, incompleto = incompleto)
  }

  indirectas_hogar <- c("construir", "reparar", "quehacer")
  actividades_instrumento <- c(indirectas_hogar, "comun")
  indirecto <- sumar_completo(indirectas_hogar, indirectas_hogar)
  cuidado_amplio <- sumar_completo("cuidado", c("cuidado", "traslado"))
  trabajo_hogar_armonizado <- sumar_completo(
    c("cuidado", indirectas_hogar),
    c("cuidado", "traslado", indirectas_hogar)
  )
  total_instrumento <- sumar_completo(
    c("cuidado", actividades_instrumento),
    c("cuidado", "compras", "traslado", actividades_instrumento)
  )

  for (actividad in actividades) {
    data[[paste0("t_", actividad)]] <- minutos[[actividad]] / 60
    data[[paste0("t_", actividad, "_estado")]] <- estados[[actividad]]
  }
  data$tiempo_medible <- bateria_medible
  data$tiempo_version_instrumento <- ifelse(
    version_6, "6_actividades_cuidado_incluye_traslado",
    ifelse(version_8, "8_actividades_traslado_separado", NA_character_)
  )
  data$t_cuidado_definicion <- ifelse(
    version_6, "cuidado_incluye_traslado",
    ifelse(version_8, "cuidado_directo_sin_traslado", NA_character_)
  )
  data$t_cuidado_directo <- ifelse(version_8, data$t_cuidado, NA_real_)
  data$t_cuidado_directo_estado <- ifelse(
    version_8, data$t_cuidado_estado, "no_aplica_instrumento"
  )
  data$t_cuidado_amplio <- cuidado_amplio$valor
  data$t_cuidado_amplio_incompleto <- cuidado_amplio$incompleto
  data$t_trabajo_hogar_indirecto_armonizado <- indirecto$valor
  data$t_trabajo_hogar_indirecto_armonizado_incompleto <- indirecto$incompleto
  data$t_trabajo_hogar_armonizado <- trabajo_hogar_armonizado$valor
  data$t_trabajo_hogar_armonizado_incompleto <-
    trabajo_hogar_armonizado$incompleto
  data$t_total_instrumento <- total_instrumento$valor
  data$t_total_instrumento_incompleto <- total_instrumento$incompleto

  data |>
    sjlabelled::var_labels(
      t_estudiar = "Tiempo dedicado a estudiar o tomar cursos (horas)",
      t_cuidado = "Tiempo del reactivo de cuidado; incluye traslados hasta 2012 (horas)",
      t_cuidado_directo = "Cuidado directo sin pago, identificable desde 2013 (horas)",
      t_cuidado_amplio = "Cuidado sin pago y traslados de integrantes del hogar (horas)",
      t_construir = "Tiempo dedicado a construir o ampliar la vivienda (horas)",
      t_reparar = "Tiempo dedicado a reparar bienes del hogar (horas)",
      t_quehacer = "Tiempo dedicado a quehaceres del hogar (horas)",
      t_comun = "Tiempo dedicado a servicios gratuitos a la comunidad (horas)",
      t_compras = "Tiempo dedicado a compras, tramites y seguridad del hogar (horas)",
      t_traslado = "Tiempo dedicado a traslados de integrantes del hogar (horas)",
      tiempo_medible = "Bateria de uso del tiempo con alguna respuesta observada",
      tiempo_version_instrumento = "Version sustantiva de la bateria de uso del tiempo",
      t_cuidado_definicion = "Contenido del reactivo fuente de cuidado",
      t_trabajo_hogar_indirecto_armonizado = "Construccion, reparacion y quehaceres para el propio hogar; contenido armonizado (horas)",
      t_trabajo_hogar_armonizado = "Cuidado amplio y trabajo indirecto para el propio hogar; contenido armonizado (horas)",
      t_total_instrumento = "Todos los reactivos no educativos, incluidos servicios comunitarios (horas)"
    )
}
