args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("Uso: Rscript generar_diccionario_variables.R <carpeta_paquete> <salida_csv>")
}

carpeta_paquete <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
salida_csv <- args[[2]]
archivos_r <- list.files(
  file.path(carpeta_paquete, "R"),
  pattern = "\\.[Rr]$",
  full.names = TRUE
)

nombre_llamada <- function(x) {
  if (!is.call(x)) return("")
  paste(deparse(x[[1]], width.cutoff = 500L), collapse = "")
}

texto_literal <- function(x) {
  if (is.character(x) && length(x) == 1L) return(x)
  NA_character_
}

extraer_funcion <- function(cuerpo, funcion, archivo) {
  creadas <- character()
  etiquetas <- list()

  recorrer <- function(x) {
    if (!is.call(x) && !is.pairlist(x) && !is.expression(x)) return(invisible(NULL))

    if (is.call(x)) {
      llamada <- nombre_llamada(x)
      argumentos <- as.list(x)[-1]
      nombres <- names(argumentos)
      if (is.null(nombres)) nombres <- rep("", length(argumentos))

      if (llamada %in% c(
        "mutate", "dplyr::mutate", "summarise", "dplyr::summarise",
        "summarize", "dplyr::summarize", "transmute", "dplyr::transmute"
      )) {
        candidatos <- nombres[nzchar(nombres)]
        candidatos <- setdiff(candidatos, c(".by", ".groups", ".keep", ".before", ".after"))
        creadas <<- union(creadas, candidatos)

        for (i in seq_along(argumentos)) {
          variable <- nombres[[i]]
          rhs <- argumentos[[i]]
          if (!nzchar(variable) || !is.call(rhs)) next
          rhs_nombre <- nombre_llamada(rhs)
          if (rhs_nombre %in% c("set_label", "sjlabelled::set_label")) {
            rhs_args <- as.list(rhs)[-1]
            etiqueta <- if (length(rhs_args) >= 2L) texto_literal(rhs_args[[2]]) else NA_character_
            if (!is.na(etiqueta)) etiquetas[[variable]] <<- etiqueta
          }
        }
      }

      if (llamada %in% c("var_labels", "sjlabelled::var_labels")) {
        for (i in seq_along(argumentos)) {
          variable <- nombres[[i]]
          if (!nzchar(variable)) next
          etiqueta <- texto_literal(argumentos[[i]])
          if (!is.na(etiqueta)) etiquetas[[variable]] <<- etiqueta
        }
      }
    }

    for (i in seq_along(x)) {
      try(recorrer(x[[i]]), silent = TRUE)
    }
    invisible(NULL)
  }

  recorrer(cuerpo)
  creadas <- creadas[!startsWith(creadas, ".")]

  if (length(creadas) == 0L) return(NULL)

  data.frame(
    variable_nombre = creadas,
    descripcion = vapply(
      creadas,
      function(variable) {
        valor <- etiquetas[[variable]]
        if (is.null(valor)) "Pendiente de documentar" else valor
      },
      character(1)
    ),
    funcion = funcion,
    stringsAsFactors = FALSE
  )
}

resultados <- list()

for (archivo in archivos_r) {
  expresiones <- tryCatch(parse(archivo, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(expresiones)) next

  for (expresion in expresiones) {
    if (!is.call(expresion) || length(expresion) < 3L) next
    if (!identical(as.character(expresion[[1]]), "<-")) next
    if (!is.symbol(expresion[[2]]) || !is.call(expresion[[3]])) next
    if (!identical(as.character(expresion[[3]][[1]]), "function")) next

    funcion <- as.character(expresion[[2]])
    cuerpo <- expresion[[3]][[3]]
    tabla <- extraer_funcion(cuerpo, funcion, basename(archivo))
    if (!is.null(tabla)) resultados[[length(resultados) + 1L]] <- tabla
  }
}

diccionario <- do.call(rbind, resultados)
diccionario <- unique(diccionario)

# Variables auxiliares creadas durante el cálculo pero eliminadas antes de que
# la función devuelva el resultado. No forman parte del diccionario de salida.
variables_internas <- c(
  "needs_manual_1d", "periodo_sinco", "sinco3d_nuevo", "sinco4d_str",
  "trimestre_n", "mismatch_raw", "var_exp_previa", "aplicar_reglas",
  "cmo", "str_cmo", "cmo2d", "cmo3d", "ent"
)
diccionario <- diccionario[!diccionario$variable_nombre %in% variables_internas, ]

# Orden habitual del flujo de procesamiento. Cuando una variable vuelve a
# aparecer en una función posterior (por ejemplo, porque se resume o conserva),
# el diccionario la atribuye únicamente a la primera función que la construye.
orden_funciones <- c(
  "crear_folios",
  "procesar_vars_sociodemo",
  "procesar_vars_hogar",
  "procesar_tiempo",
  "ipc_enoe",
  "imputa_ingocup",
  "armoniza_sinco",
  "cmo_to_sinco",
  "cmo_to_sinco1d",
  "procesar_vars_laborales",
  "calcular_desajuste_estadistico",
  "procesar_contribucion_hogar",
  "procesar_cuidado_extra",
  "procesar_estudio_trabajo",
  "procesar_libro1",
  "class_cuidado_rem",
  "procesar_cuidado_remunerado"
)

seleccionar_primera_funcion <- function(tabla) {
  rango <- match(tabla$funcion, orden_funciones)
  rango[is.na(rango)] <- length(orden_funciones) + 1L
  elegida <- which.min(rango)

  # Una función posterior puede tener una etiqueta más completa. Se aprovecha
  # esa descripción sin cambiar la función a la que se atribuye la creación.
  descripciones_validas <- tabla$descripcion[
    tabla$descripcion != "Pendiente de documentar"
  ]
  descripcion <- if (length(descripciones_validas) > 0L) {
    descripciones_validas[[1]]
  } else {
    "Pendiente de documentar"
  }

  data.frame(
    variable_nombre = tabla$variable_nombre[[elegida]],
    descripcion = descripcion,
    funcion = tabla$funcion[[elegida]],
    stringsAsFactors = FALSE
  )
}

diccionario <- do.call(
  rbind,
  lapply(split(diccionario, diccionario$variable_nombre), seleccionar_primera_funcion)
)

# Estas salidas se crean mediante asignación `data$variable <- valor`, que el
# recorrido sintáctico deliberadamente simple no detecta. Se declaran aquí para
# que el catálogo sea reproducible y no dependa de una edición manual del CSV.
actividad_tiempo <- c(
  "estudiar", "cuidado", "construir", "reparar",
  "quehacer", "comun", "compras", "traslado"
)
documentadas <- data.frame(
  variable_nombre = c(
    "trabajo_cuidado_mercado", "trabajo_cuidado_rem",
    "cuidado_posicion_remunerada", "cuidado_sin_pago",
    "estado_ingreso_cuidado", "periodo_referencia_mismatch2",
    "ponderador_mismatch2", "trimestres_referencia_mismatch2",
    "unidad_referencia_mismatch2", "tiempo_medible",
    "t_total_incompleto", "t_total0_incompleto",
    "t_total_parcial", "t_total0_parcial",
    paste0("t_", actividad_tiempo, "_estado"),
    paste0("t_", actividad_tiempo, "_legacy"),
    "t_total_legacy", "t_total0_legacy",
    "t_total_hrs_legacy", "t_total_hrs0_legacy"
  ),
  descripcion = c(
    "Indicador principal de inserción ocupacional en el cuidado de mercado",
    "Alias deprecado de trabajo_cuidado_mercado",
    "Posición en el trabajo de cuidado de mercado que presupone remuneración",
    "Posición en el trabajo de cuidado de mercado declarada sin pago",
    "Estado de observación o imputación del ingreso en el cuidado de mercado",
    "Periodo usado para la referencia estadística de escolaridad",
    "Ponderador usado para la referencia estadística de escolaridad",
    "Número de trimestres acumulados en la referencia estadística",
    "Unidad analítica de la referencia estadística",
    "Batería de uso del tiempo con alguna respuesta observada",
    "Total afectado por duración o realización desconocida",
    "Total sin traslados ni compras afectado por duración o realización desconocida",
    "Suma parcial de actividades con duración observada, en minutos",
    "Suma parcial sin traslados ni compras, en minutos",
    paste("Estado de medición del tiempo dedicado a", actividad_tiempo),
    paste("Versión histórica en horas del tiempo dedicado a", actividad_tiempo),
    "Suma histórica que recodifica faltantes y códigos especiales a cero, en minutos",
    "Suma histórica sin compras ni traslados, en minutos",
    "Suma histórica de actividades del hogar y cuidado, en horas",
    "Suma histórica sin compras ni traslados, en horas"
  ),
  funcion = c(
    rep("procesar_cuidado_remunerado", 5),
    rep("calcular_desajuste_estadistico", 4),
    rep("procesar_tiempo", 25)
  ),
  stringsAsFactors = FALSE
)
diccionario <- diccionario[
  !diccionario$variable_nombre %in% documentadas$variable_nombre,
  , drop = FALSE
]
diccionario <- rbind(diccionario, documentadas)
diccionario <- diccionario[order(diccionario$variable_nombre), ]
rownames(diccionario) <- NULL

dir.create(dirname(salida_csv), recursive = TRUE, showWarnings = FALSE)
write.csv(diccionario, salida_csv, row.names = FALSE, fileEncoding = "UTF-8")

cat("Variables-función:", nrow(diccionario), "\n")
cat("Variables únicas:", length(unique(diccionario$variable_nombre)), "\n")
cat("Pendientes de documentar:", sum(diccionario$descripcion == "Pendiente de documentar"), "\n")
