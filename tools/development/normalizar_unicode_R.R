#!/usr/bin/env Rscript

# Auditoría y normalización controlada de caracteres no ASCII en R/*.R.
# Por defecto sólo inventaría. Use --write para convertir literales a escapes
# Unicode y transliterar comentarios, verificando que los valores de todos los
# literales permanezcan idénticos.

args <- commandArgs(trailingOnly = TRUE)
escritura <- "--write" %in% args
raiz <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
archivos <- sort(list.files(file.path(raiz, "R"), pattern = "[.]R$", full.names = TRUE))

tiene_no_ascii <- function(x) {
  vapply(x, function(y) any(utf8ToInt(enc2utf8(y)) > 127L), logical(1))
}

escapar_unicode <- function(x) {
  enteros <- utf8ToInt(enc2utf8(x))
  paste0(vapply(enteros, function(cp) {
    if (cp <= 127L) return(intToUtf8(cp))
    if (cp <= 65535L) return(sprintf("\\u%04X", cp))
    sprintf("\\U%08X", cp)
  }, character(1)), collapse = "")
}

transliterar_comentario <- function(x) {
  salida <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "byte")
  if (is.na(salida) || tiene_no_ascii(salida)) {
    stop("No se pudo transliterar de forma ASCII un comentario: ", x)
  }
  salida
}

valor_literal <- function(x) {
  valor <- eval(parse(text = x, encoding = "UTF-8"), envir = baseenv())
  if (!is.character(valor) || length(valor) != 1L) {
    stop("Token STR_CONST inesperado: ", x)
  }
  enc2utf8(valor)
}

inventariar <- function() {
  tokens <- list()
  k <- 0L
  for (archivo in archivos) {
    datos <- getParseData(parse(archivo, keep.source = TRUE, encoding = "UTF-8"))
    datos <- datos[tiene_no_ascii(datos$text),
                   c("line1", "col1", "line2", "col2", "token", "text")]
    if (!nrow(datos)) next
    datos$archivo <- substring(normalizePath(archivo, winslash = "/"), nchar(raiz) + 2L)
    k <- k + 1L
    tokens[[k]] <- datos
  }
  if (!length(tokens)) return(data.frame())
  do.call(rbind, tokens)
}

crear_linea_base <- function(tokens) {
  x <- tokens[tokens$token == "STR_CONST", , drop = FALSE]
  if (!nrow(x)) return(data.frame())
  x$valor <- vapply(x$text, valor_literal, character(1))
  x$ordinal <- ave(seq_len(nrow(x)), interaction(x$archivo, x$line1, drop = TRUE),
                   FUN = seq_along)
  x[, c("archivo", "line1", "ordinal", "valor")]
}

inventario <- inventariar()
if (!nrow(inventario)) {
  cat("No hay caracteres no ASCII en R/*.R.\n")
  quit(status = 0L)
}

if (!all(inventario$token %in% c("STR_CONST", "COMMENT"))) {
  print(inventario[!inventario$token %in% c("STR_CONST", "COMMENT"), ])
  stop("Hay caracteres no ASCII fuera de literales o comentarios.")
}
if (any(inventario$line1 != inventario$line2)) {
  stop("Hay tokens no ASCII multilínea; requieren revisión manual.")
}

linea_base <- crear_linea_base(inventario)
caracteres <- do.call(rbind, lapply(seq_len(nrow(inventario)), function(i) {
  cps <- utf8ToInt(enc2utf8(inventario$text[[i]]))
  cps <- cps[cps > 127L]
  if (!length(cps)) return(NULL)
  data.frame(
    archivo = inventario$archivo[[i]],
    linea = inventario$line1[[i]],
    token = inventario$token[[i]],
    caracter = intToUtf8(cps, multiple = TRUE),
    codigo = sprintf("U+%04X", cps),
    stringsAsFactors = FALSE
  )
}))

resumen <- aggregate(
  rep(1L, nrow(caracteres)),
  caracteres[c("archivo", "linea", "token", "caracter", "codigo")],
  sum
)
names(resumen)[[ncol(resumen)]] <- "ocurrencias"
resumen <- resumen[order(resumen$archivo, resumen$linea, resumen$token, resumen$codigo), ]

write.csv(resumen, file.path(raiz, "control", "inventario_no_ascii_0.2.0.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(linea_base, file.path(raiz, "inst", "extdata", "unicode_string_baseline.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("Archivos con Unicode:", length(unique(inventario$archivo)), "\n")
cat("Literales:", sum(inventario$token == "STR_CONST"), "\n")
cat("Comentarios:", sum(inventario$token == "COMMENT"), "\n")

if (!escritura) {
  cat("Inventario escrito; no se modificaron R/*.R. Use --write tras revisarlo.\n")
  quit(status = 0L)
}

for (archivo in archivos) {
  relativo <- substring(normalizePath(archivo, winslash = "/"), nchar(raiz) + 2L)
  cambios <- inventario[inventario$archivo == relativo, , drop = FALSE]
  if (!nrow(cambios)) next
  lineas <- readLines(archivo, encoding = "UTF-8", warn = FALSE)
  cambios <- cambios[order(cambios$line1, cambios$col1, decreasing = TRUE), ]
  for (i in seq_len(nrow(cambios))) {
    n <- cambios$line1[[i]]
    inicio <- cambios$col1[[i]]
    fin <- cambios$col2[[i]]
    original <- substr(lineas[[n]], inicio, fin)
    reemplazo <- if (cambios$token[[i]] == "STR_CONST") {
      escapar_unicode(original)
    } else {
      transliterar_comentario(original)
    }
    prefijo <- if (inicio > 1L) substr(lineas[[n]], 1L, inicio - 1L) else ""
    sufijo <- if (fin < nchar(lineas[[n]])) substr(lineas[[n]], fin + 1L, nchar(lineas[[n]])) else ""
    lineas[[n]] <- paste0(prefijo, reemplazo, sufijo)
  }
  writeLines(lineas, archivo, useBytes = TRUE)
}

despues <- inventariar()
if (nrow(despues)) {
  print(despues)
  stop("La normalización dejó caracteres no ASCII en R/*.R.")
}

recolectar_valores <- function() {
  salida <- list()
  k <- 0L
  for (archivo in archivos) {
    relativo <- substring(normalizePath(archivo, winslash = "/"), nchar(raiz) + 2L)
    datos <- getParseData(parse(archivo, keep.source = TRUE, encoding = "UTF-8"))
    datos <- datos[datos$token == "STR_CONST", , drop = FALSE]
    if (!nrow(datos)) next
    datos$valor <- vapply(datos$text, valor_literal, character(1))
    datos <- datos[tiene_no_ascii(datos$valor), , drop = FALSE]
    if (!nrow(datos)) next
    datos$archivo <- relativo
    datos$ordinal <- ave(seq_len(nrow(datos)), interaction(datos$archivo, datos$line1, drop = TRUE),
                         FUN = seq_along)
    k <- k + 1L
    salida[[k]] <- datos[, c("archivo", "line1", "ordinal", "valor")]
  }
  do.call(rbind, salida)
}

valores_despues <- recolectar_valores()
rownames(linea_base) <- NULL
rownames(valores_despues) <- NULL
if (!identical(linea_base, valores_despues)) {
  stop("Los valores visibles de los literales cambiaron durante la normalización.")
}
cat("Normalización terminada con equivalencia exacta de", nrow(linea_base), "literales.\n")
