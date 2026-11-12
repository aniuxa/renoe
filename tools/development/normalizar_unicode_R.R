#!/usr/bin/env Rscript
# Normaliza literales R a escapes ASCII sin alterar sus valores visibles.
# Sin --write solo informa; con --write valida todos los literales antes de
# escribir y actualiza la referencia para la prueba Unicode.
args <- commandArgs(trailingOnly = TRUE)
escritura <- '--write' %in% args
raiz <- normalizePath(getwd(), winslash = '/', mustWork = TRUE)
archivos <- sort(list.files(file.path(raiz, 'R'), pattern = '[.]R$', full.names = TRUE))
tiene_no_ascii <- function(x) vapply(x, function(y) any(utf8ToInt(enc2utf8(y)) > 127L), logical(1))
escapar <- function(x) paste0(vapply(utf8ToInt(enc2utf8(x)), function(cp) {
  if (cp <= 127L) intToUtf8(cp) else if (cp <= 65535L) sprintf('\\u%04X', cp) else sprintf('\\U%08X', cp)
}, character(1)), collapse = '')
valor <- function(x) enc2utf8(eval(parse(text = x, encoding = 'UTF-8'), envir = baseenv()))
tokens <- function(lineas) {
  p <- getParseData(parse(text = lineas, keep.source = TRUE, encoding = 'UTF-8'))
  p[p$token == 'STR_CONST', , drop = FALSE]
}
resultados <- lapply(archivos, function(archivo) {
  original <- readLines(archivo, encoding = 'UTF-8', warn = FALSE)
  antes <- tokens(original)
  valores_antes <- vapply(antes$text, valor, character(1))
  cambios <- antes[tiene_no_ascii(antes$text), , drop = FALSE]
  if (nrow(cambios) && any(cambios$line1 != cambios$line2)) stop('Literal Unicode multilinea: ', archivo)
  despues <- original
  if (nrow(cambios)) {
    cambios <- cambios[order(cambios$line1, cambios$col1, decreasing = TRUE), , drop = FALSE]
    for (i in seq_len(nrow(cambios))) {
      n <- cambios$line1[i]; inicio <- cambios$col1[i]; fin <- cambios$col2[i]
      token <- substr(despues[n], inicio, fin)
      if (!identical(token, cambios$text[i])) stop('Posicion inesperada en ', archivo, ':', n)
      antes_texto <- if (inicio > 1L) substr(despues[n], 1L, inicio - 1L) else ''
      despues_texto <- if (fin < nchar(despues[n])) substr(despues[n], fin + 1L, nchar(despues[n])) else ''
      despues[n] <- paste0(antes_texto, escapar(token), despues_texto)
    }
  }
  comprobacion <- tokens(despues)
  valores_despues <- vapply(comprobacion$text, valor, character(1))
  if (!identical(unname(valores_antes), unname(valores_despues))) stop('Cambio de literales en ', archivo)
  list(archivo=archivo, lineas=despues, cambios=nrow(cambios), tokens=comprobacion, valores=valores_despues)
})
cat('Literales a normalizar:', sum(vapply(resultados, `[[`, integer(1), 'cambios')), '\n')
if (!escritura) {
  cat('Auditoria sin escritura. Use --write para aplicar.\n')
  quit(status=0L)
}
referencia <- lapply(resultados, function(r) {
  x <- r$tokens
  x$valor <- r$valores
  x <- x[tiene_no_ascii(x$valor), , drop=FALSE]
  if (!nrow(x)) return(NULL)
  x$archivo <- paste0('R/', basename(r$archivo))
  x$ordinal <- ave(seq_len(nrow(x)), interaction(x$archivo, x$line1, drop=TRUE), FUN=seq_along)
  x[,c('archivo','line1','ordinal','valor')]
})
referencia <- do.call(rbind, referencia)
for (r in resultados) if (r$cambios > 0L) writeLines(r$lineas, r$archivo, useBytes=TRUE)
write.csv(referencia, file.path(raiz, 'inst/extdata/unicode_string_baseline.csv'), row.names=FALSE, fileEncoding='UTF-8')
cat('Referencia actualizada con', nrow(referencia), 'literales visibles; valores verificados.\n')
