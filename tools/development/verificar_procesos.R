#!/usr/bin/env Rscript

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
process_dir <- file.path(root, "maintainers", "processes")
expected <- c(
  "README.md",
  "P01-paquete-release.md",
  "P02-documentacion-pkgdown.md",
  "P03-adquisicion-preparacion-enoe.md",
  "P04-armonizacion-clasificadores.md",
  "P05-historico-staging-invariantes.md",
  "P06-paneles-pini.md",
  "P07-tabulados-productos-academicos.md",
  "P08-bundle-shiny.md",
  "P09-correccion-hotfix-rollback.md"
)
required_sections <- c(
  "## Prop\u00F3sito y alcance",
  "## Usuario y responsable",
  "## Entrada can\u00F3nica y productor",
  "## Pasos y decisiones",
  "## Salidas y consumidores",
  "## Escenarios y perfiles aplicables",
  "## Controles y compuertas GO/NO-GO",
  "## Trazabilidad m\u00EDnima",
  "## Dependencias y regla de invalidaci\u00F3n descendente",
  "## Reanudaci\u00F3n y rollback",
  "## Documentaci\u00F3n que debe actualizarse",
  "## Historial de cambios",
  "## Esquema reproducible"
)
failures <- character()
fail <- function(...) failures <<- c(failures, paste0(...))
read_utf8 <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con))
  text <- readChar(con, nchars = file.info(path)$size, useBytes = TRUE)
  Encoding(text) <- "UTF-8"
  text
}

missing <- expected[!file.exists(file.path(process_dir, expected))]
if (length(missing)) fail("Faltan archivos: ", paste(missing, collapse = ", "))

balanced <- function(text) {
  chars <- strsplit(text, "", fixed = TRUE)[[1L]]
  opening <- c("(" = ")", "[" = "]", "{" = "}")
  stack <- character()
  for (ch in chars) {
    if (ch %in% names(opening)) stack <- c(stack, opening[[ch]])
    if (ch %in% unname(opening)) {
      if (!length(stack) || tail(stack, 1L) != ch) return(FALSE)
      stack <- head(stack, -1L)
    }
  }
  !length(stack)
}

validate_mermaid <- function(label, text, expected_blocks = 1L) {
  pattern <- "(?s)```mermaid[[:space:]]*\\n(.*?)\\n```"
  matches <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1L]]
  if (length(matches) != expected_blocks) {
    fail(label, ": esperaba ", expected_blocks, " bloque Mermaid; encontró ", length(matches), ".")
    return(invisible())
  }
  for (block in matches) {
    source <- sub(pattern, "\\1", block, perl = TRUE)
    lines <- trimws(strsplit(source, "\n", fixed = TRUE)[[1L]])
    lines <- lines[nzchar(lines)]
    if (!grepl("^flowchart (TD|LR)$", lines[[1L]])) fail(label, ": encabezado Mermaid inválido.")
    body <- lines[-1L]
    if (length(body) && any(!grepl("(-->|-\\..*[.]?->)", body, perl = TRUE))) {
      fail(label, ": hay líneas Mermaid fuera del subconjunto flowchart permitido.")
    }
    if (!balanced(source)) fail(label, ": delimitadores Mermaid desbalanceados.")
    if (!grepl("[{].*[}]", source)) fail(label, ": falta una decisión explícita.")
    if (!grepl("\\[\\(", source)) fail(label, ": falta un artefacto de evidencia.")
  }
}

for (file in expected[grepl("^P[0-9]{2}-", expected)]) {
  path <- file.path(process_dir, file)
  if (!file.exists(path)) next
  text <- read_utf8(path)
  id <- sub("^(P[0-9]{2}).*$", "\\1", file)
  if (!grepl(paste0("\\*\\*ID estable:\\*\\* `", id, "`"), text)) fail(file, ": ID interno incorrecto.")
  positions <- vapply(required_sections, function(x) regexpr(x, text, fixed = TRUE)[1L], integer(1L))
  if (any(positions < 0L)) fail(file, ": faltan secciones: ", paste(required_sections[positions < 0L], collapse = ", "))
  if (all(positions > 0L) && is.unsorted(positions, strictly = TRUE)) fail(file, ": orden de secciones inválido.")
  validate_mermaid(file, text)
}

master_path <- file.path(process_dir, "README.md")
if (file.exists(master_path)) {
  master <- read_utf8(master_path)
  for (id in sprintf("P%02d", 1:9)) if (!grepl(id, master, fixed = TRUE)) fail("README.md: falta ", id, ".")
  validate_mermaid("README.md", master)
}

for (file in expected) {
  path <- file.path(process_dir, file)
  if (!file.exists(path)) next
  text <- read_utf8(path)
  pattern <- "\\[[^]]+\\]\\(([^)#][^)]*)\\)"
  matches <- gregexpr(pattern, text, perl = TRUE)
  links <- regmatches(text, matches)[[1L]]
  if (!length(links) || identical(links, "")) next
  targets <- sub(pattern, "\\1", links, perl = TRUE)
  targets <- sub("#.*$", "", targets)
  targets <- targets[!grepl("^(https?:|mailto:)", targets)]
  for (target in targets) {
    resolved <- file.path(dirname(path), target)
    if (!file.exists(resolved)) fail(file, ": enlace relativo roto: ", target)
  }
}

if (length(failures)) {
  cat("Documentación operativa: FALLÓ\n", paste0("- ", unique(failures), collapse = "\n"), "\n", sep = "")
  quit(status = 1L)
}
cat("Documentación operativa: OK; 9 fichas, índice, enlaces y Mermaid estructuralmente válidos.\n")
