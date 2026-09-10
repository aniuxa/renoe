#!/usr/bin/env Rscript

raiz <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
docs <- file.path(raiz, "docs")
if (!dir.exists(docs)) stop("No existe docs/.")
if (!requireNamespace("xml2", quietly = TRUE)) stop("Falta el paquete xml2.")

html <- list.files(docs, pattern = "[.]html$", recursive = TRUE, full.names = TRUE)
fallos <- character()
anclas <- list()

ids_de <- function(archivo) {
  clave <- normalizePath(archivo, winslash = "/", mustWork = FALSE)
  if (is.null(anclas[[clave]])) {
    doc <- xml2::read_html(archivo)
    anclas[[clave]] <<- unique(stats::na.omit(xml2::xml_attr(
      xml2::xml_find_all(doc, "//*[@id]"), "id"
    )))
  }
  anclas[[clave]]
}

for (archivo in html) {
  doc <- xml2::read_html(archivo)
  nodos <- xml2::xml_find_all(doc, "//*[@href or @src]")
  referencias <- c(xml2::xml_attr(nodos, "href"), xml2::xml_attr(nodos, "src"))
  referencias <- unique(stats::na.omit(referencias))
  referencias <- referencias[nzchar(referencias)]

  for (referencia in referencias) {
    if (grepl("^(https?:|mailto:|tel:|javascript:|data:|//)", referencia)) next
    partes <- strsplit(referencia, "#", fixed = TRUE)[[1L]]
    ruta_ref <- sub("[?].*$", "", partes[[1L]])
    ancla <- if (length(partes) > 1L) utils::URLdecode(partes[[2L]]) else ""

    destino <- if (!nzchar(ruta_ref)) {
      archivo
    } else {
      ruta_ref <- utils::URLdecode(ruta_ref)
      ruta_ref <- sub("^/renoe/", "", ruta_ref)
      ruta_ref <- sub("^/", "", ruta_ref)
      if (startsWith(referencia, "/")) file.path(docs, ruta_ref) else file.path(dirname(archivo), ruta_ref)
    }
    if (dir.exists(destino)) destino <- file.path(destino, "index.html")
    if (!file.exists(destino)) {
      fallos <- c(fallos, paste0(sub(paste0("^", docs, "/"), "", archivo), " -> ", referencia))
      next
    }
    if (nzchar(ancla) && grepl("[.]html$", destino, ignore.case = TRUE) &&
        !ancla %in% ids_de(destino)) {
      fallos <- c(fallos, paste0(sub(paste0("^", docs, "/"), "", archivo), " -> #", ancla))
    }
  }
}

texto <- unlist(lapply(html, readLines, warn = FALSE, encoding = "UTF-8"))
if (any(grepl("https://orcid.org/https://orcid.org/", texto, fixed = TRUE))) {
  fallos <- c(fallos, "ORCID con prefijo duplicado")
}
versionados <- system2("git", c("ls-files", "docs"), stdout = TRUE)
residuos <- versionados[grepl("(^|/)(desktop[.]ini|[.]DS_Store)$", versionados,
                              ignore.case = TRUE)]
if (length(residuos)) fallos <- c(fallos, paste("Residuo versionado:", residuos))

cat("HTML revisados:", length(html), "\n")
cat("Fallos locales:", length(fallos), "\n")
if (length(fallos)) {
  cat(paste0("- ", unique(fallos), collapse = "\n"), "\n")
  quit(status = 1L)
}
