#!/usr/bin/env Rscript

# Ejecutor local seguro para desarrollar y revisar renoe.
# Uso: Rscript tools/development/validar_paquete_local.R --mode=inspect

args <- commandArgs(trailingOnly = TRUE)
valor_arg <- function(nombre, default = NULL) {
  prefijo <- paste0("--", nombre, "=")
  hallado <- args[startsWith(args, prefijo)]
  if (!length(hallado)) return(default)
  sub(prefijo, "", hallado[[length(hallado)]], fixed = TRUE)
}
tiene_arg <- function(nombre) paste0("--", nombre) %in% args

modo <- valor_arg("mode", "inspect")
raiz <- normalizePath(valor_arg("root", getwd()), winslash = "/", mustWork = TRUE)
desc_path <- file.path(raiz, "DESCRIPTION")
if (!file.exists(desc_path)) stop("La raíz no contiene DESCRIPTION: ", raiz)
dcf <- read.dcf(desc_path)
if (!identical(unname(dcf[1, "Package"]), "renoe")) stop("La raíz no es el paquete renoe.")
if (!identical(unname(dcf[1, "Version"]), "0.3.2")) stop("Se exige la fuente renoe 0.3.2.")
if (grepl("(^|/)2025(/|$)", raiz)) stop("Se rechazó una ruta de 2025: use el árbol activo de 2026.")

salida_solicitada <- valor_arg("output", NULL)
salida <- if (is.null(salida_solicitada)) {
  file.path(tempdir(), paste0("renoe-local-", format(Sys.time(), "%Y%m%dT%H%M%S")))
} else normalizePath(salida_solicitada, winslash = "/", mustWork = FALSE)

esta_dentro <- function(ruta, base) {
  ruta <- tolower(normalizePath(ruta, winslash = "/", mustWork = FALSE))
  base <- sub("/+$", "", tolower(normalizePath(base, winslash = "/", mustWork = FALSE)))
  identical(ruta, base) || startsWith(ruta, paste0(base, "/"))
}
validar_salida <- function() {
  prohibidas <- raiz
  if (any(vapply(prohibidas, function(x) esta_dentro(salida, x), logical(1)))) {
    stop("La salida solicitada apunta a un destino productivo: ", salida)
  }
  dir.create(salida, recursive = TRUE, showWarnings = FALSE)
}
crear_snapshot <- function() {
  validar_salida()
  destino <- file.path(salida, "source")
  dir.create(destino, recursive = TRUE, showWarnings = FALSE)
  relativos <- list.files(raiz, recursive = TRUE, all.files = TRUE,
                          full.names = FALSE, include.dirs = TRUE, no.. = TRUE)
  relativos <- gsub("\\\\", "/", relativos)
  excluido <- grepl("^(\\.git|\\.Rproj\\.user)(/|$)", relativos)
  relativos <- relativos[!excluido]
  origenes <- file.path(raiz, relativos)
  son_dir <- dir.exists(origenes)
  for (rel in relativos[son_dir]) {
    dir.create(file.path(destino, rel), recursive = TRUE, showWarnings = FALSE)
  }
  archivos <- relativos[!son_dir]
  for (rel in archivos) {
    objetivo <- file.path(destino, rel)
    dir.create(dirname(objetivo), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(file.path(raiz, rel), objetivo, overwrite = FALSE, copy.date = TRUE)) {
      stop("No se pudo copiar al snapshot: ", rel)
    }
  }
  destino
}
necesita <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Falta el paquete de desarrollo: ", pkg)
}

exports_namespace <- function() {
  lineas <- readLines(file.path(raiz, "NAMESPACE"), warn = FALSE)
  exports <- grep("^export\\(", lineas, value = TRUE)
  exports <- sub("^export\\(", "", exports)
  exports <- sub("\\)$", "", exports)
  sort(unique(gsub("\"", "", exports, fixed = TRUE)))
}
mostrar_inspeccion <- function() {
  cat("Raíz activa: ", raiz, "\n", sep = "")
  cat("Paquete/versión: ", dcf[1, "Package"], " ", dcf[1, "Version"], "\n", sep = "")
  cat("URL: ", dcf[1, "URL"], "\n", sep = "")
  cat("BugReports: ", dcf[1, "BugReports"], "\n", sep = "")
  cat("Exports declarados: ", length(exports_namespace()), "\n", sep = "")
  cat("docs/pkgdown.yml: ", if (file.exists(file.path(raiz, "docs", "pkgdown.yml"))) "presente" else "ausente", "\n", sep = "")
  cat("Salida reservada (no creada): ", salida, "\n", sep = "")
}

if (modo == "inspect") {
  mostrar_inspeccion()
} else if (modo == "load") {
  necesita("pkgload")
  pkgload::load_all(raiz, reset = TRUE, export_all = FALSE, quiet = FALSE)
  cat("Namespace cargado desde fuente; exports: ", length(getNamespaceExports("renoe")), "\n")
} else if (modo == "document") {
  if (!tiene_arg("allow-source-write")) {
    stop("document modifica NAMESPACE/man; repita con --allow-source-write tras revisar git status.")
  }
  necesita("devtools")
  devtools::document(pkg = raiz)
  cat("Revise inmediatamente: git diff -- NAMESPACE man/\n")
} else if (modo == "tests") {
  necesita("testthat")
  testthat::test_local(raiz, reporter = "summary", stop_on_failure = TRUE)
} else if (modo == "test-file") {
  necesita("pkgload"); necesita("testthat")
  archivo <- valor_arg("file", NULL)
  if (is.null(archivo)) stop("Use --file=tests/testthat/test-....R")
  archivo <- normalizePath(file.path(raiz, archivo), winslash = "/", mustWork = TRUE)
  if (!esta_dentro(archivo, file.path(raiz, "tests", "testthat"))) stop("La prueba debe estar en tests/testthat.")
  pkgload::load_all(raiz, reset = TRUE, export_all = FALSE, quiet = TRUE)
  testthat::test_file(archivo, reporter = "summary", stop_on_failure = TRUE)
} else if (modo == "exports") {
  necesita("pkgload")
  pkgload::load_all(raiz, reset = TRUE, export_all = FALSE, quiet = TRUE)
  declarados <- exports_namespace()
  cargados <- sort(getNamespaceExports("renoe"))
  cat("Sólo NAMESPACE: ", paste(setdiff(declarados, cargados), collapse = ", "), "\n", sep = "")
  cat("Sólo namespace cargado: ", paste(setdiff(cargados, declarados), collapse = ", "), "\n", sep = "")
  if (length(setdiff(declarados, cargados)) || length(setdiff(cargados, declarados))) stop("Exports no coinciden.")
} else if (modo == "fixtures") {
  necesita("pkgload"); necesita("testthat")
  pkgload::load_all(raiz, reset = TRUE, export_all = FALSE, quiet = TRUE)
  fixtures <- c("test-calendario-2026.R", "test-fusion-2022t1.R",
                "test-ipc-enoe.R", "test-procesar-tiempo-regresion.R")
  for (f in fixtures) testthat::test_file(file.path(raiz, "tests", "testthat", f),
                                          reporter = "summary", stop_on_failure = TRUE)
} else if (modo == "install") {
  validar_salida()
  lib <- file.path(salida, "library")
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  rbin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  estado <- system2(rbin, c("CMD", "INSTALL", "--no-multiarch",
                            paste0("--library=", shQuote(lib)), shQuote(raiz)))
  if (!identical(estado, 0L)) stop("Falló la instalación aislada.")
  cat("Biblioteca aislada: ", lib, "\n", sep = "")
} else if (modo == "build") {
  snapshot <- crear_snapshot()
  build_dir <- file.path(salida, "build")
  dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(build_dir)
  rbin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  estado <- system2(rbin, c("CMD", "build", "--no-manual", shQuote(snapshot)))
  if (!identical(estado, 0L)) stop("Falló R CMD build.")
  cat("Build aislado en: ", build_dir, "\n", sep = "")
} else if (modo == "check") {
  snapshot <- crear_snapshot(); necesita("rcmdcheck")
  res <- rcmdcheck::rcmdcheck(path = snapshot, args = "--no-manual",
                              check_dir = file.path(salida, "check"), error_on = "never")
  print(res)
  if (length(res$errors) || length(res$warnings) || length(res$notes)) quit(status = 1L)
} else if (modo == "pkgdown") {
  snapshot <- crear_snapshot(); necesita("pkgdown")
  destino <- file.path(salida, "pkgdown")
  if (esta_dentro(destino, file.path(raiz, "docs"))) stop("Nunca construir en docs/ con este script.")
  pkgdown::build_site(pkg = snapshot, override = list(destination = destino), preview = FALSE,
                      new_process = TRUE, install = TRUE)
  cat("Preview local: ", file.path(destino, "index.html"), "\n", sep = "")
} else {
  stop("Modo desconocido. Use inspect, load, document, tests, test-file, exports, fixtures, install, build, check o pkgdown.")
}
