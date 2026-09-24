#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  hit <- args[startsWith(args, prefix)]
  if (!length(hit)) return(default)
  sub(prefix, "", hit[[length(hit)]], fixed = TRUE)
}

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
failures <- character()
fail <- function(...) failures <<- c(failures, paste0(...))
read_utf8 <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

version <- unname(read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")[[1L]])
version_pattern <- "[0-9]+[.][0-9]+[.][0-9]+"

assert_one_version <- function(label, text, prefix = "R package version") {
  normalized <- gsub("\n[[:space:]]*>[[:space:]]*", " ", text, perl = TRUE)
  pattern <- paste0(prefix, "[[:space:]]+", version_pattern)
  hits <- regmatches(normalized, gregexpr(pattern, normalized, perl = TRUE))[[1L]]
  found <- unique(sub(paste0("^", prefix, "[[:space:]]+"), "", hits, perl = TRUE))
  if (!length(found)) fail(label, ": no contiene una versión verificable.")
  if (length(found) && !identical(found, version)) {
    fail(label, ": esperaba ", version, "; encontró ", paste(found, collapse = ", "), ".")
  }
}

cff_path <- file.path(root, "CITATION.cff")
if (!file.exists(cff_path)) {
  fail("Falta CITATION.cff.")
} else {
  if (!requireNamespace("yaml", quietly = TRUE)) stop("Falta el paquete yaml.")
  cff <- yaml::yaml.load(read_utf8(cff_path))
  if (!identical(as.character(cff$version), version)) {
    fail("CITATION.cff: esperaba ", version, "; encontró ", as.character(cff$version), ".")
  }
}

citation_path <- file.path(root, "inst", "CITATION")
if (!file.exists(citation_path)) fail("Falta inst/CITATION.") else {
  assert_one_version("inst/CITATION", read_utf8(citation_path))
}
for (name in c("README.Rmd", "README.md")) {
  path <- file.path(root, name)
  if (!file.exists(path)) fail("Falta ", name, ".") else assert_one_version(name, read_utf8(path))
}

news <- readLines(file.path(root, "NEWS.md"), warn = FALSE, encoding = "UTF-8")
heading <- news[grepl("^# renoe ", news)][1L]
news_version <- sub(paste0("^# renoe (", version_pattern, ").*$"), "\\1", heading)
if (!identical(news_version, version)) fail("NEWS.md: el primer encabezado no corresponde a ", version, ".")

expected_tag <- paste0("v", version)
tag <- arg_value("tag", if (identical(Sys.getenv("GITHUB_REF_TYPE"), "tag")) Sys.getenv("GITHUB_REF_NAME") else "")
if (nzchar(tag) && !identical(tag, expected_tag)) fail("Tag: esperaba ", expected_tag, "; encontró ", tag, ".")

docs_arg <- arg_value("docs", "")
if (nzchar(docs_arg)) {
  docs <- normalizePath(docs_arg, winslash = "/", mustWork = TRUE)
  for (name in c("index.html", "authors.html")) {
    path <- file.path(docs, name)
    if (!file.exists(path)) fail("Falta ", path, ".") else assert_one_version(paste0("docs/", name), read_utf8(path))
  }
  for (name in c("news/index.html", "articles/index.html", "reference/index.html")) {
    path <- file.path(docs, name)
    if (!file.exists(path)) {
      fail("Falta docs/", name, ".")
    } else if (!grepl(version, read_utf8(path), fixed = TRUE)) {
      fail("docs/", name, " no menciona la versión ", version, ".")
    }
  }
}

public_url <- arg_value("public-url", "")
if (nzchar(public_url)) {
  base <- sub("/+$", "", public_url)
  fetch <- function(url) {
    tryCatch(paste(readLines(url, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
             error = function(e) { fail("No se pudo leer ", url, ": ", conditionMessage(e)); "" })
  }
  home <- fetch(paste0(base, "/index.html"))
  authors <- fetch(paste0(base, "/authors.html"))
  if (nzchar(home)) assert_one_version("URL pública /index.html", home)
  if (nzchar(authors)) assert_one_version("URL pública /authors.html", authors)
  for (name in c("news/index.html", "articles/index.html", "reference/index.html")) {
    page <- fetch(paste0(base, "/", name))
    if (nzchar(page) && !grepl(version, page, fixed = TRUE)) {
      fail("La URL pública /", name, " no menciona la versión ", version, ".")
    }
  }
}

if (length(failures)) {
  cat("Control de release: FALLÓ\n", paste0("- ", unique(failures), collapse = "\n"), "\n", sep = "")
  quit(status = 1L)
}
cat("Control de release: OK; versión coherente ", version, ".\n", sep = "")
