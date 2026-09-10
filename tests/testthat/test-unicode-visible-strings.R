tiene_no_ascii <- function(x) {
  vapply(x, function(y) any(utf8ToInt(enc2utf8(y)) > 127L), logical(1))
}

valor_literal <- function(x) {
  enc2utf8(eval(parse(text = x, encoding = "UTF-8"), envir = baseenv()))
}

recolectar_literales_fuente <- function(raiz) {
  archivos <- sort(list.files(file.path(raiz, "R"), pattern = "[.]R$", full.names = TRUE))
  salida <- list()
  k <- 0L
  for (archivo in archivos) {
    datos <- getParseData(parse(archivo, keep.source = TRUE, encoding = "UTF-8"))
    datos <- datos[datos$token == "STR_CONST", , drop = FALSE]
    datos$valor <- vapply(datos$text, valor_literal, character(1))
    datos <- datos[tiene_no_ascii(datos$valor), , drop = FALSE]
    if (!nrow(datos)) next
    datos$archivo <- paste0("R/", basename(archivo))
    datos$ordinal <- ave(
      seq_len(nrow(datos)),
      interaction(datos$archivo, datos$line1, drop = TRUE),
      FUN = seq_along
    )
    k <- k + 1L
    salida[[k]] <- datos[, c("archivo", "line1", "ordinal", "valor")]
  }
  resultado <- do.call(rbind, salida)
  rownames(resultado) <- NULL
  resultado
}

test_that("los literales Unicode conservan sus valores visibles", {
  candidatos <- c(
    normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = FALSE),
    normalizePath(".", mustWork = FALSE)
  )
  raices <- candidatos[
    file.exists(file.path(candidatos, "DESCRIPTION")) &
      dir.exists(file.path(candidatos, "R"))
  ]
  if (!length(raices)) skip("El código fuente R no está disponible en esta instalación")

  ruta_baseline <- system.file(
    "extdata", "unicode_string_baseline.csv", package = "renoe"
  )
  if (!nzchar(ruta_baseline)) {
    ruta_baseline <- file.path(
      raices[[1L]], "inst", "extdata", "unicode_string_baseline.csv"
    )
  }
  esperado <- utils::read.csv(
    ruta_baseline,
    fileEncoding = "UTF-8",
    stringsAsFactors = FALSE
  )
  observado <- recolectar_literales_fuente(raices[[1L]])
  expect_identical(esperado, observado)
  expect_false(any(grepl("\u00C3", esperado$valor, fixed = TRUE)))
  expect_true(any(grepl("Gerardo Dami\u00E1n Hern\u00E1ndez", esperado$valor, fixed = TRUE)))
})

test_that("los metadatos conservan nombres y roles confirmados", {
  metadatos <- utils::packageDescription("renoe")
  autores <- eval(parse(text = metadatos$`Authors@R`))
  nombres <- format(autores, include = c("given", "family"))

  expect_identical(nombres, c(
    "Ana Escoto",
    "Gerardo Dami\u00E1n Hern\u00E1ndez",
    "Gabriela Cervantes"
  ))
  expect_setequal(autores[[1L]]$role, c("aut", "cre"))
  expect_identical(autores[[2L]]$role, "ctb")
  expect_identical(autores[[3L]]$role, "ctb")
  expect_identical(unname(autores[[1L]]$comment[["ORCID"]]), "0000-0001-7259-0001")
  expect_identical(unname(autores[[2L]]$comment[["ORCID"]]), "0009-0002-7604-3886")
})
