test_that("los tres escenarios son explícitos y monotónicos", {
  x <- data.frame(
    anio = c(2012L, 2012L, 2020L), trim = c(2L, 2L, 1L),
    p3coe = c(8200L, 1105L, 4111L), p4a = c(6111L, 6111L, 6111L),
    pos_ocu = 1L, tue2 = 1L
  )
  oficial <- suppressWarnings(armonizar_sinco(x, escenario = "official_strict"))
  aceptado <- armonizar_sinco(x, escenario = "integrated_accepted")
  legado <- armonizar_sinco(x, escenario = "analysis_legacy")
  expect_true(all(oficial$sinco_escenario == "official_strict"))
  expect_true(all(aceptado$sinco_escenario == "integrated_accepted"))
  expect_true(all(legado$sinco_escenario == "analysis_legacy"))
  expect_lte(sum(!is.na(oficial$sinco1d)), sum(!is.na(aceptado$sinco1d)))
  expect_lte(sum(!is.na(aceptado$sinco1d)), sum(!is.na(legado$sinco1d)))
})

test_that("el descenso oficial conserva los bucles por dígitos", {
  codigos <- data.frame(
    cmo_4d = c(1001L, 1002L), sinco4d = c(2111L, NA_integer_),
    sinco3d = c(211L, 522L)
  )
  x <- data.frame(
    anio = 2012L, trim = 2L, p3coe = c(1001L, 1002L),
    pos_ocu = 1L, tue2 = 1L
  )
  y <- armonizar_sinco(x, codigos = codigos, escenario = "official_strict")
  expect_equal(as.integer(y$sinco2011_granularidad), c(4L, 3L))
  expect_equal(as.integer(y$sinco3d), c(211L, 522L))
  expect_equal(as.integer(y$sinco2d), c(21L, 52L))
  expect_equal(as.integer(y$sinco1d), c(2L, 5L))
})

test_that("9999 permanece no comparable en todos los escenarios", {
  x <- data.frame(anio = 2022L, trim = 1L, p3coe = 9999L)
  puente <- data.frame(
    sinco2019 = c("9999", "9999"), sinco2011 = c("1111", "2222")
  )
  for (escenario in c("official_strict", "integrated_accepted", "analysis_legacy")) {
    y <- armonizar_sinco(x, correspondencia_2019 = puente,
                         escenario = escenario)
    expect_true(y$sinco2011_codigo_especial)
    expect_false(y$sinco2011_comparable)
  }
})

test_that("una equivalencia oficial es invariante por consumidor", {
  codigos <- data.frame(cmo_4d = 1001L, sinco4d = 2111L, sinco3d = 211L)
  x <- data.frame(anio = 2012L, trim = 2L, p3coe = 1001L,
                  pos_ocu = 1L, tue2 = 1L)
  valores <- vapply(
    c("official_strict", "integrated_accepted", "analysis_legacy"),
    function(e) armonizar_sinco(x, codigos = codigos,
                                escenario = e)$sinco4d_base2011[[1L]],
    integer(1L)
  )
  expect_equal(unname(valores), rep(2111L, 3L))
})

test_that("el rescate manual 1d solo completa faltantes", {
  x <- data.frame(
    anio = c(2012L, 2012L), trim = c(2L, 2L),
    p3coe = c(8124L, 8124L), pos_ocu = c(1L, 1L), tue2 = c(1L, 1L),
    sinco1d = c(9L, NA_integer_)
  )
  y <- cmo_to_sinco1d(x)
  expect_equal(as.integer(y$sinco1d), c(9L, 5L))
})

test_that("el alias publicado sólo depreca y delega", {
  x <- data.frame(anio = 2020L, trim = 1L, p3coe = 4111L)
  expect_warning(
    y <- armoniza_sinco(x, escenario = "official_strict"),
    "deprecated|desuso|remplazado|reemplazado"
  )
  z <- armonizar_sinco(x, escenario = "official_strict")
  expect_equal(y$sinco4d_base2011, z$sinco4d_base2011)
})

test_that("el wrapper declara el orden canónico", {
  texto <- paste(deparse(body(procesar_variables_enoe)), collapse = "\n")
  posiciones <- vapply(
    c("armonizar_scian\\(", "armonizar_sinco\\(",
      "armonizar_carreras\\(", "procesar_vars_laborales\\("),
    function(patron) regexpr(patron, texto)[[1L]], integer(1L)
  )
  expect_true(all(posiciones > 0L))
  expect_true(all(diff(posiciones) > 0L))
})

test_that("productos académicos reutiliza el escenario recibido", {
  x <- data.frame(
    sinco2011_comparable = TRUE,
    sinco_escenario = "official_strict",
    campo_arm8_horizontal = "1"
  )
  expect_error(
    procesar_productos_academicos(x, escenario = "integrated_accepted"),
    "no coincide"
  )
})
