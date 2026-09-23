test_that("calcular_desajuste_estadistico conserva filas, universo y signo", {
  datos <- data.frame(
    id = 1:7, anio = rep(2025, 7), trim = rep(4, 7),
    clase2 = c(1, 1, 1, 1, 2, 1, 1),
    sinco1d = c(3, 3, 3, 3, 3, NA, 3),
    anios_es = c(8, 10, 12, 14, 20, 10, NA), fac = rep(1, 7)
  )

  resultado <- calcular_desajuste_estadistico(datos, "trimestre")

  expect_identical(resultado$id, datos$id)
  expect_equal(nrow(resultado), nrow(datos))
  expect_identical(resultado[names(datos)], datos)
  expect_equal(resultado$esco_ref[1:4], rep(11, 4))
  expect_equal(resultado$mismatch2[1:4], c(1, 0, 0, -1))
  expect_true(all(is.na(resultado$esco_ref[5:7])))
  expect_true(all(is.na(resultado$mismatch2[5:7])))
  expect_false(any(grepl("^\\.", names(resultado))))
  etiquetas <- names(attr(resultado$mismatch2, "labels"))
  expect_true(grepl("^Sobreeducaci", etiquetas[1]))
  expect_identical(etiquetas[2], "Ajuste")
  expect_true(grepl("^Subeducaci", etiquetas[3]))
})

test_that("la referencia admite ponderación y ponderador anual ajustado", {
  datos <- data.frame(
    anio = rep(2025, 4), trim = c(1, 1, 2, 2), clase2 = rep(1, 4),
    sinco1d = rep(4, 4), anios_es = c(6, 10, 14, 18),
    fac = c(1, 1, 1, 7), fac_anual = rep(1, 4)
  )

  ponderado <- calcular_desajuste_estadistico(datos, "anio", anio_incompleto = "permitir")
  no_ponderado <- calcular_desajuste_estadistico(datos, "anio", ponderado = FALSE, anio_incompleto = "permitir")
  ajustado <- calcular_desajuste_estadistico(
    datos, "anio", variable_ponderador = "fac_anual", anio_incompleto = "permitir"
  )

  expect_equal(unique(ponderado$esco_ref), 15.6)
  expect_equal(unique(no_ponderado$esco_ref), 12)
  expect_equal(unique(ajustado$esco_ref), 12)
})

test_that("la referencia anual exige trimestres previamente unidos", {
  uno <- data.frame(
    anio = c(2025, 2025), trim = c(1, 1), clase2 = c(1, 1),
    sinco1d = c(3, 3), anios_es = c(9, 11), fac = c(1, 1)
  )
  dos <- rbind(uno, transform(uno, trim = 2, anios_es = c(11, 13)))

  expect_error(calcular_desajuste_estadistico(uno, "anio"), "cuatro trimestres")
  expect_warning(
    parcial <- calcular_desajuste_estadistico(
      dos, "anio", anio_incompleto = "advertir"
    ),
    "incompleto"
  )
  expect_equal(unique(parcial$esco_ref), 11)
  expect_true(all(parcial$trimestres_referencia_mismatch2 == 2L))
})

test_that("la referencia trimestral separa periodos", {
  datos <- data.frame(
    anio = rep(2025, 4), trim = c(1, 1, 2, 2), clase2 = rep(1, 4),
    sinco1d = rep(3, 4), anios_es = c(8, 10, 12, 14), fac = rep(1, 4)
  )
  resultado <- calcular_desajuste_estadistico(datos, "trimestre")
  expect_equal(as.numeric(resultado$esco_ref), c(9, 9, 13, 13))
})

test_that("mismatch2 se recalcula sin crear alias y la función es idempotente", {
  datos <- data.frame(
    anio = c(2025, 2025), trim = c(1, 1), clase2 = c(1, 1),
    sinco1d = c(3, 3), anios_es = c(8, 12), fac = c(1, 1),
    mismatch2 = c(-1, 1), esco_norm = c(10, 10)
  )

  una <- calcular_desajuste_estadistico(datos, "trimestre")
  dos <- calcular_desajuste_estadistico(una, "trimestre")

  expect_false("mismatch2_legacy" %in% names(una))
  expect_false("mismatch2_legacy" %in% names(dos))
  expect_identical(una$esco_norm, datos$esco_norm)
  expect_equal(una$esco_ref, dos$esco_ref)
  expect_equal(una$mismatch2, dos$mismatch2)
})

test_that("se validan ponderador, umbral y columnas", {
  datos <- data.frame(
    anio = 2025, trim = 1, clase2 = 1, sinco1d = 3, anios_es = 10
  )
  expect_error(calcular_desajuste_estadistico(datos, "trimestre"), "ponderador.*fac")
  expect_error(
    calcular_desajuste_estadistico(datos, "trimestre", ponderado = FALSE, umbral_anios = -1),
    "umbral_anios"
  )
  expect_error(
    calcular_desajuste_estadistico(datos[, names(datos) != "sinco1d"], "trimestre", ponderado = FALSE),
    "sinco1d"
  )
})

test_that("cero filas conserva el esquema", {
  datos <- data.frame(
    anio = integer(), trim = integer(), clase2 = integer(),
    sinco1d = integer(), anios_es = numeric(), fac = numeric()
  )
  resultado <- calcular_desajuste_estadistico(datos, "trimestre")
  expect_equal(nrow(resultado), 0)
  expect_true(all(c("esco_ref", "mismatch2", "periodo_referencia_mismatch2",
                    "ponderador_mismatch2", "trimestres_referencia_mismatch2",
                    "unidad_referencia_mismatch2") %in% names(resultado)))
})

test_that("la referencia predeterminada es trimestral y registra metadatos", {
  datos <- data.frame(
    anio = rep(2025, 4), trim = c(1, 1, 2, 2), clase2 = 1,
    sinco1d = 3, anios_es = c(8, 10, 12, 14), fac = 1
  )
  resultado <- calcular_desajuste_estadistico(datos)

  expect_equal(as.numeric(resultado$esco_ref), c(9, 9, 13, 13))
  expect_true(all(resultado$periodo_referencia_mismatch2 == "trimestre"))
  expect_true(all(resultado$ponderador_mismatch2 == "fac"))
  expect_true(all(resultado$trimestres_referencia_mismatch2 == 1L))
  expect_true(all(resultado$unidad_referencia_mismatch2 == "persona-trimestre"))
})

test_that("la referencia anual completa conserva personas-trimestre repetidas", {
  datos <- data.frame(
    persona = rep("misma", 4), anio = 2025, trim = 1:4, clase2 = 1,
    sinco1d = 3, anios_es = c(8, 10, 12, 14), fac_anual = c(1, 1, 1, 5)
  )
  resultado <- calcular_desajuste_estadistico(
    datos, "anio", variable_ponderador = "fac_anual", anio_incompleto = "permitir"
  )

  expect_equal(nrow(resultado), 4)
  expect_identical(resultado$persona, datos$persona)
  expect_equal(unique(resultado$esco_ref), 12.5)
  expect_true(all(resultado$trimestres_referencia_mismatch2 == 4L))
  expect_true(all(resultado$ponderador_mismatch2 == "fac_anual"))
})
