test_that("procesar_estudio_trabajo crea la clasificación general", {
  datos <- data.frame(
    clase2 = c(4, 1, 1, 3, 0, 4),
    cs_p17 = c(1, 2, 1, 2, 2, 9),
    p2e = c(3, NA, NA, 6, NA, 4)
  )

  resultado <- procesar_estudio_trabajo(datos)

  expect_equal(resultado$situacion_estudio_trabajo[1:4], 1:4)
  expect_true(is.na(resultado$situacion_estudio_trabajo[5]))
  expect_true(is.na(resultado$situacion_estudio_trabajo[6]))
  expect_equal(resultado$no_estudia_no_trabaja[1:4], c(0, 0, 0, 1))
})

test_that("indicadores NEET pueden superponerse y tipo_neet fija prioridad", {
  datos <- data.frame(
    clase2 = c(2, 3, 3, 4, 1),
    cs_p17 = c(2, 2, 2, 2, 2),
    p2e = c(NA, 4, 6, 4, NA)
  )

  resultado <- procesar_estudio_trabajo(datos)

  expect_equal(as.numeric(resultado$neet_buscador), c(1, 0, 0, 0, NA))
  expect_equal(as.numeric(resultado$neet_cuidador), c(0, 1, 0, 1, NA))
  expect_equal(as.numeric(resultado$neet_disponible), c(0, 1, 1, 0, NA))
  expect_equal(as.numeric(resultado$tipo_neet), c(1, 2, 3, 2, NA))
  expect_equal(
    sjlabelled::get_label(resultado$neet_disponible),
    "Persona que no estudia ni trabaja y está disponible para trabajar"
  )
})

test_that("procesar_estudio_trabajo valida insumos", {
  expect_error(
    procesar_estudio_trabajo(data.frame(clase2 = 1, cs_p17 = 2)),
    "p2e"
  )
})
