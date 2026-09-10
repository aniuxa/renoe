test_that("armoniza_sinco usa el corte correcto entre 2012-II y 2012-III", {
  datos <- data.frame(
    anio = c(2012, 2012),
    trim = c(2, 3),
    p3coe = c(7121, 2436),
    pos_ocu = c(1, 1),
    tue2 = c(1, 1)
  )

  resultado <- armoniza_sinco(datos)

  # 2012-II todavía se interpreta mediante la concordancia CMO-SINCO.
  # CMO 7121 no tiene destino 4d en la tabla y se resuelve con la regla 1d.
  expect_true(is.na(resultado$sinco4d[1]))
  expect_equal(resultado$sinco1d[1], 8)

  # Desde 2012-III p3coe ya es SINCO y debe conservarse directamente.
  expect_equal(resultado$sinco4d[2], 2436)
  expect_equal(resultado$sinco3d[2], 243)
  expect_equal(resultado$sinco2d[2], 24)
  expect_equal(resultado$sinco1d[2], 2)
  expect_type(resultado$sinco1d, "integer")
})

test_that("cmo_to_sinco1d no aplica reglas CMO desde 2012-III", {
  datos <- data.frame(
    anio = c(2012, 2012),
    trim = c(2, 3),
    p3coe = c(7121, 7121),
    pos_ocu = c(1, 1),
    tue2 = c(1, 1)
  )

  resultado <- cmo_to_sinco1d(datos)

  expect_equal(resultado$sinco1d[1], 8)
  expect_true(is.na(resultado$sinco1d[2]))
  expect_type(resultado$sinco1d, "integer")
  expect_false(any(c(
    "aplicar_reglas", "cmo", "str_cmo", "cmo2d", "cmo3d"
  ) %in% names(resultado)))
})
