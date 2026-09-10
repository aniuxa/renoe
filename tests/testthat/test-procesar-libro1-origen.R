test_that("procesar_libro1 identifica Estados Unidos por anio y trimestre", {
  datos <- data.frame(
    anio = c(2005, 2005, 2024, 2024, 2024),
    trim = c(1, 1, 1, 1, 1),
    l_nac_c = c(201, 225, 201, 221, 225),
    p2g2 = NA_real_,
    sexo = 1,
    cs_p13_1 = 4,
    clase2 = 1,
    pos_ocu = 1,
    coe_tipo = "basico"
  )

  resultado <- procesar_libro1(datos)

  # En 2005-I no existe 221: 201 se interpreta como Estados Unidos.
  expect_equal(resultado$codigo_eeuu_usado[1:2], c(201, 201))
  expect_equal(resultado$region_origen_long[1], 1)

  # En 2024-I sí existe 221: 201 conserva su significado moderno (Anguila).
  expect_equal(resultado$codigo_eeuu_usado[3:5], c(221, 221, 221))
  expect_equal(resultado$region_origen_long[3], 3)
  expect_equal(resultado$region_origen_long[4], 1)

  # Guatemala se mantiene igual en ambos clasificadores.
  expect_equal(resultado$region_origen_long[c(2, 5)], c(2, 2))
})

test_that("mujer_universitaria excluye normal y carreras tecnicas", {
  datos <- data.frame(
    anio = rep(2025, 8),
    trim = rep("t4", 8),
    l_nac_c = rep(1, 8),
    p2g2 = NA_real_,
    sexo = c(2, 2, 2, 2, 2, 1, 1, 2),
    cs_p13_1 = c("05", "06", "07", "08", "09", "07", "06", "99"),
    clase2 = 1,
    pos_ocu = 1,
    coe_tipo = "basico"
  )

  resultado <- procesar_libro1(datos)

  expect_equal(
    as.integer(resultado$educacion_universitaria),
    c(0, 0, 1, 1, 1, 1, 0, NA)
  )
  expect_equal(
    as.integer(resultado$mujer_universitaria),
    c(0, 0, 1, 1, 1, 0, 0, NA)
  )
})
