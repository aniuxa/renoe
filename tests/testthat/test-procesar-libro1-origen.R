test_that("origen usa el catalogo correspondiente al periodo", {
  x <- data.frame(
    anio = c(2012L, 2012L, 2012L, 2021L, 2021L, 2021L, 2021L),
    trim = c(2L, 2L, 3L, 3L, 3L, 3L, 3L),
    l_nac_c = c(201L, 221L, 201L, 221L, 800L, 998L, 999L),
    p2g2 = NA_real_, sexo = 2L, cs_p13_1 = 7L,
    clase2 = 1L, pos_ocu = 1L
  )

  y <- procesar_libro1(x)

  expect_equal(as.numeric(y$codigo_eeuu_usado),
               c(201, 201, 221, 221, 221, 221, 221))
  expect_equal(as.numeric(y$region_origen_long), c(1, 3, 3, 1, 0, 6, NA))
  expect_equal(as.numeric(y$extr), c(1, 1, 1, 1, 0, 1, NA))
  expect_equal(as.numeric(y$extr_especificado), c(1, 1, 1, 1, NA, 0, NA))
  expect_equal(
    as.character(y$origen_nivel_detalle),
    c(
      "Pais extranjero identificado", "Pais extranjero identificado",
      "Pais extranjero identificado", "Pais extranjero identificado",
      "Mexico sin entidad comparable", "Pais extranjero no especificado",
      NA
    )
  )
})

test_that("997 conserva Mexico aunque no identifique la entidad", {
  x <- data.frame(
    anio = 2024L, trim = 1L, l_nac_c = 997L, p2g2 = NA_real_,
    sexo = 1L, cs_p13_1 = 3L, clase2 = 1L, pos_ocu = 1L
  )
  y <- procesar_libro1(x)

  expect_equal(as.numeric(y$region_origen_long), 0)
  expect_equal(as.numeric(y$extr), 0)
  expect_true(is.na(y$extr_especificado))
  expect_equal(as.character(y$origen_nivel_detalle),
               "Mexico sin entidad comparable")
})

test_that("origen se recalcula sin conservar resultados anteriores", {
  x <- data.frame(
    anio = c(2022L, 2022L), trim = c(1L, 1L),
    l_nac_c = c(800L, 998L), p2g2 = NA_real_,
    sexo = 1L, cs_p13_1 = 3L, clase2 = 1L, pos_ocu = 1L,
    origen_codigo = c(-1, -1), codigo_eeuu_usado = c(-1, -1),
    region_origen_long = c(99, 99), extr = c(9, 9),
    extr_especificado = c(9, 9),
    origen_nivel_detalle = c("legacy", "legacy")
  )

  y <- procesar_libro1(x)

  expect_equal(as.numeric(y$origen_codigo), c(800, 998))
  expect_equal(as.numeric(y$codigo_eeuu_usado), c(221, 221))
  expect_equal(as.numeric(y$region_origen_long), c(0, 6))
  expect_equal(as.numeric(y$extr), c(0, 1))
  expect_equal(as.numeric(y$extr_especificado), c(NA, 0))
  expect_equal(
    as.character(y$origen_nivel_detalle),
    c("Mexico sin entidad comparable", "Pais extranjero no especificado")
  )
  expect_false(any(grepl("legacy", as.character(y$origen_nivel_detalle))))
})

test_that("no ocupacion por cuidados normaliza ceros iniciales", {
  x <- data.frame(
    anio = rep(2023L, 3), trim = rep(1L, 3), l_nac_c = rep(9L, 3),
    p2g2 = c("09", "04", NA), sexo = rep(2L, 3),
    cs_p13_1 = rep(3L, 3), clase2 = c(3L, 4L, 1L), pos_ocu = NA_real_
  )
  y <- procesar_libro1(x)

  expect_equal(as.numeric(y$no_ocupacion_cuidados), c(1, 0, NA))
})
