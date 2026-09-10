test_that("procesar_clases_damian construye las cuatro clases", {
  correspondencia <- data.frame(
    sinco4d = c(2211L, 4111L, 7111L, 9111L),
    isco88 = c(2111L, 4111L, 7111L, 9211L)
  )

  resultado <- procesar_clases_damian(
    data.frame(
      sinco4d = correspondencia$sinco4d,
      pos_ocu = 1L,
      emple7c = 5L,
      clase2 = 1L
    ),
    correspondencia = correspondencia,
    usar_puente_cmo = FALSE
  )

  expect_equal(as.integer(resultado$clase_ocu_damian), 1:4)
  expect_equal(as.integer(resultado$manual_damian), c(0L, 0L, 1L, 1L))
  expect_equal(as.integer(resultado$calificada_damian), c(1L, 0L, 1L, 0L))
  expect_true(all(resultado$cobertura_isco88_damian == 1L))
  expect_equal(
    attr(resultado$clase_ocu_damian, "label"),
    "Clase ocupacional manual/no manual y calificada/no calificada"
  )
  expect_equal(
    unname(attr(resultado$clase_ocu_damian, "labels")),
    c(1, 2, 3, 4)
  )
})

test_that("procesar_clases_damian recupera por gran grupo y conserva NA", {
  correspondencia <- data.frame(sinco4d = integer(), isco88 = integer())

  resultado <- procesar_clases_damian(
    data.frame(
      sinco4d = c(2992L, 6999L, NA_integer_),
      pos_ocu = 1L,
      emple7c = 5L,
      clase2 = c(1L, 1L, 4L)
    ),
    correspondencia = correspondencia,
    usar_puente_cmo = FALSE
  )

  expect_equal(
    as.integer(resultado$clase_ocu_damian),
    c(1L, 4L, NA_integer_)
  )
  expect_equal(
    as.integer(resultado$cobertura_isco88_damian),
    c(0L, 0L, NA_integer_)
  )
  expect_equal(
    as.vector(resultado$metodo_clase_damian),
    c(
      "Recuperacion por gran grupo SINCO",
      "Recuperacion por gran grupo SINCO",
      "Sin codigo SINCO"
    )
  )
})
