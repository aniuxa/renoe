test_that("quehaceres no se etiquetan como cuidado", {
  x <- data.frame(
    clase2 = c(1, 2, 3, 4, 4),
    cs_p17 = c(2, 2, 2, 2, 2),
    p2e = c(NA, NA, NA, 4, 6)
  )

  y <- procesar_estudio_trabajo(x)

  expect_equal(as.numeric(y$no_estudia_no_trabaja), c(0, 1, 1, 1, 1))
  expect_equal(as.numeric(y$neet_quehaceres), c(NA, 0, 0, 1, 0))
  expect_false("neet_cuidador" %in% names(y))
  expect_equal(as.numeric(y$tipo_neet), c(NA, 1, 3, 2, 4))
})
