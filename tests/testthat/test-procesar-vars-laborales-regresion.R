fixture_laboral <- function(cs_p13_1, cs_p15, p3coe = 3000, clase2 = 1) {
  n <- length(cs_p13_1)
  data.frame(
    anio = rep(2020, n), trim = rep("t1", n), coe_tipo = rep("basico", n),
    p3coe = rep(p3coe, length.out = n), cs_p13_1 = cs_p13_1,
    cs_p15 = cs_p15, clase2 = rep(clase2, length.out = n),
    pos_ocu = rep(1, n), tue2 = rep(1, n), p2_4 = rep(3, n),
    p3i = rep(1, n), p3j1 = rep(1, n), anios_es = rep(10, n),
    stringsAsFactors = FALSE
  )
}

test_that("códigos educativos numéricos y con cero inicial son equivalentes", {
  p15 <- c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1)
  numericos <- fixture_laboral(0:9, p15)
  caracteres <- fixture_laboral(sprintf("%02d", 0:9), sprintf("%02d", p15))

  salida_numerica <- procesar_vars_laborales(numericos)
  salida_caracter <- procesar_vars_laborales(caracteres)

  expect_equal(salida_numerica$skill_actual, salida_caracter$skill_actual)
  expect_identical(salida_numerica$cs_p13_1, numericos$cs_p13_1)
  expect_identical(salida_caracter$cs_p13_1, caracteres$cs_p13_1)
  expect_identical(salida_caracter$cs_p15, caracteres$cs_p15)
})

test_that("códigos educativos inválidos quedan como NA en el proxy", {
  codigos <- c(as.character(0:9), "99", "", NA, "abc", "10", "1.5")
  resultado <- procesar_vars_laborales(
    fixture_laboral(codigos, rep("01", length(codigos)))
  )
  expect_false(any(is.na(resultado$skill_actual[1:10])))
  expect_true(all(is.na(resultado$skill_actual[11:length(codigos)])))
})

test_that("skill_level cubre correctamente las nueve divisiones SINCO", {
  resultado <- procesar_vars_laborales(
    fixture_laboral(rep(7, 9), rep(3, 9), p3coe = (1:9) * 1000)
  )
  expect_equal(as.numeric(resultado$skill_level), c(3, 3, rep(2, 6), 1))
})

test_that("Normal y técnica requieren antecedente válido", {
  p13 <- c(5, 5, 5, 5, 5, 6, 6, 6)
  p15 <- c(1, 2, 3, 9, NA, "01", "02", "03")
  resultado <- procesar_vars_laborales(fixture_laboral(p13, p15))
  expect_equal(as.numeric(resultado$skill_actual), c(2, 2, 3, NA, NA, 2, 2, 3))
})

test_that("mismatch conserva signo y exige población ocupada y componentes", {
  datos <- fixture_laboral(
    cs_p13_1 = c(7, 3, 2, 7, 99), cs_p15 = rep(3, 5),
    p3coe = c(9000, 3000, 3000, 3000, 3000),
    clase2 = c(1, 1, 1, 2, 1)
  )
  resultado <- procesar_vars_laborales(datos)
  expect_equal(as.numeric(resultado$mismatch), c(-1, 0, 1, NA, NA))
  etiquetas <- names(attr(resultado$mismatch, "labels"))
  expect_true(grepl("^Sobreeducaci", etiquetas[1]))
  expect_identical(etiquetas[2], "Ajuste")
  expect_true(grepl("^Subeducaci", etiquetas[3]))
})

test_that("columnas históricas de entrada se conservan sin recalcular", {
  datos <- fixture_laboral(7, 3)
  datos$esco_norm <- 12
  datos$mismatch2 <- -1
  resultado <- procesar_vars_laborales(datos)
  expect_identical(resultado$esco_norm, datos$esco_norm)
  expect_identical(resultado$mismatch2, datos$mismatch2)
})

test_that("procesar_vars_laborales admite cero filas", {
  datos <- fixture_laboral(integer(), integer())
  resultado <- procesar_vars_laborales(datos)
  expect_equal(nrow(resultado), 0)
  expect_true(all(c("skill_level", "skill_actual", "mismatch") %in% names(resultado)))
})
