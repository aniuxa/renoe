test_that("validador de fusion exige llave completa y unica", {
  x <- data.frame(a = 1:2, b = c(1, 2))
  expect_invisible(.validar_llave_union_enoe(x, c("a", "b"), "X"))
  expect_error(.validar_llave_union_enoe(x, c("a", "c"), "X"), "Faltan")

  faltante <- data.frame(a = c(1, NA), b = 1:2)
  expect_error(
    .validar_llave_union_enoe(faltante, c("a", "b"), "X"),
    "faltantes"
  )

  duplicada <- data.frame(a = c(1, 1), b = c(2, 2))
  expect_error(
    .validar_llave_union_enoe(duplicada, c("a", "b"), "X"),
    "no es unica"
  )
})

test_that("2022-T1 selecciona el HOG corregido con cobertura rural", {
  raiz <- tempfile("enoe_2022_1t_")
  carpeta <- file.path(raiz, "conjunto_de_datos_hog_enoen_2022_1t")
  corregido <- file.path(
    carpeta, "conjunto_de_datos", "conjunto_de_datos_hog_enoen_2022_1t.csv"
  )
  incompleto <- file.path(
    carpeta, "conjunto_de_datos_hog_enoen_2022_1t.csv"
  )
  dir.create(dirname(corregido), recursive = TRUE)
  writeLines("ur,mes_cal\n1,1", incompleto)
  writeLines("ur,mes_cal\n1,10\n2,11", corregido)

  x <- .leer_datos_enoe("hog", raiz, "enoen", 2022, 1)
  expect_equal(sort(x$ur), c(1, 2))
  expect_equal(sort(x$mes_cal), c(1, 2))
})
