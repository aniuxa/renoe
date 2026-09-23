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

test_that("2020-T1 no usa ur como llave entre SDEM y COE", {
  base <- data.frame(
    cd_a = 52, ent = 1, ur = 1, con = 40002, v_sel = 2,
    n_hog = 1, h_mud = 0, n_ren = 1
  )
  datos <- list(
    viv = base[c("cd_a", "ent", "ur", "con", "v_sel")],
    hog = base[c("cd_a", "ent", "ur", "con", "v_sel", "n_hog", "h_mud")],
    sdem = base,
    coe1 = transform(base, ur = 2),
    coe2 = transform(base, ur = 2)
  )

  llaves <- .llaves_union_enoe(datos, 2020, 1)
  expect_true("ur" %in% llaves$idviv)
  expect_true("ur" %in% llaves$idhog)
  expect_false("ur" %in% llaves$idsdem)
  expect_equal(llaves$idsdem, c("cd_a", "ent", "con", "v_sel", "n_hog", "h_mud", "n_ren"))
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
