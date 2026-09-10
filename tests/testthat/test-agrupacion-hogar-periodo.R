test_that("procesar_contribucion_hogar no mezcla folios entre trimestres", {
  datos <- data.frame(
    anio = rep(c(2023L, 2024L), each = 5),
    trim = 1L,
    folio2 = "hogar_repetido",
    ingocup_imp = c(10, 20, 30, 40, 50, 100, 200, 300, 400, 500),
    ipc = 2,
    hrsocup = 40,
    t_total_hrs0 = c(rep(1, 5), rep(2, 5)),
    t_total_hrs = c(rep(2, 5), rep(4, 5)),
    tam_hog = 5,
    fac = 1
  )

  resultado <- procesar_contribucion_hogar(datos)

  expect_equal(unique(resultado$ing_hog[resultado$anio == 2023]), 7500)
  expect_equal(unique(resultado$ing_hog[resultado$anio == 2024]), 75000)
  expect_equal(
    as.numeric(resultado$ing_mensual_ipc),
    datos$ingocup_imp / 2 * 100
  )
  expect_equal(unique(resultado$norem_hog0[resultado$anio == 2023]), 5)
  expect_equal(unique(resultado$norem_hog0[resultado$anio == 2024]), 10)
})

test_that("procesar_vars_hogar separa el mismo folio2 por periodo", {
  datos <- data.frame(
    anio = c(2024L, 2024L, 2024L),
    trim = c(1L, 1L, 2L),
    folio2 = "hogar_repetido",
    par_c = c(101L, 301L, 101L),
    edad = c(40L, 5L, 40L),
    sexo = c(1L, 2L, 1L),
    i_00_05 = c(0L, 1L, 0L),
    i_06_12 = 0L,
    i_13_17 = 0L,
    i_18m = c(1L, 0L, 1L),
    i_joven1 = 0L,
    i_joven2 = 0L,
    adm = 0L,
    clase2 = c(1L, 0L, 1L)
  )

  resultado <- procesar_vars_hogar(datos, anio = 2024, trimestre = 1)

  expect_equal(resultado$tam_hog[resultado$trim == 1], c(2, 2))
  expect_equal(resultado$tam_hog[resultado$trim == 2], 1)
  expect_equal(resultado$h_00_05[resultado$trim == 1], c(1, 1))
  expect_equal(resultado$h_00_05[resultado$trim == 2], 0)
})
