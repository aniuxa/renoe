test_that("armonizar_carreras_enoe reconoce los tres regimenes", {
  datos <- data.frame(
    anio = c(2012, 2012, 2021),
    trim = c("t2", "t3", "t3"),
    cs_p13_1 = c(7, 7, 7),
    cs_p14_c = c("3111", "5335", "41400")
  )

  resultado <- armonizar_carreras_enoe(datos)

  expect_equal(
    as.character(resultado$clasificador_carrera),
    c("Carreras 2005", "CMPE 2011", "CMPE 2016")
  )
  expect_equal(
    as.character(resultado$cs_p14_c_canonica),
    c("3111", "5335", "041400")
  )
  expect_equal(as.character(resultado$campo_arm8), c("5", "3", "3"))
  expect_equal(as.character(resultado$campo_arm10), c("07", "04", "04"))
  expect_equal(nrow(resultado), nrow(datos))
})

test_that("armonizar_carreras_enoe conserva ambiguedad real", {
  datos <- data.frame(
    anio = rep(2012, 3),
    trim = rep("t2", 3),
    cs_p13_1 = rep(7, 3),
    cs_p14_c = c("1512", "2092", "9999")
  )

  resultado <- armonizar_carreras_enoe(datos)

  expect_true(all(is.na(resultado$campo_arm8)))
  expect_equal(
    as.character(resultado$calidad_armonizacion),
    c("Ambigua", "Ambigua", "No especificada")
  )
})
