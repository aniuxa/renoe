test_that("el puente oficial conserva cambios directos y ambigüedades", {
  datos <- data.frame(sinco = c(2433, 2423, 2429, 1319, NA))
  resultado <- sinco2019_to_sinco2011(datos, variable_sinco = "sinco")

  expect_equal(resultado$sinco2011[1:2], c(2423L, 2412L))
  expect_true(is.na(resultado$sinco2011[3]))
  expect_equal(resultado$sinco2011_n_destinos[3], 2L)
  expect_match(resultado$sinco2011_calidad[3], "sin resolver")
  expect_true(is.na(resultado$sinco2011[4]))
  expect_equal(resultado$sinco2011_n_destinos[4], 0L)
  expect_true(is.na(resultado$sinco2011[5]))
})

test_that("armonizar_sinco aplica el cambio desde 2021-III", {
  datos <- data.frame(
    anio = c(2021, 2021, 2021, 2022),
    trim = c(2, 3, 3, 1),
    p3coe = c(2423, 2433, 2423, 2429),
    pos_ocu = 1,
    tue2 = 1
  )
  resultado <- armonizar_sinco(datos)

  expect_equal(resultado$sinco4d[1], 2423L)
  expect_equal(resultado$sinco4d[2], 2423L)
  expect_equal(resultado$sinco4d[3], 2412L)
  expect_true(is.na(resultado$sinco4d[4]))
  expect_equal(resultado$n_destinos_sinco[4], 2L)
  expect_equal(
    as.character(resultado$version_sinco_origen),
    c("SINCO 2011", "SINCO 2019", "SINCO 2019", "SINCO 2019")
  )
})
