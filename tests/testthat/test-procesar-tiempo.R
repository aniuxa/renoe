crear_bateria_tiempo <- function(anio, tipo) {
  x <- data.frame(anio = anio, coe_tipo = tipo)
  prefijo <- if (tipo == "ampliado") "p11" else "p9"
  for (i in 1:8) {
    x[[paste0(prefijo, "_", i)]] <- 1
    x[[paste0(prefijo, "_h", i)]] <- i
    x[[paste0(prefijo, "_m", i)]] <- 0
  }
  x
}

test_that("el corte documental de la bateria es 2013 en ambos cuestionarios", {
  x <- dplyr::bind_rows(
    crear_bateria_tiempo(2012L, "ampliado"),
    crear_bateria_tiempo(2012L, "basico"),
    crear_bateria_tiempo(2013L, "ampliado"),
    crear_bateria_tiempo(2013L, "basico")
  )

  y <- procesar_tiempo(x, anio = 2013, trimestre = 1)

  expect_equal(
    as.character(y$tiempo_version_instrumento),
    c(
      "6_actividades_cuidado_incluye_traslado",
      "6_actividades_cuidado_incluye_traslado",
      "8_actividades_traslado_separado",
      "8_actividades_traslado_separado"
    )
  )
  expect_equal(as.numeric(y$t_construir), c(3, 3, 5, 5))
  expect_equal(as.numeric(y$t_reparar), c(4, 4, 6, 6))
  expect_equal(as.numeric(y$t_quehacer), c(5, 5, 7, 7))
  expect_equal(as.numeric(y$t_comun), c(6, 6, 8, 8))
  expect_true(all(is.na(y$t_compras[1:2])))
  expect_true(all(is.na(y$t_traslado[1:2])))
  expect_equal(as.numeric(y$t_compras[3:4]), c(3, 3))
  expect_equal(as.numeric(y$t_traslado[3:4]), c(4, 4))
})

test_that("los conceptos comparables respetan el cambio de contenido", {
  x <- dplyr::bind_rows(
    crear_bateria_tiempo(2012L, "basico"),
    crear_bateria_tiempo(2013L, "basico")
  )

  y <- procesar_tiempo(x, anio = 2013, trimestre = 2)

  expect_equal(as.numeric(y$t_cuidado), c(2, 2))
  expect_equal(as.numeric(y$t_cuidado_directo), c(NA, 2))
  expect_equal(as.numeric(y$t_cuidado_amplio), c(2, 6))
  expect_equal(as.numeric(y$t_trabajo_hogar_indirecto_armonizado), c(12, 18))
  expect_equal(as.numeric(y$t_trabajo_hogar_armonizado), c(14, 24))
  expect_equal(as.numeric(y$t_total_instrumento), c(20, 35))
  expect_equal(
    as.character(y$t_cuidado_definicion),
    c("cuidado_incluye_traslado", "cuidado_directo_sin_traslado")
  )
})

test_that("duraciones desconocidas no se convierten en cero", {
  x <- crear_bateria_tiempo(2013L, "basico")
  x$p9_h4 <- 98
  x$p9_m4 <- 0

  y <- procesar_tiempo(x, anio = 2013, trimestre = 2)

  expect_true(is.na(y$t_traslado))
  expect_true(is.na(y$t_cuidado_amplio))
  expect_true(y$t_cuidado_amplio_incompleto)
  expect_true(is.na(y$t_trabajo_hogar_armonizado))
  expect_true(y$t_trabajo_hogar_armonizado_incompleto)
})
