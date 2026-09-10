fixture_tiempo <- function(anio, tipo = c("ampliado", "basico"),
                           horas = 1:8, minutos = rep(0, 8)) {
  tipo <- match.arg(tipo)
  prefijo <- if (tipo == "ampliado") "p11" else "p9"
  datos <- data.frame(anio = anio, coe_tipo = tipo)
  for (i in 1:8) {
    datos[[paste0(prefijo, "_h", i)]] <- horas[[i]]
    datos[[paste0(prefijo, "_m", i)]] <- minutos[[i]]
  }
  datos
}

test_that("hasta 2010 se usan los seis ítems históricos", {
  datos <- fixture_tiempo(2010, "ampliado", horas = 1:8,
                          minutos = c(0, 30, rep(0, 6)))
  salida <- procesar_tiempo(datos, 2010, 1)

  expect_equal(as.numeric(salida$t_cuidado), 2.5)
  expect_equal(as.numeric(salida$t_construir), 3)
  expect_equal(as.numeric(salida$t_reparar), 4)
  expect_equal(as.numeric(salida$t_quehacer), 5)
  expect_equal(as.numeric(salida$t_comun), 6)
  expect_equal(as.numeric(salida$t_compras), 0)
  expect_equal(as.numeric(salida$t_traslado), 0)
  expect_equal(salida$t_compras_estado, "no_aplica_instrumento")
  expect_equal(salida$t_traslado_estado, "no_aplica_instrumento")
  expect_equal(as.numeric(salida$t_total), 1230)
  expect_equal(as.numeric(salida$t_total_hrs), 20.5)
})

test_that("desde 2011 se usan los ocho ítems en cuestionario ampliado", {
  datos <- fixture_tiempo(2011, "ampliado", horas = 1:8,
                          minutos = c(0, 30, 15, 30, 0, 0, 0, 0))
  salida <- procesar_tiempo(datos, 2011, 1)

  expect_equal(as.numeric(salida$t_cuidado), 2.5)
  expect_equal(as.numeric(salida$t_compras), 3.25)
  expect_equal(as.numeric(salida$t_traslado), 4.5)
  expect_equal(as.numeric(salida$t_construir), 5)
  expect_equal(as.numeric(salida$t_reparar), 6)
  expect_equal(as.numeric(salida$t_quehacer), 7)
  expect_equal(as.numeric(salida$t_comun), 8)
  expect_equal(as.numeric(salida$t_total), 2175)
  expect_equal(as.numeric(salida$t_total0), 1710)
  expect_equal(as.numeric(salida$t_total_hrs), 36.25)
  expect_equal(as.numeric(salida$t_total_hrs0), 28.5)
})

test_that("desde 2011 se usan los ocho ítems en cuestionario básico", {
  datos <- fixture_tiempo(2011, "basico", horas = 1:8)
  salida <- procesar_tiempo(datos, 2011, 2)

  expect_equal(
    unname(unlist(salida[c("t_compras", "t_traslado", "t_construir",
                           "t_reparar", "t_quehacer", "t_comun")])),
    c(3, 4, 5, 6, 7, 8)
  )
})

test_that("t_cuidado convierte una sola vez horas y minutos", {
  datos <- fixture_tiempo(2025, "basico", horas = c(0, 2, rep(0, 6)),
                          minutos = c(0, 30, rep(0, 6)))
  salida <- procesar_tiempo(datos, 2025, 4)

  expect_equal(as.numeric(salida$t_cuidado), 2.5)
  expect_equal(as.numeric(salida$t_total), 150)
  expect_equal(as.numeric(salida$t_total_hrs), 2.5)
})

test_that("98, 99, no selección y batería ausente conservan estados distintos", {
  datos <- data.frame(anio = rep(2025, 5), coe_tipo = "basico")
  for (i in 1:8) {
    datos[[paste0("p9_", i)]] <- NA_character_
    datos[[paste0("p9_h", i)]] <- NA_character_
    datos[[paste0("p9_m", i)]] <- NA_character_
  }
  datos$p9_2[1:3] <- "2"
  datos$p9_h2[1:3] <- c("01", "98", "99")
  datos$p9_m2[1:3] <- "00"
  datos$p9_1[4] <- "1"
  datos$p9_h1[4] <- "02"
  datos$p9_m1[4] <- "30"
  original <- datos

  salida <- procesar_tiempo(datos, 2025, 4)

  expect_identical(salida[names(original)], original)
  expect_equal(as.numeric(salida$t_cuidado), c(1, NA, NA, 0, NA))
  expect_equal(
    salida$t_cuidado_estado,
    c("duracion_observada", "realizada_duracion_desconocida",
      "realizacion_desconocida", "no_seleccionada", "bateria_no_medible")
  )
  expect_equal(as.numeric(salida$t_total), c(60, NA, NA, 0, NA))
  expect_equal(as.numeric(salida$t_total_parcial), c(60, 0, 0, 0, NA))
  expect_identical(as.logical(salida$t_total_incompleto), c(FALSE, TRUE, TRUE, FALSE, NA))
  expect_identical(as.logical(salida$t_total0_incompleto), c(FALSE, TRUE, TRUE, FALSE, NA))
  expect_equal(salida$t_cuidado_legacy, c(1, 0, 0, 0, 0))
  expect_equal(as.numeric(salida$t_total_legacy), c(60, 0, 0, 0, 0))
})

test_that("el modo histórico es explícito y admite códigos numéricos", {
  caracteres <- fixture_tiempo(2025, "basico", horas = sprintf("%02d", 0:7),
                               minutos = rep("00", 8))
  numericos <- fixture_tiempo(2025, "basico", horas = 0:7,
                             minutos = rep(0, 8))

  moderno_chr <- procesar_tiempo(caracteres, 2025, 4)
  moderno_num <- procesar_tiempo(numericos, 2025, 4)
  historico <- procesar_tiempo(
    caracteres, 2025, 4, tratamiento_faltantes = "historico_cero"
  )

  expect_equal(moderno_chr$t_total, moderno_num$t_total)
  expect_equal(as.numeric(historico$t_total), as.numeric(historico$t_total_legacy))
  expect_equal(as.numeric(historico$t_cuidado), as.numeric(historico$t_cuidado_legacy))
})

test_that("procesar_tiempo es idempotente y acepta cero filas", {
  datos <- fixture_tiempo(2025, "basico", horas = rep(0, 8))
  una <- procesar_tiempo(datos, 2025, 4)
  dos <- procesar_tiempo(una, 2025, 4)
  expect_identical(dos, una)

  vacio <- datos[FALSE, ]
  resultado <- procesar_tiempo(vacio, 2025, 4)
  expect_equal(nrow(resultado), 0)
  expect_true(all(c("tiempo_medible", "t_total_parcial",
                    "t_total_incompleto", "t_total_legacy") %in% names(resultado)))
})
