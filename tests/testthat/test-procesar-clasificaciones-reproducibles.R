test_that("la cascada reproducible SINCO 2019 resuelve cada salida por separado", {
  x <- data.frame(
    anio = rep(2021L, 4), trim = rep(3L, 4),
    p3coe = c(2531L, 2429L, 7341L, 5299L),
    p4a = rep(6211L, 4), clase2 = rep(1L, 4),
    pos_ocu = rep(1L, 4), emple7c = rep(2L, 4), tue2 = rep(2L, 4)
  )
  x <- armonizar_sinco(x)
  y <- procesar_clasificaciones_reproducibles(x)

  expect_equal(y$susceptible_teletrabajo, c(1L, 0L, 0L, 0L))
  expect_false(anyNA(y$trabajo_cuidado_mercado))
  expect_equal(as.integer(y$grupo_ocu9_damian), c(2L, 2L, 7L, 5L))
  expect_equal(y$grupo_ocu9_damian_capa[3], "consenso")
  expect_equal(y$grupo_ocu9_damian_capa[4], "autor")
  expect_true(all(y$estado_revision_clasificaciones ==
                    "GO_CON_ADVERTENCIAS_DOCUMENTADAS_2026_09_19"))
  expect_equal(
    y$perfil_clasificaciones_reproducibles,
    rep("cascada_reproducible_v1", 4)
  )
  expect_false("perfil_clasificaciones_academicas" %in% names(y))
  expect_false("cuida_total" %in% names(y))
  expect_false("trabajo_cuidado_rem" %in% names(y))
})

test_that("una corrida previa no contamina la reconstruccion", {
  x <- data.frame(
    anio = 2021L, trim = 3L, p3coe = 8154L, p4a = 6211L,
    clase2 = 1L, pos_ocu = 1L, emple7c = 2L, tue2 = 2L,
    grupo_ocu9_damian = 99L, clase_egp13_damian = 99L
  )
  x <- armonizar_sinco(x)
  y <- procesar_clasificaciones_reproducibles(x)

  expect_equal(as.integer(y$grupo_ocu9_damian), 8L)
  expect_false(any(grepl("[.]x$|[.]y$", names(y))))
})

test_that("las capas nunca sobrescriben un resultado oficial", {
  x <- data.frame(
    anio = 2021L, trim = 2L, p3coe = 2211L, p4a = 6211L,
    clase2 = 1L, pos_ocu = 1L, emple7c = 2L, tue2 = 2L
  )
  x <- armonizar_sinco(x)
  y <- procesar_clasificaciones_reproducibles(x)

  expect_equal(as.integer(y$sinco4d_base2011), 2211L)
  expect_equal(y$grupo_ocu9_damian_capa, "official")
})

test_that("la capa ENOE resuelve solo empleadores sin tamano con tue2 validado", {
  x <- data.frame(
    anio = rep(2025L, 4), trim = rep(2L, 4),
    p3coe = c(8342L, 8342L, 8342L, 7221L), p4a = rep(6211L, 4),
    clase2 = rep(1L, 4), pos_ocu = rep(2L, 4), emple7c = rep(7L, 4),
    tue2 = c(2L, 5L, 1L, 5L)
  )
  x <- armonizar_sinco(x)
  y <- procesar_clasificaciones_reproducibles(x)

  expect_equal(as.integer(y$clase_egp13_damian), c(5L, 5L, NA, 5L))
  expect_equal(y$clase_egp13_damian_capa, c("enoe", "enoe", NA, "enoe"))
  expect_equal(y$egp_tamano_inferido_enoe,
               c("pequeno", "pequeno", NA, "pequeno"))
  expect_equal(as.integer(y$tam_est_damian), rep(9L, 4))
})

test_that("skill_level no interpreta 9999 como ocupacion elemental", {
  x <- data.frame(
    anio = c(2020L, 2020L, 2021L, 2021L, 2021L),
    trim = c(1L, 1L, 3L, 3L, 3L),
    p3coe = c(9999L, 9311L, 5299L, 1329L, 9599L),
    p4a = rep(6211L, 5), clase2 = rep(1L, 5), pos_ocu = rep(1L, 5),
    emple7c = rep(2L, 5), tue2 = rep(2L, 5)
  )
  x <- armonizar_sinco(x)
  y <- procesar_clasificaciones_reproducibles(x)

  expect_true(is.na(y$skill_level[[1L]]))
  expect_equal(as.integer(y$sinco4d_base2011[[1L]]), 9999L)
  expect_equal(as.integer(y$codigo_ocupacion_original[[1L]]), 9999L)
  expect_equal(y$skill_level[[2L]], 1L)
  expect_true(is.na(y$skill_level_capa[[1L]]))
  expect_true(is.na(y$skill_level_regla_id[[1L]]))
  expect_equal(y$skill_level[3:5], c(2L, 3L, 1L))
  expect_equal(
    y$skill_level_capa[3:5], rep("oficial_catalogo_observado", 3)
  )
  expect_equal(
    y$skill_level_regla_id[3:5], rep("SKILL_SINCO2019_1D_OBSERVADO", 3)
  )
})

test_that("la armonizacion recorre 4d, 3d, 2d y 1d", {
  x <- data.frame(
    anio = c(2021L, 2021L), trim = c(3L, 3L),
    p3coe = c(8154L, 7341L)
  )
  y <- armonizar_sinco(x)

  expect_equal(as.integer(y$sinco3d), c(815L, NA_integer_))
  expect_equal(as.integer(y$sinco2d), c(81L, 73L))
  expect_equal(as.integer(y$sinco1d), c(8L, 7L))
  expect_equal(as.character(y$nivel_maximo_sinco), c("3d", "2d"))
})
