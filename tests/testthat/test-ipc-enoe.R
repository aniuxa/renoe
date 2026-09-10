con_ipc_temporal <- function(tabla, codigo) {
  testthat::with_mocked_bindings(
    .leer_ipc_enoe = function() tabla,
    .package = "renoe",
    code = codigo
  )
}

test_that("ipc_enoe conserva filas, orden y admite cero filas", {
  tabla <- data.frame(anio = 2026, trim = 2, ipc = 145.496333333333)
  datos <- data.frame(id = c(3L, 1L, 2L))
  salida <- con_ipc_temporal(tabla, ipc_enoe(datos, 2026, 2))
  vacia <- con_ipc_temporal(tabla, ipc_enoe(datos[FALSE, , drop = FALSE], 2026, 2))

  expect_identical(salida$id, datos$id)
  expect_equal(salida$ipc, rep(tabla$ipc, nrow(datos)))
  expect_equal(nrow(vacia), 0L)
  expect_type(vacia$ipc, "double")
})

test_that("ipc_enoe exige año y trimestre escalares e íntegros", {
  datos <- data.frame(id = 1L)
  invalidos <- list(
    list(c(2025, 2026), 1), list(NA_real_, 1), list(Inf, 1),
    list(2026.5, 1), list("2026", 1), list(2026, c(1, 2)),
    list(2026, 0), list(2026, 5), list(2026, 1.5), list(2026, "2")
  )
  for (caso in invalidos) {
    expect_error(ipc_enoe(datos, caso[[1]], caso[[2]]), "debe ser")
  }
})

test_that("ipc_enoe valida el esquema y dominio del recurso", {
  datos <- data.frame(id = 1L)
  casos <- list(
    1:3,
    data.frame(anio = 2026, trim = 2),
    data.frame(anio = "2026", trim = 2, ipc = 100),
    data.frame(anio = 2026, trim = 0, ipc = 100),
    data.frame(anio = 2026, trim = 2, ipc = NA_real_),
    data.frame(anio = 2026, trim = 2, ipc = 0)
  )
  for (tabla in casos) {
    expect_error(con_ipc_temporal(tabla, ipc_enoe(datos, 2026, 2)),
                 "ipc.rds")
  }
})

test_that("ipc_enoe informa claves duplicadas y ausentes", {
  datos <- data.frame(id = 1L)
  duplicada <- data.frame(anio = c(2026, 2026), trim = c(2, 2), ipc = c(1, 1))
  completa <- data.frame(anio = 2026, trim = 2, ipc = 1)

  expect_error(
    con_ipc_temporal(duplicada, ipc_enoe(datos, 2026, 2)),
    "claves duplicadas"
  )
  expect_error(
    con_ipc_temporal(completa, ipc_enoe(datos, 2026, 3)),
    "IPC para 2026-T3"
  )
})

test_that("el recurso activo cubre 2005-T1 a 2026-T2", {
  ruta <- system.file("extdata", "ipc.rds", package = "renoe")
  ipc <- readRDS(ruta)
  expect_identical(names(ipc), c("anio", "trim", "ipc"))
  expect_equal(nrow(ipc), 86L)
  expect_identical(ipc[1, c("anio", "trim")], data.frame(anio = 2005, trim = 1))
  expect_equal(unname(unlist(ipc[86, c("anio", "trim")])), c(2026, 2))
  expect_identical(anyDuplicated(ipc[c("anio", "trim")]), 0L)
  expect_true(all(is.finite(ipc$ipc) & ipc$ipc > 0))
})
