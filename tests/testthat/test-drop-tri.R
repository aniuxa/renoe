test_that("drop_tri elimina solamente prefijo y sufijo", {
  x <- data.frame(
    cve_ent_tri = 1,
    archivo_triangular = 2,
    micve_variable = 3,
    check.names = FALSE
  )
  y <- drop_tri(x)
  expect_equal(
    names(y),
    c("ent", "archivo_triangular", "micve_variable")
  )
  expect_equal(nrow(y), nrow(x))
})

test_that("drop_tri rechaza colisiones en vez de perder una columna", {
  x <- data.frame(valor = 1, valor_tri = 2, check.names = FALSE)
  expect_error(drop_tri(x), "columnas duplicadas")
})
