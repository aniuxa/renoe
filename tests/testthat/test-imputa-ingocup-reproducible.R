datos_imputacion <- function() {
  n <- 12L
  data.frame(
    folio3 = sprintf("p%02d", seq_len(n)), trim = "t1", anio = 2021L,
    ingocup = c(1000, 1200, 1400, NA, 1800, 2000, NA, 2400, 2600, NA, 3000, 3200),
    p6b1 = c(1, 1, 1, 7, 1, 1, 7, 1, 1, 7, 1, 1),
    ing7c = 1, pos_ocu = 1, clase2 = 1,
    edad = seq(20, 42, 2), anios_es = rep(c(9, 12, 16), 4),
    sex = rep(c(1, 2), 6), ent = 9,
    hrsocup = seq(35, 46), t_loc = rep(1:4, 3)
  )
}

test_that("imputacion es invariante al orden de entrada", {
  x <- datos_imputacion()
  y1 <- imputa_ingocup(x, seed = 31415)
  y2 <- imputa_ingocup(x[c(12:1), ], seed = 31415)
  y2 <- y2[match(y1$folio3, y2$folio3), ]
  expect_equal(as.numeric(y1$ingocup_imp), as.numeric(y2$ingocup_imp))
  expect_equal(as.numeric(y1$imp_ingocup), as.numeric(y2$imp_ingocup))
})

test_that("imputacion valida la semilla", {
  expect_error(imputa_ingocup(datos_imputacion(), seed = NA), "seed")
})
