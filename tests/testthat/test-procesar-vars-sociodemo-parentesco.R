datos_parentesco_minimos <- function(par_c) {
  data.frame(
    sex = 1L, eda = 30L, cs_p17 = 2L, e_con = 6L,
    anios_esc = 9L, par_c = par_c, t_loc = 1L, ent = 9L
  )
}

test_that("parentesco usa el catalogo vigente en cada periodo", {
  antiguo <- procesar_vars_sociodemo(
    datos_parentesco_minimos(c(101L, 205L, 305L, 401L)),
    anio = 2012L, trimestre = 2L
  )
  nuevo <- procesar_vars_sociodemo(
    datos_parentesco_minimos(c(101L, 204L, 304L, 401L)),
    anio = 2012L, trimestre = 3L
  )

  expect_equal(as.numeric(antiguo$parentesco), c(1, 2, 3, 4))
  expect_equal(as.numeric(nuevo$parentesco), c(1, 2, 3, 4))
})

test_that("el codigo antiguo 305 no se conserva fuera de su catalogo", {
  antiguo <- procesar_vars_sociodemo(
    datos_parentesco_minimos(305L), anio = 2010L, trimestre = 1L
  )
  nuevo <- procesar_vars_sociodemo(
    datos_parentesco_minimos(305L), anio = 2013L, trimestre = 1L
  )

  expect_equal(as.numeric(antiguo$parentesco), 3)
  expect_equal(as.numeric(nuevo$parentesco), 4)
})

test_that("zona_econ reproduce las ocho regiones socioeconomicas", {
  x <- datos_parentesco_minimos(rep(101L, 32L))
  x$ent <- 1:32
  y <- procesar_vars_sociodemo(x, anio = 2026L, trimestre = 2L)

  expect_equal(
    as.numeric(y$zona_econ),
    c(5, 2, 2, 8, 1, 3, 7, 2, 6, 2, 5, 7, 4, 3, 6, 3,
      6, 3, 1, 7, 4, 5, 8, 5, 2, 2, 8, 1, 4, 4, 8, 5)
  )
  expect_setequal(as.numeric(y$zona_econ), 1:8)
})
