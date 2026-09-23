datos_cuidado_extra <- function() {
  data.frame(
    anio = 2021L, trim = "t1", folio2 = c("a", "a", "b", "b"),
    fac = 10, ent = 9, edad = c(40, 15, 40, 15), sexo = c(2, 1, 2, 1),
    clase2 = c(1, 4, 1, 4), hrsocup = c(40, NA, 40, NA),
    t_cuidado_directo = c(0, 0, 0, 2),
    t_cuidado_amplio = c(0, 1, 0, 3),
    t_trabajo_hogar_armonizado = c(10, 5, 10, 2),
    tipo_hog_lab = "Nuclear", tipo_hog2_lab = "Nuclear",
    jefa_mujer = 1, jefe_hombre = 0, quintil_ing_hog_pc = 2,
    ing_hog_pc = 100, ing_hog_pc_sego = c(0, 100, 0, 100),
    h_00_05 = 0, h_06_12 = 0, h_13_17 = 1,
    d_00_05 = 0, d_06_12 = 0, d_13_17 = 1
  )
}

test_that("cuidado adolescente distingue estimando amplio y directo", {
  y <- procesar_cuidado_extra(datos_cuidado_extra())
  expect_true(unique(y$hay_adolescente_cuidado_amplio[y$folio2 == "a"]))
  expect_false(unique(y$hay_adolescente_cuidado_directo[y$folio2 == "a"]))
  expect_equal(unique(y$n_adolescentes_cuidado_directo[y$folio2 == "a"]), 0)
  expect_true(unique(y$hay_adolescente_cuidado_directo[y$folio2 == "b"]))
  expect_equal(unique(y$horas_cuidado_amplio_adolescentes[y$folio2 == "b"]), 3)
  expect_equal(unique(y$horas_cuidado_directo_adolescentes[y$folio2 == "b"]), 2)
})

test_that("cuidado adolescente no se fuerza a cero cuando no es identificable", {
  x <- datos_cuidado_extra()
  x$anio <- 2012L
  x$t_cuidado_directo <- NA_real_
  y <- procesar_cuidado_extra(x)
  expect_true(all(is.na(y$n_adolescentes_cuidado_directo)))
  expect_true(all(is.na(y$hay_adolescente_cuidado_directo)))
  expect_false(any(is.na(y$n_adolescentes_cuidado_amplio)))
})
