datos_contribucion <- function() {
  data.frame(
    anio = 2021L, trim = "t1",
    folio2 = c("a", "b", "b", "b"),
    ingocup_imp = c(100, 300, NA, NA),
    ipc = 100, hrsocup = c(40, 40, NA, NA),
    t_trabajo_hogar_armonizado = c(10, 20, 10, 0),
    t_total_instrumento = c(12, 22, 12, 2),
    tam_hog = c(1, 3, 3, 3), fac = c(10, 20, 20, 20)
  )
}

test_that("agregados y quintiles de hogar son constantes dentro del hogar", {
  y <- procesar_contribucion_hogar(datos_contribucion())
  expect_equal(unique(y$ing_hog[y$folio2 == "b"]), 300)
  expect_equal(unique(y$norem_hog_armonizado[y$folio2 == "b"]), 30)
  expect_length(unique(y$quintil_ing_hog_pc[y$folio2 == "b"]), 1L)
  expect_length(unique(y$quintil_norem_pc_armonizado[y$folio2 == "b"]), 1L)
})

test_that("un hogar sin valores observados conserva NA", {
  x <- datos_contribucion()
  x$ingocup_imp[x$folio2 == "b"] <- NA_real_
  x$t_trabajo_hogar_armonizado[x$folio2 == "b"] <- NA_real_
  x$t_total_instrumento[x$folio2 == "b"] <- NA_real_
  y <- procesar_contribucion_hogar(x)
  expect_true(all(is.na(y$ing_hog[y$folio2 == "b"])))
  expect_true(all(is.na(y$norem_hog_armonizado[y$folio2 == "b"])))
  expect_true(all(is.na(y$norem_hog_instrumento[y$folio2 == "b"])))
})

test_that("el factor debe ser constante dentro del hogar", {
  x <- datos_contribucion()
  x$fac[3] <- 99
  expect_error(procesar_contribucion_hogar(x), "constante")
})
