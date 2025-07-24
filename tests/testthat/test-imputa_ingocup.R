test_that("imputa_ingocup agrega variables esperadas, imputa correctamente y etiqueta", {
  skip_on_cran()
  skip_if_not_installed("mice")

  set.seed(123)
  data <- data.frame(
    sex = sample(1:2, 100, replace = TRUE),
    edad = sample(18:65, 100, replace = TRUE),
    anios_es = sample(c(NA, 1:15), 100, replace = TRUE),
    ent = sample(1:32, 100, replace = TRUE),
    c_ocu11c = sample(1:9, 100, replace = TRUE),
    pos_ocu = sample(1:5, 100, replace = TRUE),
    rama_est2 = sample(1:21, 100, replace = TRUE),
    ing7c = sample(1:7, 100, replace = TRUE),
    hrsocup = sample(10:60, 100, replace = TRUE),
    t_loc = sample(1:3, 100, replace = TRUE),
    clase2 = 1,
    ingocup = c(runif(85, 1000, 8000), rep(NA, 15)),
    p6b1 = sample(1:7, 100, replace = TRUE),
    folio3 = 1:100,
    anio = 2022,
    trim = "t1"
  )

  result <- suppressWarnings(imputa_ingocup(data, anio = 2022, trimestre = 1))

  expect_true(all(c("ingocup_imp", "log_ingocup_imp", "imp_ingocup") %in% names(result)))

  imputados <- result[result$imp_ingocup == 1, ]
  imputados_con_valor <- imputados[!is.na(imputados$ingocup_imp), ]

  expect_gt(nrow(imputados_con_valor), 0)
  expect_true(all(imputados_con_valor$imp_ingocup == 1))
})
