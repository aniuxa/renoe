test_that("crear_folios reproduce las llaves esperadas y conserva filas", {
  x <- data.frame(
    cd_a = c(1, 1), ent = c(9, 9), con = c(2, 2), v_sel = c(3, 3),
    ca = c(4, 4), tipo = c(1, 1), mes_cal = c(2, 2),
    n_hog = c(1, 1), h_mud = c(0, 0), n_ren = c(1, 2)
  )
  y <- crear_folios(x)
  expect_equal(nrow(y), nrow(x))
  expect_equal(as.character(y$folio), rep("1_9_2_3", 2))
  expect_equal(as.character(y$folio2), rep("1_9_2_3_4_1_2_1_0", 2))
  expect_equal(
    as.character(y$folio3),
    c("1_9_2_3_4_1_2_1_0_1", "1_9_2_3_4_1_2_1_0_2")
  )
})

test_that("crear_folios valida llaves base y acepta cero filas", {
  expect_error(
    crear_folios(data.frame(cd_a = 1, ent = 1, con = 1)),
    "v_sel"
  )
  x <- data.frame(cd_a = integer(), ent = integer(), con = integer(),
                  v_sel = integer(), n_ren = integer())
  expect_equal(nrow(crear_folios(x)), 0L)
})
