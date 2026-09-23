datos_hogar_minimos <- function() {
  data.frame(
    anio = 2021L, trim = "t3",
    folio2 = c("a", "a", "a", "b", "b", "c"),
    par_c = c(101L, 201L, 301L, 101L, 401L, 501L),
    sexo = c(1L, 2L, 2L, 2L, 1L, 1L),
    edad = c(40, 38, 5, 70, 90, 30),
    i_00_05 = c(0L, 0L, 1L, 0L, 0L, 0L),
    i_06_12 = 0L, i_13_17 = 0L, i_18m = c(1L, 1L, 0L, 1L, 1L, 1L),
    i_joven1 = 0L, i_joven2 = 0L,
    adm = c(0L, 0L, 0L, 1L, 1L, 0L),
    clase2 = c(1L, 1L, 4L, 4L, 4L, 1L)
  )
}

test_that("procesar_vars_hogar conserva filas y calcula una vez por hogar", {
  x <- datos_hogar_minimos()
  y <- procesar_vars_hogar(x, anio = 2021L, trimestre = 3L)

  expect_equal(nrow(y), nrow(x))
  expect_equal(as.numeric(y$family[y$folio2 == "a"]), rep(7, 3))
  expect_equal(as.numeric(y$familyt[y$folio2 == "a"]), rep(4, 3))
  expect_equal(as.numeric(y$tam_hog[y$folio2 == "a"]), rep(3, 3))
  expect_equal(as.numeric(y$men[y$folio2 == "a"]), rep(1, 3))
  expect_equal(as.numeric(y$p_lab[y$folio2 == "a"]), rep(2, 3))
  expect_equal(as.numeric(y$family[y$folio2 == "b"]), rep(4, 2))
  expect_equal(as.numeric(y$tam_hog[y$folio2 == "b"]), rep(2, 2))
  expect_equal(as.numeric(y$family[y$folio2 == "c"]), 99)
  expect_equal(
    unique(y$tipologia_hogar_regla_id[y$folio2 == "a"]),
    "HOGAR_TIPOLOGIA_RELA2_RELA6"
  )
  expect_true(is.na(y$tipologia_hogar_regla_id[y$folio2 == "c"]))
})

test_that("hogares con el mismo folio se separan por periodo", {
  x <- datos_hogar_minimos()[c(1, 2), ]
  x$folio2 <- "igual"
  x$anio <- c(2021L, 2022L)
  x$trim <- c("t3", "t3")

  y <- procesar_vars_hogar(x, anio = 2021L, trimestre = 3L)

  expect_equal(as.numeric(y$tam_hog), c(1, 1))
  expect_equal(as.numeric(y$family), c(1, 99))
})

test_that("la tabla de tipologia cubre una vez los 32 patrones", {
  tabla <- read.csv(system.file(
    "extdata/clasificacion_tipologia_hogar.csv",
    package = "renoe"
  ))
  llaves <- c(
    "tiene_conyuge", "tiene_hijos", "tiene_ascendientes",
    "tiene_otros_parientes", "tiene_no_parientes"
  )
  patrones_esperados <- c(
    "00000", "10000", "01000", "00100", "00010", "00001",
    "11000", "10100", "10010", "10001", "01100", "01010",
    "01001", "00110", "00101", "00011", "11100", "11010",
    "11001", "10110", "10101", "10011", "01110", "01101",
    "01011", "00111", "01111", "10111", "11011", "11101",
    "11110", "11111"
  )
  tabla <- tabla[order(tabla$family), ]
  expect_equal(nrow(tabla), 32L)
  expect_false(anyDuplicated(tabla[llaves]) > 0L)
  expect_equal(do.call(paste0, tabla[llaves]), patrones_esperados)
  expect_equal(tabla$family, 1:32)
})
