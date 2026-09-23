fixture_productos_academicos <- function() {
  data.frame(
    anio = c(2012L, 2012L, 2021L, 2021L, 2022L, 2022L, 2026L, 2020L),
    trim = c(2L, 2L, 2L, 3L, 1L, 1L, 2L, 1L),
    p3coe = c(8124L, 1001L, 2211L, 5299L, 2531L, 9999L, 7341L, 9311L),
    p4a = c(6111L, 6111L, 6211L, 6211L, 6211L, 6211L, 4611L, 6211L),
    clase2 = 1L,
    pos_ocu = c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L),
    emple7c = c(2L, 2L, 2L, 2L, 5L, 2L, 7L, 2L),
    tue2 = c(2L, 2L, 2L, 2L, 2L, 2L, 5L, 2L),
    campo_arm8_horizontal = c("1", "1", "2", "2", "3", "3", "4", "4")
  )
}

columnas_golden_academico <- c(
  "anio", "trim", "p3coe", "sinco4d_base2011", "sinco3d", "sinco2d", "sinco1d",
  "sinco_escenario", "clasificaciones_escenario", "susceptible_teletrabajo",
  "trabajo_cuidado_mercado", "skill_level", "grupo_ocu9_damian",
  "clase_egp13_damian", "grupo_ocu9_damian_capa", "clase_egp13_damian_capa",
  "perfil_productos_reproducibles", "productos_academicos_escenario", "perfil_publicacion"
)

normalizar_golden <- function(x) {
  data.frame(lapply(x, function(z) {
    out <- as.character(z)
    out[is.na(z)] <- NA_character_
    out
  }), check.names = FALSE, stringsAsFactors = FALSE)
}

test_that("la ruta publica analysis_legacy es equivalente al consumidor canonico", {
  armonizada <- armonizar_sinco(fixture_productos_academicos(), escenario = "analysis_legacy")
  publica <- suppressWarnings(procesar_productos_academicos(armonizada, escenario = "analysis_legacy"))
  directa <- suppressWarnings(procesar_clasificaciones_reproducibles(armonizada, escenario = "analysis_legacy"))
  comunes <- intersect(names(directa), names(publica))
  expect_equal(publica[comunes], directa[comunes])
  expect_true(all(publica$productos_academicos_escenario == "analysis_legacy"))
  expect_true(all(publica$perfil_publicacion == "academic_reproducible"))
  expect_true(is.na(publica$skill_level[publica$p3coe == 9999L]))
})

test_that("analysis_legacy coincide con el golden academico versionado", {
  armonizada <- armonizar_sinco(fixture_productos_academicos(), escenario = "analysis_legacy")
  actual <- suppressWarnings(procesar_productos_academicos(armonizada, escenario = "analysis_legacy"))
  actual <- normalizar_golden(actual[columnas_golden_academico])
  golden <- utils::read.csv(
    testthat::test_path("golden", "productos_academicos_analysis_legacy.csv"),
    colClasses = "character", na.strings = "NA", check.names = FALSE
  )
  expect_identical(actual, golden)
})

test_that("los otros escenarios no reciben la marca publica academica", {
  x <- fixture_productos_academicos()
  for (escenario in c("official_strict", "integrated_accepted")) {
    armonizada <- armonizar_sinco(x, escenario = escenario)
    salida <- suppressWarnings(procesar_productos_academicos(armonizada, escenario = escenario))
    expect_true(all(is.na(salida$perfil_publicacion)))
    expect_true(all(salida$productos_academicos_escenario == escenario))
  }
})
