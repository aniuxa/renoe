# Referencias generadas con dineq 0.1.0 y Hmisc 5.2.6 antes de sustituir
# la dependencia. Vease tools/generar_fixture_quintiles_dineq.R.
capturar_cuantiles <- function(f) {
  avisos <- character()
  valor <- tryCatch(
    withCallingHandlers(f(), warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart('muffleWarning')
    }),
    error = function(e) structure(conditionMessage(e), class = 'error_cuantiles')
  )
  list(valor = valor, avisos = avisos)
}

leer_referencia_quintiles <- function() {
  readRDS(testthat::test_path('fixtures', 'quintiles_dineq_0.1.0.rds'))
}

test_that('los quintiles conservan los resultados historicos en casos limite', {
  casos <- leer_referencia_quintiles()$casos
  for (nombre in names(casos)) {
    caso <- casos[[nombre]]
    observado <- capturar_cuantiles(function() .quintiles_ponderados(caso$x, 5, caso$w))
    if (inherits(caso$esperado$valor, 'error_cuantiles')) {
      expect_true(inherits(observado$valor, 'error_cuantiles'), info = nombre)
    } else {
      expect_identical(observado$valor, caso$esperado$valor, info = nombre)
    }
    expect_equal(length(observado$avisos), length(caso$esperado$avisos), info = nombre)
  }
})

test_that('los quintiles conservan las asignaciones y errores de referencia', {
  expect_identical(.quintiles_ponderados(1:10, 5, rep(1, 10)),
                   as.numeric(rep(1:5, each = 2)))
  expect_identical(.quintiles_ponderados(c(rep(0, 5), 1:5), 5, rep(1, 10)),
                   c(1, 1, 1, 1, 1, 3, 4, 4, 5, 5))
  expect_identical(.quintiles_ponderados(c(1, 2, NA, 4, 5, 6), 5, c(1, NA, 1, 0, 1, 1)),
                   c(1, NA, NA, 2, 3, 5))
  expect_error(.quintiles_ponderados(1:5, 5, c(1, 1, -1, 1, 1)), 'weight is negative')
  expect_error(.quintiles_ponderados(1:5, 5, rep(0, 5)), 'weights are zero')
})

test_that('los quintiles conservan 100 distribuciones historicas simuladas', {
  simulados <- leer_referencia_quintiles()$simulados
  for (i in seq_along(simulados)) {
    caso <- simulados[[i]]
    expect_identical(.quintiles_ponderados(caso$x, 5, caso$w), caso$esperado,
                     info = paste('replica', i))
  }
})

test_that('el flujo de hogar conserva los quintiles historicos de 0.3.2', {
  referencia <- readRDS(testthat::test_path('fixtures', 'flujo_contribucion_032.rds'))
  salida <- procesar_contribucion_hogar(referencia$datos)
  for (variable in names(referencia$esperado)) {
    expect_identical(as.numeric(salida[[variable]]), referencia$esperado[[variable]], info=variable)
  }
  expect_true(all(vapply(split(salida$quintil_ing_hog_pc, interaction(salida$anio, salida$folio2)),
    function(x) length(unique(x)) == 1L, logical(1))))
})