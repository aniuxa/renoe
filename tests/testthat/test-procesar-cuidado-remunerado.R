test_that("wrapper distingue los cortes y preserva la armonizacion general", {
  x <- data.frame(anio=c(2012,2012,2021,2021), trim=c('t2','t3','t2','t3'),
                  p3coe=c(8200,2331,2421,2433), sinco3d=c(999,233,242,242),
                  p4a=c(8140,6111,6211,6211), clase2=1,
                  codigo_ocupacion_original=c(8200,2331,2421,2433))
  original <- x
  y <- suppressMessages(procesar_cuidado_remunerado(x))
  expect_identical(x, original)
  expect_identical(y[names(x)], x)
  expect_equal(y$clasificador_ocupacion, c('CMO','SINCO 2011','SINCO 2011','SINCO 2019'))
  expect_equal(y$codigo_ocupacion_armonizado, c(511L,233L,242L,243L))
  expect_equal(as.numeric(y$class_ocu), c(13,11,12,12))
  expect_equal(as.numeric(y$trabajo_cuidado_rem), rep(1,4))
  expect_equal(as.numeric(y$trabajo_cuidado_mercado), rep(1,4))
  expect_match(y$calidad_armonizacion_cuidado[1], 'multiple')
  expect_identical(suppressMessages(procesar_cuidado_remunerado(y)), y)
})
test_that("la versión SCIAN sigue el corte oficial de 2021-T3", {
  x <- data.frame(
    anio = c(2005, 2021, 2021), trim = c(1, 2, 3),
    p3coe = c(2331, 2421, 2433), p4a = c(6111, 6211, 6211), clase2 = 1
  )

  y <- suppressMessages(procesar_cuidado_remunerado(x))
  expect_equal(
    as.character(y$scian_version_cuidado),
    c("SCIAN-Hogares 2007", "SCIAN-Hogares 2007", "SCIAN-Hogares 2018")
  )
})

test_that("ocupacion e industria se distinguen con las reglas del articulo", {
  x <- data.frame(p3coe=c(2331,4111,9611,4111,2411),
                  p4a=c(4611,6111,8140,8140,5413), clase2=1)
  y <- procesar_cuidado_remunerado(x,2022,1)
  expect_equal(as.numeric(y$care_w), c(3,4,2,2,0))
  expect_equal(as.numeric(y$trabajo_cuidado_rem), c(1,1,1,0,0))
  expect_equal(as.numeric(y$care_industry), c(0,1,2,2,3))
  expect_identical(y[names(x)], x)
})

test_that("faltantes, no ocupados y codigos no aplicables no son no cuidado", {
  x <- data.frame(p3coe=c(2331,2331,9999,0,NA,2331,2331,2331.5),
                  p4a=c(6111,6111,6111,6111,6111,9999,NA,6111),
                  clase2=c(2,NA,1,1,1,1,1,1))
  y <- procesar_cuidado_remunerado(x,2022,1)
  expect_true(all(is.na(y$trabajo_cuidado_rem)))
  expect_false(any(y$cuidado_ocupacion_medible[1:5]))
  expect_false(any(y$cuidado_actividad_medible[6:7]))
})

test_that("CMO 8200 no contamina otros periodos ni pierde columnas personalizadas", {
  x <- data.frame(anio=c(2012,2022),trim=1,p3coe=c(8200,4111),
                  ocupacion=c(NA,411),p4a=8140,clase2=1,cmo_original=8200)
  y <- suppressMessages(class_cuidado_rem(x, variable_ocupacion='ocupacion'))
  expect_equal(as.numeric(y$class_ocu), c(13,0))
  expect_equal(as.logical(y$cuidado_cmo_8200_domestico),c(TRUE,FALSE))
})

test_that("wrapper acepta columnas personalizadas y tipos habituales", {
  x <- data.frame(ocup=factor(c('2331','4111')), rama=factor(c('6111','6111')), ocupado=1)
  y <- procesar_cuidado_remunerado(x,2022,'t1', variable_codigo='ocup',
                                  variable_actividad='rama',variable_ocupado='ocupado')
  expect_equal(as.numeric(y$class_ocu), c(11,0))
  expect_identical(y$codigo_ocupacion_original_cuidado,x$ocup)
  z <- data.frame(sinco3d=243,p4a=6211,clase2=1)
  expect_equal(as.numeric(procesar_cuidado_remunerado(z,2022,1)$class_ocu),12)
  z$sinco4d_base2011 <- 2423
  expect_error(procesar_cuidado_remunerado(z,2022,1),'armonizado')
})

test_that("validaciones rechazan insumos incompletos y periodos contradictorios", {
  x <- data.frame(p3coe=2331,p4a=6111,clase2=1)
  expect_error(procesar_cuidado_remunerado(x),'periodo')
  expect_error(procesar_cuidado_remunerado(x,2022,1.5),'Periodo')
  expect_error(procesar_cuidado_remunerado(x,2004,1),'Periodo')
  expect_error(procesar_cuidado_remunerado(x,2022,NA),'Periodo')
  expect_error(procesar_cuidado_remunerado(x,2022,c(1,2)),'Longitud')
  x$anio <- 2021
  expect_error(procesar_cuidado_remunerado(x,2022,1),'Conflicto')
  expect_error(procesar_cuidado_remunerado(x[c('p3coe','clase2')],2022,1),'p4a')
  expect_error(procesar_cuidado_remunerado(x[c('p4a','clase2')],2022,1),'ocupacion')
  expect_error(procesar_cuidado_remunerado(x[c('p4a','clase2')],2012,1),'CMO')
})

test_that("puente conserva casos conocidos, faltantes y proteccion de salidas", {
  x <- data.frame(p3coe=c(8200,1330,8888,NA))
  y <- cmo_to_sinco11_care(x)
  expect_equal(as.integer(y$sinco11),c(5113L,2332L,NA_integer_,NA_integer_))
  expect_equal(as.integer(y$sinco11_n_destinos),c(3L,1L,NA_integer_,NA_integer_))
  expect_match(y$sinco11_calidad[3],'Sin correspondencia')
  expect_identical(cmo_to_sinco11_care(y),y)
  expect_error(cmo_to_sinco11_care(y,sobrescribir=FALSE),'Ya existen')
})

test_that("wrapper procesa cero filas", {
  x <- data.frame(anio=integer(),trim=integer(),p3coe=integer(),p4a=integer(),clase2=integer())
  y <- procesar_cuidado_remunerado(x)
  expect_equal(nrow(y),0)
  expect_true(all(c('class_ocu','trabajo_cuidado_mercado','trabajo_cuidado_rem',
                    'cuidado_posicion_remunerada','cuidado_sin_pago',
                    'estado_ingreso_cuidado') %in% names(y)))
})

test_that("cuidado de mercado separa posición e ingreso observado o imputado", {
  x <- data.frame(
    anio = 2025, trim = 4, p3coe = 2331, p4a = 6111, clase2 = 1,
    pos_ocu = c(1, 1, 4, 2),
    ingocup = c(5000, NA, 0, NA),
    ingocup_imp = c(5000, 4800, 0, NA),
    imp_ingocup = c(0, 1, 0, 0),
    sin_pago = c(0, 0, 1, 0)
  )
  original <- x
  y <- procesar_cuidado_remunerado(x)

  expect_identical(y[names(original)], original)
  expect_equal(as.numeric(y$trabajo_cuidado_mercado), rep(1, 4))
  expect_equal(as.numeric(y$trabajo_cuidado_rem), rep(1, 4))
  expect_equal(as.numeric(y$cuidado_posicion_remunerada), c(1, 1, 0, 1))
  expect_equal(as.numeric(y$cuidado_sin_pago), c(0, 0, 1, 0))
  expect_equal(
    as.character(y$estado_ingreso_cuidado),
    c("observado_positivo", "imputado_positivo",
      "sin_ingreso_identificado", "no_determinado")
  )
  expect_identical(procesar_cuidado_remunerado(y), y)
})

test_that("estado de cuidado queda NA fuera del universo o sin medición", {
  x <- data.frame(
    anio = 2025, trim = 4, p3coe = c(2331, NA, 4111),
    p4a = c(6111, 6111, 4611), clase2 = c(2, 1, 1),
    pos_ocu = c(1, 1, 1), ingocup = c(5000, 5000, 5000),
    ingocup_imp = c(5000, 5000, 5000), imp_ingocup = 0, sin_pago = 0
  )
  y <- procesar_cuidado_remunerado(x)

  expect_true(all(is.na(y$trabajo_cuidado_mercado[1:2])))
  expect_true(all(is.na(y$cuidado_posicion_remunerada[1:2])))
  expect_true(all(is.na(y$cuidado_sin_pago[1:2])))
  expect_true(all(is.na(y$estado_ingreso_cuidado[1:2])))
  expect_equal(as.numeric(y$trabajo_cuidado_mercado[3]), 0)
  expect_equal(as.numeric(y$cuidado_posicion_remunerada[3]), 0)
  expect_equal(as.numeric(y$cuidado_sin_pago[3]), 0)
  expect_true(is.na(y$estado_ingreso_cuidado[3]))
})
