test_that('la disponibilidad central bloquea periodos no validados', {
  expect_true(.validar_periodo_enoe(2005, 1))
  expect_true(.validar_periodo_enoe(2026, 2))
  for (periodo in list(c(2026, 3), c(2026, 4), c(2027, 1))) {
    expect_error(.construir_url_enoe(periodo[1], periodo[2]), 'ultimo validado es 2026-T2')
    expect_error(descarga_enoe(periodo[1], periodo[2]), 'ultimo validado es 2026-T2')
    expect_error(carga_enoe(periodo[1], periodo[2]), 'ultimo validado es 2026-T2')
    expect_warning(expect_null(info_trimestre(periodo[1], periodo[2])), 'periodo no habilitado')
  }
  expect_error(.validar_periodo_enoe(2020, 2), 'No existe 2020-T2')
  for (x in list(NA_real_, Inf, 2026.5, '2026', c(2025, 2026))) {
    expect_error(.validar_periodo_enoe(x, 1), 'escalares enteros')
  }
})

test_that('la descarga rechaza formatos y opciones invalidas antes de actuar', {
  expect_error(descarga_enoe(2026, 1, formato='invalido'), 'arg')
  for (x in list(0, -1, 1.5, NA_real_, Inf, c(1, 2))) {
    expect_error(descarga_enoe(2026, 1, intentos=x), 'intentos')
  }
  expect_error(carga_enoe(2026, 1, timeout_sec=0), 'timeout_sec')
  expect_error(carga_enoe(2026, 1, verificar_url=NA), 'verificar_url')
})

test_that('un ZIP invalido no se acepta ni se extrae', {
  invalido <- tempfile(fileext='.zip')
  writeLines(rep('contenido HTML incorrecto', 100), invalido)
  expect_false(.zip_enoe_valido(invalido))
  expect_false(.extraer_zip_enoe(invalido, tempfile()))
  expect_true(.zip_enoe_valido(testthat::test_path('fixtures','descarga_zip_valido.zip')))
})

test_that('se reintenta y restaura timeout antes de aceptar un ZIP', {
  fixture <- testthat::test_path('fixtures','descarga_zip_valido.zip')
  llamadas <- 0L
  testthat::local_mocked_bindings(download.file=function(url, destfile, ...) {
    llamadas <<- llamadas + 1L
    if (llamadas == 1L) return(1L)
    file.copy(fixture, destfile)
    0L
  }, .package='utils')
  anterior <- getOption('timeout')
  destino <- tempfile(fileext='.zip')
  expect_true(.descargar_zip_enoe('https://ejemplo.test/a.zip', destino, intentos=2, timeout=15))
  expect_identical(llamadas, 2L)
  expect_true(.zip_enoe_valido(destino))
  expect_identical(getOption('timeout'), anterior)
})

test_that('la descarga rechaza HTML y conserva un archivo previo en fallos', {
  llamadas <- 0L
  testthat::local_mocked_bindings(download.file=function(url, destfile, ...) {
    llamadas <<- llamadas + 1L
    writeLines(rep('respuesta HTML', 100), destfile)
    0L
  }, .package='utils')
  destino <- tempfile(fileext='.zip')
  writeLines('archivo anterior', destino)
  expect_warning(expect_false(.descargar_zip_enoe('https://ejemplo.test/a.zip', destino, intentos=2)), 'ZIP invalido')
  expect_identical(llamadas, 2L)
  expect_identical(readLines(destino), 'archivo anterior')
})

test_that('verificar_url controla la alternativa documentada de 2018-T2', {
  fixture <- testthat::test_path('fixtures','descarga_zip_valido.zip')
  urls <- character()
  testthat::local_mocked_bindings(download.file=function(url, destfile, ...) {
    urls <<- c(urls, url)
    if (length(urls) %% 2L == 1L) return(1L)
    file.copy(fixture, destfile); 0L
  }, .package='utils')
  origen <- 'https://ejemplo.test/conjunto_de_datos_enoe2018_2t_csv.zip'
  expect_true(.descargar_zip_enoe(origen, tempfile(), intentos=2, verificar_url=TRUE))
  expect_match(urls[2], 'enoe_2018_2t')
  urls <- character()
  expect_true(.descargar_zip_enoe(origen, tempfile(), intentos=2, verificar_url=FALSE))
  expect_identical(urls, rep(origen, 2))
})

test_that('la cache de archivos exige cinco tablas legibles y no vacias', {
  carpeta <- tempfile(); dir.create(carpeta)
  archivos <- file.path(carpeta, paste0(1:5,'.rds'))
  for (archivo in archivos) saveRDS(data.frame(id=1), archivo)
  expect_true(.cache_archivos_enoe_valida(archivos, 'rds'))
  writeLines('archivo corrupto', archivos[3])
  expect_false(.cache_archivos_enoe_valida(archivos, 'rds'))
  saveRDS(data.frame(id=numeric()), archivos[3])
  expect_false(.cache_archivos_enoe_valida(archivos, 'rds'))
})

test_that('la cache CSV exige encabezado y al menos una fila', {
  carpeta <- tempfile(); dir.create(carpeta)
  tablas <- c('viv','hog','sdem','coe1','coe2')
  archivos <- file.path(carpeta, paste0('conjunto_de_datos_',tablas,'_enoe_2026_1t.csv'))
  for (archivo in archivos) writeLines(c('id','1'), archivo)
  expect_true(.verificar_cache(carpeta, tablas, 'enoe', 2026, 1))
  writeLines('id', archivos[3])
  expect_false(.verificar_cache(carpeta, tablas, 'enoe', 2026, 1))
})

test_that('descarga escribe cinco tablas y vuelve a usar solo cache valida', {
  carpeta <- tempfile(); dir.create(carpeta)
  anterior <- getwd(); setwd(carpeta); on.exit(setwd(anterior), add=TRUE)
  llamadas <- 0L
  testthat::local_mocked_bindings(
    .descargar_zip_enoe=function(...) { llamadas <<- llamadas + 1L; TRUE },
    .extraer_zip_enoe=function(...) TRUE,
    .leer_datos_enoe=function(...) data.frame(id=1:2),
    .estandarizar_ids=function(df, ...) df,
    .package='renoe'
  )
  expect_true(descarga_enoe(2026, 1, formato='rds'))
  expect_identical(llamadas, 1L)
  expect_true(descarga_enoe(2026, 1, formato='rds'))
  expect_identical(llamadas, 1L)
  writeLines('cache corrupta', 'datos/sdem2026_1t.rds')
  expect_true(descarga_enoe(2026, 1, formato='rds'))
  expect_identical(llamadas, 2L)
  expect_equal(nrow(readRDS('datos/sdem2026_1t.rds')), 2L)
})

test_that('carga no acepta un trimestre con tablas incompletas', {
  carpeta <- tempfile(); dir.create(carpeta)
  anterior <- getwd(); setwd(carpeta); on.exit(setwd(anterior), add=TRUE)
  testthat::local_mocked_bindings(
    .verificar_cache=function(...) TRUE,
    .leer_datos_enoe=function(tabla, ...) if (tabla == 'coe2') NULL else data.frame(id=1),
    .estandarizar_ids=function(df, ...) df,
    .package='renoe'
  )
  expect_error(carga_enoe(2026, 1, list=TRUE, rapida=TRUE), 'coe2')
})
