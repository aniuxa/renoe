test_that(".construir_url_enoe genera URLs correctas", {
  # Caso especial 2017
  url_2017 <- .construir_url_enoe(2017, 1)
  expect_equal(url_2017$prefijo, "enoe")
  expect_true(grepl("2017_trim1_enoe_csv.zip", url_2017$url))

  # Caso especial 2018-T1
  url_2018t1 <- .construir_url_enoe(2018, 1)
  expect_equal(url_2018t1$prefijo, "enoe")

  # Caso especial 2018-T2
  url_2018t2 <- .construir_url_enoe(2018, 2)
  expect_true(grepl("2018_2t_csv.zip", url_2018t2$url))

  # Caso ENOEN (2020-T3)
  url_2020t3 <- .construir_url_enoe(2020, 3)
  expect_equal(url_2020t3$prefijo, "enoen")

  url_2020t1 <- .construir_url_enoe(2020, 1)
  expect_equal(url_2020t1$prefijo, "enoe")
  expect_match(url_2020t1$url, "/microdatos/2020trim1_csv[.]zip$")

  # Caso normal (2023)
  url_2023 <- .construir_url_enoe(2023, 1)
  expect_equal(url_2023$prefijo, "enoe")
  expect_true(grepl("enoe_2023_1t", url_2023$url))
})

test_that(".obtener_id_vars devuelve variables correctas", {
  # Caso tradicional (2005-2020T2)
  vars_trad <- .obtener_id_vars(2015, 1)
  expect_true(all(c("fac", "t_loc", "est_d") %in% vars_trad))

  # Caso ENOEN (2020T3-2022T4)
  vars_enoen <- .obtener_id_vars(2021, 1)
  expect_true(all(c("fac_tri", "fac_men") %in% vars_enoen))

  # Caso reciente (2023+)
  vars_reciente <- .obtener_id_vars(2023, 1)
  expect_true(all(c("est_d_tri", "t_loc_tri") %in% vars_reciente))
})

test_that(".estandarizar_ids convierte variables correctamente", {
  df_test <- data.frame(
    cd_a = c("1", "2", "3"),
    ent = c("01", "02", "ABC"),
    fac = c("1.5", "2.3", "NA"),
    texto = c("a", "b", "c")
  )

  df_std <- .estandarizar_ids(df_test, 2015, 1)
  expect_true(is.numeric(df_std$cd_a))
  expect_true(is.numeric(df_std$ent))
  expect_true(is.numeric(df_std$fac))
  expect_true(is.character(df_std$texto)) # No debería cambiar
})

test_that("2020-T1 exige y lee las cinco tablas de microdatos vigentes", {
  tmp <- tempfile()
  dir.create(tmp)
  tablas <- c("viv", "hog", "sdem", "coe1", "coe2")
  nombres <- c(
    viv = "ENOE_VIVT120.csv", hog = "ENOE_HOGT120.csv",
    sdem = "ENOE_SDEMT120.csv", coe1 = "ENOE_COE1T120.csv",
    coe2 = "ENOE_COE2T120.csv"
  )
  expect_false(.verificar_cache(tmp, tablas, "enoe", 2020, 1))
  for (archivo in nombres) writeLines("ENT,CON,V_SEL,N_HOG,H_MUD,N_REN,P3\n1,1,1,1,0,1,4211", file.path(tmp, archivo))
  expect_true(.verificar_cache(tmp, tablas, "enoe", 2020, 1))
  coe1 <- .leer_datos_enoe("coe1", tmp, "enoe", 2020, 1)
  expect_equal(coe1$p3, 4211)
})

test_that(".estandarizar_ids normaliza identificadores cve de 2025-T3 en adelante", {
  df_test <- data.frame(
    cve_ent = c("01", "02"), cve_mun = c("003", "004"),
    cve_loc = c("0005", "0006"), cve_ageb = c("00007", "00008"),
    cvegeo = c("01003", "02004"), stringsAsFactors = FALSE
  )
  df_std <- .estandarizar_ids(df_test, 2026, 1)

  expect_true(all(c("ent", "mun", "loc", "ageb", "cvegeo") %in% names(df_std)))
  expect_false(any(c("cve_ent", "cve_mun", "cve_loc", "cve_ageb") %in% names(df_std)))
  expect_equal(df_std$ent, c(1, 2))
  expect_equal(df_std$mun, c(3, 4))
  expect_identical(df_std$loc, c("0005", "0006"))
  expect_identical(df_std$ageb, c("00007", "00008"))
  expect_identical(df_std$cvegeo, c("01003", "02004"))
})

test_that("la URL normal de 2026 coincide con los ZIP publicados", {
  t1 <- .construir_url_enoe(2026, 1)
  t2 <- .construir_url_enoe(2026, 2)
  expect_match(t1$url, "conjunto_de_datos_enoe_2026_1t_csv[.]zip$")
  expect_match(t2$url, "conjunto_de_datos_enoe_2026_2t_csv[.]zip$")
  expect_identical(t1$prefijo, "enoe")
  expect_identical(t2$prefijo, "enoe")
})

test_that("las funciones internas no inventan periodos 2026 aún no publicados", {
  expect_error(.construir_url_enoe(2026, 3), "2026-T1 y 2026-T2")
  expect_error(.construir_url_enoe(2026, 4), "2026-T1 y 2026-T2")
  expect_error(.construir_url_enoe(2020, 2), "No existe 2020-T2")
})

test_that(".descargar_zip_enoe maneja errores correctamente", {
  skip_on_cran()
  skip_if_offline()

  # Suprime warnings esperados durante la prueba
  testthat::expect_false(
    suppressWarnings(
      .descargar_zip_enoe("http://url_invalida_que_no_existe_12345", tempfile(), intentos = 1)
    )
  )

  testthat::expect_false(
    suppressWarnings(
      .descargar_zip_enoe("esto_no_es_una_url", tempfile(), intentos = 1)
    )
  )

  testthat::expect_false(
    .descargar_zip_enoe("", tempfile())
  )

  # Verifica que realmente se generen warnings en caso de error
  testthat::expect_warning(
    .descargar_zip_enoe("http://url_invalida", tempfile(), intentos = 2),
    "Fallo en la descarga"
  )
})

test_that(".procesar_etiquetas_enoe funciona con datos de prueba", {
  # Crear datos y estructura de prueba
  test_dir <- tempfile()
  dir.create(test_dir)
  dir.create(file.path(test_dir, "conjunto_de_datos_sdem_enoe_2020_1t"))
  dir.create(file.path(test_dir, "conjunto_de_datos_sdem_enoe_2020_1t", "diccionario_de_datos"))

  # Diccionario de prueba
  dic_data <- "Nombre del campo,Longitud,Tipo,Nemónico,Catálogo,Rango de claves\nPregunta 1 Edad,2,Numeric,eda,SI,\"01 a 99\"\n"
  writeLines(dic_data, file.path(test_dir, "conjunto_de_datos_sdem_enoe_2020_1t", "diccionario_de_datos", "diccionario_datos_sdem_enoe_2020_1t.csv"))

  # Datos de prueba
  df_test <- data.frame(eda = c(25, 30, 40))

  df_etiquetado <- .procesar_etiquetas_enoe(df_test, "sdem", 2020, 1, test_dir, "enoe")
  expect_equal(sjlabelled::get_label(df_etiquetado$eda), "Edad")

  # Limpieza
  unlink(test_dir, recursive = TRUE)
})
