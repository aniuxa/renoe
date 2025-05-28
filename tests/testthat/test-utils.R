context("Pruebas para funciones auxiliares")

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
