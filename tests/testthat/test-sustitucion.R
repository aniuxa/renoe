test_that("Sustitución de archivos 2022T1 funciona correctamente", {
  tablas <- c("hog", "viv", "sdem", "coe1", "coe2")

  # Crear directorio temporal simulado
  tmp_dir <- tempfile("unzip_enoe_2022_1t")
  for (tabla in tablas) {
    subdir <- file.path(tmp_dir, paste0("conjunto_de_datos_", tabla, "_enoen_2022_1t"))
    dir.create(subdir, recursive = TRUE, showWarnings = FALSE)
    # Crear archivos vacíos
    file.create(file.path(subdir, paste0("conjunto_de_datos_", tabla, "_enoen_2022_1t.csv")))
  }

  # Ejecutar la sustitución
  .sustituir_todo_enoe_2022t1(tmp_dir)

  for (tabla in tablas) {
    ruta <- file.path(
      tmp_dir,
      paste0("conjunto_de_datos_", tabla, "_enoen_2022_1t"),
      paste0("conjunto_de_datos_", tabla, "_enoen_2022_1t.csv")
    )
    expect_true(file.exists(ruta))
    expect_gt(file.size(ruta), 1000)

    df <- .leer_datos_enoe(tabla, tmp_dir, "enoen", 2022, 1)
    expect_s3_class(df, "data.frame")
    expect_true(nrow(df) > 10)
  }
})
