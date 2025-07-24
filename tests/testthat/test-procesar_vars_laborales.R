test_that("procesar_vars_laborales agrega y etiqueta variables correctamente", {
  datos <- data.frame(
    anio = c(2012, 2012, 2014, 2014),
    trim = c("t1", "t2", "t1", "t3"),
    coe_tipo = c("ampliado", "ampliado", "básico", "ampliado"),
    p3coe = c(7121, 4190, 1110, 1110),
    cs_p13_1 = c(6, 2, 3, 8),
    clase2 = c(1, 1, 2, 3),
    pos_ocu = c(1, 1, 1, 4),
    tue2 = c(3, 7, 1, 2),
    p2_4 = c(3, 4, NA, 4),
    p3i = c(1, 2, 9, NA),
    p3j = c(1, 2, 1, 2),
    p3j1 = c(1, 0, 9, NA),
    p3k1 = c(2, 1, 1, 2),
    anios_es = c(0, 20, 14, 2),
    stringsAsFactors = FALSE
  )

  procesado <- procesar_vars_laborales(datos)

  # Verifica que se crean variables esperadas
  expect_true(all(c("skill_level", "skill_actual", "mismatch",
                    "mismatch2", "status_seq", "temporal", "temporal_seq") %in% names(procesado)))

  # Verifica que las variables tengan etiquetas (get_label)
  expect_equal(sjlabelled::get_label(procesado$skill_level), "Nivel de habilidad requerido por la ocupación")
  expect_equal(sjlabelled::get_label(procesado$mismatch), "Desajuste educativo")

  # Verifica contenido de etiquetas (get_labels)
  lbls <- sjlabelled::get_labels(procesado$skill_level, values = "as.name")
  expect_equal(unname(lbls["3"]), "Terciaria")

  # Verifica condiciones de contrato y temporalidad
  expect_equal(procesado$contrato0[1], 1)
  expect_equal(procesado$contrato1[1], 0)  # ajustado: contrato indefinido
  expect_equal(procesado$temporal[1], 2)   # ajustado: asalariado con contrato indefinido
  expect_equal(procesado$temporal_seq[3], 0)  # fuera de la Población Ocupada

  # Revisión de no pérdida de observaciones
  expect_equal(nrow(procesado), nrow(datos))
})
