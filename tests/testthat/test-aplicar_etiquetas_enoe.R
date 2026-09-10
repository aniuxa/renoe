test_that("aplicar_etiquetas_enoe restaura etiquetas sin crear factores", {
  datos <- data.frame(
    sexo = c(1, 2, NA),
    clase2 = c(1, 4, 0)
  )

  resultado <- aplicar_etiquetas_enoe(datos, informar = FALSE)

  expect_equal(attr(resultado$sexo, "label"), "Sexo")
  expect_s3_class(resultado$sexo, "haven_labelled")
  expect_false(is.factor(resultado$sexo))
  expect_equal(unname(attr(resultado$sexo, "labels")), c(1, 2))
  expect_equal(as.numeric(resultado$sexo), datos$sexo)
})

test_that("aplicar_etiquetas_enoe respeta etiquetas existentes", {
  datos <- data.frame(sexo = c(1, 2))
  attr(datos$sexo, "label") <- "Etiqueta propia"

  resultado <- aplicar_etiquetas_enoe(datos, informar = FALSE)

  expect_equal(attr(resultado$sexo, "label"), "Etiqueta propia")
})
