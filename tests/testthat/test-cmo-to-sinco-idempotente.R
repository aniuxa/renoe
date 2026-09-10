test_that("cmo_to_sinco puede ejecutarse dos veces sin crear sufijos", {
  codigos <- data.frame(
    cmo_4d = c(1101, 1102),
    sinco4d = c(1111, 1112),
    sinco3d = c(111, 111)
  )
  datos <- data.frame(
    p3coe = c(1101, 1102),
    sinco4d.x = c(9999, 9999),
    sinco4d.y = c(8888, 8888),
    sinco3d.x = c(999, 999),
    sinco3d.y = c(888, 888)
  )

  primera <- cmo_to_sinco(datos, codigos = codigos)
  segunda <- cmo_to_sinco(primera, codigos = codigos)

  expect_equal(segunda$sinco4d, c(1111, 1112))
  expect_equal(segunda$sinco3d, c(111, 111))
  expect_false(any(grepl("^sinco(3d|4d)\\.[xy]$", names(segunda))))
  expect_equal(names(segunda), names(primera))
})
