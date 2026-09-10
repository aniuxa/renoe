test_that("info_trimestre incorpora los trimestres publicados de 2026", {
  t1 <- info_trimestre(2026, 1)
  t2 <- info_trimestre(2026, 2)

  expect_equal(t1$trimestre, "t126")
  expect_equal(t1$coe_tipo, "ampliado")
  expect_equal(t1$coe_v, "v6a")
  expect_equal(t1$sdem_v, "v5a")
  expect_equal(t1$fd, "v5")
  expect_equal(t1$encoding, "UTF-8")

  expect_equal(t2$trimestre, "t226")
  expect_equal(t2$coe_tipo, "basico")
  expect_equal(t2$coe_v, "v7")
  expect_equal(t2$sdem_v, "v5a")
  expect_equal(t2$fd, "v5")
  expect_equal(t2$encoding, "UTF-8")
})

test_that("info_trimestre no habilita trimestres de 2026 aún no publicados", {
  expect_warning(
    resultado <- info_trimestre(2026, 3),
    "t326"
  )
  expect_null(resultado)
})
