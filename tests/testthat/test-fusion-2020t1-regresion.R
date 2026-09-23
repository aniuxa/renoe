test_that("2020-T1 recupera COE sin alterar filas, fac ni universo", {
  sdem <- data.frame(
    tipo = 1, mes_cal = 1, cd_a = 52, ca = 1, ent = 1,
    ur = 1, con = c(40002, 40003, 40004), v_sel = 2,
    n_hog = 1, h_mud = 0, n_ren = 1:3,
    fac = c(100, 200, 300), clase1 = 1
  )
  coe1 <- sdem[c(
    "tipo", "mes_cal", "cd_a", "ca", "ent", "ur", "con", "v_sel",
    "n_hog", "h_mud", "n_ren"
  )]
  coe1$ur <- c(2, 1, 2)
  coe1$p3 <- c(1234, 5678, 9999)
  coe2 <- coe1
  coe2$p3 <- NULL
  coe2$p4a <- c(1, 2, 3)
  viv <- unique(sdem[c("tipo", "mes_cal", "cd_a", "ca", "ent", "ur", "con", "v_sel")])
  hog <- unique(sdem[c(
    "tipo", "mes_cal", "cd_a", "ca", "ent", "ur", "con", "v_sel",
    "n_hog", "h_mud"
  )])
  datos <- list(viv = viv, hog = hog, sdem = sdem, coe1 = coe1, coe2 = coe2)

  llaves_nuevas <- .llaves_union_enoe(datos, 2020, 1)$idsdem
  llaves_antiguas <- c(llaves_nuevas, "ur")
  llaves_antiguas <- intersect(names(sdem), llaves_antiguas)

  expect_false("ur" %in% llaves_nuevas)
  expect_true("ur" %in% llaves_antiguas)
  expect_equal(anyDuplicated(sdem[llaves_nuevas]), 0L)
  expect_equal(anyDuplicated(coe1[llaves_nuevas]), 0L)
  expect_equal(anyDuplicated(coe2[llaves_nuevas]), 0L)
  expect_invisible(.validar_llave_union_enoe(sdem, llaves_nuevas, "SDEM"))
  expect_invisible(.validar_llave_union_enoe(coe1, llaves_nuevas, "COE1"))
  expect_invisible(.validar_llave_union_enoe(coe2, llaves_nuevas, "COE2"))

  union_antigua <- dplyr::left_join(sdem, coe1, by = llaves_antiguas)
  union_corregida <- dplyr::left_join(sdem, coe1, by = llaves_nuevas)

  expect_equal(sum(is.na(union_antigua$p3)), 2L)
  expect_equal(sum(is.na(union_corregida$p3)), 0L)
  expect_equal(nrow(union_corregida), nrow(sdem))
  expect_equal(sum(union_corregida$fac), sum(sdem$fac))
  expect_equal(sum(union_corregida$fac[union_corregida$clase1 == 1]), 600)
  expect_equal(union_corregida$p3, c(1234, 5678, 9999))
  expect_equal(union_corregida$ur.x, c(1, 1, 1))
  expect_equal(union_corregida$ur.y, c(2, 1, 2))
})

test_that("la excepción de ur está limitada exactamente a 2020-T1", {
  base <- data.frame(
    tipo = 1, mes_cal = 1, cd_a = 52, ca = 1, ent = 1, ur = 1,
    con = 40002, v_sel = 2, n_hog = 1, h_mud = 0, n_ren = 1
  )
  datos <- list(
    viv = base[c("tipo", "mes_cal", "cd_a", "ca", "ent", "ur", "con", "v_sel")],
    hog = base[c("tipo", "mes_cal", "cd_a", "ca", "ent", "ur", "con", "v_sel", "n_hog", "h_mud")],
    sdem = base, coe1 = base, coe2 = base
  )

  expect_false("ur" %in% .llaves_union_enoe(datos, 2020, 1)$idsdem)
  expect_true("ur" %in% .llaves_union_enoe(datos, 2019, 4)$idsdem)
  expect_true("ur" %in% .llaves_union_enoe(datos, 2020, 3)$idsdem)
  expect_true("ur" %in% .llaves_union_enoe(datos, 2021, 1)$idsdem)
})