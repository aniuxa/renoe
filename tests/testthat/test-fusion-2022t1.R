test_that("2022-T1 conserva población rural y toma ur de SDEM", {
  sustituir_original <- getFromNamespace(
    ".sustituir_todo_enoe_2022t1",
    "renoe"
  )
  ids_viv <- data.frame(
    tipo = 1, mes_cal = 1, cd_a = 1, ent = 1, con = 1, v_sel = 1
  )
  tablas <- list(
    viv = transform(ids_viv, ur = 1),
    hog = transform(ids_viv, n_hog = 1, h_mud = 0, ur = 1, per = 122),
    sdem = transform(
      ids_viv[rep(1, 2), ], n_hog = 1, h_mud = 0, n_ren = 1:2,
      ur = c(1, 2), per = 122, r_def = 0, c_res = 1
    ),
    coe1 = transform(
      ids_viv[rep(1, 2), ], n_hog = 1, h_mud = 0, n_ren = 1:2,
      p1 = 1
    ),
    coe2 = transform(
      ids_viv[rep(1, 2), ], n_hog = 1, h_mud = 0, n_ren = 1:2,
      p6 = 1
    )
  )

  testthat::with_mocked_bindings(
    {
      carpeta <- tempfile("fusion-2022t1-")
      dir.create(carpeta)
      anterior <- setwd(carpeta)
      on.exit(setwd(anterior), add = TRUE)
      dir.create("zip/enoe_2022_1t", recursive = TRUE)

      resultado <- suppressMessages(fusion_enoe(2022, 1, rapida = TRUE))
      expect_equal(nrow(resultado), 2)
      expect_equal(resultado$ur, c(1, 2))
      expect_true(all(resultado$per == 122))

      expect_warning(
        resultado_compat <- suppressMessages(
          fusion_enoe(2022, 1, rapida = TRUE, fusion_robusta = FALSE)
        ),
        "robusta para conservar"
      )
      expect_equal(nrow(resultado_compat), 2)
      expect_equal(resultado_compat$ur, c(1, 2))
    },
    .sustituir_todo_enoe_2022t1 = function(...) invisible(NULL),
    .leer_datos_enoe = function(tabla, ...) tablas[[tabla]],
    .estandarizar_ids = function(df, ...) df,
    .package = "renoe"
  )

  expect_identical(
    getFromNamespace(".sustituir_todo_enoe_2022t1", "renoe"),
    sustituir_original
  )
})
