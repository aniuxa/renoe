.validar_llave_union_enoe <- function(data, claves, tabla) {
  faltantes <- setdiff(claves, names(data))
  if (length(faltantes)) {
    stop(
      "Faltan llaves en ", tabla, ": ", paste(faltantes, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyNA(data[claves])) {
    stop("La llave de ", tabla, " contiene valores faltantes.", call. = FALSE)
  }
  if (anyDuplicated(data[claves])) {
    stop(
      "La llave de ", tabla, " no es unica: ",
      paste(claves, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Fusionar tablas de la ENOE
#'
#' Une las tablas de vivienda, hogar, sociodemografico y componentes COE
#' en un unico data frame.
#'
#' @encoding UTF-8
#' @param anio Ano del trimestre (2005-2026).
#' @param trimestre Numero del trimestre (1-4).
#' @param rapida Logico. Si `TRUE`, omite el etiquetado de variables.
#' @param formato Formato de salida ("parquet", "rds" o "dta"). Opcional.
#' @param guardar Logico. Si `TRUE` y se especifica formato, guarda el archivo fusionado.
#' @param intentos Numero de intentos para cargar datos (por defecto 3).
#' @param ... Otros parametros para pasar a `carga_enoe()`.
#'
#' @return Un data frame con las tablas fusionadas. Si se especifica formato y
#'   `guardar = TRUE`, guarda el archivo en el subdirectorio `"datos"` con el nombre
#'   `"enoe_fusion_ANIO_TRIMESTREt.FORMATO"`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Fusionar tablas para el T3 de 2020
#' datos2020 <- fusion_enoe(2020, 3)
#'
#' # Fusionar y guardar como Parquet
#' fusion_enoe(2019, 2, formato = "parquet", guardar = TRUE)
#' }
#' @family descarga_documenta_enoe

fusion_enoe <- function(anio, trimestre, rapida = FALSE, formato = NULL,
                        guardar = !is.null(formato), intentos = 3, ...) {

  tablas <- c("viv", "hog", "sdem", "coe1", "coe2")
  url_info <- .construir_url_enoe(anio, trimestre)
  unzip_dir <- paste0("zip/enoe_", anio, "_", trimestre, "t")
  prefijo <- url_info$prefijo

  limpiar_sufijos_join <- function(df, preferir_y = character()) {
    for (variable in preferir_y) {
      izquierda <- paste0(variable, ".x")
      derecha <- paste0(variable, ".y")
      if (all(c(izquierda, derecha) %in% names(df))) {
        df[[izquierda]] <- dplyr::coalesce(df[[derecha]], df[[izquierda]])
      }
    }
    df %>%
      dplyr::select(-dplyr::ends_with(".y")) %>%
      dplyr::rename_with(
        ~ stringr::str_remove(.x, "\\.x$"),
        dplyr::ends_with(".x")
      )
  }

  auditar_union <- function(izquierda, derecha, claves, etapa, tipo, salida) {
    claves_izq <- dplyr::distinct(izquierda, dplyr::across(dplyr::all_of(claves)))
    claves_der <- dplyr::distinct(derecha, dplyr::across(dplyr::all_of(claves)))
    data.frame(
      etapa = etapa,
      tipo_union = tipo,
      filas_izquierda = nrow(izquierda),
      filas_derecha = nrow(derecha),
      llaves_duplicadas_izquierda = sum(duplicated(izquierda[claves])),
      llaves_duplicadas_derecha = sum(duplicated(derecha[claves])),
      llaves_sin_pareja_izquierda = nrow(dplyr::anti_join(
        claves_izq, claves_der, by = claves
      )),
      llaves_sin_pareja_derecha = nrow(dplyr::anti_join(
        claves_der, claves_izq, by = claves
      )),
      filas_salida = nrow(salida),
      stringsAsFactors = FALSE
    )
  }

  if (!dir.exists(unzip_dir)) {
    message("Archivos no encontrados localmente. Usando carga_enoe() para descargar y procesar.")
    datos <- carga_enoe(
      anio = anio,
      trimestre = trimestre,
      list = TRUE,
      rapida = rapida,
      intentos = intentos,
      ...
    )
  } else {
    message("Cargando archivos desde ", unzip_dir)

    datos <- lapply(tablas, function(tabla) {
      df <- .leer_datos_enoe(tabla, unzip_dir, prefijo, anio, trimestre)

      if (!is.null(df)) {
        df <- .estandarizar_ids(df, anio, trimestre)

        if (!rapida) {
          df <- .procesar_etiquetas_enoe(df, tabla, anio, trimestre, unzip_dir, prefijo)
        }
      }

      df
    }) |>
      stats::setNames(tablas)
  }

  if (!all(tablas %in% names(datos))) {
    stop("Faltan tablas requeridas: ", paste(setdiff(tablas, names(datos)), collapse = ", "))
  }

  message("\nFusionando tablas para ", anio, " trimestre ", trimestre, "...")

  posibles_idviv  <- c("tipo", "mes_cal", "cd_a", "ca", "ent", "ur", "con", "v_sel")
    posibles_idhog  <- c(posibles_idviv, "n_hog", "h_mud")
    posibles_idsdem <- c(posibles_idhog, "n_ren")

    idviv <- Reduce(intersect, list(posibles_idviv, names(datos$viv), names(datos$hog)))
    idhog <- Reduce(intersect, list(posibles_idhog, names(datos$hog), names(datos$sdem)))
    idsdem <- Reduce(intersect, list(posibles_idsdem, names(datos$sdem), names(datos$coe1), names(datos$coe2)))

    if (length(idviv) == 0) {
      stop("No se encontraron variables de uni\u00F3n entre viv y hog.")
    }
    if (length(idhog) == 0) {
      stop("No se encontraron variables de uni\u00F3n entre hog y sdem.")
    }
    if (length(idsdem) == 0) {
      stop("No se encontraron variables de uni\u00F3n entre sdem y coe.")
    }

    .validar_llave_union_enoe(datos$viv, idviv, "VIV")
    .validar_llave_union_enoe(datos$hog, idhog, "HOG")
    .validar_llave_union_enoe(datos$sdem, idsdem, "SDEM")
    .validar_llave_union_enoe(datos$coe1, idsdem, "COE1")
    .validar_llave_union_enoe(datos$coe2, idsdem, "COE2")

    coe_union <- datos$coe1 %>%
      dplyr::left_join(datos$coe2, by = idsdem)
    auditoria_fusion <- list(auditar_union(
      datos$coe1, datos$coe2, idsdem, "COE1-COE2", "left", coe_union
    ))
    coe_fusionado <- coe_union %>%
      limpiar_sufijos_join() %>%
      dplyr::rename_with(
        ~ paste0(.x, "coe"),
        dplyr::any_of(c("p1", "p3", "p4_1", "p4_2"))
      )

    viv_hog_union <- datos$hog %>%
      dplyr::left_join(datos$viv, by = idviv)
    auditoria_fusion[[2L]] <- auditar_union(
      datos$hog, datos$viv, idviv, "HOG-VIV", "left", viv_hog_union
    )
    viv_hog <- viv_hog_union %>% limpiar_sufijos_join()
    sdem_filtrado <- datos$sdem %>% dplyr::filter(r_def == 0, c_res != 2)
    auditoria_fusion[[3L]] <- data.frame(
      etapa = "FILTRO-SDEM", tipo_union = "filtro",
      filas_izquierda = nrow(datos$sdem), filas_derecha = NA_integer_,
      llaves_duplicadas_izquierda = 0L,
      llaves_duplicadas_derecha = NA_integer_,
      llaves_sin_pareja_izquierda = nrow(datos$sdem) - nrow(sdem_filtrado),
      llaves_sin_pareja_derecha = NA_integer_,
      filas_salida = nrow(sdem_filtrado)
    )
    sdem_union <- sdem_filtrado %>% dplyr::left_join(viv_hog, by = idhog)
    auditoria_fusion[[4L]] <- auditar_union(
      sdem_filtrado, viv_hog, idhog, "SDEM-HOG", "left", sdem_union
    )
    hogares_sin_hog <- auditoria_fusion[[4L]]$llaves_sin_pareja_izquierda
    if (hogares_sin_hog > 0L) {
      warning(
        "HOG no cubre ", hogares_sin_hog,
        " llaves de SDEM; se conservan las personas y quedan NA auxiliares."
      )
    }
    sdem_con_hogar <- sdem_union %>% limpiar_sufijos_join()
    enoe_union <- sdem_con_hogar %>%
      dplyr::left_join(coe_fusionado, by = idsdem)
    auditoria_fusion[[5L]] <- auditar_union(
      sdem_con_hogar, coe_fusionado, idsdem, "SDEM-COE", "left", enoe_union
    )
    enoe_fusionado <- enoe_union %>%
      limpiar_sufijos_join()

    auditoria_fusion <- dplyr::bind_rows(auditoria_fusion)
    attr(enoe_fusionado, "auditoria_fusion") <- auditoria_fusion
    for (i in seq_len(nrow(auditoria_fusion))) {
      z <- auditoria_fusion[i, ]
      message(
        z$etapa, ": ", z$filas_izquierda, " + ", z$filas_derecha,
        " -> ", z$filas_salida, "; sin pareja izq=",
        z$llaves_sin_pareja_izquierda, ", der=",
        z$llaves_sin_pareja_derecha
      )
    }

  n_sdem <- nrow(datos$sdem[datos$sdem$r_def == 0 & datos$sdem$c_res != 2, ])
  n_fusion <- nrow(enoe_fusionado)

  message("Filas esperadas tras el filtro (sdem): ", n_sdem)
  message("Filas en la tabla fusionada final: ", n_fusion)

  if (n_fusion != n_sdem) {
    stop(
      "La fusi\u00F3n can\u00F3nica no conserv\u00F3 el n\u00FAmero esperado de filas de SDEM: ",
      n_fusion, " frente a ", n_sdem, ". No se guardar\u00E1 el resultado.",
      call. = FALSE
    )
  } else if (n_fusion == 0) {
    warning("La tabla fusionada est\u00E1 vac\u00EDa. Verificar posibles errores.")
  }

  if (!is.null(formato) && guardar) {
    ruta_salida <- file.path("datos", paste0("enoe_fusion_", anio, "_", trimestre, "t.", formato))
    dir.create(dirname(ruta_salida), recursive = TRUE, showWarnings = FALSE)
    message("Guardando como: ", ruta_salida)

    switch(
      formato,
      "parquet" = arrow::write_parquet(enoe_fusionado, ruta_salida),
      "rds"     = saveRDS(enoe_fusionado, ruta_salida),
      "dta"     = haven::write_dta(enoe_fusionado, ruta_salida),
      stop("Formato no soportado: ", formato)
    )
  }

  return(enoe_fusionado)
}
