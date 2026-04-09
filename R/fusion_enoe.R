#' Fusionar tablas de la ENOE
#'
#' Une las tablas de vivienda, hogar, sociodemográfico y componentes COE
#' en un único data frame.
#'
#' @encoding UTF-8
#' @param anio Año del trimestre (2005-2025).
#' @param trimestre Número del trimestre (1-4).
#' @param rapida Lógico. Si `TRUE`, omite el etiquetado de variables.
#' @param formato Formato de salida ("parquet", "rds" o "dta"). Opcional.
#' @param guardar Lógico. Si `TRUE` y se especifica formato, guarda el archivo fusionado.
#' @param intentos Número de intentos para cargar datos (por defecto 3).
#' @param fusion_robusta Lógico. Si `TRUE`, utiliza claves de fusión basadas en las variables disponibles.
#' @param ... Otros parámetros para pasar a `carga_enoe()`.
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
                        guardar = !is.null(formato), intentos = 3,
                        fusion_robusta = TRUE, ...) {

  tablas <- c("viv", "hog", "sdem", "coe1", "coe2")
  url_info <- .construir_url_enoe(anio, trimestre)
  unzip_dir <- paste0("zip/enoe_", anio, "_", trimestre, "t")
  prefijo <- url_info$prefijo

  limpiar_sufijos_join <- function(df) {
    df %>%
      dplyr::select(-dplyr::ends_with(".y")) %>%
      dplyr::rename_with(
        ~ stringr::str_remove(.x, "\\.x$"),
        dplyr::ends_with(".x")
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
    if (anio == 2022 && trimestre == 1) {
      .sustituir_todo_enoe_2022t1(unzip_dir)
    }

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

  if (fusion_robusta) {
    posibles_idviv  <- c("tipo", "mes_cal", "cd_a", "ca", "ent", "con", "v_sel")
    posibles_idhog  <- c(posibles_idviv, "n_hog", "h_mud")
    posibles_idsdem <- c(posibles_idhog, "n_ren")

    idviv <- Reduce(intersect, list(posibles_idviv, names(datos$viv), names(datos$hog)))
    idhog <- Reduce(intersect, list(posibles_idhog, names(datos$hog), names(datos$sdem)))
    idsdem <- Reduce(intersect, list(posibles_idsdem, names(datos$sdem), names(datos$coe1), names(datos$coe2)))

    if (length(idviv) == 0) {
      stop("No se encontraron variables de unión entre viv y hog.")
    }
    if (length(idhog) == 0) {
      stop("No se encontraron variables de unión entre hog y sdem.")
    }
    if (length(idsdem) == 0) {
      stop("No se encontraron variables de unión entre sdem y coe.")
    }

    coe_fusionado <- datos$coe1 %>%
      dplyr::left_join(datos$coe2, by = idsdem) %>%
      limpiar_sufijos_join() %>%
      dplyr::rename_with(
        ~ paste0(.x, "coe"),
        dplyr::any_of(c("p1", "p3", "p4_1", "p4_2"))
      )

    enoe_fusionado <- datos$viv %>%
      dplyr::inner_join(datos$hog, by = idviv) %>%
      limpiar_sufijos_join() %>%
      dplyr::inner_join(datos$sdem, by = idhog) %>%
      limpiar_sufijos_join() %>%
      dplyr::filter(r_def == 0, c_res != 2) %>%
      dplyr::left_join(coe_fusionado, by = idsdem) %>%
      limpiar_sufijos_join()

  } else {
    columnas_comunes <- intersect(names(datos$coe1), names(datos$coe2))

    coe_fusionado <- datos$coe1 %>%
      dplyr::left_join(datos$coe2, by = columnas_comunes) %>%
      limpiar_sufijos_join() %>%
      dplyr::rename_with(
        ~ paste0(.x, "coe"),
        dplyr::any_of(c("p1", "p3", "p4_1", "p4_2"))
      )

    enoe_fusionado <- datos$viv %>%
      dplyr::left_join(datos$hog, by = intersect(names(datos$viv), names(datos$hog))) %>%
      limpiar_sufijos_join() %>%
      dplyr::left_join(datos$sdem, by = intersect(names(datos$hog), names(datos$sdem))) %>%
      limpiar_sufijos_join() %>%
      dplyr::filter(r_def == 0, c_res != 2) %>%
      dplyr::left_join(coe_fusionado, by = intersect(names(datos$sdem), names(coe_fusionado))) %>%
      limpiar_sufijos_join()
  }

  n_sdem <- nrow(datos$sdem[datos$sdem$r_def == 0 & datos$sdem$c_res != 2, ])
  n_fusion <- nrow(enoe_fusionado)

  message("Filas esperadas tras el filtro (sdem): ", n_sdem)
  message("Filas en la tabla fusionada final: ", n_fusion)

  if (n_fusion == 0) {
    warning("La tabla fusionada está vacía. Verificar posibles errores.")
  } else if (n_fusion > n_sdem) {
    warning("La tabla fusionada tiene MÁS filas que las esperadas después del filtro. Verificar duplicaciones.")
  } else if (n_fusion < n_sdem) {
    warning("La tabla fusionada tiene MENOS filas que las esperadas después del filtro. Posible pérdida en joins.")
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
