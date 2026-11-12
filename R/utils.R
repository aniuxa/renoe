#' Leer archivos de datos de la ENOE desde directorio descomprimido
#'
#' @keywords internal
#' @encoding UTF-8
.leer_datos_enoe <- function(tabla, unzip_dir, prefijo, anio, trimestre) {
  patrones <- list(
    viv  = "conjunto_de_datos_viv.*\\.csv$",
    hog  = "conjunto_de_datos_hog.*\\.csv$",
    sdem = "conjunto_de_datos_sdem.*\\.csv$",
    coe1 = "conjunto_de_datos_coe1.*\\.csv$",
    coe2 = "conjunto_de_datos_coe2.*\\.csv$"
  )

  # 2020-T1 debe leerse desde la edicion vigente de microdatos. Los archivos
  # de datos abiertos del mismo trimestre dejan P3 vacio en registros que si
  # estan codificados en la publicacion de microdatos.
  if (anio == 2020 && trimestre == 1) {
    nombres_microdatos <- c(
      viv = "ENOE_VIVT120.csv", hog = "ENOE_HOGT120.csv",
      sdem = "ENOE_SDEMT120.csv", coe1 = "ENOE_COE1T120.csv",
      coe2 = "ENOE_COE2T120.csv"
    )
    archivo <- file.path(unzip_dir, nombres_microdatos[[tabla]])
    if (!file.exists(archivo)) {
      warning("No se encontro el archivo oficial vigente para ", tabla, " en ", archivo)
      return(NULL)
    }
  }

  # El ZIP oficial de 2022-T1 contiene dos componentes complementarios de
  # HOG. El archivo raiz conserva el ambito urbano con meses 1-3; el archivo
  # incluido en `conjunto_de_datos/` aporta el ambito rural con meses
  # desplazados 10-12 y 99. Se integran explicitamente antes de fusionar.
  else if (anio == 2022 && trimestre == 1) {
    nombre_archivo <- paste0("conjunto_de_datos_", tabla, "_enoen_2022_1t.csv")
    carpeta <- file.path(
      unzip_dir, paste0("conjunto_de_datos_", tabla, "_enoen_2022_1t")
    )
    ruta <- file.path(carpeta, nombre_archivo)
    if (identical(tabla, "hog")) {
      ruta_rural <- file.path(carpeta, "conjunto_de_datos", nombre_archivo)
      if (!all(file.exists(c(ruta, ruta_rural)))) {
        warning("No se encontraron ambos componentes oficiales de HOG 2022-T1.")
        return(NULL)
      }
      leer_hog <- function(path) {
        x <- suppressWarnings(readr::read_csv(
          path, locale = readr::locale(encoding = "UTF-8"),
          show_col_types = FALSE, progress = FALSE
        ))
        names(x) <- tolower(names(x))
        x
      }
      urbano <- leer_hog(ruta)
      rural <- leer_hog(ruta_rural)
      rural <- rural[rural$ur == 2, , drop = FALSE]
      mapa_mes <- c("10" = 1, "11" = 2, "12" = 3, "99" = 96)
      rural$mes_cal <- unname(mapa_mes[as.character(rural$mes_cal)])
      if (anyNA(rural$mes_cal)) {
        warning("HOG rural 2022-T1 contiene meses fuera del mapa oficial.")
        return(NULL)
      }
      return(dplyr::bind_rows(urbano, rural))
    }
    if (!file.exists(ruta)) {
      warning("No se encontr\u00F3 el archivo oficial requerido para ", tabla,
              " en ", ruta, ".")
      ruta <- NULL
    }
    archivo <- ruta
  } else {
    archivos <- list.files(unzip_dir, pattern = patrones[[tabla]],
                           recursive = TRUE, full.names = TRUE)
    if (length(archivos) == 0) {
      warning("No se encontr\u00F3 archivo para la tabla ", tabla, " en ", unzip_dir)
      return(NULL)
    }
    archivo <- archivos[1]
  }

  # Abortamos si no hay archivo valido
  if (is.null(archivo) || !file.exists(archivo)) {
    warning("Archivo no disponible para tabla ", tabla, " en ", unzip_dir)
    return(NULL)
  }

  # Leer con el encoding correcto
  encoding <- if (anio >= 2020) "UTF-8" else "latin1"

  tryCatch({
    datos <- suppressWarnings(
      readr::read_csv(
        file = archivo,
        locale = readr::locale(encoding = encoding),
        show_col_types = FALSE,
        progress = FALSE
      )
    )
    names(datos) <- tolower(names(datos))
    return(datos)
  }, error = function(e) {
    warning("Error al leer ", archivo, ": ", e$message)
    return(NULL)
  })
}


#' Funcion interna que determina la estructura de URL apropiada segun el año y trimestre
#' @keywords internal
.construir_url_enoe <- function(anio, trimestre) {
  base_url <- "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/datosabiertos/"

  .validar_periodo_enoe(anio, trimestre)

  # Para 2020-T1 se usa una sola edicion de las cinco tablas de microdatos.
  if (anio == 2020 && trimestre == 1) {
    return(list(
      url = "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/2020trim1_csv.zip",
      zip_file = "zip/2020trim1_csv.zip",
      prefijo = "enoe"
    ))
  }

  # Caso especial para 2017 y 2018 T1
  if ((anio == 2017) || (anio == 2018 && trimestre == 1)) {
    return(list(
      url = paste0(base_url, anio, "/", anio, "_trim", trimestre, "_enoe_csv.zip"),
      zip_file = paste0("zip/", anio, "_trim", trimestre, "_enoe.zip"),
      prefijo = "enoe"
    ))
  }
  # Caso especial para 2018 T2
  else if (anio == 2018 && trimestre == 2) {
    return(list(
      url = paste0(base_url, anio, "/conjunto_de_datos_enoe", anio, "_", trimestre, "t_csv.zip"),
      zip_file = paste0("zip/conjunto_de_datos_enoe", anio, "_", trimestre, "t.zip"),
      prefijo = "enoe"
    ))
  }
  # Casos ENOEN (2020 T3 - 2022 T4)
  else if ((anio == 2020 && trimestre >= 3) || (anio >= 2021 && anio <= 2022)) {
    return(list(
      url = paste0(base_url, anio, "/conjunto_de_datos_enoen_", anio, "_", trimestre, "t_csv.zip"),
      zip_file = paste0("zip/conjunto_de_datos_enoen_", anio, "_", trimestre, "t.zip"),
      prefijo = "enoen"
    ))
  }
  # Estructura normal
  else {
    return(list(
      url = paste0(base_url, anio, "/conjunto_de_datos_enoe_", anio, "_", trimestre, "t_csv.zip"),
      zip_file = paste0("zip/conjunto_de_datos_enoe_", anio, "_", trimestre, "t.zip"),
      prefijo = "enoe"
    ))
  }
}

#' Lista completa de variables de identificacion ENOE/ENOEN
#' @keywords internal
.obtener_id_vars <- function(anio, trimestre) {
  # Variables base
  base_vars <- c("cd_a", "ent", "con", "upm", "d_sem", "n_pro_viv",
                 "v_sel", "n_hog", "h_mud", "n_ent", "per", "n_ren",
                 "r_def", "n_inf", "ur", "mun")

  # Validaciones
  if (!is.numeric(anio)) stop("El a\u00F1o debe ser num\u00E9rico")
  if (!trimestre %in% 1:4) stop("El trimestre debe ser entre 1 y 4")

  # Caso ENOEN (2020T3-2022T4)
  if ((anio == 2020 && trimestre >= 3) || (anio >= 2021 && anio <= 2022)) {
    return(c(base_vars, "mes_cal", "tipo", "ca",
             "est_d_tri", "est_d_men", "fac_tri", "fac_men",
             "t_loc_tri", "t_loc_men"))
  }

  # Caso ENOE reciente (2023+)
  if (anio >= 2023) {
    return(c(base_vars, "mes_cal", "tipo", "est_d_tri", "fac_tri",
             "t_loc_tri", "t_loc_men"))
  }

  # Caso ENOE tradicional (2005-2020T2)
  return(c(base_vars, "fac", "t_loc", "est_d"))
}

#' Estandariza variables de identificacion
#' @keywords internal
.estandarizar_ids <- function(df, anio, trimestre) {
  if (is.null(df) || ncol(df) == 0) return(df)

  # A partir de 2025-T3, INEGI sustituyo varios nombres geograficos por cve_*.
  # Se recuperan los nombres historicos antes de convertir las llaves para que
  # el resto del paquete y los paneles reciban un esquema estable.
  if (anio > 2025 || (anio == 2025 && trimestre >= 3)) {
    equivalencias <- c(
      cve_ent = "ent", cve_mun = "mun", cve_loc = "loc", cve_ageb = "ageb"
    )
    for (nuevo in names(equivalencias)) {
      anterior <- unname(equivalencias[[nuevo]])
      if (nuevo %in% names(df) && !anterior %in% names(df)) {
        names(df)[names(df) == nuevo] <- anterior
      } else if (nuevo %in% names(df) && anterior %in% names(df)) {
        previo <- trimws(as.character(df[[anterior]]))
        reemplazo <- as.character(df[[nuevo]])
        usar_nuevo <- is.na(previo) | previo == ""
        previo[usar_nuevo] <- reemplazo[usar_nuevo]
        df[[anterior]] <- previo
        df[[nuevo]] <- NULL
      }
    }
  }

  id_vars <- .obtener_id_vars(anio, trimestre)
  vars_a_convertir <- intersect(names(df), id_vars)

  convertir_a_numerico <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    ifelse(grepl("^[-+]?\\d+\\.?\\d*$", x), as.numeric(x), NA_real_)
  }

  if (length(vars_a_convertir) > 0) {
    df <- suppressWarnings(
      dplyr::mutate(
        df,
        dplyr::across(
          .cols = dplyr::all_of(vars_a_convertir),
          .fns = convertir_a_numerico
        )
      )
    )
  }

  if (anio == 2011 && trimestre == 1) {
    if ("est_d" %in% names(df)) df$est_d <- as.numeric(df$est_d)
    if ("t_loc" %in% names(df)) df$t_loc <- as.numeric(df$t_loc)
  }

  return(df)
}


.zip_enoe_valido <- function(archivo) {
  if (!file.exists(archivo) || is.na(file.size(archivo)) || file.size(archivo) == 0) return(FALSE)
  tryCatch({
    contenido <- suppressWarnings(utils::unzip(archivo, list = TRUE))
    is.data.frame(contenido) && nrow(contenido) > 0L && any(!grepl('/$', contenido$Name))
  }, error = function(e) FALSE)
}

#' Descarga robusta de archivos ZIP
#' @keywords internal
.descargar_zip_enoe <- function(url, destfile, intentos = 3, timeout = 300,
                               verificar_url = TRUE) {
  if (missing(url) || is.null(url) || length(url) != 1L || is.na(url) || !nzchar(url)) return(FALSE)
  .validar_opciones_descarga(intentos, timeout, verificar_url, TRUE)
  previo <- getOption('timeout')
  on.exit(options(timeout = previo), add = TRUE)
  options(timeout = timeout)
  alterna <- if (verificar_url && grepl('enoe2018_2t', url, fixed = TRUE)) {
    sub('enoe2018_2t', 'enoe_2018_2t', url, fixed = TRUE)
  } else NULL
  for (i in seq_len(intentos)) {
    temporal <- tempfile(pattern = '.descarga_enoe_', tmpdir = dirname(destfile), fileext = '.zip')
    direccion <- if (!is.null(alterna) && i > 1L) alterna else url
    status <- tryCatch(suppressWarnings(utils::download.file(
      direccion, temporal, mode = 'wb', quiet = TRUE, method = 'libcurl'
    )), error = function(e) 1L)
    valido <- identical(as.integer(status), 0L) && .zip_enoe_valido(temporal)
    if (valido && isTRUE(file.copy(temporal, destfile, overwrite = TRUE))) {
      unlink(temporal)
      return(TRUE)
    }
    if (file.exists(temporal)) unlink(temporal)
    if (i < intentos) Sys.sleep(2)
  }
  warning('Fallo en la descarga o ZIP invalido (ultimo intento)', call. = FALSE)
  FALSE
}

.cache_archivos_enoe_valida <- function(archivos, formato) {
  if (!all(file.exists(archivos)) || anyNA(file.size(archivos)) || any(file.size(archivos) <= 0)) return(FALSE)
  all(vapply(archivos, function(archivo) {
    tryCatch({
      datos <- switch(formato,
        parquet = arrow::read_parquet(archivo, as_data_frame = FALSE),
        rds = readRDS(archivo), dta = haven::read_dta(archivo)
      )
      !is.null(datos) && length(dim(datos)) == 2L && nrow(datos) > 0L && ncol(datos) > 0L
    }, error = function(e) FALSE)
  }, logical(1)))
}

#' Descomprension de archivos
#' @keywords internal
.extraer_zip_enoe <- function(zipfile, exdir) {
  if (!.zip_enoe_valido(zipfile)) return(FALSE)
  tryCatch({
    contenido <- utils::unzip(zipfile, list = TRUE)$Name
    utils::unzip(zipfile, exdir = exdir)
    all(file.exists(file.path(exdir, contenido)))
  }, error = function(e) {
    warning('Error al extraer ZIP: ', e$message, call. = FALSE)
    FALSE
  })
}

.verificar_cache <- function(unzip_dir, tablas, prefijo, anio, trimestre) {
  if (!dir.exists(unzip_dir)) return(FALSE)
  tiene_contenido <- function(archivo) {
    tryCatch({
      file.exists(archivo) && !is.na(file.size(archivo)) && file.size(archivo) > 0 &&
        length(readLines(archivo, n = 2L, warn = FALSE, encoding = 'UTF-8')) >= 2L
    }, error = function(e) FALSE)
  }
  if (anio == 2020 && trimestre == 1) {
    nombres <- c(viv='ENOE_VIVT120.csv', hog='ENOE_HOGT120.csv',
                 sdem='ENOE_SDEMT120.csv', coe1='ENOE_COE1T120.csv', coe2='ENOE_COE2T120.csv')
    return(all(vapply(file.path(unzip_dir, nombres[tablas]), tiene_contenido, logical(1))))
  }
  patrones <- c(viv='conjunto_de_datos_viv.*\\.csv$', hog='conjunto_de_datos_hog.*\\.csv$',
                sdem='conjunto_de_datos_sdem.*\\.csv$', coe1='conjunto_de_datos_coe1.*\\.csv$',
                coe2='conjunto_de_datos_coe2.*\\.csv$')
  all(vapply(tablas, function(tabla) {
    archivos <- list.files(unzip_dir, pattern = patrones[[tabla]], recursive = TRUE, full.names = TRUE)
    length(archivos) > 0L && any(vapply(archivos, tiene_contenido, logical(1)))
  }, logical(1)))
}

# Extrae en una carpeta nueva y solo sustituye la cache si estan las cinco
# tablas. Los temporales y respaldos siempre permanecen dentro de zip/.
.preparar_extraccion_enoe <- function(zipfile, destino, tablas, prefijo, anio, trimestre) {
  raiz <- normalizePath(dirname(destino), winslash = '/', mustWork = TRUE)
  destino <- file.path(raiz, basename(destino))
  temporal <- tempfile('.renoe_extraccion_', tmpdir = raiz)
  respaldo <- tempfile('.renoe_respaldo_', tmpdir = raiz)
  limpiar <- function(ruta) {
    if (!dir.exists(ruta)) return(invisible(NULL))
    real <- normalizePath(ruta, winslash = '/', mustWork = TRUE)
    if (tolower(dirname(real)) != tolower(raiz) || !startsWith(basename(real), '.renoe_')) {
      stop('Ruta temporal fuera del directorio de cache: ', real, call. = FALSE)
    }
    unlink(real, recursive = TRUE)
    invisible(NULL)
  }
  dir.create(temporal)
  on.exit(limpiar(temporal), add = TRUE)
  if (!.extraer_zip_enoe(zipfile, temporal) ||
      !.verificar_cache(temporal, tablas, prefijo, anio, trimestre)) {
    stop('El ZIP no contiene las cinco tablas ENOE legibles y no vacias.', call. = FALSE)
  }
  tenia_cache <- dir.exists(destino)
  if (file.exists(destino) && !tenia_cache) stop('La ruta de cache no es una carpeta.', call. = FALSE)
  if (tenia_cache && !file.rename(destino, respaldo)) {
    stop('No se pudo resguardar la cache anterior.', call. = FALSE)
  }
  if (!file.rename(temporal, destino)) {
    if (tenia_cache && !file.rename(respaldo, destino)) {
      stop('No se pudo restaurar la cache anterior; respaldo en ', respaldo, call. = FALSE)
    }
    stop('No se pudo activar la nueva cache.', call. = FALSE)
  }
  if (tenia_cache) limpiar(respaldo)
  invisible(TRUE)
}

#' Procesamiento de etiquetas y metadatos
#' @keywords internal
.procesar_etiquetas_enoe <- function(datos, tabla, anio, trimestre, unzip_dir, prefijo) {
  if (is.null(datos) || ncol(datos) == 0) return(datos)

  limpiar_etiqueta <- function(x) stringr::str_remove(x, "^Pregunta\\s*\\d+\\s*")

  # === Diccionario: etiquetas de variables ===
  dic_path <- file.path(
    unzip_dir,
    paste0("conjunto_de_datos_", tabla, "_", prefijo, "_", anio, "_", trimestre, "t"),
    "diccionario_de_datos",
    paste0("diccionario_datos_", tabla, "_", prefijo, "_", anio, "_", trimestre, "t.csv")
  )

  if (file.exists(dic_path)) {
    dic <- readr::read_csv(
      dic_path,
      skip = 1,
      col_names = c("NOMBRE_CAMPO", "LONGITUD", "TIPO", "NEMONICO", "CATALOGO", "RANGO_CLAVES"),
      locale = readr::locale(encoding = if (anio >= 2013 & anio <= 2019) "latin1" else "UTF-8"),
      show_col_types = FALSE
    )

    for (i in seq_len(nrow(dic))) {
      var <- dic$NEMONICO[i]
      if (!is.na(var) && var %in% colnames(datos)) {
        datos[[var]] <- sjlabelled::set_label(datos[[var]], limpiar_etiqueta(dic$NOMBRE_CAMPO[i]))
      }
    }
  }

  # === Catalogos: etiquetas de valores ===
  cat_dir <- file.path(
    unzip_dir,
    paste0("conjunto_de_datos_", tabla, "_", prefijo, "_", anio, "_", trimestre, "t"),
    "catalogos"
  )

  if (dir.exists(cat_dir)) {
    cat_files <- list.files(cat_dir, pattern = "\\.csv$", full.names = TRUE)
    for (f in cat_files) {
      var <- tools::file_path_sans_ext(basename(f))
      if (var %in% colnames(datos) && !var %in% c("loc", "mun", "par_c")) {


        try({
          cat_data <- readr::read_csv(
            f,
            skip = 1,
            col_names = c("CVE", "DESCRIP"),
            locale = readr::locale(encoding = if (anio >= 2013 & anio <= 2019) "latin1" else "UTF-8"),
            show_col_types = FALSE
          )
          # Limpieza y coercion de valores
          if (ncol(cat_data) == 2) {
            cat_data <- dplyr::filter(cat_data, !is.na(CVE))
            if (!is.numeric(cat_data$CVE) && all(grepl("^[0-9]+$", cat_data$CVE))) {
              cat_data$CVE <- as.numeric(cat_data$CVE)
            }
            datos[[var]] <- sjlabelled::set_labels(
              datos[[var]],
              labels = stats::setNames(as.character(cat_data$DESCRIP), cat_data$CVE)
            )
          }
        }, silent = TRUE)
      }
    }
  }

  return(datos)
}
