#' @encoding UTF-8
#' Leer archivos de datos de la ENOE desde directorio descomprimido
#' @keywords internal
.leer_datos_enoe <- function(tabla, unzip_dir, prefijo, anio, trimestre) {
  patrones <- list(
    viv = "conjunto_de_datos_viv.*\\.csv$",
    hog = "conjunto_de_datos_hog.*\\.csv$",
    sdem = "conjunto_de_datos_sdem.*\\.csv$",
    coe1 = "conjunto_de_datos_coe1.*\\.csv$",
    coe2 = "conjunto_de_datos_coe2.*\\.csv$"
  )

  archivos <- list.files(unzip_dir, pattern = patrones[[tabla]],
                         recursive = TRUE, full.names = TRUE)

  if (length(archivos) == 0) {
    warning("No se encontró archivo para la tabla ", tabla, " en ", unzip_dir)
    return(NULL)
  }

  archivo <- archivos[1]
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
#' Función interna que determina la estructura de URL apropiada según el año y trimestre
#' @keywords internal
.construir_url_enoe <- function(anio, trimestre) {
  base_url <- "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/datosabiertos/"

  # Validaciones
  if (!is.numeric(anio) || !is.numeric(trimestre)) {
    stop("Año y trimestre deben ser numéricos")
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

#' Lista completa de variables de identificación ENOE/ENOEN
#' @keywords internal
.obtener_id_vars <- function(anio, trimestre) {
  # Variables base
  base_vars <- c("cd_a", "ent", "con", "upm", "d_sem", "n_pro_viv",
                 "v_sel", "n_hog", "h_mud", "n_ent", "per", "n_ren",
                 "r_def", "n_inf", "ur", "mun")

  # Validaciones
  if (!is.numeric(anio)) stop("El año debe ser numérico")
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

#' Estandariza variables de identificación
#' @keywords internal
.estandarizar_ids <- function(df, anio, trimestre) {
  if (is.null(df) || ncol(df) == 0) return(df)

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

  # Manejo especial para 2011 T1
  if (anio == 2011 && trimestre == 1) {
    if ("est_d" %in% names(df)) df$est_d <- as.numeric(df$est_d)
    if ("t_loc" %in% names(df)) df$t_loc <- as.numeric(df$t_loc)
  }

  return(df)
}


#' Descarga robusta de archivos ZIP
#' @keywords internal
.descargar_zip_enoe <- function(url, destfile, intentos = 3, timeout = 300) {
  # Validación inicial silenciosa
  if (missing(url) || is.null(url) || url == "") {
    return(FALSE)
  }

  for (i in 1:intentos) {
    tryCatch({
      options(timeout = timeout)
      quiet <- ifelse(i == 1, TRUE, FALSE) # Solo mostrar mensajes después del primer intento

      status <- suppressWarnings(
        if (i > 1 && grepl("2018.*2t", url)) {
          url_alt <- gsub("enoe2018", "enoe_2018", url)
          if (!quiet) message("Intento ", i, ": Probando formato alternativo")
          utils::download.file(url_alt, destfile, mode = "wb", quiet = TRUE)
        } else {
          utils::download.file(url, destfile, mode = "wb", quiet = TRUE, method = "libcurl")
        }
      )

      if (!file.exists(destfile) || file.size(destfile) < 1000) {
        if (file.exists(destfile)) unlink(destfile)
        return(FALSE)
      }
      return(TRUE)
    }, error = function(e) {
      if (file.exists(destfile)) unlink(destfile)
      if (i == intentos) {
        warning("Fallo en la descarga (último intento)", call. = FALSE)
      }
      return(FALSE)
    })
    if (i < intentos) Sys.sleep(2)
  }
  return(FALSE)
}
#' Descomprensión de archivos
#' @keywords internal
.extraer_zip_enoe <- function(zipfile, exdir) {
  if (!file.exists(zipfile)) return(FALSE)

  tryCatch({
    files <- utils::unzip(zipfile, list = TRUE)$Name
    if (length(files) == 0) return(FALSE)

    utils::unzip(zipfile, exdir = exdir)
    TRUE
  }, error = function(e) {
    tryCatch({
      dir_alt <- paste0(exdir, "_alt")
      suppressWarnings(utils::unzip(zipfile, exdir = dir_alt, list = TRUE))
      if (dir.exists(dir_alt)) {
        file.rename(dir_alt, exdir)
        TRUE
      } else {
        FALSE
      }
    }, error = function(e) {
      warning("Error crítico al extraer ZIP: ", e$message)
      FALSE
    })
  })
}

# Funciones auxiliares corregidas
.verificar_cache <- function(unzip_dir, tablas, prefijo, anio, trimestre) {
  if (!dir.exists(unzip_dir)) return(FALSE)

  patrones <- list(
    viv = "conjunto_de_datos_viv.*\\.csv$",
    hog = "conjunto_de_datos_hog.*\\.csv$",
    sdem = "conjunto_de_datos_sdem.*\\.csv$",
    coe1 = "conjunto_de_datos_coe1.*\\.csv$",
    coe2 = "conjunto_de_datos_coe2.*\\.csv$"
  )

  archivos_presentes <- sapply(tablas, function(tabla) {
    length(list.files(unzip_dir, pattern = patrones[[tabla]], recursive = TRUE)) > 0
  })

  all(archivos_presentes)
}

.cargar_desde_cache <- function(unzip_dir, tablas, prefijo, anio, trimestre) {
  lapply(tablas, function(tabla) {
    # Buscar en ambas posibles ubicaciones
    path1 <- file.path(
      unzip_dir,
      paste0("conjunto_de_datos_", tabla, "_", prefijo, "_", anio, "_", trimestre, "t.csv")
    )

    path2 <- file.path(
      unzip_dir,
      paste0("conjunto_de_datos_", prefijo, "_", anio, "_", trimestre, "t"),
      paste0("conjunto_de_datos_", tabla, "_", prefijo, "_", anio, "_", trimestre, "t.csv")
    )

    archivo <- if (file.exists(path1)) path1 else path2

    if (file.exists(archivo)) {
      tryCatch({
        readr::read_csv(archivo, show_col_types = FALSE)
      }, error = function(e) {
        warning("Error al leer ", archivo, ": ", e$message)
        NULL
      })
    } else {
      NULL
    }
  }) |> stats::setNames(tablas)
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

  # === Catálogos: etiquetas de valores ===
  cat_dir <- file.path(
    unzip_dir,
    paste0("conjunto_de_datos_", tabla, "_", prefijo, "_", anio, "_", trimestre, "t"),
    "catalogos"
  )

  if (dir.exists(cat_dir)) {
    cat_files <- list.files(cat_dir, pattern = "\\.csv$", full.names = TRUE)
    for (f in cat_files) {
      var <- tools::file_path_sans_ext(basename(f))
      if (var %in% colnames(datos) && !var %in% c("loc", "mun")) {
        try({
          cat_data <- readr::read_csv(
            f,
            skip = 1,
            col_names = c("CVE", "DESCRIP"),
            locale = readr::locale(encoding = if (anio >= 2013 & anio <= 2019) "latin1" else "UTF-8"),
            show_col_types = FALSE
          )
          # Limpieza y coerción de valores
          if (ncol(cat_data) == 2) {
            cat_data <- dplyr::filter(cat_data, !is.na(CVE))
            # Si CVE no es numérico pero los valores lo parecen, forzar
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
