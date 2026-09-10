#' Imputar ingresos ocupacionales con MICE
#'
#' Esta funcion aplica imputacion de ingresos mensuales (`ingocup`) para personas ocupadas en la ENOE
#' utilizando modelos de imputacion multiple con el paquete `mice`. Se utiliza el logaritmo del ingreso
#' como variable objetivo y se imputan los valores faltantes en funcion de variables donantes como edad,
#' escolaridad, ocupacion, horas trabajadas, entre otras.
#'
#' La imputacion se realiza primero por bloques de sexo y entidad federativa. Los casos que no pueden
#' imputarse dentro de esos bloques pasan a un modelo conjunto de respaldo, en el cual el sexo y la
#' entidad se incorporan como covariables. Si no existen las variables `folio3`, `anio` o `trim`, se
#' generan automaticamente con funciones auxiliares (`crear_folios()` y `procesar_vars_sociodemo()`).
#'
#' @encoding UTF-8
#' @param data Un data frame con personas ocupadas (`clase2 == 1`) y variables de ingreso (`ingocup`),
#'        variables donantes y metadatos de identificacion.
#' @param vars_donantes Vector con nombres de variables que se utilizaran como predictores para la imputacion.
#' @param id_vars Vector con nombres de variables identificadoras (por defecto: `folio3`, `trim`, `anio`).
#' @param method Metodo de imputacion utilizado por `mice` (por defecto: `"pmm"`).
#' @param seed Semilla aleatoria para reproducibilidad.
#' @param plot Logico. Si `TRUE`, se muestra un grafico comparando la distribucion del ingreso original vs imputado.
#' @param anio Ano del trimestre, si `data` no contiene esta variable.
#' @param trimestre Trimestre del ano (1-4), si `data` no contiene esta variable.
#'
#' @return Un data frame con las variables:
#' \describe{
#'   \item{ingocup_imp}{Ingreso mensual imputado}
#'   \item{log_ingocup_imp}{Logaritmo del ingreso imputado}
#'   \item{imp_ingocup}{Indicador binario de si el ingreso fue imputado (1 = si)}
#' }
#'
#' @details
#' La imputacion de ingresos se realiza unicamente para personas ocupadas (`clase2 == 1`)
#' con datos validos de edad, y en caso de estar disponible, tambien de anos de escolaridad (`anios_es`).
#'
#' La variable a imputar es el logaritmo natural del ingreso mensual (`log_ingocup_imp`), y la imputacion
#' se realiza utilizando el metodo especificado (por defecto `"pmm"`, predictive mean matching) a traves del paquete `mice`.
#'
#' Las imputaciones se hacen primero de forma separada por bloques definidos por el sexo (`sex`) y la
#' entidad federativa (`ent`), para capturar mejor las heterogeneidades contextuales. Cuando un bloque
#' no contiene donantes o variacion suficiente, sus casos pendientes se imputan conjuntamente usando
#' `sex`, `ent` y las demas variables donantes disponibles como predictores. Las variables identificadoras
#' nunca se usan como predictores.
#'
#' Las variables utilizadas como predictoras ("donantes") incluyen, si estan presentes:
#' - `edad`: Edad en anos.
#' - `anios_es`: Anos aprobados de escolaridad.
#' - `c_ocu11c`: 11 grandes grupos ocupacionales.
#' - `pos_ocu`: Posicion en la ocupacion.
#' - `rama_est2`: Rama de actividad.
#' - `ing7c`: Indicador de percepcion de ingresos.
#' - `ent`: Clave de entidad federativa.
#' - `hrsocup`: Horas trabajadas a la semana.
#' - `t_loc`: Tamano de localidad.
#'
#' Solo se consideran aquellas variables donantes que estan disponibles en el conjunto de datos.
#'
#' @export
#' @family procesamiento_enoe

imputa_ingocup <- function(data,
                           vars_donantes = c("edad", "anios_es", "c_ocu11c", "pos_ocu", "rama_est2",
                                             "ing7c", "ent", "hrsocup", "t_loc"),
                           id_vars = c("folio3", "trim", "anio"),
                           method = "pmm",
                           seed = 1234,
                           plot = FALSE,
                           anio = NULL,
                           trimestre = NULL) {
  requireNamespace("dplyr")
  requireNamespace("mice")
  requireNamespace("ggplot2")
  requireNamespace("sjlabelled")

  if (!"folio3" %in% names(data)) {
    message("Variable 'folio3' no encontrada. Se crea con `crear_folios()`...")
    data <- crear_folios(data)
  }

  if (!all(c("anio", "trim") %in% names(data))) {
    if (is.null(anio) | is.null(trimestre)) {
      stop("Faltan 'anio' y/o 'trim' en el data.frame y no se proporcionaron como argumentos.")
    }
    message("Variables 'anio' y/o 'trim' no encontradas. Se procesan con `procesar_vars_sociodemo()`...")
    data <- procesar_vars_sociodemo(data, anio = anio, trimestre = trimestre)
  }

  data <- data %>%
    dplyr::mutate(
      .renoe_fila_imputacion = dplyr::row_number(),
      ingocup_imp     = ingocup,
      miss_income4    = p6b1 > 6,
      ingocup_imp     = dplyr::if_else(miss_income4 & ingocup_imp == 0, NA_real_, ingocup_imp),
      ingocup_imp     = dplyr::if_else(ing7c == 6 & is.na(ingocup_imp), 0, ingocup_imp),
      ingocup_imp     = dplyr::if_else(pos_ocu == 4 & is.na(ingocup_imp), 0, ingocup_imp),
      sin_pago        = dplyr::if_else(ingocup_imp == 0, 1, 0, missing = 0),
      miss_to_impute  = is.na(ingocup_imp),
      log_ingocup_imp = log(ingocup_imp + 1)
    )

  donantes_disponibles <- intersect(vars_donantes, names(data))

  filtro_imputar <- data$clase2 == 1 & !is.na(data$edad)
  if ("anios_es" %in% names(data)) {
    filtro_imputar <- filtro_imputar & !is.na(data$anios_es)
  }

  imputar_df <- data %>%
    dplyr::filter(filtro_imputar) %>%
    dplyr::select(log_ingocup_imp, sex, ent,
                  dplyr::all_of(donantes_disponibles),
                  dplyr::all_of(id_vars), .renoe_fila_imputacion)

  periodos <- data %>%
    dplyr::distinct(anio, trim) %>%
    dplyr::mutate(periodo = paste0(anio, " ", trim)) %>%
    dplyr::pull(periodo)
  etiqueta_periodo <- if (length(periodos) == 1L) {
    paste0("[", periodos, "] ")
  } else {
    paste0("[", length(periodos), " periodos] ")
  }

  n_total_validos <- sum(filtro_imputar)
  n_total_clase2  <- sum(data$clase2 == 1, na.rm = TRUE)
  n_imputar <- sum(is.na(imputar_df$log_ingocup_imp))

  message(etiqueta_periodo, "Total ocupados con datos v\u00E1lidos: ", n_total_validos)
  message(etiqueta_periodo, "Casos a imputar (NA en log_ingocup_imp): ", n_imputar)

  imputar_vector <- function(df, predictores) {
    objetivo <- "log_ingocup_imp"
    predictores <- intersect(unique(predictores), names(df))
    predictores <- setdiff(predictores, c(objetivo, id_vars, ".renoe_fila_imputacion"))

    predictores <- predictores[vapply(
      df[predictores],
      function(x) dplyr::n_distinct(x[!is.na(x)]) > 1L,
      logical(1)
    )]

    if (sum(!is.na(df[[objetivo]])) < 2L || length(predictores) == 0L) {
      stop("No hay suficientes donantes o predictores con variaci\u00F3n.")
    }

    modelo <- as.data.frame(df[c(objetivo, predictores)])
    metodos <- rep("", ncol(modelo))
    names(metodos) <- names(modelo)
    metodos[objetivo] <- method

    matriz <- matrix(
      0,
      nrow = ncol(modelo),
      ncol = ncol(modelo),
      dimnames = list(names(modelo), names(modelo))
    )
    matriz[objetivo, predictores] <- 1

    imp <- suppressWarnings(
      mice::mice(
        modelo,
        m = 1,
        method = metodos,
        predictorMatrix = matriz,
        maxit = 10,
        seed = seed,
        printFlag = FALSE
      )
    )
    mice::complete(imp, 1)[[objetivo]]
  }

  imputados_list <- imputar_df %>%
    dplyr::group_split(sex, ent, .keep = TRUE) %>%
    lapply(function(df) {
      if (sum(is.na(df$log_ingocup_imp)) == 0) return(df)

      tryCatch({
        df$log_ingocup_imp <- imputar_vector(
          df,
          setdiff(donantes_disponibles, c("sex", "ent"))
        )
        df
      }, error = function(e) {
        df
      })
    })
  imputados_total <- dplyr::bind_rows(imputados_list)

  pendientes_respaldo <- imputados_total %>%
    dplyr::filter(is.na(log_ingocup_imp)) %>%
    dplyr::pull(.renoe_fila_imputacion)

  if (length(pendientes_respaldo) > 0L) {
    message(
      etiqueta_periodo,
      "Respaldo conjunto para ", length(pendientes_respaldo),
      " casos; `sex` se usa como covariable."
    )

    respaldo <- tryCatch(
      imputar_vector(
        imputar_df,
        unique(c("sex", "ent", donantes_disponibles))
      ),
      error = function(e) {
        message(
          etiqueta_periodo,
          "No fue posible ejecutar la imputaci\u00F3n de respaldo: ",
          conditionMessage(e)
        )
        imputar_df$log_ingocup_imp
      }
    )

    valores_respaldo <- data.frame(
      .renoe_fila_imputacion = imputar_df$.renoe_fila_imputacion,
      log_ingocup_respaldo = respaldo
    )

    imputados_total <- imputados_total %>%
      dplyr::left_join(valores_respaldo, by = ".renoe_fila_imputacion") %>%
      dplyr::mutate(
        log_ingocup_imp = dplyr::if_else(
          .renoe_fila_imputacion %in% pendientes_respaldo,
          log_ingocup_respaldo,
          log_ingocup_imp
        )
      ) %>%
      dplyr::select(-log_ingocup_respaldo)
  }

  data <- data %>%
    dplyr::left_join(
      imputados_total %>%
        dplyr::select(.renoe_fila_imputacion, log_ingocup_imp_imp = log_ingocup_imp),
      by = ".renoe_fila_imputacion"
    ) %>%
    dplyr::mutate(
      log_ingocup_imp = dplyr::if_else(
        is.na(log_ingocup_imp) & !is.na(log_ingocup_imp_imp),
        log_ingocup_imp_imp, log_ingocup_imp
      ),
      ingocup_imp = exp(log_ingocup_imp) - 1,
      imp_ingocup = dplyr::if_else(miss_to_impute & !is.na(log_ingocup_imp), 1, 0, missing = 0)
    ) %>%
    dplyr::select(-log_ingocup_imp_imp, -.renoe_fila_imputacion)

  n_imputados_final <- data %>%
    dplyr::filter(filtro_imputar) %>%
    dplyr::filter(miss_to_impute == TRUE & !is.na(log_ingocup_imp)) %>%
    nrow()

  pct_imputados <- if (n_imputar > 0) round(100 * n_imputados_final / n_imputar, 2) else 0
  pct_validos   <- round(100 * n_imputados_final / n_total_validos, 2)
  pct_clase2    <- round(100 * n_imputados_final / n_total_clase2, 2)

  message(etiqueta_periodo, "Casos efectivamente imputados: ", n_imputados_final)
  message(etiqueta_periodo, "Porcentaje entre los que ten\u00EDan NA: ", pct_imputados, "%")
  message(etiqueta_periodo, "Porcentaje sobre v\u00E1lidos (edad + anios_es): ", pct_validos, "%")
  message(etiqueta_periodo, "Porcentaje sobre total de ocupados (clase2 == 1): ", pct_clase2, "%")

  data <- data %>%
    sjlabelled::var_labels(
      miss_income4 = "Ingreso no declarado identificado mediante P6B1",
      sin_pago = "Persona ocupada sin ingreso laboral",
      miss_to_impute = "Ingreso laboral faltante seleccionado para imputaci\u00F3n"
    )

  if (plot) {
    g <- data %>%
      dplyr::filter(clase2 == 1) %>%
      ggplot2::ggplot() +
      ggplot2::geom_density(ggplot2::aes(x = ingocup), color = "blue", na.rm = TRUE) +
      ggplot2::geom_density(ggplot2::aes(x = ingocup_imp), color = "red", na.rm = TRUE) +
      ggplot2::labs(
        title = "Distribuci\u00F3n del ingreso mensual",
        x = "Ingreso mensual",
        y = "Densidad",
        caption = "Azul: original / Rojo: imputado"
      ) +
      ggplot2::theme_minimal()
    print(g)
  }

  data <- data %>%
    dplyr::mutate(
      ingocup_imp     = sjlabelled::set_label(ingocup_imp,     "Ingreso mensual imputado"),
      log_ingocup_imp = sjlabelled::set_label(log_ingocup_imp, "Logaritmo del ingreso mensual imputado"),
      imp_ingocup     = sjlabelled::set_label(imp_ingocup,     "Indicador de imputaci\u00F3n de ingreso (1 = imputado)")
    )

  return(data)
}
