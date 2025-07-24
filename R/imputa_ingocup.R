#' Imputar ingresos ocupacionales con MICE
#'
#' Esta función aplica imputación de ingresos mensuales (`ingocup`) para personas ocupadas en la ENOE
#' utilizando modelos de imputación múltiple con el paquete `mice`. Se utiliza el logaritmo del ingreso
#' como variable objetivo y se imputan los valores faltantes en función de variables donantes como edad,
#' escolaridad, ocupación, horas trabajadas, entre otras.
#'
#' La imputación se realiza por bloques según sexo y entidad federativa. Si no existen las variables `folio3`,
#' `anio` o `trim`, se generan automáticamente con funciones auxiliares (`crear_folios()` y `procesar_vars_sociodemo()`).
#'
#' @encoding UTF-8
#' @param data Un data frame con personas ocupadas (`clase2 == 1`) y variables de ingreso (`ingocup`),
#'        variables donantes y metadatos de identificación.
#' @param vars_donantes Vector con nombres de variables que se utilizarán como predictores para la imputación.
#' @param id_vars Vector con nombres de variables identificadoras (por defecto: `folio3`, `trim`, `anio`).
#' @param method Método de imputación utilizado por `mice` (por defecto: `"pmm"`).
#' @param seed Semilla aleatoria para reproducibilidad.
#' @param plot Lógico. Si `TRUE`, se muestra un gráfico comparando la distribución del ingreso original vs imputado.
#' @param anio Año del trimestre, si `data` no contiene esta variable.
#' @param trimestre Trimestre del año (1–4), si `data` no contiene esta variable.
#'
#' @return Un data frame con las variables:
#' \describe{
#'   \item{ingocup_imp}{Ingreso mensual imputado}
#'   \item{log_ingocup_imp}{Logaritmo del ingreso imputado}
#'   \item{imp_ingocup}{Indicador binario de si el ingreso fue imputado (1 = sí)}
#' }
#'
#' @details
#' La imputación de ingresos se realiza únicamente para personas ocupadas (`clase2 == 1`)
#' con datos válidos de edad, y en caso de estar disponible, también de años de escolaridad (`anios_es`).
#'
#' La variable a imputar es el logaritmo natural del ingreso mensual (`log_ingocup_imp`), y la imputación
#' se realiza utilizando el método especificado (por defecto `"pmm"`, predictive mean matching) a través del paquete `mice`.
#'
#' Las imputaciones se hacen de forma separada por bloques definidos por el sexo (`sex`) y la entidad federativa (`ent`),
#' para capturar mejor las heterogeneidades contextuales.
#'
#' Las variables utilizadas como predictoras ("donantes") incluyen, si están presentes:
#' - `edad`: Edad en años.
#' - `anios_es`: Años aprobados de escolaridad.
#' - `c_ocu11c`: 11 grandes grupos ocupacionales.
#' - `pos_ocu`: Posición en la ocupación.
#' - `rama_est2`: Rama de actividad.
#' - `ing7c`: Indicador de percepción de ingresos.
#' - `ent`: Clave de entidad federativa.
#' - `hrsocup`: Horas trabajadas a la semana.
#' - `t_loc`: Tamaño de localidad.
#'
#' Solo se consideran aquellas variables donantes que están disponibles en el conjunto de datos.
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
                  dplyr::all_of(id_vars))

  n_total_validos <- sum(filtro_imputar)
  n_total_clase2  <- sum(data$clase2 == 1, na.rm = TRUE)
  n_imputar <- sum(is.na(imputar_df$log_ingocup_imp))

  message("Total ocupados con datos válidos: ", n_total_validos)
  message("Casos a imputar (NA en log_ingocup_imp): ", n_imputar)

  imputados_list <- imputar_df %>%
    dplyr::group_split(sex, ent, .keep = TRUE) %>%
    lapply(function(df) {
      if (sum(is.na(df$log_ingocup_imp)) == 0) return(df)

      tryCatch({
        imp <- mice::mice(df, m = 1, method = method, maxit = 10, seed = seed, printFlag = FALSE)
        mice::complete(imp, 1)
      }, error = function(e) {
        message("⚠️  Error al imputar grupo sex = ", unique(df$sex),
                ", ent = ", unique(df$ent), ": ", conditionMessage(e))
        df  # Devuelve el grupo sin imputar
      })
    })
  imputados_total <- dplyr::bind_rows(imputados_list)

  data <- data %>%
    dplyr::left_join(
      imputados_total %>%
        dplyr::select(dplyr::all_of(id_vars), log_ingocup_imp_imp = log_ingocup_imp),
      by = id_vars
    ) %>%
    dplyr::mutate(
      log_ingocup_imp = dplyr::if_else(
        is.na(log_ingocup_imp) & !is.na(log_ingocup_imp_imp),
        log_ingocup_imp_imp, log_ingocup_imp
      ),
      ingocup_imp = exp(log_ingocup_imp) - 1,
      imp_ingocup = dplyr::if_else(miss_to_impute & !is.na(log_ingocup_imp), 1, 0, missing = 0)
    ) %>%
    dplyr::select(-log_ingocup_imp_imp)

  n_imputados_final <- data %>%
    dplyr::filter(filtro_imputar) %>%
    dplyr::filter(miss_to_impute == TRUE & !is.na(log_ingocup_imp)) %>%
    nrow()

  pct_imputados <- if (n_imputar > 0) round(100 * n_imputados_final / n_imputar, 2) else 0
  pct_validos   <- round(100 * n_imputados_final / n_total_validos, 2)
  pct_clase2    <- round(100 * n_imputados_final / n_total_clase2, 2)

  message("Casos efectivamente imputados: ", n_imputados_final)
  message("Porcentaje entre los que tenían NA: ", pct_imputados, "%")
  message("Porcentaje sobre válidos (edad + anios_es): ", pct_validos, "%")
  message("Porcentaje sobre total de ocupados (clase2 == 1): ", pct_clase2, "%")

  if (plot) {
    g <- data %>%
      dplyr::filter(clase2 == 1) %>%
      ggplot2::ggplot() +
      ggplot2::geom_density(ggplot2::aes(x = ingocup), color = "blue", na.rm = TRUE) +
      ggplot2::geom_density(ggplot2::aes(x = ingocup_imp), color = "red", na.rm = TRUE) +
      ggplot2::labs(
        title = "Distribución del ingreso mensual",
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
      imp_ingocup     = sjlabelled::set_label(imp_ingocup,     "Indicador de imputación de ingreso (1 = imputado)")
    )

  return(data)
}
