#' Convertir codigos CMO a SINCO (3 y 4 digitos)
#'
#' Esta funcion toma codigos CMO de 4 digitos -ya sea proporcionados directamente en `cmo_4d`
#' o a traves de `p3coe`- y devuelve su correspondencia con codigos SINCO 2011 (3 y 4 digitos),
#' usando una tabla de equivalencias incluida en el paquete. La cobertura
#' depende del nivel de desagregacion y de las capas seleccionadas.
#'
#' @param data Un data.frame que contenga la variable `cmo_4d` o `p3coe`.
#' @param codigos Opcional: data.frame de equivalencias. Si se omite, se usa una tabla interna del paquete.
#' @param var_origen Nombre de la variable que contiene el codigo CMO (por defecto `cmo_4d`).
#' @param keep_labels Logico. Si TRUE, mantiene las etiquetas si existen.
#' @param usar_reglas_enoe Compatibilidad: TRUE activa todas las capas;
#'   FALSE selecciona solo oficial. NULL utiliza `capas`.
#' @param capas Capas habilitadas: `oficial`, `panel`, `enoe` y `consenso`, todas por
#'   defecto. Oficial siempre se incluye; panel aplica reglas validadas
#'   longitudinalmente usando CMO; enoe requiere variables auxiliares ENOE.
#'   Las columnas `sinco3d` y `sinco4d` preexistentes se reemplazan de manera
#'   explicita para que la funcion pueda ejecutarse nuevamente sin crear
#'   sufijos `.x` y `.y`.
#'
#' La capa consenso actua posteriormente en
#' [procesar_clasificaciones_reproducibles()];
#' no aumenta la desagregacion SINCO identificada por esta funcion.
#'
#' @return El `data.frame` original con columnas adicionales: `cmo_4d`, `sinco4d` y `sinco3d`.
#' @export
#'
#' @examples
#' datos <- data.frame(p3coe = c(1101, 1102, 1167))
#' cmo_to_sinco(datos)
cmo_to_sinco <- function(data, codigos = NULL, var_origen = "cmo_4d",
                         keep_labels = FALSE, usar_reglas_enoe = NULL,
                         capas = c("oficial", "panel", "enoe", "consenso")) {
  # Crear cmo_4d desde p3coe si no esta presente
  if (!"cmo_4d" %in% names(data)) {
    if ("p3coe" %in% names(data)) {
      data <- data %>%
        dplyr::mutate(cmo_4d = .data[["p3coe"]])
      var_origen <- "cmo_4d"
    } else {
      stop("El data frame debe contener 'cmo_4d' o 'p3coe'.")
    }
  }

  # Cargar tabla de correspondencias si no se proporciono
  data <- data %>%
    dplyr::mutate(
      cmo_4d = sjlabelled::set_label(
        cmo_4d,
        "Codigo CMO original estandarizado a cuatro digitos"
      )
    )

  perfil_predeterminado <- is.null(codigos)
  if (perfil_predeterminado) {
    codigos <- readr::read_csv(
      system.file("extdata", "cmo_sinco_total.csv", package = "renoe"),
      show_col_types = FALSE
    )
  }

  # Filtrar y unir correspondencias
  codigos <- codigos %>%
    dplyr::select(cmo_4d, sinco4d, sinco3d) %>%
    dplyr::distinct()

  data <- data %>%
    dplyr::select(
      -dplyr::matches("^sinco(3d|4d)(\\.[xy])?$"),
      -dplyr::any_of(c(
        "regla_cmo_sinco", "tipo_regla_cmo_sinco",
        "alcance_regla_cmo_sinco", "detalle_regla_cmo_sinco",
        "n_destinos_regla_cmo_sinco"
      ))
    ) %>%
    dplyr::left_join(codigos, by = setNames("cmo_4d", var_origen))
  capas <- .normalizar_capas_cmo_sinco(capas, usar_reglas_enoe)

  data$regla_cmo_sinco <- rep(NA_character_, nrow(data))
  data$tipo_regla_cmo_sinco <- rep(NA_character_, nrow(data))
  data$alcance_regla_cmo_sinco <- rep(NA_character_, nrow(data))
  data$detalle_regla_cmo_sinco <- rep(NA_character_, nrow(data))
  data$n_destinos_regla_cmo_sinco <- rep(NA_integer_, nrow(data))
  perfil_oficial <- NULL
  posicion_oficial <- rep(NA_integer_, nrow(data))
  if (perfil_predeterminado) {
    ruta_oficial <- system.file(
      "extdata", "puente_cmo_sinco2011_oficial.csv", package = "renoe"
    )
    if (!nzchar(ruta_oficial)) {
      stop("No se encontro el puente oficial CMO-SINCO 2011.")
    }
    perfil_oficial <- utils::read.csv(
      ruta_oficial, colClasses = "character", check.names = FALSE
    )
    cmo_txt <- ifelse(
      is.na(data$cmo_4d), NA_character_,
      stringr::str_pad(as.character(data$cmo_4d), 4L, "left", "0")
    )
    posicion_oficial <- match(cmo_txt, perfil_oficial$source_code)
    # La tabla historica es un insumo analitico; no puede ganar a una
    # correspondencia oficial ni resolver por si sola sus multiples.
    destino_oficial <- suppressWarnings(as.integer(
      perfil_oficial$target_4d_unique[posicion_oficial]
    ))
    data$sinco4d <- destino_oficial
    data$sinco3d <- ifelse(
      is.na(destino_oficial), NA_integer_, destino_oficial %/% 10L
    )
    unico_4d <- !is.na(destino_oficial)
    data$regla_cmo_sinco[unico_4d] <- "CMO_OFFICIAL_UNIQUE_4D"
    data$tipo_regla_cmo_sinco[unico_4d] <- "official_unique_4d"
    data$alcance_regla_cmo_sinco[unico_4d] <- "general_clasificadores"
    data$detalle_regla_cmo_sinco[unico_4d] <-
      "Correspondencia oficial inequivoca a cuatro digitos"
    data$n_destinos_regla_cmo_sinco[unico_4d] <- 1L
  }

  # La tabla historica tambien almacena convergencias de tres digitos en
  # la columna sinco4d. No deben contarse ni consumirse como destinos exactos.
  base_code <- suppressWarnings(as.integer(as.character(data$sinco4d)))
  base_is_3d <- !is.na(base_code) & base_code >= 100L & base_code <= 999L
  fill_base_3d <- base_is_3d & is.na(data$sinco3d)
  data$sinco3d[fill_base_3d] <- base_code[fill_base_3d]
  data$sinco4d[base_is_3d] <- NA_integer_

  resolved_base <- !is.na(data$sinco4d) & !perfil_predeterminado
  data$regla_cmo_sinco[resolved_base] <- "CMO_SINCO_TABLA_BASE"
  data$tipo_regla_cmo_sinco[resolved_base] <- "base_crosswalk_4d"
  data$alcance_regla_cmo_sinco[resolved_base] <- "general_clasificadores"
  data$detalle_regla_cmo_sinco[resolved_base] <-
    "Correspondencia de cuatro digitos en la tabla base del paquete"
  data$n_destinos_regla_cmo_sinco[resolved_base] <- 1L


  derived_3d <- suppressWarnings(
    as.integer(as.character(data$sinco4d)) %/% 10L
  )
  fill_3d <- is.na(data$sinco3d) & !is.na(derived_3d)
  data$sinco3d[fill_3d] <- derived_3d[fill_3d]

  if (any(c("panel", "enoe") %in% capas)) {
    data <- .aplicar_reglas_cmo_sinco_enoe(data, capas)
  }
  if (perfil_predeterminado) {
    destino_3d <- suppressWarnings(as.integer(
      perfil_oficial$target_3d_unique[posicion_oficial]
    ))
    convergente <- is.na(data$sinco4d) & !is.na(destino_3d)
    data$sinco3d[convergente] <- destino_3d[convergente]
    data$regla_cmo_sinco[convergente] <- "CMO_OFFICIAL_CONSENSUS_3D"
    data$tipo_regla_cmo_sinco[convergente] <-
      "official_convergence_3d"
    data$alcance_regla_cmo_sinco[convergente] <-
      "general_clasificadores"
    data$detalle_regla_cmo_sinco[convergente] <-
      "Destinos oficiales multiples a 4d convergentes a 3d"
    data$n_destinos_regla_cmo_sinco[convergente] <-
      suppressWarnings(as.integer(
        perfil_oficial$n_destinations_4d[posicion_oficial[convergente]]
      ))
  }
  data <- .aplicar_convergencias_cmo_sinco_3d(data)
  return(data)
}
