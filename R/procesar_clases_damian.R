#' Clasificar ocupaciones para el capitulo de Gerardo Damian Hernandez
#'
#' Traduce el codigo ocupacional armonizado a SINCO 2011 hacia ISCO-88 y
#' construye la tipologia de cuatro clases utilizada en la propuesta
#' *Veinte anos de trabajo femenino en Mexico*: trabajo no manual calificado,
#' no manual no calificado, manual calificado y manual no calificado.
#'
#' La correspondencia SINCO 2011-ISCO-88 reproduce, en orden de prioridad, el
#' do-file de Gerardo Damian Hernandez `sinco-isco88.do`. Las variables
#' auxiliares permiten auditar la cobertura y distinguir asignaciones directas
#' de recuperaciones realizadas con el gran grupo SINCO.
#'
#' @param data Un data frame con `sinco4d_base2011` o `sinco4d`,
#'   `pos_ocu` y `emple7c`. Cuando existe `sinco4d_base2011`, esta
#'   variable canonica tiene prioridad y no se aplica un puente alternativo.
#' @param correspondencia Tabla opcional con columnas `sinco4d` e `isco88`.
#'   Si se omite, se usa la correspondencia distribuida con el paquete.
#' @param recuperar_sin_isco Si es `TRUE`, clasifica casos sin equivalencia
#'   ISCO-88 mediante el gran grupo SINCO, siguiendo el do-file original.
#' @param usar_puente_cmo Si es `TRUE`, aplica antes de 2012-III el puente
#'   determinista de Damian usado por `cmo_to_sinco11_care()`. Este puente
#'   conserva la primera regla del do-file y no constituye una equivalencia
#'   oficial o biunivoca. Por ello el valor por defecto es `FALSE`; su uso debe
#'   ser explicito y corresponde al escenario historico.
#' @param escenario Contrato publico de decision. `official_strict` excluye
#'   rescates condicionados; `integrated_accepted` agrega reglas EGP
#'   aceptadas; `analysis_legacy` agrega el puente CMO historico.
#' @param legacy Compatibilidad explicita para seleccionar `analysis_legacy`.
#'
#' @return El mismo data frame con `isco88_damian`, `grupo_ocu9_damian`,
#'   `clase_ocu_damian`, `calificada_damian`, `manual_damian`,
#'   `supervisa_damian`, `clase_egp13_damian`, sus agrupaciones y variables de
#'   cobertura y metodo.
#' @export
#' @encoding UTF-8
#'
#' @references
#' Damian Hernandez, G. (2026). *Quince anos de trabajo femenino en Mexico:
#' tipo de hogar y clase ocupacional, 2005-2020*. Propuesta de capitulo para
#' el proyecto PAPIIT IN305925.
#'
#' Solis, P., Chavez Molina, E. y Cobos, D. (2019). Propuesta de adaptacion del
#' esquema EGP para America Latina, citada en el programa original.
#'
#' @examples
#' datos <- data.frame(
#'   sinco4d = c(2211, 4111, 7111, 9111, NA),
#'   pos_ocu = c(1, 1, 1, 1, NA),
#'   emple7c = c(5, 5, 5, 5, NA),
#'   clase2 = c(1, 1, 1, 1, 4)
#' )
#' procesar_clases_damian(datos)
#' @family procesamiento_enoe
procesar_clases_damian <- function(
    data,
    correspondencia = NULL,
    recuperar_sin_isco = TRUE,
    usar_puente_cmo = NULL,
    escenario = c("integrated_accepted", "official_strict", "analysis_legacy"),
    legacy = NULL) {

  contrato <- .normalizar_escenario_clasificador(escenario, legacy)
  if (is.null(usar_puente_cmo)) {
    usar_puente_cmo <- contrato$escenario == "analysis_legacy"
  } else if (isTRUE(usar_puente_cmo) && !contrato$legacy) {
    contrato$escenario <- "analysis_legacy"
    contrato$legacy <- TRUE
  }

  variable_sinco_damian <- if ("sinco4d_base2011" %in% names(data)) {
    "sinco4d_base2011"
  } else {
    "sinco4d"
  }
  faltantes_data <- setdiff(
    c(variable_sinco_damian, "pos_ocu", "emple7c"),
    names(data)
  )
  if (length(faltantes_data) > 0L) {
    stop(
      "Faltan variables requeridas: ",
      paste(faltantes_data, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(correspondencia)) {
    ruta <- system.file(
      "extdata", "correspondencia_sinco2011_isco88_damian.csv",
      package = "renoe"
    )
    if (!nzchar(ruta)) {
      stop(
        "No se encontro la correspondencia SINCO 2011-ISCO-88 del paquete.",
        call. = FALSE
      )
    }
    correspondencia <- utils::read.csv(
      ruta, stringsAsFactors = FALSE, na.strings = c("", "NA")
    )
  }

  requeridas <- c("sinco4d", "isco88")
  faltantes <- setdiff(requeridas, names(correspondencia))
  if (length(faltantes) > 0L) {
    stop(
      "Faltan columnas en `correspondencia`: ",
      paste(faltantes, collapse = ", "),
      call. = FALSE
    )
  }

  cruce <- correspondencia |>
    dplyr::transmute(
      sinco4d = suppressWarnings(as.integer(as.character(sinco4d))),
      isco88_damian = suppressWarnings(as.integer(as.character(isco88)))
    ) |>
    dplyr::filter(!is.na(sinco4d)) |>
    dplyr::distinct(sinco4d, .keep_all = TRUE)

  supervisores <- c(
    2311L, 2630L, 2640L, 3101L, 3201L, 4201L, 5101L, 5201L,
    5301L, 5401L, 6101L, 6201L, 7101L, 7201L, 7301L, 7401L,
    7501L, 7601L, 8101L, 8201L, 8301L, 9601L
  )

  sinco_damian <- suppressWarnings(as.integer(
    as.character(data[[variable_sinco_damian]])
  ))
  sinco_damian[!is.na(sinco_damian) & sinco_damian == 9999L] <- NA_integer_
  calidad_cmo <- rep(NA_character_, nrow(data))
  n_destinos_cmo <- rep(NA_integer_, nrow(data))

  puede_aplicar_cmo <- usar_puente_cmo &&
    all(c("p3coe", "anio", "trim") %in% names(data))

  if (puede_aplicar_cmo) {
    trimestre_n <- suppressWarnings(as.integer(sub(
      "^t", "", tolower(as.character(data$trim))
    )))
    periodo_cmo <- data$anio < 2012L |
      (data$anio == 2012L & trimestre_n <= 2L)

    remanente_cmo <- which(periodo_cmo & is.na(sinco_damian))
    if (length(remanente_cmo)) {
      puente <- renoe::cmo_to_sinco11_care(
        data.frame(p3coe = data$p3coe[remanente_cmo]),
        variable_cmo = "p3coe",
        sobrescribir = TRUE
      )
      destino <- suppressWarnings(as.integer(puente$sinco11))
      destino[is.na(destino) | destino < 1000L |
                destino >= 9999L] <- NA_integer_
      sinco_damian[remanente_cmo] <- destino
      calidad_cmo[remanente_cmo] <- puente$sinco11_calidad
      n_destinos_cmo[remanente_cmo] <- puente$sinco11_n_destinos
    }
  }

  data$sinco4d_damian <- sinco_damian
  data$fuente_sinco_damian <- if (variable_sinco_damian == "sinco4d_base2011") {
    rep("SINCO 2011 canonico de armonizar_sinco", nrow(data))
  } else {
    rep("sinco4d proporcionado al modulo", nrow(data))
  }
  rescate_cmo <- !is.na(calidad_cmo) & !is.na(sinco_damian)
  data$fuente_sinco_damian[rescate_cmo] <-
    "Puente analitico CMO de Damian en remanente; no equivalencia oficial"
  data$calidad_cmo_damian <- calidad_cmo
  data$n_destinos_cmo_damian <- n_destinos_cmo

  data <- data |>
    dplyr::left_join(
      dplyr::rename(cruce, sinco4d_damian = sinco4d),
      by = "sinco4d_damian"
    ) |>
    dplyr::mutate(
      grupo_ocu9_damian = dplyr::case_when(
        dplyr::between(sinco4d_damian, 1111L, 1999L) ~ 1L,
        dplyr::between(sinco4d_damian, 2111L, 2992L) ~ 2L,
        dplyr::between(sinco4d_damian, 3101L, 3999L) ~ 3L,
        dplyr::between(sinco4d_damian, 4111L, 4999L) ~ 4L,
        dplyr::between(sinco4d_damian, 5101L, 5999L) ~ 5L,
        dplyr::between(sinco4d_damian, 6101L, 6999L) ~ 6L,
        dplyr::between(sinco4d_damian, 7101L, 7999L) ~ 7L,
        dplyr::between(sinco4d_damian, 8101L, 8999L) ~ 8L,
        dplyr::between(sinco4d_damian, 9111L, 9998L) ~ 9L,
        TRUE ~ NA_integer_
      ),
      clase_isco = dplyr::case_when(
        dplyr::between(isco88_damian, 1100L, 3241L) ~ 1L,
        dplyr::between(isco88_damian, 3450L, 3460L) ~ 1L,
        dplyr::between(isco88_damian, 3242L, 3449L) ~ 2L,
        dplyr::between(isco88_damian, 3470L, 5999L) ~ 2L,
        dplyr::between(isco88_damian, 7111L, 8340L) ~ 3L,
        dplyr::between(isco88_damian, 6111L, 6299L) ~ 4L,
        dplyr::between(isco88_damian, 9111L, 9333L) ~ 4L,
        TRUE ~ NA_integer_
      ),
      clase_recuperada = dplyr::case_when(
        !recuperar_sin_isco | !is.na(clase_isco) ~ NA_integer_,
        grupo_ocu9_damian == 1L ~ 1L,
        # El do-file asignaba el grupo 2 a manual calificado. Se corrige como
        # no manual calificado: profesionistas y tecnicos no son manuales.
        grupo_ocu9_damian == 2L ~ 1L,
        grupo_ocu9_damian %in% 3:5 ~ 2L,
        grupo_ocu9_damian == 6L ~ 4L,
        grupo_ocu9_damian %in% 7:9 ~ 3L,
        TRUE ~ NA_integer_
      ),
      clase_ocu_damian = dplyr::coalesce(clase_isco, clase_recuperada),
      metodo_clase_damian = dplyr::case_when(
        !is.na(clase_isco) ~ "Correspondencia SINCO-ISCO88",
        !is.na(clase_recuperada) ~ "Recuperacion por gran grupo SINCO",
        is.na(sinco4d_damian) ~ "Sin codigo SINCO",
        TRUE ~ "Sin clasificar"
      ),
      cobertura_isco88_damian = dplyr::case_when(
        is.na(sinco4d_damian) ~ NA_integer_,
        !is.na(isco88_damian) ~ 1L,
        TRUE ~ 0L
      ),
      cobertura_clase_damian = dplyr::case_when(
        is.na(sinco4d_damian) ~ NA_integer_,
        !is.na(clase_ocu_damian) ~ 1L,
        TRUE ~ 0L
      ),
      calificada_damian = dplyr::case_when(
        clase_ocu_damian %in% c(1L, 3L) ~ 1L,
        clase_ocu_damian %in% c(2L, 4L) ~ 0L,
        TRUE ~ NA_integer_
      ),
      manual_damian = dplyr::case_when(
        clase_ocu_damian %in% c(3L, 4L) ~ 1L,
        clase_ocu_damian %in% c(1L, 2L) ~ 0L,
        TRUE ~ NA_integer_
      ),
      supervisa_damian = dplyr::case_when(
        is.na(sinco4d_damian) ~ NA_integer_,
        sinco4d_damian %in% supervisores ~ 1L,
        TRUE ~ 0L
      )
    ) |>
    dplyr::select(-clase_isco, -clase_recuperada)

  # Posicion y tamano equivalentes a los insumos armonizados de IPUMS usados
  # en el do-file. `emple7c` es una variable precalculada del SDEM y mantiene
  # estas siete categorias durante toda la serie ENOE.
  data <- data |>
    dplyr::mutate(
      posocup_damian = dplyr::case_when(
        pos_ocu == 0L ~ 1L,
        pos_ocu == 1L ~ 4L,
        pos_ocu == 2L ~ 2L,
        pos_ocu == 3L ~ 3L,
        pos_ocu == 4L ~ 4L,
        TRUE ~ NA_integer_
      ),
      tam_est_damian = dplyr::case_when(
        emple7c == 1L ~ 1L,
        emple7c == 2L ~ 2L,
        emple7c %in% 3:4 ~ 3L,
        emple7c %in% 5:6 ~ 4L,
        emple7c == 7L ~ 9L,
        TRUE ~ NA_integer_
      )
    )

  isco <- data$isco88_damian
  sinco <- data$sinco4d_damian
  pos <- data$posocup_damian
  tam <- data$tam_est_damian
  elegible <- if ("clase2" %in% names(data)) {
    suppressWarnings(as.integer(as.character(data$clase2))) == 1L
  } else {
    !is.na(sinco)
  }
  egp <- rep(NA_integer_, nrow(data))

  asignar <- function(condicion, valor) {
    idx <- is.na(egp) & elegible & !is.na(condicion) & condicion
    egp[idx] <<- as.integer(valor)
  }

  # Reglas secuenciales del do-file: la primera regla aplicable tiene
  # prioridad, como los `replace ... if clase == -1` de Stata.
  asignar(dplyr::between(isco, 1100L, 1210L), 1L)
  asignar(dplyr::between(isco, 1211L, 1319L) & tam == 4L, 1L)
  asignar(isco == 1237L & tam == 9L, 1L)
  asignar(dplyr::between(isco, 2111L, 2310L), 1L)
  asignar(isco == 2351L, 1L)
  asignar(dplyr::between(isco, 2411L, 2429L), 1L)
  asignar(dplyr::between(isco, 2432L, 2446L), 1L)
  asignar(pos == 2L & tam == 4L, 1L)
  asignar(isco == 3460L, 3L)

  asignar(dplyr::between(isco, 1211L, 1319L) & tam < 4L, 2L)
  asignar(dplyr::between(isco, 2111L, 2460L), 2L)
  asignar(dplyr::between(isco, 3111L, 3123L), 2L)
  asignar(dplyr::between(isco, 3132L, 3212L), 2L)
  asignar(dplyr::between(isco, 3221L, 3232L), 2L)
  asignar(dplyr::between(isco, 3310L, 3340L), 2L)
  asignar(sinco %in% c(2122L, 2311L, 2512L, 2522L, 2640L,
                       3101L, 3201L, 4201L, 5401L), 2L)

  asignar(dplyr::between(isco, 4111L, 4130L), 3L)
  asignar(dplyr::between(isco, 4132L, 4223L), 3L)
  asignar(dplyr::between(isco, 3411L, 3449L), 3L)
  asignar(isco %in% c(3242L, 5111L), 3L)

  asignar(isco %in% c(5220L, 5113L, 5122L, 5141L, 5143L, 5149L,
                      3480L, 4131L, 5132L, 5139L) & pos == 4L, 4L)
  asignar(isco == 5133L, 4L)

  asignar(pos == 2L & tam < 4L & (isco < 6111L | isco >= 7111L), 5L)
  asignar(pos < 4L & tam < 4L & isco == 4131L, 5L)

  asignar(pos == 3L & isco < 6111L, 6L)
  asignar(pos == 3L & dplyr::between(isco, 7111L, 7431L), 6L)
  asignar(pos == 3L & dplyr::between(isco, 7433L, 8340L), 6L)

  sup_cond_pos <- c(2122L, 2263L, 2311L, 2512L, 2522L, 2640L,
                    2827L, 3101L, 3201L, 4201L, 5301L, 6101L,
                    7201L, 7401L, 7601L, 8101L, 8134L, 8201L, 8301L)
  sup_sin_pos <- c(2630L, 5101L, 5201L, 6201L, 7101L, 7301L,
                   7501L, 9601L)
  asignar(pos == 4L & sinco %in% sup_cond_pos, 7L)
  asignar(sinco %in% sup_sin_pos, 7L)
  asignar(pos == 4L & isco %in% c(
    3131L, 3450L, 3471L, 3472L, 3473L, 3474L, 3475L,
    5161L, 5162L, 5210L
  ), 7L)

  asignar(pos == 4L & dplyr::between(isco, 7111L, 8340L) & tam == 4L, 8L)
  asignar(pos == 4L & dplyr::between(isco, 7111L, 8340L) &
            (tam < 4L | tam == 9L), 9L)

  # ISCO 9211 corresponde a trabajo agricola y queda reservado para las
  # reglas EGP12/EGP13 de abajo. La exclusion explicita evita que la
  # excepcion dependa silenciosamente del orden secuencial de `asignar()`.
  elementales_no_agricolas <- dplyr::between(isco, 9111L, 9333L) &
    isco != 9211L

  asignar(pos == 4L & elementales_no_agricolas & tam == 4L, 10L)
  asignar(pos == 4L & isco %in% c(5123L, 5169L, 9151L) & tam >= 4L, 10L)

  asignar(pos == 4L & elementales_no_agricolas &
            (tam < 4L | tam == 9L), 11L)
  asignar(pos == 4L & isco %in% c(5123L, 5169L, 9151L) & tam < 4L, 11L)
  asignar(pos == 3L & elementales_no_agricolas & tam < 4L, 11L)
  asignar(isco == 9113L & tam < 4L, 11L)
  asignar(sinco == 1006L & pos == 3L, 11L)

  # Predicados agricolas explicitos; 9211 ya fue excluido de EGP10/11.
  agricola_9211_cuenta_propia <- isco == 9211L & pos == 3L & tam < 4L
  agricola_9211_subordinada <- isco == 9211L & pos == 4L

  asignar(pos == 3L & dplyr::between(isco, 6111L, 6299L), 12L)
  asignar(pos == 2L & dplyr::between(isco, 6111L, 6299L) & tam < 4L, 12L)
  asignar(agricola_9211_cuenta_propia, 12L)

  asignar(pos == 4L & dplyr::between(isco, 6111L, 6299L), 13L)
  asignar(agricola_9211_subordinada, 13L)

  # Resoluciones finales documentadas por Damian para casos residuales.
  asignar(sinco == 9999L, 4L)
  asignar(isco == 110L, 1L)
  asignar(dplyr::between(isco, 1221L, 1319L), 2L)
  asignar(isco == 3241L, 4L)
  asignar(isco == 5122L, 6L)
  asignar(isco == 5220L, 4L)
  asignar(isco == 7124L, 9L)
  asignar(sinco %in% c(1131L, 1133L), 1L)
  asignar(sinco %in% c(1619L, 2113L, 2134L, 2141L, 2649L), 2L)
  asignar(sinco == 3232L, 3L)
  asignar(sinco == 5999L, 7L)
  asignar(sinco == 7344L, 9L)
  asignar(sinco == 9733L, 10L)

  regla_egp <- ifelse(!is.na(egp), "EGP_DAMIAN_SINCO_ISCO88", NA_character_)
  evidencia_egp <- ifelse(!is.na(egp), "derived", NA_character_)

  # Reglas aceptadas del consumidor EGP. No rellenan SINCO canonico.
  if (contrato$escenario != "official_strict" &&
      all(c("anio", "trim", "p3coe", "p4a", "sinco4d_base2011") %in%
          names(data))) {
    trim_n <- suppressWarnings(as.integer(sub(
      "^t", "", tolower(as.character(data$trim))
    )))
    periodo_2012t2 <- suppressWarnings(as.integer(data$anio)) == 2012L &
      trim_n == 2L
    ruta_revision <- system.file(
      "extdata", "metodologia_cmo_sinco",
      "revision_rescate_egp13_original_2012t2.csv", package = "renoe"
    )
    if (nzchar(ruta_revision)) {
      revision <- utils::read.csv(
        ruta_revision, stringsAsFactors = FALSE, check.names = FALSE
      )
      llave <- function(a, b, c, d) paste(a, b, c, d, sep = "|")
      indice <- match(
        llave(data$p3coe, data$pos_ocu, data$emple7c, data$p4a),
        llave(revision$p3coe, revision$pos_ocu, revision$emple7c, revision$p4a)
      )
      pendiente_revision <- periodo_2012t2 & !is.na(indice) &
        is.na(suppressWarnings(as.integer(as.character(
          data$sinco4d_base2011
        ))))
      if (contrato$escenario == "integrated_accepted") {
        egp[pendiente_revision] <- NA_integer_
        regla_egp[pendiente_revision] <- NA_character_
        evidencia_egp[pendiente_revision] <- NA_character_
      }
      rescate_d12 <- periodo_2012t2 & is.na(egp) & !is.na(indice) &
        revision$revision[indice] == "original_coincide_con_consenso_oficial"
      egp[rescate_d12] <- suppressWarnings(as.integer(
        revision$egp_oficial[indice[rescate_d12]]
      ))
      regla_egp[rescate_d12] <- "D12_RESCATE_UNANIMIDAD"
      evidencia_egp[rescate_d12] <- "conditional_accepted"
    }
    p4f_d13 <- if ("p4f" %in% names(data)) data$p4f == 2L else
      rep(FALSE, nrow(data))
    rescate_d13 <- periodo_2012t2 & is.na(egp) & data$p3coe == 7201L &
      data$p4a == 4690L & p4f_d13
    rescate_d13[is.na(rescate_d13)] <- FALSE
    egp[rescate_d13] <- 3L
    regla_egp[rescate_d13] <- "D13_7201_P4A4690_P4F2"
    evidencia_egp[rescate_d13] <- "conditional_accepted"
    rescate_d14 <- periodo_2012t2 & is.na(egp) & data$p3coe == 1204L &
      data$p4a == 8112L
    rescate_d14[is.na(rescate_d14)] <- FALSE
    egp[rescate_d14] <- 2L
    regla_egp[rescate_d14] <- "D14_1204_P4A8112"
    evidencia_egp[rescate_d14] <- "conditional_accepted"
  }

  data$clase_egp13_damian <- egp
  data$egp_escenario <- rep(contrato$escenario, nrow(data))
  data$egp_regla_id <- regla_egp
  data$egp_evidence_level <- evidencia_egp
  data$egp_transportable <- is.na(calidad_cmo)
  data$egp_motivo_no_clasificacion <- dplyr::case_when(
    !elegible ~ "fuera_universo_ocupado",
    is.na(egp) & is.na(sinco) ~ "sin_sinco_comparable",
    is.na(egp) ~ "sin_regla_egp_aplicable",
    TRUE ~ NA_character_
  )
  data <- data |>
    dplyr::mutate(
      cobertura_egp_damian = dplyr::case_when(
        !elegible ~ NA_integer_,
        !is.na(clase_egp13_damian) ~ 1L,
        TRUE ~ 0L
      ),
      clase_egp7_damian = dplyr::case_when(
        clase_egp13_damian %in% 1:2 ~ 1L,
        clase_egp13_damian %in% 3:4 ~ 2L,
        clase_egp13_damian %in% 5:6 ~ 3L,
        clase_egp13_damian %in% 7:9 ~ 4L,
        clase_egp13_damian %in% 10:11 ~ 5L,
        clase_egp13_damian == 12L ~ 6L,
        clase_egp13_damian == 13L ~ 7L,
        TRUE ~ NA_integer_
      ),
      clase_alt6_damian = dplyr::case_when(
        clase_egp13_damian %in% c(1L, 2L, 5L) ~ 1L,
        clase_egp13_damian %in% c(3L, 7L) ~ 2L,
        clase_egp13_damian %in% c(4L, 8L) ~ 3L,
        clase_egp13_damian == 6L ~ 4L,
        clase_egp13_damian %in% c(9L, 10L, 11L) ~ 5L,
        clase_egp13_damian %in% c(12L, 13L) ~ 6L,
        TRUE ~ NA_integer_
      ),
      macro_egp4_damian = dplyr::case_when(
        clase_egp7_damian == 1L ~ 1L,
        clase_egp7_damian %in% 2:4 ~ 2L,
        clase_egp7_damian == 5L ~ 3L,
        clase_egp7_damian %in% 6:7 ~ 4L,
        TRUE ~ NA_integer_
      ),
      macro_solis4_damian = dplyr::case_when(
        clase_egp7_damian %in% 1:2 ~ 1L,
        clase_egp7_damian %in% 3:4 ~ 2L,
        clase_egp7_damian == 5L ~ 3L,
        clase_egp7_damian %in% 6:7 ~ 4L,
        TRUE ~ NA_integer_
      ),
      egp3_damian = dplyr::case_when(
        clase_egp7_damian == 1L ~ 1L,
        clase_egp7_damian %in% c(2L, 3L, 4L, 6L) ~ 2L,
        clase_egp7_damian %in% c(5L, 7L) ~ 3L,
        TRUE ~ NA_integer_
      ),
      baja_damian = dplyr::case_when(
        clase_egp13_damian %in% 11:13 ~ 1L,
        !is.na(clase_egp13_damian) ~ 0L,
        TRUE ~ NA_integer_
      ),
      alta_damian = dplyr::case_when(
        clase_egp13_damian %in% c(1L, 2L, 5L) ~ 1L,
        !is.na(clase_egp13_damian) ~ 0L,
        TRUE ~ NA_integer_
      ),
      autoempleo_damian = dplyr::case_when(
        posocup_damian == 3L ~ 1L,
        posocup_damian %in% c(1L, 2L, 4L) ~ 0L,
        TRUE ~ NA_integer_
      )
    ) |>
    sjlabelled::var_labels(
      isco88_damian = "Codigo ISCO-88 derivado de SINCO 2011 (correspondencia de Gerardo Dami\u00E1n Hern\u00E1ndez)",
      sinco4d_damian = "Codigo SINCO 2011 especifico para el modulo de Gerardo Dami\u00E1n Hern\u00E1ndez",
      fuente_sinco_damian = "Variable SINCO 2011 utilizada por el modulo de clase",
      calidad_cmo_damian = "Calidad del puente CMO-SINCO usado por el modulo de Gerardo Dami\u00E1n Hern\u00E1ndez",
      n_destinos_cmo_damian = "Numero de destinos SINCO posibles desde CMO en el modulo de Gerardo Dami\u00E1n Hern\u00E1ndez",
      grupo_ocu9_damian = "Gran grupo ocupacional SINCO en nueve categorias",
      clase_ocu_damian = "Clase ocupacional manual/no manual y calificada/no calificada",
      metodo_clase_damian = "Metodo de asignacion de la clase ocupacional",
      cobertura_isco88_damian = "Indicador de correspondencia SINCO-ISCO88 disponible",
      cobertura_clase_damian = "Indicador de clase ocupacional disponible",
      calificada_damian = "Indicador de trabajo calificado",
      manual_damian = "Indicador de trabajo manual",
      supervisa_damian = "Indicador de ocupacion de supervision"
      ,posocup_damian = "Posicion ocupacional armonizada para reproducir el modulo de Damian"
      ,tam_est_damian = "Tamano del establecimiento derivado de emple7c"
      ,clase_egp13_damian = "Esquema EGP adaptado en trece clases"
      ,cobertura_egp_damian = "Indicador de disponibilidad de clase EGP"
      ,clase_egp7_damian = "Esquema EGP agrupado en siete clases"
      ,clase_alt6_damian = "Esquema alternativo agrupado en seis clases"
      ,macro_egp4_damian = "Macroclases EGP en cuatro categorias"
      ,macro_solis4_damian = "Macroclases de Solis en cuatro categorias"
      ,egp3_damian = "Esquema EGP agrupado en tres clases"
      ,baja_damian = "Indicador de posiciones ocupacionales bajas"
      ,alta_damian = "Indicador de posiciones ocupacionales altas"
      ,autoempleo_damian = "Indicador de trabajo por cuenta propia"
    ) |>
    sjlabelled::val_labels(
      clase_ocu_damian = c(
        "No manual calificado" = 1,
        "No manual no calificado" = 2,
        "Manual calificado" = 3,
        "Manual no calificado" = 4
      ),
      calificada_damian = c("No calificado" = 0, "Calificado" = 1),
      manual_damian = c("No manual" = 0, "Manual" = 1),
      supervisa_damian = c("No" = 0, "Si" = 1)
      ,clase_egp13_damian = c(
        "I. Clase de servicio alta" = 1,
        "II. Clase de servicio baja" = 2,
        "IIIa. No manual de rutina alto" = 3,
        "IIIb. No manual de rutina bajo" = 4,
        "IVa. Pequenos empleadores" = 5,
        "IVb. Cuenta propia calificada" = 6,
        "V. Supervisores manuales y tecnicos bajos" = 7,
        "VIg. Manual calificado en establecimiento grande" = 8,
        "VIp. Manual calificado en establecimiento pequeno" = 9,
        "VIIag. Manual no calificado en establecimiento grande" = 10,
        "VIIap. Manual no calificado pequeno o cuenta propia" = 11,
        "IVc. Pequenos propietarios y cuenta propia agricolas" = 12,
        "VIIb. Subordinados agricolas" = 13
      )
    )

  data
}
