#' Procesar la cascada de clasificaciones reproducibles
#'
#' Ejecuta separadamente, para cada clasificacion consumidora de ocupacion, la
#' precedencia acordada: oficial, panel, auxiliares ENOE/SCIAN, consenso y
#' reglas de la autora o el autor. Una capa solo completa valores faltantes y
#' nunca reescribe una asignacion de una capa anterior.
#'
#' Teletrabajo y cuidados se calculan con el catalogo observado correspondiente
#' al periodo. Las salidas de Damian se calculan primero con el SINCO 2011
#' armonizado. Para las correspondencias multiples de SINCO 2019 se acepta el
#' resultado de una salida cuando todos los destinos oficiales producen la
#' misma categoria; el remanente pasa finalmente por las reglas de Damian sobre
#' el codigo observado. Este proceso no inventa ni rellena SINCO 2011 canonico.
#'
#' @param data Microdatos que ya contienen la salida detallada de
#'   [armonizar_sinco()].
#' @param correspondencia_damian Tabla opcional SINCO 2011-ISCO88.
#' @param puente_2019 Tabla oficial larga SINCO 2019-SINCO 2011.
#' @param reglas_consenso Tabla aceptada de consenso por clasificacion para el
#'   corte CMO-SINCO 2011.
#' @param escenario Contrato publico de decision. `official_strict` conserva
#'   solo resultados oficiales; `integrated_accepted` aplica la cascada
#'   aceptada; `analysis_legacy` habilita ademas rescates historicos.
#' @return El data frame con las clasificaciones y, para cada salida, columnas
#'   terminadas en `_capa`, `_regla_id` y `_nivel_digitos`.
#' @export
procesar_clasificaciones_reproducibles <- function(
    data,
    correspondencia_damian = NULL,
    puente_2019 = NULL,
    reglas_consenso = NULL,
    escenario = c("integrated_accepted", "official_strict", "analysis_legacy")) {
  escenario <- match.arg(escenario)
  if (!is.data.frame(data)) stop("`data` debe ser un data frame.", call. = FALSE)
  requeridas <- c(
    "anio", "trim", "p3coe", "p4a", "clase2", "pos_ocu", "emple7c", "tue2",
    "sinco4d_base2011", "sinco3d", "calidad_puente_sinco"
  )
  faltantes <- setdiff(requeridas, names(data))
  if (length(faltantes)) {
    stop("Faltan variables para la cascada reproducible: ",
         paste(faltantes, collapse = ", "), call. = FALSE)
  }

  if (is.null(correspondencia_damian)) {
    correspondencia_damian <- utils::read.csv(
      .archivo_extdata_reproducible("correspondencia_sinco2011_isco88_damian.csv"),
      stringsAsFactors = FALSE, na.strings = c("", "NA")
    )
  }
  if (is.null(puente_2019)) {
    puente_2019 <- utils::read.csv(
      .archivo_extdata_reproducible("puente_sinco2019_sinco2011.csv"),
      stringsAsFactors = FALSE, na.strings = c("", "NA")
    )
  }
  if (is.null(reglas_consenso)) {
    reglas_consenso <- utils::read.csv(
      .archivo_extdata_reproducible(
        "metodologia_cmo_sinco", "reglas_consenso_clasificaciones_panel.csv"
      ),
      stringsAsFactors = FALSE, na.strings = c("", "NA")
    )
  }

  periodo <- .periodo_reproducible(data$anio, data$trim)
  es_cmo <- periodo <= 20122L
  es_sinco_observado <- periodo >= 20123L
  es_2019 <- periodo >= 20213L

  # Teletrabajo es una clasificacion del catalogo observado, no del puente a
  # SINCO 2011. Asi se evita convertir ambiguedad del puente en no respuesta.
  salida <- clasificar_susceptibilidad_teletrabajo(
    data,
    variable_sinco = "p3coe",
    base_sinco = "observada",
    sobrescribir = TRUE
  )
  tele_valido <- !is.na(salida$susceptible_teletrabajo)
  salida$susceptible_teletrabajo_capa <- ifelse(
    tele_valido, "oficial_catalogo_observado", NA_character_
  )
  salida$susceptible_teletrabajo_regla_id <- ifelse(
    tele_valido, "TELETRABAJO_LISTA_VERSIONADA", NA_character_
  )
  salida$susceptible_teletrabajo_nivel_digitos <- ifelse(
    tele_valido, "4d", NA_character_
  )

  # Clasificaciones agregadas que requieren solamente el gran grupo SINCO.
  sinco1 <- suppressWarnings(as.integer(as.character(salida$sinco1d)))
  codigo_ocupacion_observado <- suppressWarnings(as.integer(
    as.character(salida$p3coe)
  ))
  ocupada <- suppressWarnings(as.integer(as.character(salida$clase2))) == 1L
  salida$skill_level <- dplyr::case_when(
    ocupada & codigo_ocupacion_observado == 9999L ~ NA_integer_,
    ocupada & sinco1 %in% 1:2 ~ 3L,
    ocupada & sinco1 %in% 3:8 ~ 2L,
    ocupada & sinco1 == 9L ~ 1L,
    TRUE ~ NA_integer_
  )
  gran_grupo_observado <- codigo_ocupacion_observado %/% 1000L
  rescate_skill_observado <- es_2019 & ocupada &
    is.na(salida$skill_level) & codigo_ocupacion_observado >= 1000L &
    codigo_ocupacion_observado <= 9998L & gran_grupo_observado %in% 1:9
  rescate_skill_observado[is.na(rescate_skill_observado)] <- FALSE
  salida$skill_level[rescate_skill_observado] <- dplyr::case_when(
    gran_grupo_observado[rescate_skill_observado] %in% 1:2 ~ 3L,
    gran_grupo_observado[rescate_skill_observado] %in% 3:8 ~ 2L,
    gran_grupo_observado[rescate_skill_observado] == 9L ~ 1L,
    TRUE ~ NA_integer_
  )
  salida$skill_level_capa <- ifelse(
    !is.na(salida$skill_level), .capa_base_academica(salida), NA_character_
  )
  salida$skill_level_regla_id <- ifelse(
    !is.na(salida$skill_level), "SKILL_SINCO_1D", NA_character_
  )
  salida$skill_level_nivel_digitos <- ifelse(
    !is.na(salida$skill_level), "1d", NA_character_
  )
  salida$skill_level_capa[rescate_skill_observado] <-
    "oficial_catalogo_observado"
  salida$skill_level_regla_id[rescate_skill_observado] <-
    "SKILL_SINCO2019_1D_OBSERVADO"
  salida$skill_level_nivel_digitos[rescate_skill_observado] <- "1d"

  if ("cs_p13_1" %in% names(salida)) {
    educacion <- .normalizar_codigo_educativo(salida$cs_p13_1, 0:9)
    antecedente <- if ("cs_p15" %in% names(salida)) {
      .normalizar_codigo_educativo(salida$cs_p15, 1:3)
    } else {
      rep(NA_integer_, nrow(salida))
    }
    salida$skill_actual <- dplyr::case_when(
      educacion %in% 0:1 ~ 0L,
      educacion == 2L ~ 1L,
      educacion %in% 3:4 ~ 2L,
      educacion %in% 5:6 & antecedente %in% 1:2 ~ 2L,
      educacion %in% 5:6 & antecedente == 3L ~ 3L,
      educacion %in% 7:9 ~ 3L,
      TRUE ~ NA_integer_
    )
    diferencia <- salida$skill_level - salida$skill_actual
    salida$mismatch <- dplyr::case_when(
      ocupada & diferencia < 0 ~ -1L,
      ocupada & diferencia == 0 ~ 0L,
      ocupada & diferencia > 0 ~ 1L,
      TRUE ~ NA_integer_
    )
    salida$mismatch_capa <- ifelse(
      !is.na(salida$mismatch), salida$skill_level_capa, NA_character_
    )
    salida$mismatch_regla_id <- ifelse(
      !is.na(salida$mismatch), "MISMATCH_SKILL_1D_EDUCACION", NA_character_
    )
    salida$mismatch_nivel_digitos <- ifelse(
      !is.na(salida$mismatch), "1d", NA_character_
    )
  }

  # Cuidado usa el catalogo observado desde 2012-T3. En CMO se separa primero
  # la ruta canonica de su puente historico, para poder respetar precedencia.
  cuidado_base_input <- data
  pendiente_cmo_3d <- es_cmo & is.na(suppressWarnings(as.integer(
    as.character(cuidado_base_input$sinco3d)
  )))
  cuidado_base_input$p3coe[pendiente_cmo_3d] <- NA
  cuidado_base <- procesar_cuidado_remunerado(cuidado_base_input)

  cuidado_observado_input <- data
  marcas_canonicas <- c(
    "sinco4d_base2011", "calidad_puente_sinco", "sinco2011_granularidad"
  )
  cuidado_observado_input[intersect(
    marcas_canonicas, names(cuidado_observado_input)
  )] <- NULL
  cuidado_observado <- procesar_cuidado_remunerado(cuidado_observado_input)

  variable_cuidado <- "trabajo_cuidado_mercado"
  valor_cuidado <- cuidado_base[[variable_cuidado]]
  valor_cuidado[es_sinco_observado] <-
    cuidado_observado[[variable_cuidado]][es_sinco_observado]
  capa_cuidado <- ifelse(!is.na(valor_cuidado), "oficial", NA_character_)
  regla_cuidado <- ifelse(
    !is.na(valor_cuidado), "CUIDADO_CATALOGO_Y_REGLAS_VERSIONADAS", NA_character_
  )
  nivel_cuidado <- ifelse(!is.na(valor_cuidado), "3d", NA_character_)

  reglas_cuidado <- .reglas_consenso_aceptadas(
    reglas_consenso, variable_cuidado
  )
  candidato_cuidado <- cuidado_observado[[variable_cuidado]]
  aplicado_cuidado <- .buscar_regla_consenso(
    data$p3coe, candidato_cuidado, reglas_cuidado
  )
  rescate_cuidado <- es_cmo & is.na(valor_cuidado) &
    !is.na(aplicado_cuidado$valor)
  valor_cuidado[rescate_cuidado] <- aplicado_cuidado$valor[rescate_cuidado]
  capa_cuidado[rescate_cuidado] <- "consenso"
  regla_cuidado[rescate_cuidado] <- aplicado_cuidado$regla[rescate_cuidado]

  legado_cuidado <- escenario == "analysis_legacy" & es_cmo &
    is.na(valor_cuidado) & !is.na(candidato_cuidado)
  valor_cuidado[legado_cuidado] <- candidato_cuidado[legado_cuidado]
  capa_cuidado[legado_cuidado] <- "autor"
  regla_cuidado[legado_cuidado] <- paste0(
    "DAMIAN_CUIDADO_", data$p3coe[legado_cuidado]
  )
  nivel_cuidado[legado_cuidado] <- "3d"

  # Se conservan todas las columnas auxiliares del calculo observado de cuidado
  # desde 2012-T3 y las de la ruta base en CMO.
  columnas_cuidado <- grep(
    "^(cuida_|trabajo_cuidado_|cuidado_|estado_ingreso_cuidado$|scian_version_cuidado$|sinco_version_cuidado$|clasificador_ocupacion_cuidado$)",
    names(cuidado_observado), value = TRUE
  )
  for (v in columnas_cuidado) {
    if (!v %in% names(cuidado_base)) next
    z <- cuidado_base[[v]]
    z[es_sinco_observado] <- cuidado_observado[[v]][es_sinco_observado]
    salida[[v]] <- z
  }
  salida[[variable_cuidado]] <- valor_cuidado
  salida$trabajo_cuidado_mercado_capa <- capa_cuidado
  salida$trabajo_cuidado_mercado_regla_id <- regla_cuidado
  salida$trabajo_cuidado_mercado_nivel_digitos <- nivel_cuidado

  # Las salidas de Damian se recalculan sin reutilizar columnas derivadas de una
  # corrida anterior.
  entrada_damian <- .limpiar_salidas_damian(salida)
  base_damian <- procesar_clases_damian(
    entrada_damian,
    correspondencia = correspondencia_damian,
    usar_puente_cmo = FALSE,
    escenario = "official_strict"
  )
  candidato_damian <- procesar_clases_damian(
    entrada_damian,
    correspondencia = correspondencia_damian,
    usar_puente_cmo = escenario != "official_strict",
    escenario = if (escenario == "official_strict") {
      "official_strict"
    } else {
      "analysis_legacy"
    }
  )

  salidas_damian <- .salidas_reproducibles_damian()
  capa_base <- .capa_base_academica(data)
  for (v in salidas_damian) {
    if (!v %in% names(base_damian)) next
    base_damian[[paste0(v, "_capa")]] <- ifelse(
      !is.na(base_damian[[v]]), capa_base, NA_character_
    )
    base_damian[[paste0(v, "_regla_id")]] <- ifelse(
      !is.na(base_damian[[v]]), .regla_base_academica(data), NA_character_
    )
    base_damian[[paste0(v, "_nivel_digitos")]] <- ifelse(
      !is.na(base_damian[[v]]), .nivel_salida_damian(v), NA_character_
    )
  }

  # Capa 3 ENOE para el hueco EGP de empleadores cuyo numero de trabajadores
  # no fue especificado. La validacion temporal 2023-2026 muestra que las
  # unidades no constituidas en sociedad (tue2=2) y el sector informal
  # (tue2=5) corresponden a empleadores pequenos en 97.7% y 99.8% de la
  # poblacion con tamano conocido. La regla se restringe al periodo validado y
  # solo completa salidas EGP que siguen faltantes; nunca altera el codigo
  # ocupacional, ISCO88 ni una clasificacion previa.
  pos_ocu <- suppressWarnings(as.integer(as.character(data$pos_ocu)))
  emple7c <- suppressWarnings(as.integer(as.character(data$emple7c)))
  tue2 <- suppressWarnings(as.integer(as.character(data$tue2)))
  regla_egp_enoe <- "EGP_EMPLEADOR_TAMANO_NO_ESPEC_TUE2_PEQUENO"
  elegible_egp_enoe <- periodo >= 20231L & pos_ocu == 2L &
    emple7c == 7L & tue2 %in% c(2L, 5L)
  elegible_egp_enoe[is.na(elegible_egp_enoe)] <- FALSE

  entrada_egp_enoe <- entrada_damian
  entrada_egp_enoe$emple7c[elegible_egp_enoe] <- 2L
  candidato_egp_enoe <- procesar_clases_damian(
    entrada_egp_enoe,
    correspondencia = correspondencia_damian,
    usar_puente_cmo = FALSE
  )
  aplicado_egp_enoe <- rep(FALSE, nrow(data))
  for (v in intersect(.salidas_egp_damian(), names(base_damian))) {
    idx <- elegible_egp_enoe & is.na(base_damian[[v]]) &
      !is.na(candidato_egp_enoe[[v]])
    base_damian[[v]][idx] <- candidato_egp_enoe[[v]][idx]
    base_damian[[paste0(v, "_capa")]][idx] <- "enoe"
    base_damian[[paste0(v, "_regla_id")]][idx] <- regla_egp_enoe
    base_damian[[paste0(v, "_nivel_digitos")]][idx] <-
      "posicion+tue2+isco88"
    if (v == "clase_egp13_damian") aplicado_egp_enoe[idx] <- TRUE
  }
  if ("cobertura_egp_damian" %in% names(base_damian)) {
    base_damian$cobertura_egp_damian[aplicado_egp_enoe] <- 1L
  }
  base_damian$egp_tamano_inferido_enoe <- ifelse(
    aplicado_egp_enoe, "pequeno", NA_character_
  )
  base_damian$egp_tue2_origen <- ifelse(
    aplicado_egp_enoe, tue2, NA_integer_
  )
  base_damian$egp_regla_enoe <- ifelse(
    aplicado_egp_enoe, regla_egp_enoe, NA_character_
  )

  # Capa 4 en el corte CMO-SINCO: consenso ya aceptado para cada salida.
  for (v in intersect(salidas_damian, names(base_damian))) {
    reglas_v <- .reglas_consenso_aceptadas(reglas_consenso, v)
    aplicado <- .buscar_regla_consenso(
      data$p3coe, candidato_damian[[v]], reglas_v
    )
    idx <- es_cmo & is.na(base_damian[[v]]) & !is.na(aplicado$valor)
    base_damian[[v]][idx] <- aplicado$valor[idx]
    base_damian[[paste0(v, "_capa")]][idx] <- "consenso"
    base_damian[[paste0(v, "_regla_id")]][idx] <- aplicado$regla[idx]
    base_damian[[paste0(v, "_nivel_digitos")]][idx] <-
      .nivel_salida_damian(v)
  }

  # Capa 4 en SINCO 2019: unanimidad de la propia salida entre todos los
  # destinos oficiales. No se escoge un destino ocupacional.
  consenso_2019 <- .consenso_oficial_damian_2019(
    entrada_damian, puente_2019, correspondencia_damian, es_2019
  )
  for (v in intersect(salidas_damian, names(base_damian))) {
    valor_consenso <- consenso_2019[[v]]
    if (is.null(valor_consenso)) next
    idx <- es_2019 & is.na(base_damian[[v]]) & !is.na(valor_consenso)
    base_damian[[v]][idx] <- valor_consenso[idx]
    base_damian[[paste0(v, "_capa")]][idx] <- "consenso"
    base_damian[[paste0(v, "_regla_id")]][idx] <- paste0(
      "CONSENSO_OFICIAL_SINCO2019_", data$p3coe[idx], "_", v
    )
    base_damian[[paste0(v, "_nivel_digitos")]][idx] <-
      .nivel_salida_damian(v)
  }

  # Capa 5: reglas de Damian, exclusiva de `analysis_legacy`. En CMO reproduce
  # su puente historico; desde SINCO 2019 ejecuta sus reglas y bucles sobre el
  # codigo observado. `integrated_accepted` se detiene tras consenso.
  directo_2019_input <- entrada_damian
  codigo_observado <- suppressWarnings(as.integer(as.character(data$p3coe)))
  directo_2019_input$sinco4d_base2011 <- codigo_observado
  directo_2019_input$sinco4d <- codigo_observado
  directo_2019 <- procesar_clases_damian(
    directo_2019_input,
    correspondencia = correspondencia_damian,
    usar_puente_cmo = FALSE
  )

  if (escenario == "analysis_legacy") {
    for (v in intersect(salidas_damian, names(base_damian))) {
      idx_cmo <- es_cmo & is.na(base_damian[[v]]) &
        !is.na(candidato_damian[[v]])
      base_damian[[v]][idx_cmo] <- candidato_damian[[v]][idx_cmo]
      base_damian[[paste0(v, "_capa")]][idx_cmo] <- "autor"
      base_damian[[paste0(v, "_regla_id")]][idx_cmo] <- paste0(
        "DAMIAN_CMO_", data$p3coe[idx_cmo], "_", v
      )
      base_damian[[paste0(v, "_nivel_digitos")]][idx_cmo] <-
        .nivel_salida_damian(v)

      idx_2019 <- es_2019 & is.na(base_damian[[v]]) &
        !is.na(directo_2019[[v]])
      base_damian[[v]][idx_2019] <- directo_2019[[v]][idx_2019]
      base_damian[[paste0(v, "_capa")]][idx_2019] <- "autor"
      base_damian[[paste0(v, "_regla_id")]][idx_2019] <- paste0(
        "DAMIAN_SINCO2019_", data$p3coe[idx_2019], "_", v
      )
      base_damian[[paste0(v, "_nivel_digitos")]][idx_2019] <-
        .nivel_salida_damian(v)
    }
  }

  # Copiar al resultado solamente las salidas y trazas recalculadas de Damian.
  columnas_damian <- unique(c(
    grep("_damian$|^egp_|^posocup_damian$|^tam_est_damian$|^fuente_sinco_damian$|^calidad_cmo_damian$|^n_destinos_cmo_damian$",
         names(base_damian), value = TRUE),
    unlist(lapply(salidas_damian, function(v) paste0(
      v, c("_capa", "_regla_id", "_nivel_digitos")
    )))
  ))
  for (v in intersect(columnas_damian, names(base_damian))) {
    salida[[v]] <- base_damian[[v]]
  }
  if (escenario == "official_strict") {
    columnas_capa <- grep("_capa$", names(salida), value = TRUE)
    capas_oficiales <- c("official", "oficial", "oficial_catalogo_observado")
    for (columna_capa in columnas_capa) {
      variable <- sub("_capa$", "", columna_capa)
      if (!variable %in% names(salida)) next
      no_oficial <- !is.na(salida[[columna_capa]]) &
        !as.character(salida[[columna_capa]]) %in% capas_oficiales
      salida[[variable]][no_oficial] <- NA
      salida[[columna_capa]][no_oficial] <- NA_character_
      regla <- paste0(variable, "_regla_id")
      nivel <- paste0(variable, "_nivel_digitos")
      if (regla %in% names(salida)) salida[[regla]][no_oficial] <- NA_character_
      if (nivel %in% names(salida)) salida[[nivel]][no_oficial] <- NA_character_
    }
    pendiente_egp <- is.na(salida$clase_egp13_damian)
    for (v in intersect(
      c("egp_regla_id", "egp_evidence_level", "egp_transportable"),
      names(salida)
    )) salida[[v]][pendiente_egp] <- NA
  }
  if (escenario == "integrated_accepted") {
    capas_autor <- intersect(
      paste0(c(variable_cuidado, salidas_damian), "_capa"), names(salida)
    )
    conteo_autor <- vapply(capas_autor, function(v) {
      sum(as.character(salida[[v]]) == "autor", na.rm = TRUE)
    }, integer(1L))
    if (any(conteo_autor > 0L)) {
      detalle_autor <- paste0(
        names(conteo_autor)[conteo_autor > 0L], "=",
        conteo_autor[conteo_autor > 0L], collapse = ", "
      )
      stop(
        "`integrated_accepted` no puede contener asignaciones de la capa autor: ",
        detalle_autor, ".", call. = FALSE
      )
    }
  }
  salida$clasificaciones_escenario <- rep(escenario, nrow(salida))
  salida$perfil_clasificaciones_reproducibles <- "cascada_reproducible_v1"
  salida$estado_revision_clasificaciones <-
    "GO_CON_ADVERTENCIAS_DOCUMENTADAS_2026_09_19"
  salida
}

.archivo_extdata_reproducible <- function(...) {
  partes <- c(...)
  instalado <- do.call(system.file, c(list("extdata"), as.list(partes),
                                      list(package = "renoe")))
  if (nzchar(instalado)) return(instalado)
  desarrollo <- file.path("inst", "extdata", partes)
  if (file.exists(desarrollo)) return(desarrollo)
  stop("No se encontro el insumo metodologico: ",
       paste(partes, collapse = "/"), call. = FALSE)
}

.periodo_reproducible <- function(anio, trim) {
  a <- suppressWarnings(as.integer(as.character(anio)))
  t <- suppressWarnings(as.integer(sub(
    "^t", "", tolower(as.character(trim))
  )))
  a * 10L + t
}

.salidas_reproducibles_damian <- function() {
  c(
    "grupo_ocu9_damian", "isco88_damian", "clase_ocu_damian",
    "calificada_damian", "manual_damian", "supervisa_damian",
    "clase_egp13_damian", "clase_egp7_damian", "clase_alt6_damian",
    "macro_egp4_damian", "macro_solis4_damian", "egp3_damian",
    "baja_damian", "alta_damian", "autoempleo_damian"
  )
}

.salidas_egp_damian <- function() {
  c(
    "clase_egp13_damian", "clase_egp7_damian", "clase_alt6_damian",
    "macro_egp4_damian", "macro_solis4_damian", "egp3_damian",
    "baja_damian", "alta_damian"
  )
}

.limpiar_salidas_damian <- function(data) {
  patron <- paste0(
    "(_damian$|_damian_(capa|regla_id|nivel_digitos)$|^egp_escenario$|",
    "^egp_regla_id$|^egp_evidence_level$|^egp_transportable$|",
    "^egp_motivo_no_clasificacion$|^fuente_sinco_damian$|",
    "^calidad_cmo_damian$|^n_destinos_cmo_damian$)"
  )
  data[grep(patron, names(data), invert = TRUE)]
}

.reglas_consenso_aceptadas <- function(reglas, clasificacion) {
  if (!all(c("cmo", "classification", "old_value", "new_value",
             "accepted", "rule_id") %in% names(reglas))) {
    stop("La tabla de consenso no tiene el esquema esperado.", call. = FALSE)
  }
  aceptada <- tolower(as.character(reglas$accepted)) %in% c("true", "1", "si")
  z <- reglas[aceptada & reglas$classification == clasificacion, , drop = FALSE]
  z$cmo <- suppressWarnings(as.integer(as.character(z$cmo)))
  z$old_value <- suppressWarnings(as.integer(as.character(z$old_value)))
  z$new_value <- suppressWarnings(as.integer(as.character(z$new_value)))
  z
}

.buscar_regla_consenso <- function(codigo, valor_candidato, reglas) {
  n <- length(codigo)
  salida <- list(valor = rep(NA_integer_, n), regla = rep(NA_character_, n))
  if (!nrow(reglas)) return(salida)
  clave_regla <- paste(reglas$cmo, reglas$old_value, sep = "|")
  no_ambigua <- !duplicated(clave_regla) & !duplicated(clave_regla, fromLast = TRUE)
  reglas <- reglas[no_ambigua, , drop = FALSE]
  clave_regla <- paste(reglas$cmo, reglas$old_value, sep = "|")
  clave <- paste(
    suppressWarnings(as.integer(as.character(codigo))),
    suppressWarnings(as.integer(as.character(valor_candidato))), sep = "|"
  )
  posicion <- match(clave, clave_regla)
  salida$valor <- reglas$new_value[posicion]
  salida$regla <- as.character(reglas$rule_id[posicion])
  salida
}

.capa_base_academica <- function(data) {
  if ("sinco_decision_layer" %in% names(data)) {
    z <- as.character(data$sinco_decision_layer)
    z[is.na(z)] <- "oficial"
    return(z)
  }
  rep("oficial", nrow(data))
}

.regla_base_academica <- function(data) {
  z <- if ("regla_cmo_sinco" %in% names(data)) {
    as.character(data$regla_cmo_sinco)
  } else {
    rep(NA_character_, nrow(data))
  }
  z[is.na(z) | !nzchar(z)] <- "SINCO_OBSERVADO_O_EQUIVALENCIA_OFICIAL"
  z
}

.nivel_salida_damian <- function(variable) {
  if (variable == "grupo_ocu9_damian") return("1d")
  if (variable %in% c("clase_ocu_damian", "calificada_damian",
                      "manual_damian")) return("1d_o_isco88")
  if (variable == "autoempleo_damian") return("posicion")
  if (variable %in% c("clase_egp7_damian", "clase_alt6_damian",
                      "macro_egp4_damian", "macro_solis4_damian",
                      "egp3_damian", "baja_damian", "alta_damian")) {
    return("clasificacion_consenso")
  }
  "4d_o_consenso"
}

.consenso_oficial_damian_2019 <- function(
    data, puente, correspondencia, es_2019) {
  requeridas <- c("sinco2019", "sinco2011")
  if (!all(requeridas %in% names(puente))) {
    stop("El puente SINCO 2019 no tiene el esquema esperado.", call. = FALSE)
  }
  p <- data.frame(
    sinco2019 = suppressWarnings(as.integer(as.character(puente$sinco2019))),
    sinco2011 = suppressWarnings(as.integer(as.character(puente$sinco2011)))
  )
  p <- unique(p[!is.na(p$sinco2019) & !is.na(p$sinco2011), , drop = FALSE])
  destinos <- split(p$sinco2011, p$sinco2019)
  observado <- suppressWarnings(as.integer(as.character(data$p3coe)))
  listas <- destinos[as.character(observado)]
  cantidades <- lengths(listas)
  filas <- which(es_2019 & cantidades > 1L)
  resultado <- setNames(
    lapply(.salidas_reproducibles_damian(), function(x) rep(NA_integer_, nrow(data))),
    .salidas_reproducibles_damian()
  )
  if (!length(filas)) return(resultado)

  indice <- rep(filas, cantidades[filas])
  destino <- as.integer(unlist(listas[filas], use.names = FALSE))
  expandido <- data[indice, , drop = FALSE]
  expandido$sinco4d_base2011 <- destino
  expandido$sinco4d <- destino
  clasificado <- procesar_clases_damian(
    .limpiar_salidas_damian(expandido),
    correspondencia = correspondencia,
    usar_puente_cmo = FALSE
  )
  grupo <- split(seq_along(indice), indice)
  for (v in intersect(names(resultado), names(clasificado))) {
    consenso <- vapply(grupo, function(ii) {
      valores <- suppressWarnings(as.integer(as.character(clasificado[[v]][ii])))
      if (anyNA(valores) || length(unique(valores)) != 1L) return(NA_integer_)
      valores[[1L]]
    }, integer(1L))
    resultado[[v]][as.integer(names(consenso))] <- consenso
  }
  resultado
}
