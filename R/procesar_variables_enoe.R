#' Procesar variables clave de la ENOE en una sola funcion
#'
#' Ruta canonica que aplica en un orden fijo las transformaciones utilizadas
#' por los productos del libro. Conserva filas y orden y valida la llave de
#' persona al terminar.
#'
#' Aplica automaticamente las funciones:
#' - `drop_tri()`: Cuando se trata de la ENOEN, renombra automaticamente variables terminadas en `_tri` a su forma base (por ejemplo, `fac_tri` ? `fac`).
#' - `crear_folios()`: Genera identificadores unicos de vivienda, hogar y persona.
#' - `procesar_vars_sociodemo()`: Crea variables de edad, sexo y grupos etarios.
#' - `procesar_vars_hogar()`: Clasifica hogares por tipo, tamano y dependencia.
#' - `armonizar_carreras()`, `procesar_vars_laborales()`,
#'   `calcular_desajuste_estadistico()` y `calcular_desajuste_horizontal()`:
#'   armonizan educacion y trabajo.
#' - `procesar_tiempo()`: Calcula horas en actividades del hogar y cuidado,
#'   con corte instrumental explicito en 2013.
#' - `ipc_enoe()`: Anade una variable con el IPC nacional del trimestre correspondiente.
#' - `imputa_ingocup()`: Imputa el ingreso ocupacional con `mice` para personas ocupadas.
#' - `procesar_contribucion_hogar()` y `procesar_cuidado_extra()`: agregan
#'   recursos, tiempos y capacidad del hogar.
#' - `procesar_estudio_trabajo()`, `procesar_libro1()` y
#'   `procesar_clasificaciones_reproducibles()`: construyen las salidas finales.
#'
#' @encoding UTF-8
#' @param data Un data frame con las tablas fusionadas de la ENOE (por ejemplo, salida de `fusion_enoe()`).
#' @param anio Ano del trimestre (numerico).
#' @param trimestre Trimestre numerico (1-4).
#' @param semilla Semilla de la imputacion de ingreso.
#' @param perfil_carreras Perfil de evidencia para armonizar carreras.
#' @param usar_puente_2005 Si se permite el puente experimental de carreras de
#'   2005. Por defecto es `FALSE`.
#' @param escenario_clasificadores Escenario explicito para SINCO y sus
#'   consumidores.
#'
#' @return Un data frame con variables sociodemograficas, estructura del hogar, uso del tiempo, IPC y variables imputadas.
#' @export
#'
#' @seealso [procesar_vars_sociodemo()], [procesar_vars_hogar()], [ipc_enoe()], [imputa_ingocup()]
#'
#' @examples
#' \dontrun{
#' datos <- fusion_enoe(2022, 1)
#' datos_proc <- procesar_variables_enoe(datos, 2022, 1)
#' dplyr::glimpse(datos_proc)
#' table(datos_proc$tipo_hog_lab, useNA = "always")
#' }
#'
#' @family procesamiento_enoe
procesar_variables_enoe <- function(
    data, anio, trimestre, semilla = 1234,
    perfil_carreras = c("panel_validado", "oficial", "experimental"),
    usar_puente_2005 = FALSE,
    escenario_clasificadores = c(
      "integrated_accepted", "official_strict", "analysis_legacy"
    )) {
  perfil_carreras <- match.arg(perfil_carreras)
  escenario_clasificadores <- match.arg(escenario_clasificadores)
  filas_iniciales <- nrow(data)

  data <- data %>%
    drop_tri() %>%
    crear_folios() %>%
    procesar_vars_sociodemo(anio = anio, trimestre = trimestre) %>%
    procesar_vars_hogar(anio = anio, trimestre = trimestre) %>%
    armonizar_scian() %>%
    armonizar_sinco(escenario = escenario_clasificadores) %>%
    armonizar_carreras(
      perfil = perfil_carreras,
      usar_puente_2005 = usar_puente_2005,
      salida = "auditable"
    ) %>%
    procesar_vars_laborales(escenario = escenario_clasificadores) %>%
    calcular_desajuste_estadistico(periodo_referencia = "trimestre") %>%
    calcular_desajuste_horizontal() %>%
    procesar_tiempo(anio = anio, trimestre = trimestre) %>%
    ipc_enoe(anio = anio, trimestre = trimestre) %>%
    imputa_ingocup(seed = semilla) %>%
    procesar_contribucion_hogar() %>%
    procesar_cuidado_extra() %>%
    procesar_estudio_trabajo() %>%
    procesar_libro1() %>%
    procesar_clasificaciones_reproducibles(
      escenario = escenario_clasificadores
    )

  if (nrow(data) != filas_iniciales) {
    stop("La ruta canonica cambio el numero de filas.", call. = FALSE)
  }
  if (!"folio3" %in% names(data) || anyNA(data$folio3) ||
      anyDuplicated(data$folio3)) {
    stop("La ruta canonica no termino con una llave `folio3` unica.",
         call. = FALSE)
  }
  data
}
