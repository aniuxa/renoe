# ==============================================================================
# PUENTE CMO A SINCO 2011 PARA EL ANALISIS DE CUIDADOS
# Basado en "cmo a sinco11.do" de Damian (material del proyecto, 2026)
# ==============================================================================

#' Crear un puente CMO-SINCO 2011 para estudiar trabajo de cuidado
#'
#' Funcion auxiliar construida para armonizar las ocupaciones necesarias en el
#' analisis longitudinal del trabajo de cuidado de mercado. Se basa en el
#' do-file `cmo a sinco11.do` preparado por Damian (2026).
#'
#' No es una conversion oficial, general ni biunivoca entre CMO y SINCO 2011.
#' No debe utilizarse para afirmar que todas las personas recibieron una
#' ocupacion SINCO exacta ni para estudiar ocupaciones ajenas al objetivo de
#' cuidados sin revisar antes la concordancia correspondiente.
#'
#' Para reproducir el do-file, cuando un CMO tiene varios destinos conserva en
#' `sinco11` el destino de la primera regla, ya que el codigo Stata solo
#' reemplaza mientras `sinco11 == -1`. Esa eleccion es mecanica: no resuelve la
#' ambiguedad sustantiva. Por ello la funcion tambien informa el numero de
#' destinos posibles y la calidad de la correspondencia. La clasificacion final
#' de cuidados debe resolver los casos multiples en categorias amplias: si los
#' destinos coinciden en la misma categoria de cuidado puede utilizarse esa
#' categoria; si discrepan, se requiere una regla analitica explicita.
#'
#' @param data Data frame que contiene la ocupacion codificada en CMO.
#' @param variable_cmo Nombre de la variable CMO; en las bases fusionadas suele
#'   ser `p3coe`.
#' @param sobrescribir Si es FALSE, detiene la ejecucion cuando ya existen las
#'   variables de salida.
#'
#' @return El data frame con `cmo_original`, `sinco11`, `sinco3d`,
#'   `sinco11_n_destinos` y `sinco11_calidad`.
#' @export
#' @family cuidado_remunerado
#'
#' @examples
#' \dontrun{
#' datos <- datos |>
#'   cmo_to_sinco11_care(variable_cmo = "p3coe")
#' }
cmo_to_sinco11_care <- function(
    data,
    variable_cmo = "p3coe",
    sobrescribir = TRUE
) {
  if (!variable_cmo %in% names(data)) {
    stop("No existe la variable CMO `", variable_cmo, "` en `data`.")
  }

  salidas <- c(
    "cmo_original", "sinco11", "sinco3d",
    "sinco11_n_destinos", "sinco11_calidad"
  )
  existentes <- intersect(salidas, names(data))
  if (length(existentes) > 0 && !sobrescribir) {
    stop(
      "Ya existen variables de salida: ",
      paste(existentes, collapse = ", "),
      ". Use `sobrescribir = TRUE` unicamente si desea reemplazarlas."
    )
  }

  ruta <- system.file("extdata", "concordancia_cmo_sinco_cuidado.csv",
                      package = "renoe")
  if (!nzchar(ruta)) stop("No se encuentra la concordancia de cuidado.")
  tabla <- utils::read.csv(ruta, colClasses = "integer")
  .cmo <- tabla$cmo
  .sinco11 <- tabla$sinco11
  .n_destinos <- tabla$n_destinos

  cmo_num <- suppressWarnings(
    as.integer(as.character(data[[variable_cmo]]))
  )
  posicion <- match(cmo_num, .cmo)
  convertido <- .sinco11[posicion]
  n_destinos <- .n_destinos[posicion]

  data$cmo_original <- cmo_num
  data$sinco11 <- as.integer(convertido)
  data$sinco3d <- as.integer(data$sinco11 %/% 10)
  data$sinco11_n_destinos <- as.integer(n_destinos)
  data$sinco11_calidad <- dplyr::case_when(
    is.na(cmo_num) ~ "CMO faltante",
    is.na(convertido) ~ "Sin correspondencia en Damian",
    n_destinos == 1L ~ "Correspondencia unica",
    n_destinos > 1L ~ "Correspondencia multiple: primera regla de Damian",
    TRUE ~ NA_character_
  )

  data |>
    sjlabelled::var_labels(
      cmo_original = "Codigo CMO original usado en la homologacion",
      sinco11 = "Codigo SINCO 2011 asignado desde CMO",
      sinco3d = "Codigo SINCO 2011 a tres digitos",
      sinco11_n_destinos = "Numero de destinos SINCO posibles para el codigo CMO",
      sinco11_calidad = "Calidad de la homologacion CMO a SINCO 2011"
    )
}
