#' Procesar variables clave de la ENOE en una sola función
#'
#' Función envolvente (`wrapper`) que aplica en cadena varias funciones de procesamiento sobre los microdatos de la ENOE.
#' Crea variables sociodemográficas, estructura del hogar, uso del tiempo, imputación de ingresos e información del IPC.
#'
#' Aplica automáticamente las funciones:
#' - `crear_folios()`: Genera identificadores únicos de vivienda, hogar y persona.
#' - `drop_tri()`: Cuando se trata de la ENOEN, renombra automáticamente variables terminadas en `_tri` a su forma base (por ejemplo, `fac_tri` → `fac`).
#' - `procesar_vars_sociodemo()`: Crea variables de edad, sexo y grupos etarios.
#' - `procesar_vars_hogar()`: Clasifica hogares por tipo, tamaño y dependencia.
#' - `procesar_tiempo()`: Calcula minutos y horas en actividades del hogar y cuidado.
#' - `ipc_enoe()`: Añade una variable con el IPC nacional del trimestre correspondiente.
#' - `imputa_ingocup()`: Imputa el ingreso ocupacional con `mice` para personas ocupadas.
#'
#' @encoding UTF-8
#' @param data Un data frame con las tablas fusionadas de la ENOE (por ejemplo, salida de `fusion_enoe()`).
#' @param anio Año del trimestre (numérico).
#' @param trimestre Trimestre numérico (1–4).
#'
#' @return Un data frame con variables sociodemográficas, estructura del hogar, uso del tiempo, IPC y variables imputadas.
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
procesar_variables_enoe <- function(data, anio, trimestre ) {
  data<-data %>%
    crear_folios() %>%
    drop_tri() %>%
    procesar_vars_sociodemo(anio=anio, trimestre=trimestre) %>%
    procesar_vars_hogar() %>%
    procesar_tiempo() %>%
    ipc_enoe(anio=anio, trimestre=trimestre) %>%
    imputa_ingocup() %>%
    procesar_vars_laborales()

  return(data)
}
