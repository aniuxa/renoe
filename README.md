
# renoe <img src="man/figures/logo.png" align="right" height="120" />

**renoe** es un paquete en desarrollo para facilitar la descarga, carga,
fusión, procesamiento y análisis de los microdatos de la Encuesta
Nacional de Ocupación y Empleo (ENOE) del INEGI.

Permite trabajar de forma reproducible y eficiente con los datos de los
distintos trimestres y formatos de cuestionario (básico o ampliado),
siguiendo un flujo de trabajo estructurado que abarca desde la descarga
hasta el procesamiento avanzado de variables.

## Instalación

Puedes instalar la versión en desarrollo desde GitHub con:

``` r
# install.packages("remotes")
remotes::install_github("aniuxa/renoe")
```

## Descarga y documentación (`@family descarga_documenta_enoe`)

Estas funciones permiten obtener y manejar los datos directamente desde
las fuentes oficiales del INEGI:

| Función | Descripción |
|----|----|
| `descarga_enoe()` | Descarga el archivo ZIP oficial del trimestre desde el sitio del INEGI. |
| `carga_enoe()` | Extrae, etiqueta y carga directamente las tablas de un trimestre. |
| `fusion_enoe()` | Fusiona las tablas `viv`, `hog`, `sdem`, `coe1` y `coe2` en un solo objeto. |
| `descargar_cuestionarios()` | Descarga los cuestionarios PDF disponibles por trimestre. |
| `info_trimestre()` | Devuelve metadatos sobre el cuestionario usado (básico o ampliado). |

------------------------------------------------------------------------

## Procesamiento de microdatos (`@family procesamiento_enoe`)

Una vez fusionadas las tablas, estas funciones permiten procesar paso a
paso los datos de la ENOE:

| Función | Descripción |
|----|----|
| `procesar_variables_enoe()` | Función *wrapper* que aplica en cadena todas las funciones de procesamiento recomendadas: variables sociodemográficas, estructura del hogar, uso del tiempo, IPC, imputación de ingresos y variables laborales. Es el orden lógico sugerido para preparar los microdatos. |
| `crear_folios()` | Crea identificadores únicos para vivienda, hogar y persona. |
| `drop_tri()` | Renombra variables terminadas en `_tri` en la ENOEN para compatibilidad. |
| `procesar_vars_sociodemo()` | Genera variables de edad, sexo y grupos etarios. |
| `procesar_vars_hogar()` | Clasifica hogares por tipo, tamaño y tasas de dependencia. |
| `procesar_tiempo()` | Calcula tiempo dedicado a cuidados y trabajo no remunerado (horas/minutos). |
| `ipc_enoe()` | Agrega el índice de precios al consumidor (IPC) correspondiente al trimestre. |
| `imputa_ingocup()` | Imputa ingresos ocupacionales para personas ocupadas usando `mice`. |
| `procesar_vars_laborales()` | Clasifica tipo de contrato, experiencia laboral y desajuste educativo. |

### Ejemplo

``` r
datos <- fusion_enoe(2022, 1)
datos_proc <- procesar_variables_enoe(datos, anio = 2022, trimestre = 1)
dplyr::glimpse(datos_proc)
table(datos_proc$tipo_hog_lab, useNA = "always")
```

------------------------------------------------------------------------

## Funciones internas y auxiliares

Estas funciones no están exportadas, pero son utilizadas por las
funciones principales para manejar la descarga, verificación y lectura
de archivos:

| Función interna | Descripción |
|----|----|
| `.construir_url_enoe()` | Genera la URL de descarga para un trimestre específico. |
| `.descargar_zip_enoe()` | Descarga y guarda el archivo ZIP de microdatos. |
| `.extraer_zip_enoe()` | Extrae los contenidos del ZIP en una carpeta temporal. |
| `.verificar_cache()` | Verifica si los archivos ya están descargados y extraídos. |
| `.leer_datos_enoe()` | Lee una tabla específica (`viv`, `hog`, etc.) desde el ZIP extraído. |
| `.procesar_etiquetas_enoe()` | Asigna etiquetas a las variables utilizando los diccionarios de INEGI. |
| `.cargar_desde_cache()` | Carga directamente datos previamente extraídos del ZIP. |
| `.estandarizar_ids()` | Unifica identificadores a través de años con diferentes convenciones. |

------------------------------------------------------------------------

## Formatos soportados

- `.parquet`: Compacto, rápido y compatible con Python.
- `.dta`: Compatible con Stata.
- `.rds`: Eficiente para análisis en R.

------------------------------------------------------------------------

## Validación y manejo de excepciones

- Se incluye validación del número de filas esperadas posterior al
  filtrado (`r_def == 0 & c_res != 2`).
- Para el trimestre **2022T1** se utilizan archivos alternativos
  descargados del sitio de microdatos del INEGI.
- Si se detectan anomalías (menos filas de lo esperado, duplicaciones),
  se emiten advertencias para revisión manual.

------------------------------------------------------------------------

## Estructura esperada de carpetas

    renoe/
    ├── zip/      # Contiene los .zip descargados y carpetas extraídas por trimestre
    ├── datos/    # Contiene los archivos fusionados guardados por la persona usuaria

------------------------------------------------------------------------

## Créditos

Este paquete ha sido desarrollado como parte del proyecto PAPIIT
IN305925 *“Retos de la inserción laboral en México”* (UNAM).  
Para sugerencias, contribuciones o reportes de errores, puedes abrir un
*issue* en GitHub o contactar a la autora.

## Cómo citar este paquete

Si utilizas el paquete `renoe` en tus investigaciones o publicaciones,
por favor cita de la siguiente manera:

> Escoto Castillo, Ana Ruth. (2025). *renoe: Herramientas para el
> análisis de la Encuesta Nacional de Ocupación y Empleo (ENOE) en R*.
> Versión 0.1.3. Disponible en: <https://github.com/aniuxa/renoe>

También puedes usar la función `citation("renoe")` en R para obtener la
referencia en formato BibTeX.
