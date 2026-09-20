
# renoe <img src="man/figures/logo.png" alt="Logo del paquete renoe" align="right" height="120" />

**renoe** es un paquete en desarrollo para facilitar la descarga, carga,
fusión, procesamiento y análisis de los microdatos de la Encuesta
Nacional de Ocupación y Empleo (ENOE) del INEGI desde 2005.

La versión 0.3.0 cubre los trimestres habilitados desde 2005-T1 hasta
2026-T2. En 2026 están publicados y auditados T1 y T2; T3 y T4
permanecen deshabilitados hasta su publicación y validación.

Permite trabajar de forma reproducible y eficiente con los datos de los
distintos trimestres y formatos de cuestionario (básico o ampliado),
siguiendo un flujo de trabajo estructurado que abarca desde la descarga
hasta el procesamiento avanzado de variables sociodemográficas,
laborales, de uso del tiempo, estructura del hogar e ingresos.

## Instalación

Puedes instalar la versión en desarrollo desde GitHub. Se recomienda
`pak`, que resuelve e instala automáticamente las dependencias:

``` r
# install.packages("pak")
pak::pkg_install("aniuxa/renoe")
```

También se conservan las alternativas con `remotes` o `devtools`:

``` r
# install.packages("remotes")
remotes::install_github("aniuxa/renoe")

# install.packages("devtools")
devtools::install_github("aniuxa/renoe")
```

## Descarga y documentación (`@family descarga_documenta_enoe`)

Estas funciones permiten obtener y manejar los datos directamente desde
las fuentes oficiales del INEGI:

| Función | Descripción |
|----|----|
| `descarga_enoe()` | Descarga el archivo ZIP oficial del trimestre desde el sitio del INEGI. |
| `carga_enoe()` | Extrae, etiqueta y carga directamente las tablas de un trimestre. |
| `fusion_enoe()` | Fusiona las tablas `viv`, `hog`, `sdem`, `coe1` y `coe2` en un solo objeto, usando una estrategia robusta basada en identificadores disponibles. |
| `descargar_cuestionarios()` | Descarga los cuestionarios PDF disponibles por trimestre. |
| `info_trimestre()` | Devuelve metadatos sobre el cuestionario usado (básico o ampliado). |

------------------------------------------------------------------------

## Procesamiento de microdatos (`@family procesamiento_enoe`)

Una vez fusionadas las tablas, estas funciones permiten procesar paso a
paso los datos de la ENOE:

| Función | Descripción |
|----|----|
| `procesar_variables_enoe()` | Función *wrapper* que aplica en cadena las funciones recomendadas de procesamiento. Dependiendo de la versión del paquete, puede incluir variables sociodemográficas, estructura del hogar, tiempo, IPC, imputación de ingresos y variables laborales. |
| `crear_folios()` | Crea identificadores únicos para vivienda, hogar y persona. |
| `drop_tri()` | Renombra variables terminadas en `_tri` en la ENOEN para compatibilidad. |
| `procesar_vars_sociodemo()` | Genera variables de sexo, edad, grupos etarios, asistencia escolar, estado conyugal, parentesco resumido, ruralidad y zona económica regional. |
| `procesar_vars_hogar()` | Clasifica hogares por estructura, composición, tamaño del hogar sin servicio doméstico y tasas de dependencia. |
| `procesar_tiempo()` | Calcula tiempo dedicado a estudio, cuidado, quehaceres, construcción, reparación, compras, traslados y comunidad. Las variables específicas se expresan en horas. |
| `ipc_enoe()` | Agrega el índice de precios al consumidor (IPC) correspondiente al trimestre. |
| `imputa_ingocup()` | Imputa ingresos ocupacionales para personas ocupadas usando `mice`. |
| `procesar_vars_laborales()` | Clasifica tipo de contrato, experiencia laboral previa, temporalidad y desajuste entre competencia ocupacional y escolaridad. |
| `calcular_desajuste_estadistico()` | Calcula una referencia observada de años de escolaridad por SINCO y periodo, y clasifica sobreeducación, ajuste y subeducación. |
| `procesar_contribucion_hogar()` | Genera ingreso ocupacional individual deflactado, agregados del hogar, indicadores per cápita y quintiles ponderados de ingreso y trabajo no remunerado. |

Las siguientes funciones responden a análisis específicos del proyecto y
no se ejecutan automáticamente dentro de `procesar_variables_enoe()`:

| Función | Uso específico |
|----|----|
| `procesar_cuidado_extra()` | Construye indicadores sobre composición, jornadas laborales y capacidad del hogar para absorber necesidades de cuidado. |
| `procesar_estudio_trabajo()` | Clasifica estudio y trabajo e identifica búsqueda, cuidados y disponibilidad entre quienes no estudian ni trabajan. |
| `procesar_libro1()` | Genera indicadores del proyecto del libro: origen migratorio, no ocupación por cuidados, prestaciones, antigüedad y afiliación sindical. |
| `class_cuidado_rem()` | Clasifica ocupaciones e industrias vinculadas con el trabajo de cuidado de mercado. |
| `cmo_to_sinco11_care()` | Aplica el puente analítico CMO–SINCO 2011 utilizado por la clasificación de cuidados para 2005-I a 2012-II. No es una correspondencia general uno a uno. |

### Ejemplo de flujo recomendado

``` r
datos <- fusion_enoe(2022, 1)

datos_proc <- datos |>
  drop_tri() |>
  crear_folios() |>
  procesar_vars_sociodemo(anio = 2022, trimestre = 1) |>
  procesar_vars_hogar(anio = 2022, trimestre = 1) |>
  procesar_vars_laborales() |>
  calcular_desajuste_estadistico(periodo_referencia = "trimestre") |>
  procesar_tiempo(anio = 2022, trimestre = 1) |>
  ipc_enoe(anio = 2022, trimestre = 1) |>
  imputa_ingocup() |>
  procesar_contribucion_hogar()

dplyr::glimpse(datos_proc)
```

Para los análisis específicos del proyecto, las extensiones se agregan
después del procesamiento general:

``` r
datos_proyecto <- datos_proc |>
  procesar_cuidado_extra() |>
  procesar_libro1() |>
  procesar_cuidado_remunerado()
```

`class_cuidado_rem()` identifica el año y el trimestre y aplica
internamente `cmo_to_sinco11_care()` cuando corresponde al periodo CMO.
No es necesario ejecutar el puente por separado en el flujo habitual.

`fusion_enoe()` aplica una única ruta canónica: valida la unicidad de las
llaves, usa SDEM como tabla ancla y conserva una auditoría de cada unión.
No existe una ruta alternativa o *legacy*.

------------------------------------------------------------------------


## Modulo de cuidado de mercado

El modulo del articulo de brechas de ingreso mediante regresiones cuantílicas
agrupa `procesar_cuidado_remunerado()`, `class_cuidado_rem()` y
`cmo_to_sinco11_care()`. El wrapper opera sobre un data frame en memoria:

```r
datos_cuidado <- renoe::procesar_cuidado_remunerado(
  datos_proc, anio = 2022, trimestre = 1
)
```

Prefiere `p3coe` observado para distinguir CMO, SINCO 2011 y SINCO 2019.
Conserva `sinco3d` del procesamiento general y agrega el codigo de cuidado,
la procedencia, el metodo y la calidad de la armonizacion.
La concordancia analitica se distribuye en
`inst/extdata/concordancia_cmo_sinco_cuidado.csv`; no es un puente oficial general.
El indicador principal es `trabajo_cuidado_mercado`. Describe la insercion
ocupacional en el cuidado y no presupone remuneracion positiva. El wrapper distingue
ademas posicion remunerada, trabajo sin pago e ingreso observado, imputado,
cero o faltante.
Vease [la guia del modulo](articles/cuidado-remunerado.html).

## Funciones internas y auxiliares

Estas funciones no están exportadas, pero son utilizadas por las
funciones principales para manejar la descarga, verificación, lectura y
estandarización de archivos:

| Función interna | Descripción |
|----|----|
| `.construir_url_enoe()` | Genera la URL de descarga para un trimestre específico. |
| `.descargar_zip_enoe()` | Descarga y guarda el archivo ZIP de microdatos. |
| `.extraer_zip_enoe()` | Extrae los contenidos del ZIP en una carpeta temporal o de trabajo. |
| `.verificar_cache()` | Verifica si los archivos ya están descargados y extraídos. |
| `.leer_datos_enoe()` | Lee una tabla específica (`viv`, `hog`, etc.) desde el ZIP extraído. |
| `.procesar_etiquetas_enoe()` | Asigna etiquetas a las variables utilizando los diccionarios del INEGI. |
| `.cargar_desde_cache()` | Carga directamente datos previamente extraídos del ZIP. |
| `.estandarizar_ids()` | Unifica identificadores y nombres clave a través de años con diferentes convenciones. |
| `.homologar_tipos_join()` | Homologa tipos de variables usadas en fusiones robustas. |

------------------------------------------------------------------------

## Formatos soportados

- `.parquet`: compacto, rápido y compatible con Python.
- `.dta`: compatible con Stata.
- `.rds`: eficiente para análisis en R.

Los atributos de etiquetas se conservan en una ida y vuelta Parquet con las
versiones actuales de `arrow` en R, pero otros lectores no necesariamente los
interpretan. Por eso los CSV incluidos en `inst/extdata` son la fuente canónica
portable. `aplicar_etiquetas_enoe()` restaura esos metadatos cuando se requieren
en R o antes de exportar a Stata; las columnas analíticas permanecen numéricas.

## Diccionario de variables

El paquete incluye un diccionario de las variables derivadas, con su
descripción y la función que las genera:

``` r
diccionario <- readr::read_csv(
  system.file("extdata", "diccionario_variables.csv", package = "renoe"),
  show_col_types = FALSE
)
```

Las columnas son `variable_nombre`, `descripcion` y `funcion`.

------------------------------------------------------------------------

## Validación y manejo de excepciones

- Se incluye validación del número de filas esperadas posterior al
  filtrado (`r_def == 0 & c_res != 2`).
- Para **2022-T1** se combinan los componentes oficiales urbano y rural
  de HOG, se armonizan sus códigos de mes y se incluye `ur` en la llave.
- La fusión canónica valida las llaves y detiene el proceso si no conserva
  el universo elegible de SDEM.
- Si se detectan anomalías como menos filas de lo esperado o posibles
  duplicaciones, se emiten advertencias para revisión manual.
- En uso del tiempo se distinguen duración observada, actividad
  realizada con duración desconocida, realización desconocida y batería
  no medible. Las actividades ausentes del instrumento permanecen como
  `NA`, no como cero.

------------------------------------------------------------------------

## Estructura esperada de carpetas

``` text
renoe/
├── zip/      # Contiene los .zip descargados y carpetas extraídas por trimestre
├── datos/    # Contiene los archivos fusionados guardados por la persona usuaria
```

------------------------------------------------------------------------

## Créditos

Este paquete ha sido desarrollado como parte del proyecto PAPIIT
IN305925 *Retos de la inserción laboral en México* (UNAM).

Contribuciones de código:

- **Gerardo Damián Hernández**
  ([ORCID](https://orcid.org/0009-0002-7604-3886), `ctb`): aportó el
  do-file `sinco-isco88.do` y la correspondencia SINCO 2011–ISCO-88
  utilizada por `procesar_clases_damian()`.
- **Gabriela Cervantes** (`ctb`): aportó la clasificación SINCO 2011
  utilizada por `clasificar_susceptibilidad_teletrabajo()`.

Ana Escoto es la única autora y mantenedora formal del paquete.

Para sugerencias, contribuciones o reportes de errores, puedes abrir un
*issue* en GitHub o contactar a la autora.

### Declaración sobre el uso de inteligencia artificial

`renoe` constituye un primer esfuerzo por sistematizar, documentar y
traducir a R procedimientos derivados de la experiencia acumulada por
Ana Escoto y de los conocimientos y trayectorias compartidos por
integrantes del proyecto en el estudio de la ENOE. Incluye materiales
vinculados con el libro sobre el uso de R con la encuesta y programas
originalmente escritos en Stata.

Durante el desarrollo se utilizaron ChatGPT y, desde el 12 de agosto de
2026, Codex como herramientas de apoyo para revisar y explicar código,
proponer funciones, mejorar documentación, detectar errores y construir
y ejecutar pruebas. El conocimiento sustantivo, las decisiones
metodológicas y la validación de los resultados corresponden a la autora
y al equipo del proyecto. Todas las modificaciones asistidas por
inteligencia artificial fueron revisadas por las personas responsables
del paquete. La declaración completa se encuentra en
[`AI_USAGE.md`](AI_USAGE.md).

## Cómo citar este paquete

Si utilizas el paquete `renoe` en tus investigaciones o publicaciones,
por favor cita de la siguiente manera:

> Escoto, A. (2026). *renoe: Herramientas para trabajar con la Encuesta
> Nacional de Ocupación y Empleo (ENOE) desde 2005*. R package version
> 0.3.0. <https://aniuxa.github.io/renoe>

También puedes usar la función `citation("renoe")` en R para obtener la
referencia en formato BibTeX.
