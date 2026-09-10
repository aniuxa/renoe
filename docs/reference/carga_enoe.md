# Cargar microdatos de la ENOE

Descarga, extrae y carga las tablas de microdatos de la Encuesta
Nacional de Ocupación y Empleo (ENOE) para un trimestre específico.
Incluye la corrección automática del archivo defectuoso de hogares del
primer trimestre de 2022. Las tablas disponibles son: vivienda (viv),
hogar (hog), sociodemográfica (sdem) y los dos componentes del
cuestionario ampliado (coe1 y coe2).

## Usage

``` r
carga_enoe(
  anio,
  trimestre,
  list = FALSE,
  rapida = FALSE,
  intentos = 3,
  timeout_sec = 300,
  verificar_url = TRUE,
  cache = TRUE,
  prefijo = NULL
)
```

## Arguments

- anio:

  Año del trimestre (2005–2026)

- trimestre:

  Número del trimestre (1–4), donde:

  - 1 = Enero–Marzo

  - 2 = Abril–Junio

  - 3 = Julio–Septiembre

  - 4 = Octubre–Diciembre

- list:

  Lógico. Si TRUE, devuelve una lista con los data frames. Si FALSE (por
  defecto), los objetos se cargan al entorno global.

- rapida:

  Lógico. Si TRUE, omite el etiquetado de variables (más rápido).

- intentos:

  Número máximo de intentos de descarga (por defecto = 3).

- timeout_sec:

  Tiempo máximo de espera por intento en segundos (por defecto = 300).

- verificar_url:

  Lógico. Si TRUE (por defecto), intenta formatos alternativos de URL si
  falla la descarga.

- cache:

  Lógico. Si TRUE (por defecto), reutiliza datos descargados previamente
  si existen.

- prefijo:

  Cadena opcional para el prefijo de los archivos ("enoe" o "enoen"). Si
  no se indica, se detecta automáticamente.

## Value

Si `list = TRUE`, devuelve una lista con las cinco tablas. Si
`list = FALSE`, las tablas se cargan al entorno global con nombres como
`viv2023_t1`, `hog2023_t1`, etc.

## Details

Esta función combina varias operaciones comunes al trabajar con
microdatos de la ENOE: descarga, extracción del ZIP, lectura,
estandarización de identificadores y aplicación de etiquetas. La función
maneja de forma especial el primer trimestre de 2022, sustituyendo
automáticamente el archivo por la versión que se descarga en microdatos
de INEGI y no en datos abiertos..

## See also

[`fusion_enoe`](https://aniuxa.github.io/renoe/reference/fusion_enoe.md),
[`descarga_enoe`](https://aniuxa.github.io/renoe/reference/descarga_enoe.md),
[`procesar_vars_sociodemo`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md)

Other descarga_documenta_enoe:
[`descarga_enoe()`](https://aniuxa.github.io/renoe/reference/descarga_enoe.md),
[`descargar_cuestionarios()`](https://aniuxa.github.io/renoe/reference/descargar_cuestionarios.md),
[`fusion_enoe()`](https://aniuxa.github.io/renoe/reference/fusion_enoe.md),
[`info_trimestre()`](https://aniuxa.github.io/renoe/reference/info_trimestre.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Cargar datos al entorno global para el segundo trimestre de 2023
carga_enoe(2023, 2)

# Cargar datos como lista sin etiquetas
datos <- carga_enoe(2022, 4, list = TRUE, rapida = TRUE)

# Cargar el trimestre corregido 2022T1 desde caché o desde extdata si es necesario
carga_enoe(2022, 1)
} # }
```
