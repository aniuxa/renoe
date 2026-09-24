# Cargar microdatos de la ENOE

Descarga, extrae y carga las tablas de microdatos de la Encuesta
Nacional de Ocupacion y Empleo (ENOE) para un trimestre especifico.
Incluye la correccion automatica del archivo defectuoso de hogares del
primer trimestre de 2022. Las tablas disponibles son: vivienda (viv),
hogar (hog), sociodemografica (sdem) y los dos componentes del
cuestionario de ocupacion (coe1 y coe2).

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

  Año del trimestre (2005-2026)

- trimestre:

  Numero del trimestre (1-4), donde:

  - 1 = Enero-Marzo

  - 2 = Abril-Junio

  - 3 = Julio-Septiembre

  - 4 = Octubre-Diciembre

- list:

  Logico. Si TRUE, devuelve una lista con los data frames. Si FALSE (por
  defecto), los objetos se cargan al entorno global.

- rapida:

  Logico. Si TRUE, omite el etiquetado de variables (mas rapido).

- intentos:

  Numero maximo de intentos de descarga (por defecto = 3).

- timeout_sec:

  Tiempo maximo de espera por intento en segundos (por defecto = 300).

- verificar_url:

  Logico. Si TRUE (por defecto), intenta formatos alternativos de URL si
  falla la descarga.

- cache:

  Logico. Si TRUE (por defecto), reutiliza datos descargados previamente
  si existen.

- prefijo:

  Cadena opcional para el prefijo de los archivos ("enoe" o "enoen"). Si
  no se indica, se detecta automaticamente.

## Value

Si `list = TRUE`, devuelve una lista con las cinco tablas. Si
`list = FALSE`, las tablas se cargan al entorno global con nombres como
`viv2023_t1`, `hog2023_t1`, etc.

## Details

Esta funcion combina varias operaciones comunes al trabajar con
microdatos de la ENOE: descarga, extraccion del ZIP, lectura,
estandarizacion de identificadores y aplicacion de etiquetas. Para
2022-T1 combina el HOG urbano ubicado en la raiz del ZIP con el HOG
rural distribuido en `conjunto_de_datos/` y armoniza los codigos de mes
del segundo componente.

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

# Cargar el trimestre 2022T1 combinando ambos componentes oficiales de HOG
carga_enoe(2022, 1)
} # }
```
