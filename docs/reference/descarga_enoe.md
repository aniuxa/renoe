# Descargar archivos de microdatos de la ENOE

Descarga los archivos comprimidos de microdatos de la ENOE desde el
sitio del INEGI, los descomprime y guarda las tablas en el formato
especificado.

## Usage

``` r
descarga_enoe(
  anio,
  trimestre,
  formato = "parquet",
  intentos = 3,
  timeout_sec = 300,
  verificar_url = TRUE,
  cache = TRUE
)
```

## Arguments

- anio:

  Año del trimestre (2005-2026). Debe ser un valor numerico.

- trimestre:

  Numero del trimestre (1-4). Donde:

  - 1 = Enero-Marzo

  - 2 = Abril-Junio

  - 3 = Julio-Septiembre

  - 4 = Octubre-Diciembre

- formato:

  Formato de salida para los archivos. Puede ser:

  - "parquet" (recomendado para eficiencia)

  - "rds" (formato nativo de R)

  - "dta" (compatible con Stata)

- intentos:

  Numero maximo de intentos de descarga si falla la conexion (por
  defecto 3).

- timeout_sec:

  Tiempo maximo de espera para la descarga en segundos (por defecto
  300).

- verificar_url:

  Logico. Si TRUE (por defecto), verifica multiples formatos de URL para
  encontrar la correcta. Util cuando INEGI cambia la estructura de
  archivos.

- cache:

  Logico. Si TRUE (por defecto), usa archivos en cache si existen.

## See also

Other descarga_documenta_enoe:
[`carga_enoe()`](https://aniuxa.github.io/renoe/reference/carga_enoe.md),
[`descargar_cuestionarios()`](https://aniuxa.github.io/renoe/reference/descargar_cuestionarios.md),
[`fusion_enoe()`](https://aniuxa.github.io/renoe/reference/fusion_enoe.md),
[`info_trimestre()`](https://aniuxa.github.io/renoe/reference/info_trimestre.md)

## Examples

``` r
if (FALSE) { # \dontrun{
descarga_enoe(2023, 1)
descarga_enoe(2022, 4, formato = "dta", intentos = 5)
} # }
```
