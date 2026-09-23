# Fusionar tablas de la ENOE

Une las tablas de vivienda, hogar, sociodemografico y componentes COE en
un unico data frame.

## Usage

``` r
fusion_enoe(
  anio,
  trimestre,
  rapida = FALSE,
  formato = NULL,
  guardar = !is.null(formato),
  intentos = 3,
  ...
)
```

## Arguments

- anio:

  Ano del trimestre (2005-2026).

- trimestre:

  Numero del trimestre (1-4).

- rapida:

  Logico. Si `TRUE`, omite el etiquetado de variables.

- formato:

  Formato de salida ("parquet", "rds" o "dta"). Opcional.

- guardar:

  Logico. Si `TRUE` y se especifica formato, guarda el archivo
  fusionado.

- intentos:

  Numero de intentos para cargar datos (por defecto 3).

- ...:

  Otros parametros para pasar a
  [`carga_enoe()`](https://aniuxa.github.io/renoe/reference/carga_enoe.md).

## Value

Un data frame con las tablas fusionadas. Si se especifica formato y
`guardar = TRUE`, guarda el archivo en el subdirectorio `"datos"` con el
nombre `"enoe_fusion_ANIO_TRIMESTREt.FORMATO"`.

## See also

Other descarga_documenta_enoe:
[`carga_enoe()`](https://aniuxa.github.io/renoe/reference/carga_enoe.md),
[`descarga_enoe()`](https://aniuxa.github.io/renoe/reference/descarga_enoe.md),
[`descargar_cuestionarios()`](https://aniuxa.github.io/renoe/reference/descargar_cuestionarios.md),
[`info_trimestre()`](https://aniuxa.github.io/renoe/reference/info_trimestre.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Fusionar tablas para el T3 de 2020
datos2020 <- fusion_enoe(2020, 3)

# Fusionar y guardar como Parquet
fusion_enoe(2019, 2, formato = "parquet", guardar = TRUE)
} # }
```
