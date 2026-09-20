# Restaurar etiquetas de variables y valores de la ENOE

Restaura metadatos que pueden perderse al guardar y volver a leer
archivos Parquet. Las descripciones de variables se toman de
`diccionario_variables.csv` y las etiquetas de codigos de
`diccionario_etiquetas_valores.csv`. La funcion conserva los codigos
numericos y usa la clase `haven_labelled`, por lo que el resultado puede
guardarse como RDS o exportarse a Stata sin convertir las variables en
factores.

## Usage

``` r
aplicar_etiquetas_enoe(
  data,
  diccionario_variables = NULL,
  diccionario_valores = NULL,
  sobrescribir = FALSE,
  informar = interactive()
)
```

## Arguments

- data:

  Data frame o tibble con variables ENOE procesadas.

- diccionario_variables:

  Ruta a un CSV o data frame con las columnas `variable_nombre` y
  `descripcion`. Si es `NULL`, usa el diccionario incluido en `renoe`.

- diccionario_valores:

  Ruta a un CSV o data frame con las columnas `variable_nombre`,
  `codigo` y `etiqueta`. Si es `NULL`, usa el catalogo incluido en
  `renoe`.

- sobrescribir:

  Si es `TRUE`, sustituye etiquetas existentes. Por defecto solo
  completa etiquetas ausentes.

- informar:

  Si es `TRUE`, informa cuantas etiquetas fueron aplicadas.

## Value

El mismo objeto con atributos `label` y, para variables numericas
catalogadas, clase `haven_labelled` y etiquetas de valores.

## Details

Esta funcion esta pensada para la etapa de distribucion o exportacion.
No es necesario aplicarla antes de cada transformacion analitica.

## Examples

``` r
datos_etiquetados <- aplicar_etiquetas_enoe(
  data.frame(sexo = c(1, 2), clase2 = c(1, 4))
)
```
