# Restaurar etiquetas de variables y valores de la ENOE

Restaura metadatos que pueden perderse al guardar y volver a leer
archivos Parquet. Las descripciones de variables se toman de
`diccionario_variables.csv` y las etiquetas de códigos de
`diccionario_etiquetas_valores.csv`. La función conserva los códigos
numéricos y usa la clase `haven_labelled`, por lo que el resultado puede
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
  `codigo` y `etiqueta`. Si es `NULL`, usa el catálogo incluido en
  `renoe`.

- sobrescribir:

  Si es `TRUE`, sustituye etiquetas existentes. Por defecto sólo
  completa etiquetas ausentes.

- informar:

  Si es `TRUE`, informa cuántas etiquetas fueron aplicadas.

## Value

El mismo objeto con atributos `label` y, para variables numéricas
catalogadas, clase `haven_labelled` y etiquetas de valores.

## Details

Esta función está pensada para la etapa de distribución o exportación.
No es necesario aplicarla antes de cada transformación analítica.

## Examples

``` r
datos_etiquetados <- aplicar_etiquetas_enoe(
  data.frame(sexo = c(1, 2), clase2 = c(1, 4))
)
```
