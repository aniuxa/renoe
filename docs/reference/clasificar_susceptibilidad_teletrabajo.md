# Clasificar ocupaciones susceptibles de teletrabajo

Identifica ocupaciones cuyo contenido permite potencialmente realizar
teletrabajo. No mide si la persona teletrabajó efectivamente. La función
usa la clasificación de Gabriela Cervantes para SINCO 2011 entre
2012-III y 2021-II, y su actualización a SINCO 2019 desde 2021-III.

## Usage

``` r
clasificar_susceptibilidad_teletrabajo(
  data,
  variable_sinco = "sinco4d",
  variable_anio = "anio",
  variable_trim = "trim",
  nombre_salida = "susceptible_teletrabajo",
  sobrescribir = FALSE
)
```

## Arguments

- data:

  Data frame con el código SINCO, año y trimestre.

- variable_sinco:

  Nombre de la variable SINCO a cuatro dígitos.

- variable_anio:

  Nombre de la variable de año.

- variable_trim:

  Nombre de la variable de trimestre.

- nombre_salida:

  Nombre del indicador binario creado.

- sobrescribir:

  Si es `TRUE`, permite reemplazar variables existentes.

## Value

El mismo data frame con `nombre_salida` y `version_sinco_teletrabajo`.
El indicador vale 1 para ocupaciones susceptibles, 0 para códigos
observados no susceptibles y `NA` cuando falta el código o el periodo
queda fuera de la ventana comparable.

## Details

La actualización conserva la equivalencia sustantiva de Nutriólogos:
SINCO 2011 `2423` pasa a SINCO 2019 `2433`. El código `2423` de SINCO
2019 corresponde a Ginecólogos y obstetras y no se clasifica como
susceptible.

Periodo comparable: desde 2012-III. El corte entre clasificadores se
fija en 2021-III. Para estudiar población ocupada, filtre `clase2 == 1`
antes o después de ejecutar la función.

## Examples

``` r
datos <- data.frame(
  sinco4d = c("2423", "2433", "6111"),
  anio = c(2021, 2021, 2022),
  trim = c(2, 3, 1)
)
clasificar_susceptibilidad_teletrabajo(
  datos,
  variable_sinco = "sinco4d",
  variable_anio = "anio",
  variable_trim = "trim"
)
#>   sinco4d anio trim susceptible_teletrabajo version_sinco_teletrabajo
#> 1    2423 2021    2                       1                SINCO 2011
#> 2    2433 2021    3                       1                SINCO 2019
#> 3    6111 2022    1                       0                SINCO 2019
```
