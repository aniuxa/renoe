# Anadir IPC al conjunto fusionado de ENOE

Esta funcion agrega una columna llamada `ipc` al objeto fusionado de la
ENOE, correspondiente al promedio trimestral del Indice de Precios al
Consumidor (IPC).

## Usage

``` r
ipc_enoe(datos_fusionados, anio, trimestre)
```

## Arguments

- datos_fusionados:

  Un data.frame ya fusionado con
  [`fusion_enoe()`](https://aniuxa.github.io/renoe/reference/fusion_enoe.md).

- anio:

  Ano del trimestre (numerico).

- trimestre:

  Trimestre (1 a 4).

## Value

El mismo `data.frame` con una nueva columna `ipc`.

## Details

El archivo `ipc.rds` debe estar ubicado en `inst/extdata/` y contener
las columnas numericas `anio`, `trim` e `ipc`, una fila por trimestre.
La funcion se detiene si el recurso tiene claves duplicadas, valores
invalidos o no contiene el periodo solicitado; asi se evita propagar
ingresos deflactados ausentes sin advertencia suficiente.

## See also

Other procesamiento_enoe:
[`.armonizar_sinco_enoe_core()`](https://aniuxa.github.io/renoe/reference/dot-armonizar_sinco_enoe_core.md),
[`armonizar_carreras_enoe()`](https://aniuxa.github.io/renoe/reference/armonizar_carreras_enoe.md),
[`calcular_desajuste_estadistico()`](https://aniuxa.github.io/renoe/reference/calcular_desajuste_estadistico.md),
[`calcular_desajuste_horizontal()`](https://aniuxa.github.io/renoe/reference/calcular_desajuste_horizontal.md),
[`crear_folios()`](https://aniuxa.github.io/renoe/reference/crear_folios.md),
[`imputa_ingocup()`](https://aniuxa.github.io/renoe/reference/imputa_ingocup.md),
[`procesar_clases_damian()`](https://aniuxa.github.io/renoe/reference/procesar_clases_damian.md),
[`procesar_contribucion_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_contribucion_hogar.md),
[`procesar_cuidado_extra()`](https://aniuxa.github.io/renoe/reference/procesar_cuidado_extra.md),
[`procesar_estudio_trabajo()`](https://aniuxa.github.io/renoe/reference/procesar_estudio_trabajo.md),
[`procesar_libro1()`](https://aniuxa.github.io/renoe/reference/procesar_libro1.md),
[`procesar_tiempo()`](https://aniuxa.github.io/renoe/reference/procesar_tiempo.md),
[`procesar_variables_enoe()`](https://aniuxa.github.io/renoe/reference/procesar_variables_enoe.md),
[`procesar_vars_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_vars_hogar.md),
[`procesar_vars_laborales()`](https://aniuxa.github.io/renoe/reference/procesar_vars_laborales.md),
[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md),
[`reglas_enoe()`](https://aniuxa.github.io/renoe/reference/reglas_enoe.md),
[`sinco2019_to_sinco2011()`](https://aniuxa.github.io/renoe/reference/sinco2019_to_sinco2011.md)

## Examples

``` r
if (FALSE) { # \dontrun{
datos <- fusion_enoe(2023, 1)
datos <- ipc_enoe(datos, 2023, 1)
} # }
```
