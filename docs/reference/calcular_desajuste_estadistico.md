# Calcular una referencia estadistica de escolaridad y su desajuste

Calcula la escolaridad media observada por division SINCO y periodo
entre la poblacion ocupada. `esco_ref` es una referencia estadistica
interna, no una norma ocupacional externa. La referencia predeterminada
es trimestral.

## Usage

``` r
calcular_desajuste_estadistico(
  data,
  periodo_referencia = c("trimestre", "anio"),
  umbral_anios = 1,
  ponderado = TRUE,
  variable_ponderador = "fac",
  anio_incompleto = c("error", "advertir", "permitir")
)
```

## Arguments

- data:

  Data frame con `anio`, `trim`, `clase2`, `sinco1d` y `anios_es`.

- periodo_referencia:

  `"trimestre"` o `"anio"`.

- umbral_anios:

  Umbral simetrico en anos; por defecto 1.

- ponderado:

  Si es `TRUE`, usa el ponderador indicado.

- variable_ponderador:

  Nombre del ponderador; por defecto `fac`.

- anio_incompleto:

  Tratamiento de anos con menos de cuatro trimestres: `"error"`,
  `"advertir"` o `"permitir"`.

## Value

El mismo data frame, en el mismo orden, con `esco_ref`, `mismatch2` y
metadatos explicitos del periodo, ponderador, numero de trimestres y
unidad persona-trimestre.

## Details

La referencia anual se calcula unicamente sobre datos ya acumulados y su
unidad es persona-trimestre. La funcion no deduplica personas: la
rotacion de ENOE forma parte de los cortes transversales acumulados.
Para publicar una referencia anual se requieren cuatro trimestres por
ano; el tratamiento de anos incompletos puede cambiarse explicitamente
con `anio_incompleto`.

El ponderador se elige con `variable_ponderador`. Dividir por una
constante comun, como cuatro en un ano completo, no cambia la media,
aunque si importa para estimar totales anuales.

## See also

Other procesamiento_enoe:
[`.armonizar_sinco_enoe_core()`](https://aniuxa.github.io/renoe/reference/dot-armonizar_sinco_enoe_core.md),
[`armonizar_carreras_enoe()`](https://aniuxa.github.io/renoe/reference/armonizar_carreras_enoe.md),
[`calcular_desajuste_horizontal()`](https://aniuxa.github.io/renoe/reference/calcular_desajuste_horizontal.md),
[`crear_folios()`](https://aniuxa.github.io/renoe/reference/crear_folios.md),
[`imputa_ingocup()`](https://aniuxa.github.io/renoe/reference/imputa_ingocup.md),
[`ipc_enoe()`](https://aniuxa.github.io/renoe/reference/ipc_enoe.md),
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
datos <- data.frame(
  anio = c(2025, 2025), trim = c(1, 1), clase2 = c(1, 1),
  sinco1d = c(3, 3), anios_es = c(9, 11), fac = c(1, 1)
)
calcular_desajuste_estadistico(datos)
#>   anio trim clase2 sinco1d anios_es fac esco_ref mismatch2
#> 1 2025    1      1       3        9   1       10         0
#> 2 2025    1      1       3       11   1       10         0
#>   periodo_referencia_mismatch2 ponderador_mismatch2
#> 1                    trimestre                  fac
#> 2                    trimestre                  fac
#>   trimestres_referencia_mismatch2 unidad_referencia_mismatch2
#> 1                               1           persona-trimestre
#> 2                               1           persona-trimestre
```
