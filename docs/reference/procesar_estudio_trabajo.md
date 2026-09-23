# Clasificar la combinacion de estudio y trabajo

Construye una clasificacion general de asistencia escolar y condicion de
ocupacion, ademas de indicadores para tres situaciones dentro de la
poblacion que no estudia ni trabaja: busqueda de trabajo, dedicacion a
los quehaceres del hogar y disponibilidad laboral. Los indicadores
pueden superponerse; la variable `tipo_neet` ofrece una version
mutuamente excluyente.

## Usage

``` r
procesar_estudio_trabajo(data)
```

## Arguments

- data:

  Data frame con `clase2`, `cs_p17` y `p2e`.

## Value

El mismo data frame con la clasificacion de estudio y trabajo, el
indicador general de no estudio y no trabajo, tres indicadores de grupo
y una tipologia exclusiva.

## Details

La disponibilidad (`neet_disponible`) se conserva con ese nombre
descriptivo. Su interpretacion como proxy de desaliento requiere
justificacion en cada analisis y no es impuesta por esta funcion.

## See also

Other procesamiento_enoe:
[`.armonizar_sinco_enoe_core()`](https://aniuxa.github.io/renoe/reference/dot-armonizar_sinco_enoe_core.md),
[`armonizar_carreras_enoe()`](https://aniuxa.github.io/renoe/reference/armonizar_carreras_enoe.md),
[`calcular_desajuste_estadistico()`](https://aniuxa.github.io/renoe/reference/calcular_desajuste_estadistico.md),
[`calcular_desajuste_horizontal()`](https://aniuxa.github.io/renoe/reference/calcular_desajuste_horizontal.md),
[`crear_folios()`](https://aniuxa.github.io/renoe/reference/crear_folios.md),
[`imputa_ingocup()`](https://aniuxa.github.io/renoe/reference/imputa_ingocup.md),
[`ipc_enoe()`](https://aniuxa.github.io/renoe/reference/ipc_enoe.md),
[`procesar_clases_damian()`](https://aniuxa.github.io/renoe/reference/procesar_clases_damian.md),
[`procesar_contribucion_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_contribucion_hogar.md),
[`procesar_cuidado_extra()`](https://aniuxa.github.io/renoe/reference/procesar_cuidado_extra.md),
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
  clase2 = c(1, 1, 3, 3, 2, 4),
  cs_p17 = c(1, 2, 1, 2, 2, 2),
  p2e = c(NA, NA, 3, 4, NA, 6)
)
procesar_estudio_trabajo(datos)
#>   clase2 cs_p17 p2e situacion_estudio_trabajo no_estudia_no_trabaja
#> 1      1      1  NA                         3                     0
#> 2      1      2  NA                         2                     0
#> 3      3      1   3                         1                     0
#> 4      3      2   4                         4                     1
#> 5      2      2  NA                         4                     1
#> 6      4      2   6                         4                     1
#>   neet_buscador neet_quehaceres neet_disponible tipo_neet
#> 1            NA              NA              NA        NA
#> 2            NA              NA              NA        NA
#> 3            NA              NA              NA        NA
#> 4             0               1               1         2
#> 5             1               0               0         1
#> 6             0               0               0         4
```
