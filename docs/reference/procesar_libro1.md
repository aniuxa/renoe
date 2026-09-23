# Procesar indicadores individuales para el proyecto del libro

Crea indicadores de origen geografico, no ocupacion por cuidados,
mujeres con educacion universitaria y afiliacion sindical. La afiliacion
solo es medible en cuestionarios ampliados; esta funcion no calcula
estimaciones survey.

## Usage

``` r
procesar_libro1(data)
```

## Arguments

- data:

  Data frame individual previamente procesado por el paquete renoe.

## Value

El data frame con los nuevos indicadores individuales.

## Details

El origen geografico se reconstruye siempre a partir de `l_nac_c`,
`anio` y `trim`. No se conservan ni reutilizan versiones anteriores de
`extr`, `extr_especificado`, `region_origen_long` u
`origen_nivel_detalle` que puedan venir en `data`. La regla distingue el
cambio de catalogo de 2012-T3 y trata las claves especiales 800 y 997
como Mexico sin entidad comparable, 998 como pais extranjero no
especificado y 999 como origen no clasificable.

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
[`procesar_estudio_trabajo()`](https://aniuxa.github.io/renoe/reference/procesar_estudio_trabajo.md),
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
datos <- datos |>
  procesar_libro1()
} # }
```
