# Procesar variables sociodemográficas básicas y extendidas

Añade sexo, edad, grupos etarios, metadatos como año, trimestre y tipo
de cuestionario COE, así como variables extendidas de asistencia
escolar, estado conyugal, escolaridad, zona rural, tamaño de localidad y
zona económica regional.

## Usage

``` r
procesar_vars_sociodemo(data, anio, trimestre)
```

## Arguments

- data:

  Un data frame tipo sdem, con variables como `sex`, `eda`, `cs_p17`,
  `e_con`, `anios_esc`, `par_c`, `t_loc` y `ent`.

- anio:

  Año del trimestre.

- trimestre:

  Trimestre numérico (1-4).

## Value

Un data frame con variables sociodemográficas procesadas y etiquetadas.

## See also

Other procesamiento_enoe:
[`armoniza_sinco()`](https://aniuxa.github.io/renoe/reference/armoniza_sinco.md),
[`armonizar_carreras_enoe()`](https://aniuxa.github.io/renoe/reference/armonizar_carreras_enoe.md),
[`calcular_desajuste_estadistico()`](https://aniuxa.github.io/renoe/reference/calcular_desajuste_estadistico.md),
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
[`sinco2019_to_sinco2011()`](https://aniuxa.github.io/renoe/reference/sinco2019_to_sinco2011.md)

## Examples

``` r
if (FALSE) { # \dontrun{
datos_sdem_proc <- procesar_vars_sociodemo(sdem, anio = 2023, trimestre = 1)
} # }
```
