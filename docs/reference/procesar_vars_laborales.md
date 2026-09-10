# Procesar variables de análisis laboral y desajuste educativo

Esta función genera variables clasificatorias relacionadas con la
ocupación, el nivel educativo y el desajuste entre ambos, a partir de
los códigos de ocupación (`p3coe`) y del nivel educativo alcanzado
(`cs_p13_1`). Internamente armoniza los códigos SINCO (1, 2, 3 y 4
dígitos) usando correspondencias con códigos CMO y reglas auxiliares.
También clasifica el nivel agregado de competencia de la ocupación, un
proxy basado en escolaridad y el desajuste entre ambos. La agrupación de
competencia requerida es una aproximación a un dígito de SINCO y puede
ocultar excepciones dentro de cada división.

## Usage

``` r
procesar_vars_laborales(data)
```

## Arguments

- data:

  Un data.frame con variables como:

  - `anio`, `trimestre`: año y trimestre de la entrevista

  - `coe_tipo`: tipo de cuestionario (`"ampliado"` o `"basico"`)

  - `p3coe`: código ocupacional

  - `cs_p13_1`, `cs_p15`: nivel educativo y antecedente escolar

  - `clase2`: clase de actividad económica

  - `pos_ocu`, `tue2`: posición en la ocupación y tipo de unidad
    económica

  - `p2h4`: experiencia laboral previa

  - `p3i`, `p3j`, `p3j1`, `p3k1`: variables sobre tipo de contrato

## Value

Un data.frame con las variables originales y nuevas columnas:

- `sinco1d`, `sinco2d`, `sinco3d`, `sinco4d`

- `skill_level`, `skill_actual`

- `mismatch`

- `nunca_trabajo`, `status_seq`

- `contrato0`, `contrato1`, `temporal`, `temporal_seq`

## Details

La referencia estadística en años de escolaridad se calcula por separado
con
[`calcular_desajuste_estadistico()`](https://aniuxa.github.io/renoe/reference/calcular_desajuste_estadistico.md).
Esta separación evita construir una referencia aparentemente anual
cuando la entrada contiene un solo trimestre. Por compatibilidad, si la
entrada ya contiene `esco_norm` o `mismatch2`, esas columnas históricas
se conservan sin recalcularlas.

Además, genera variables relacionadas con la experiencia previa
(`nunca_trabajo`), el estatus laboral combinado (`status_seq`) y las
características contractuales (`contrato0`, `contrato1`, `temporal`,
`temporal_seq`) según el tipo de cuestionario.

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
[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md),
[`sinco2019_to_sinco2011()`](https://aniuxa.github.io/renoe/reference/sinco2019_to_sinco2011.md)
