# Procesar variables de estructura del hogar en la ENOE

Calcula variables derivadas sobre la composición y estructura de los
hogares a partir de los microdatos de la ENOE. Esta función requiere que
previamente se hayan generado variables sociodemográficas mediante
[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md).

## Usage

``` r
procesar_vars_hogar(data, anio, trimestre)
```

## Arguments

- data:

  Un data frame con variables como `par_c`, `edad`, `sexo`, `folio2`,
  previamente procesadas por
  [`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md).

- anio:

  Año de referencia de los datos.

- trimestre:

  Trimestre de referencia de los datos (número del 1 al 4).

## Value

Un data frame con variables derivadas de estructura del hogar y
composición demográfica, etiquetadas.

## Details

Incluye:

- Clasificación de parentesco (`relative`), ajustada al catálogo
  correspondiente según el periodo

- Tipologías de hogares (familiares, extensos, compuestos, etc.)

- Tamaño del hogar y tasas de dependencia (menores, mayores y total)

- Conteo de integrantes por grupo etario

- Indicadores dicotómicos de presencia de grupos clave (niñez, juventud,
  adultez mayor)

Las variables generadas permiten construir tipologías familiares,
caracterizar hogares según su composición y analizar necesidades de
cuidado o dependencia demográfica.

La variable `tam_hog` se calcula excluyendo al servicio doméstico y a
sus familiares, identificados en el catálogo de `par_c` mediante
`relative == 7`.

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
[`procesar_vars_laborales()`](https://aniuxa.github.io/renoe/reference/procesar_vars_laborales.md),
[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md),
[`sinco2019_to_sinco2011()`](https://aniuxa.github.io/renoe/reference/sinco2019_to_sinco2011.md)

## Examples

``` r
if (FALSE) { # \dontrun{
datos <- fusion_enoe(2021, 1)
datos <- procesar_vars_sociodemo(datos, anio = 2021, trimestre = 1)
datos <- procesar_vars_hogar(datos, anio = 2021, trimestre = 1)
table(datos$tipo_hog_lab, useNA = "always")
} # }
```
