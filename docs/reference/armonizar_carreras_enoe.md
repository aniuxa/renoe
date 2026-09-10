# Armonizar carreras de la ENOE entre 2005 y la actualidad

Identifica automáticamente el clasificador utilizado en cada
observación, normaliza la clave de carrera conservando ceros iniciales y
genera campos de formación comparables. Reconoce el Catálogo de Carreras
2005 hasta 2012-II, la CMPE 2011 entre 2012-III y 2021-II y la CMPE 2016
desde 2021-III.

## Usage

``` r
armonizar_carreras_enoe(data)
```

## Arguments

- data:

  Data frame con `anio`, `trim`, `cs_p13_1` y `cs_p14_c`.

## Value

El mismo data frame, sin cambiar el número de filas, con:
`clasificador_carrera`, `cs_p14_c_original`, `cs_p14_c_canonica`,
`nivel_carrera`, `grupo_2005`, `campo_cmpe2011`, `campo_cmpe2016`,
`campo_arm8`, `campo_arm8_desc`, `campo_arm10`, `campo_arm10_desc`,
`descripcion_2005_arm`, `calidad_armonizacion`, `elegible_carrera`,
`tiene_codigo_carrera` y `cobertura_arm8`.

## Details

La función conserva `cs_p14_c` y añade su versión original y canónica.
Las equivalencias del catálogo 2005 se aplican primero por clave
completa y sólo después por grupo. Las claves que mezclan campos
incompatibles permanecen explícitamente como ambiguas.

`campo_arm8` es la clasificación recomendada para comparaciones de toda
la serie. `campo_arm10` conserva mayor cercanía con los diez campos
amplios de la CMPE 2016. Los códigos se manejan como texto para
preservar sus ceros.

`campo_arm8` puede emplearse posteriormente para estudiar desajuste
horizontal, comparándolo con la ocupación. Esta función no calcula ese
indicador. El archivo `correspondencia_arm8_isco08_montt.csv` reproduce
la tabla normativa internacional de Montt (2015). El archivo
`correspondencia_campo_arm8_sinco3d.csv` contiene la adaptación mexicana
propuesta y versionada. Sus filas con `estado_revision = "propuesta"`
requieren validación sustantiva antes de considerarse definitivas.

## References

Montt, G. (2015). The causes and consequences of field-of-study
mismatch: An analysis using PIAAC. OECD Social, Employment and Migration
Working Papers, No. 167.
[doi:10.1787/5jrxm4dhv9r2-en](https://doi.org/10.1787/5jrxm4dhv9r2-en)

Wolbers, M. H. J. (2003). Job mismatches and their labour-market effects
among school-leavers in Europe. European Sociological Review, 19(3),
249-266.
[doi:10.1093/esr/19.3.249](https://doi.org/10.1093/esr/19.3.249)

International Labour Organization. Education and Mismatch Indicators.
<https://ilostat.ilo.org/methods/concepts-and-definitions/description-education-and-mismatch-indicators/>

Somers, M. A., Cabus, S. J., Groot, W., and van den Brink, H. M. (2019).
Horizontal mismatch between employment and field of education: Evidence
from a systematic literature review. Journal of Economic Surveys, 33(2),
567-603. [doi:10.1111/joes.12271](https://doi.org/10.1111/joes.12271)

## See also

Other procesamiento_enoe:
[`armoniza_sinco()`](https://aniuxa.github.io/renoe/reference/armoniza_sinco.md),
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
[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md),
[`sinco2019_to_sinco2011()`](https://aniuxa.github.io/renoe/reference/sinco2019_to_sinco2011.md)

## Examples

``` r
datos <- data.frame(
  anio = c(2012, 2012, 2021),
  trim = c("t2", "t3", "t3"),
  cs_p13_1 = c(7, 7, 7),
  cs_p14_c = c("3111", "5335", 41400)
)
armonizar_carreras_enoe(datos)
#>   anio trim cs_p13_1 cs_p14_c clasificador_carrera cs_p14_c_original
#> 1 2012   t2        7     3111        Carreras 2005              3111
#> 2 2012   t3        7     5335            CMPE 2011              5335
#> 3 2021   t3        7    41400            CMPE 2016             41400
#>   cs_p14_c_canonica nivel_carrera grupo_2005 campo_cmpe2011 campo_cmpe2016
#> 1              3111             7         31           <NA>           <NA>
#> 2              5335             7       <NA>            335           <NA>
#> 3            041400             7       <NA>           <NA>         041400
#>   campo_arm8 campo_arm10 descripcion_2005_arm       calidad_armonizacion
#> 1          5          07         Arquitectura Exacta por clave detallada
#> 2          3          04                 <NA>      Exacta a nivel amplio
#> 3          3          04                 <NA>      Exacta a nivel amplio
#>                               campo_arm8_desc
#> 1      Ingeniería, manufactura y construcción
#> 2 Ciencias sociales, administración y derecho
#> 3 Ciencias sociales, administración y derecho
#>                         campo_arm10_desc elegible_carrera tiene_codigo_carrera
#> 1 Ingeniería, manufactura y construcción             TRUE                 TRUE
#> 2              Administración y negocios             TRUE                 TRUE
#> 3              Administración y negocios             TRUE                 TRUE
#>   cobertura_arm8
#> 1           TRUE
#> 2           TRUE
#> 3           TRUE
```
