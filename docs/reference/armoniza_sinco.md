# Armonizar ocupaciones CMO, SINCO 2011 y SINCO 2019

Construye códigos comparables en SINCO 2011 a partir de CMO entre 2005-I
y 2012-II, SINCO 2011 observado entre 2012-III y 2021-II, y SINCO 2019
desde 2021-III. Para el último periodo utiliza la tabla de equivalencia
oficial SINCO 2011-2019 y conserva sin resolver las correspondencias
múltiples.

## Usage

``` r
armoniza_sinco(data, codigos = NULL, correspondencia_2019 = NULL)
```

## Arguments

- data:

  Data frame con `anio`, `trim` y `p3coe`.

- codigos:

  Tabla opcional de correspondencia CMO-SINCO usada antes de 2012-III.

- correspondencia_2019:

  Tabla opcional, en formato largo, del puente SINCO 2019-SINCO 2011.

## Value

El data frame con `sinco4d`, `sinco3d`, `sinco2d` y `sinco1d`
armonizados, además de variables de procedencia y calidad.

## References

INEGI (2020). *Sistema Nacional de Clasificación de Ocupaciones 2019*.
Anexo: Tabla de equivalencia SINCO 2011-2019.

Escoto Castillo, A. y Sánchez Peña, L. (2024). *El riesgo de
automatización en México: diferencias temporales y generacionales entre
las distintas ocupaciones*. CEPAL. <https://hdl.handle.net/11362/69015>

## See also

Other procesamiento_enoe:
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
[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md),
[`sinco2019_to_sinco2011()`](https://aniuxa.github.io/renoe/reference/sinco2019_to_sinco2011.md)

## Examples

``` r
datos <- data.frame(
  anio = c(2012, 2021, 2021),
  trim = c(3, 2, 3),
  p3coe = c(2436, 2423, 2433),
  pos_ocu = 1,
  tue2 = 1
)
armoniza_sinco(datos)
#>   anio trim p3coe pos_ocu tue2 cmo_4d sinco4d sinco3d codigo_ocupacion_original
#> 1 2012    3  2436       1    1   2436    2436     243                      2436
#> 2 2021    2  2423       1    1   2423    2423     242                      2423
#> 3 2021    3  2433       1    1   2433    2423     242                      2433
#>   version_sinco_origen sinco4d_base2011 n_destinos_sinco
#> 1           SINCO 2011             2436                1
#> 2           SINCO 2011             2423                1
#> 3           SINCO 2019             2423                1
#>           calidad_puente_sinco sinco2d sinco1d
#> 1         SINCO 2011 observado      24       2
#> 2         SINCO 2011 observado      24       2
#> 3 Equivalencia oficial directa      24       2
```
