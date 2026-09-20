# Armonizar ocupaciones CMO, SINCO 2011 y SINCO 2019

Construye codigos comparables en SINCO 2011 a partir de CMO entre 2005-I
y 2012-II, SINCO 2011 observado entre 2012-III y 2021-II, y SINCO 2019
desde 2021-III. Para el ultimo periodo utiliza la tabla de equivalencia
oficial SINCO 2011-2019 y conserva sin resolver las correspondencias
multiples.

## Usage

``` r
.armonizar_sinco_enoe_core(
  data,
  codigos = NULL,
  correspondencia_2019 = NULL,
  usar_reglas_enoe = NULL,
  capas = c("oficial", "panel", "enoe", "consenso"),
  detalle = TRUE
)
```

## Arguments

- data:

  Data frame con `anio`, `trim` y `p3coe`.

- codigos:

  Tabla opcional de correspondencia CMO-SINCO usada antes de 2012-III.

- correspondencia_2019:

  Tabla opcional, en formato largo, del puente SINCO 2019-SINCO 2011.

- usar_reglas_enoe:

  Compatibilidad: TRUE activa todas las capas; FALSE selecciona solo
  oficial. NULL utiliza `capas`.

- capas:

  Capas habilitadas: `oficial`, `panel`, `enoe` y `consenso`, todas por
  defecto. Oficial siempre se incluye. Panel identifica reglas validadas
  longitudinalmente; enoe habilita reglas que requieren otras preguntas
  ENOE, incluido el puente historico a un digito.

  La capa consenso actua posteriormente en
  [`procesar_clasificaciones_reproducibles()`](https://aniuxa.github.io/renoe/reference/procesar_clasificaciones_reproducibles.md);
  no aumenta la desagregacion SINCO identificada por esta funcion.

- detalle:

  Si es `TRUE`, devuelve trazabilidad auditable. La salida compacta
  conserva calidad, regla aplicada y motivo de pendiente.

## Value

El data frame con `sinco4d`, `sinco3d`, `sinco2d` y `sinco1d`
armonizados, ademas de variables de procedencia y calidad.

## References

INEGI (2020). *Sistema Nacional de Clasificacion de Ocupaciones 2019*.
Anexo: Tabla de equivalencia SINCO 2011-2019.

Escoto Castillo, A. y Sanchez Pena, L. (2024). *El riesgo de
automatizacion en Mexico: diferencias temporales y generacionales entre
las distintas ocupaciones*. CEPAL. <https://hdl.handle.net/11362/69015>

## See also

Other procesamiento_enoe:
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
  anio = c(2012, 2021, 2021),
  trim = c(3, 2, 3),
  p3coe = c(2436, 2423, 2433),
  pos_ocu = 1,
  tue2 = 1
)
armonizar_sinco(datos)
#>   anio trim p3coe pos_ocu tue2 cmo_4d sinco4d sinco3d regla_cmo_sinco
#> 1 2012    3  2436       1    1   2436    2436     243            <NA>
#> 2 2021    2  2423       1    1   2423    2423     242            <NA>
#> 3 2021    3  2433       1    1   2433    2423     242            <NA>
#>   tipo_regla_cmo_sinco alcance_regla_cmo_sinco detalle_regla_cmo_sinco
#> 1                 <NA>                    <NA>                    <NA>
#> 2                 <NA>                    <NA>                    <NA>
#> 3                 <NA>                    <NA>                    <NA>
#>   n_destinos_regla_cmo_sinco codigo_ocupacion_original version_sinco_origen
#> 1                         NA                      2436           SINCO 2011
#> 2                         NA                      2423           SINCO 2011
#> 3                         NA                      2433           SINCO 2019
#>   sinco4d_base2011 n_destinos_sinco         calidad_puente_sinco sinco2d
#> 1             2436                1         SINCO 2011 observado      24
#> 2             2423                1         SINCO 2011 observado      24
#> 3             2423                1 Equivalencia oficial directa      24
#>   sinco1d nivel_maximo_sinco         capas_sinco_activas sinco_catalogo_destino
#> 1       2                 4d oficial+panel+enoe+consenso             SINCO 2011
#> 2       2                 4d oficial+panel+enoe+consenso             SINCO 2011
#> 3       2                 4d oficial+panel+enoe+consenso             SINCO 2011
#>   sinco_decision_status sinco_decision_phase sinco_evidence_level
#> 1                unique           integrated       official_exact
#> 2                unique           integrated       official_exact
#> 3                unique           integrated       official_exact
#>   sinco_decision_layer sinco_auxiliares_usados sinco_motivo_pendiente
#> 1             official                    <NA>                   <NA>
#> 2             official                    <NA>                   <NA>
#> 3             official                    <NA>                   <NA>
#>   sinco2011_decision_status version_sinco_destino sinco2011_granularidad
#> 1                    unique            SINCO 2011                      4
#> 2                    unique            SINCO 2011                      4
#> 3                    unique            SINCO 2011                      4
#>   sinco2011_nivel_sustentado sinco2011_codigo_especial sinco2011_comparable
#> 1                         4d                     FALSE                 TRUE
#> 2                         4d                     FALSE                 TRUE
#> 3                         4d                     FALSE                 TRUE
#>   sinco2011_apto_4d sinco2011_apto_3d sinco2011_apto_1d sinco2011_prioridad
#> 1              TRUE              TRUE              TRUE                   0
#> 2              TRUE              TRUE              TRUE                   0
#> 3              TRUE              TRUE              TRUE                   1
#>   sinco2011_decision_phase sinco2011_evidence_level
#> 1               integrated           official_exact
#> 2               integrated           official_exact
#> 3               integrated           official_exact
#>             sinco2011_regla_id sinco2011_condicion_auxiliar
#> 1           SINCO2011_OBSERVED                         <NA>
#> 2           SINCO2011_OBSERVED                         <NA>
#> 3 SINCO2019_OFFICIAL_UNIQUE_4D                         <NA>
#>   sinco2011_adaptador_auxiliar codigo_ocupacion_original_txt
#> 1                         <NA>                          2436
#> 2                         <NA>                          2423
#> 3                         <NA>                          2433
#>   sinco2011_destinos_oficiales_4d sinco2011_destino_en_puente_oficial
#> 1                            2436                                TRUE
#> 2                            2423                                TRUE
#> 3                            2423                                TRUE
```
