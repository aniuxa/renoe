# Clasificar ocupaciones para el capitulo de Gerardo Damian Hernandez

Traduce el codigo ocupacional armonizado a SINCO 2011 hacia ISCO-88 y
construye la tipologia de cuatro clases utilizada en la propuesta
*Veinte anos de trabajo femenino en Mexico*: trabajo no manual
calificado, no manual no calificado, manual calificado y manual no
calificado.

## Usage

``` r
procesar_clases_damian(
  data,
  correspondencia = NULL,
  recuperar_sin_isco = TRUE,
  usar_puente_cmo = NULL,
  escenario = c("integrated_accepted", "official_strict", "analysis_legacy"),
  legacy = NULL
)
```

## Arguments

- data:

  Un data frame con `sinco4d_base2011` o `sinco4d`, `pos_ocu` y
  `emple7c`. Cuando existe `sinco4d_base2011`, esta variable canonica
  tiene prioridad y no se aplica un puente alternativo.

- correspondencia:

  Tabla opcional con columnas `sinco4d` e `isco88`. Si se omite, se usa
  la correspondencia distribuida con el paquete.

- recuperar_sin_isco:

  Si es `TRUE`, clasifica casos sin equivalencia ISCO-88 mediante el
  gran grupo SINCO, siguiendo el do-file original.

- usar_puente_cmo:

  Si es `TRUE`, aplica antes de 2012-III el puente determinista de
  Damian usado por
  [`cmo_to_sinco11_care()`](https://aniuxa.github.io/renoe/reference/cmo_to_sinco11_care.md).
  Este puente conserva la primera regla del do-file y no constituye una
  equivalencia oficial o biunivoca. Por ello el valor por defecto es
  `FALSE`; su uso debe ser explicito y corresponde al escenario
  historico.

- escenario:

  Contrato publico de decision. `official_strict` excluye rescates
  condicionados; `integrated_accepted` agrega reglas EGP aceptadas;
  `analysis_legacy` agrega el puente CMO historico.

- legacy:

  Compatibilidad explicita para seleccionar `analysis_legacy`.

## Value

El mismo data frame con `isco88_damian`, `grupo_ocu9_damian`,
`clase_ocu_damian`, `calificada_damian`, `manual_damian`,
`supervisa_damian`, `clase_egp13_damian`, sus agrupaciones y variables
de cobertura y metodo.

## Details

La correspondencia SINCO 2011-ISCO-88 reproduce, en orden de prioridad,
el do-file de Gerardo Damian Hernandez `sinco-isco88.do`. Las variables
auxiliares permiten auditar la cobertura y distinguir asignaciones
directas de recuperaciones realizadas con el gran grupo SINCO.

## References

Damian Hernandez, G. (2026). *Quince anos de trabajo femenino en Mexico:
tipo de hogar y clase ocupacional, 2005-2020*. Propuesta de capitulo
para el proyecto PAPIIT IN305925.

Solis, P., Chavez Molina, E. y Cobos, D. (2019). Propuesta de adaptacion
del esquema EGP para America Latina, citada en el programa original.

## See also

Other procesamiento_enoe:
[`.armonizar_sinco_enoe_core()`](https://aniuxa.github.io/renoe/reference/dot-armonizar_sinco_enoe_core.md),
[`armonizar_carreras_enoe()`](https://aniuxa.github.io/renoe/reference/armonizar_carreras_enoe.md),
[`calcular_desajuste_estadistico()`](https://aniuxa.github.io/renoe/reference/calcular_desajuste_estadistico.md),
[`calcular_desajuste_horizontal()`](https://aniuxa.github.io/renoe/reference/calcular_desajuste_horizontal.md),
[`crear_folios()`](https://aniuxa.github.io/renoe/reference/crear_folios.md),
[`imputa_ingocup()`](https://aniuxa.github.io/renoe/reference/imputa_ingocup.md),
[`ipc_enoe()`](https://aniuxa.github.io/renoe/reference/ipc_enoe.md),
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
  sinco4d = c(2211, 4111, 7111, 9111, NA),
  pos_ocu = c(1, 1, 1, 1, NA),
  emple7c = c(5, 5, 5, 5, NA),
  clase2 = c(1, 1, 1, 1, 4)
)
procesar_clases_damian(datos)
#>   sinco4d pos_ocu emple7c clase2 sinco4d_damian             fuente_sinco_damian
#> 1    2211       1       5      1           2211 sinco4d proporcionado al modulo
#> 2    4111       1       5      1           4111 sinco4d proporcionado al modulo
#> 3    7111       1       5      1           7111 sinco4d proporcionado al modulo
#> 4    9111       1       5      1           9111 sinco4d proporcionado al modulo
#> 5      NA      NA      NA      4             NA sinco4d proporcionado al modulo
#>   calidad_cmo_damian n_destinos_cmo_damian isco88_damian grupo_ocu9_damian
#> 1               <NA>                    NA          2111                 2
#> 2               <NA>                    NA          5220                 4
#> 3               <NA>                    NA          7111                 7
#> 4               <NA>                    NA          9211                 9
#> 5               <NA>                    NA            NA                NA
#>   clase_ocu_damian          metodo_clase_damian cobertura_isco88_damian
#> 1                1 Correspondencia SINCO-ISCO88                       1
#> 2                2 Correspondencia SINCO-ISCO88                       1
#> 3                3 Correspondencia SINCO-ISCO88                       1
#> 4                4 Correspondencia SINCO-ISCO88                       1
#> 5               NA             Sin codigo SINCO                      NA
#>   cobertura_clase_damian calificada_damian manual_damian supervisa_damian
#> 1                      1                 1             0                0
#> 2                      1                 0             0                0
#> 3                      1                 1             1                0
#> 4                      1                 0             1                0
#> 5                     NA                NA            NA               NA
#>   posocup_damian tam_est_damian clase_egp13_damian       egp_escenario
#> 1              4              4                  1 integrated_accepted
#> 2              4              4                  4 integrated_accepted
#> 3              4              4                  8 integrated_accepted
#> 4              4              4                 13 integrated_accepted
#> 5             NA             NA                 NA integrated_accepted
#>              egp_regla_id egp_evidence_level egp_transportable
#> 1 EGP_DAMIAN_SINCO_ISCO88            derived              TRUE
#> 2 EGP_DAMIAN_SINCO_ISCO88            derived              TRUE
#> 3 EGP_DAMIAN_SINCO_ISCO88            derived              TRUE
#> 4 EGP_DAMIAN_SINCO_ISCO88            derived              TRUE
#> 5                    <NA>               <NA>              TRUE
#>   egp_motivo_no_clasificacion cobertura_egp_damian clase_egp7_damian
#> 1                        <NA>                    1                 1
#> 2                        <NA>                    1                 2
#> 3                        <NA>                    1                 4
#> 4                        <NA>                    1                 7
#> 5      fuera_universo_ocupado                   NA                NA
#>   clase_alt6_damian macro_egp4_damian macro_solis4_damian egp3_damian
#> 1                 1                 1                   1           1
#> 2                 3                 2                   1           2
#> 3                 3                 2                   2           2
#> 4                 6                 4                   4           3
#> 5                NA                NA                  NA          NA
#>   baja_damian alta_damian autoempleo_damian
#> 1           0           1                 0
#> 2           0           0                 0
#> 3           0           0                 0
#> 4           1           0                 0
#> 5          NA          NA                NA
```
