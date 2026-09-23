# Armonizar carreras de la ENOE entre 2005 y la actualidad

Identifica automaticamente el clasificador utilizado en cada
observacion, normaliza la clave de carrera conservando ceros iniciales y
genera campos de formacion comparables. Reconoce el Catalogo de Carreras
2005 hasta 2012-II, la CMPE 2011 entre 2012-III y 2021-II y la CMPE 2016
desde 2021-III.

## Usage

``` r
armonizar_carreras_enoe(
  data,
  usar_puente_2005 = FALSE,
  perfil = c("panel_validado", "oficial", "experimental"),
  advertir_cobertura = TRUE,
  salida = c("auditable", "compacta")
)

armonizar_carreras(
  data,
  usar_puente_2005 = FALSE,
  perfil = c("panel_validado", "oficial", "experimental"),
  advertir_cobertura = TRUE,
  salida = c("auditable", "compacta")
)
```

## Arguments

- data:

  Data frame con `anio`, `trim`, `cs_p13_1` y `cs_p14_c`.

- usar_puente_2005:

  Si es `TRUE`, aplica los destinos preferentes aceptados del puente
  Carreras 2005 -\> CMPE 2011. El valor predeterminado es `FALSE`,
  porque para desajuste horizontal se recomienda comenzar en 2012-III.
  Aun con `FALSE`, la funcion informa la evidencia disponible por codigo
  para facilitar futuras revisiones y desempates.

- perfil:

  Ruta de evidencia que puede integrarse: `"oficial"` conserva
  unicamente la CMPE 2011 observada; `"panel_validado"` anade los
  destinos aceptados por panel; `"experimental"` anade consensos a ARM8
  cuando todos los destinos detallados plausibles pertenecen al mismo
  campo. El perfil experimental no inventa una carrera CMPE 2011
  detallada.

- advertir_cobertura:

  Si es `TRUE`, valor predeterminado, emite en cada aplicacion una
  advertencia con la cobertura ARM8 por ano dentro de la serie historica
  recomendada desde 2014-III. Usa `fac` cuando esta disponible y
  registros en caso contrario.

- salida:

  `"auditable"` conserva todas las columnas de evidencia; `"compacta"`
  conserva las variables recibidas y el contrato minimo de calidad,
  regla, fase, evidencia, granularidad, auxiliares y pendientes.

## Value

El mismo data frame, sin cambiar el numero de filas, con:
`clasificador_carrera`, `cs_p14_c_original`, `cs_p14_c_canonica`,
`nivel_carrera`, `grupo_2005`, `campo_cmpe2011`, `campo_cmpe2016`,
`campo_arm8`, `campo_arm8_desc`, `campo_arm10`, `campo_arm10_desc`,
`descripcion_2005_arm`, `calidad_armonizacion`, `elegible_carrera`,
`tiene_codigo_carrera`, `cobertura_arm8`, `carrera_cmpe2011`,
`campo_arm8_cmpe2011`, `campo_arm8_horizontal`,
`fuente_arm8_horizontal`, `nivel_cobertura_codigo`, proporciones
modales, soporte, destinos posibles, `perfil_armonizacion_carrera`,
fuente y nivel del resultado, `carrera_asistida_por_ocupacion`,
`apta_carrera_para_desajuste_horizontal` y `uso_desajuste_horizontal`.

## Details

La funcion conserva `cs_p14_c` y anade su version original y canonica.
Las equivalencias del catalogo 2005 se aplican primero por clave
completa y solo despues por grupo. Las claves que mezclan campos
incompatibles permanecen explicitamente como ambiguas.

`campo_arm8` conserva la salida historica de la funcion. Para el
desajuste horizontal se recomienda `campo_arm8_horizontal`: usa CMPE
2011 nativa y, desde 2021-III, agrega directamente el prefijo oficial de
CMPE 2016 a ARM8, sin imputar una carrera detallada CMPE 2011 ni usar
ocupacion. La variable `campo_arm8_cmpe2011` se conserva para analisis
que si requieren el puente detallado entre clasificadores. `campo_arm10`
conserva mayor cercania con los diez campos amplios de la CMPE 2016. Los
codigos se manejan como texto para preservar sus ceros.

`nivel_cobertura_codigo` distingue catalogo nativo, puente con
proporciones modales de 80% o mas, puente aceptado entre 65% y 79%, y
codigos sin regla aceptada. Las proporciones son evidencia empirica del
destino modal por codigo, no probabilidades individuales ni
equivalencias oficiales.

`campo_arm8` puede emplearse posteriormente para estudiar desajuste
horizontal, comparandolo con la ocupacion. Esta funcion no calcula ese
indicador. El archivo `correspondencia_arm8_isco08_montt.csv` reproduce
la tabla normativa internacional de Montt (2015). El archivo
`correspondencia_campo_arm8_sinco3d.csv` contiene la adaptacion mexicana
aceptada y versionada. La version 1.0.0 conserva 216 relaciones con
`estado_revision = "aceptada"`; el indicador rechaza matrices
propuestas.

La ruta integrada de carreras se construyo sin ocupacion, CMO ni SINCO.
No deben usarse codigos SINCO observados o imputados para completar una
carrera que despues se comparara con SINCO: ello introduce circularidad
mecanica. Aun una regla asistida por SINCO observado debera marcar
`carrera_asistida_por_ocupacion = TRUE` y excluirse del indicador
principal; solo podra presentarse como sensibilidad. El orden canonico
del proceso es armonizar carreras, armonizar SCIAN y finalmente
armonizar SINCO. SCIAN puede auxiliar la ultima etapa si se conserva la
procedencia, pero la carrera no puede escoger el SINCO usado por el
indicador principal.

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
[`.armonizar_sinco_enoe_core()`](https://aniuxa.github.io/renoe/reference/dot-armonizar_sinco_enoe_core.md),
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
  anio = c(2012, 2012, 2021),
  trim = c("t2", "t3", "t3"),
  cs_p13_1 = c(7, 7, 7),
  cs_p14_c = c("3111", "5335", 41400)
)
armonizar_carreras_enoe(datos)
#> Warning: Cobertura del perfil 'panel_validado' para la serie 2014-III+:
#> 2021: ARM8 100.00% clasificado (0.00% sin clasificar)
#> Advertencia de circularidad: no use SINCO observado o imputado para completar carreras que después se compararán con SINCO. El lookup integrado de carreras no usa ocupación ni SINCO.
#>   anio trim cs_p13_1 cs_p14_c clasificador_carrera cs_p14_c_original
#> 1 2012   t2        7     3111        Carreras 2005              3111
#> 2 2012   t3        7     5335            CMPE 2011              5335
#> 3 2021   t3        7    41400            CMPE 2016             41400
#>   cs_p14_c_canonica nivel_observacion_carrera nivel_carrera grupo_2005
#> 1              3111   Carreras 2005 detallado             7         31
#> 2              5335       CMPE 2011 detallado             7       <NA>
#> 3            041400       CMPE 2016 detallado             7       <NA>
#>   campo_cmpe2011 campo_cmpe2016 campo_arm8 campo_arm10 descripcion_2005_arm
#> 1           <NA>           <NA>          5          07         Arquitectura
#> 2            335           <NA>          3          04                 <NA>
#> 3           <NA>         041400          3          04                 <NA>
#>   destinos_cmpe2011_posibles estado_correspondencia_cmpe2011
#> 1                        531                       preferred
#> 2                        335                          unique
#> 3                        334                       preferred
#>   nivel_evidencia_cmpe2011 fase_correspondencia_cmpe2011 soporte_codigo_n
#> 1                    panel                      accepted             1000
#> 2           official_exact                    integrated               NA
#> 3                    panel                      accepted             4609
#>   soporte_codigo_poblacion proporcion_modal_codigo_registros
#> 1                   255424                         0.9930000
#> 2                       NA                         1.0000000
#> 3                  1235719                         0.9659362
#>   proporcion_modal_codigo_poblacion paneles_soporte_codigo
#> 1                         0.9979642                      4
#> 2                         1.0000000                     NA
#> 3                         0.9680728                      4
#>   nivel_cobertura_codigo regla_correspondencia_cmpe2011 version_regla
#> 1          alta_80_o_mas          CAR-CMPE2011-P65-0019    2026-09-13
#> 2        nativa_cmpe2011            CAR-CMPE2011-ID-335    2026-09-13
#> 3          alta_80_o_mas          CAR-CMPE2011-P65-0121    2026-09-13
#>   puente_2005_aplicado carrera_cmpe2011 campo_arm8_cmpe2011
#> 1                FALSE             <NA>                <NA>
#> 2                FALSE              335                   3
#> 3                FALSE              334                   3
#>   campo_arm8_horizontal   fuente_arm8_horizontal perfil_armonizacion_carrera
#> 1                  <NA>           sin_clasificar              panel_validado
#> 2                     3          cmpe2011_nativo              panel_validado
#> 3                     3 cmpe2016_nativo_agregado              panel_validado
#>   carrera_asistida_por_ocupacion en_serie_historica_horizontal
#> 1                          FALSE                         FALSE
#> 2                          FALSE                         FALSE
#> 3                          FALSE                          TRUE
#>                  segmento_serie_horizontal fuente_armonizacion_carrera
#> 1                    fuera_serie_principal         sin_regla_integrada
#> 2                    fuera_serie_principal           oficial_observada
#> 3 cmpe2016_cambio_clasificador_2021III_mas              panel_validado
#>   nivel_resultado_carrera apta_carrera_para_desajuste_horizontal
#> 1          sin_clasificar                                  FALSE
#> 2      cmpe2011_detallado                                  FALSE
#> 3      cmpe2011_detallado                                   TRUE
#>                                                             uso_desajuste_horizontal
#> 1                           Sólo sensibilidad: fuera de la serie principal 2014-III+
#> 2                           Sólo sensibilidad: fuera de la serie principal 2014-III+
#> 3 Continuidad ARM8 con marca de cambio CMPE 2016; validar la comparabilidad temporal
#>   carrera_catalogo_origen carrera_catalogo_destino carrera_codigo_original
#> 1           Carreras 2005                CMPE 2011                    3111
#> 2               CMPE 2011                CMPE 2011                    5335
#> 3               CMPE 2016                CMPE 2011                   41400
#>   carrera_codigo_armonizado_3d carrera_codigo_armonizado_arm8
#> 1                         <NA>                           <NA>
#> 2                          335                              3
#> 3                          334                              3
#>   carrera_nivel_maximo_sustentado carrera_decision_status
#> 1                  sin_clasificar               preferred
#> 2              cmpe2011_detallado                  unique
#> 3              cmpe2011_detallado               preferred
#>   carrera_decision_phase carrera_evidence_level      carrera_regla_id
#> 1               accepted                  panel CAR-CMPE2011-P65-0019
#> 2             integrated               observed     CMPE2011_OBSERVED
#> 3               accepted                  panel CAR-CMPE2011-P65-0121
#>   carrera_decision_layer carrera_auxiliares_usados carrera_destinos_plausibles
#> 1             unresolved     ninguno_en_aplicacion                         531
#> 2        native_observed     ninguno_en_aplicacion                         335
#> 3        panel_crosswalk     ninguno_en_aplicacion                         334
#>                carrera_motivo_pendiente       calidad_armonizacion
#> 1 destinos_multiples_sin_regla_aceptada Exacta por clave detallada
#> 2                                  <NA>      Exacta a nivel amplio
#> 3                                  <NA>      Exacta a nivel amplio
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
armonizar_carreras_enoe(datos, usar_puente_2005 = TRUE)
#> Warning: Cobertura del perfil 'panel_validado' para la serie 2014-III+:
#> 2021: ARM8 100.00% clasificado (0.00% sin clasificar)
#> Advertencia de circularidad: no use SINCO observado o imputado para completar carreras que después se compararán con SINCO. El lookup integrado de carreras no usa ocupación ni SINCO.
#>   anio trim cs_p13_1 cs_p14_c clasificador_carrera cs_p14_c_original
#> 1 2012   t2        7     3111        Carreras 2005              3111
#> 2 2012   t3        7     5335            CMPE 2011              5335
#> 3 2021   t3        7    41400            CMPE 2016             41400
#>   cs_p14_c_canonica nivel_observacion_carrera nivel_carrera grupo_2005
#> 1              3111   Carreras 2005 detallado             7         31
#> 2              5335       CMPE 2011 detallado             7       <NA>
#> 3            041400       CMPE 2016 detallado             7       <NA>
#>   campo_cmpe2011 campo_cmpe2016 campo_arm8 campo_arm10 descripcion_2005_arm
#> 1           <NA>           <NA>          5          07         Arquitectura
#> 2            335           <NA>          3          04                 <NA>
#> 3           <NA>         041400          3          04                 <NA>
#>   destinos_cmpe2011_posibles estado_correspondencia_cmpe2011
#> 1                        531                       preferred
#> 2                        335                          unique
#> 3                        334                       preferred
#>   nivel_evidencia_cmpe2011 fase_correspondencia_cmpe2011 soporte_codigo_n
#> 1                    panel                      accepted             1000
#> 2           official_exact                    integrated               NA
#> 3                    panel                      accepted             4609
#>   soporte_codigo_poblacion proporcion_modal_codigo_registros
#> 1                   255424                         0.9930000
#> 2                       NA                         1.0000000
#> 3                  1235719                         0.9659362
#>   proporcion_modal_codigo_poblacion paneles_soporte_codigo
#> 1                         0.9979642                      4
#> 2                         1.0000000                     NA
#> 3                         0.9680728                      4
#>   nivel_cobertura_codigo regla_correspondencia_cmpe2011 version_regla
#> 1          alta_80_o_mas          CAR-CMPE2011-P65-0019    2026-09-13
#> 2        nativa_cmpe2011            CAR-CMPE2011-ID-335    2026-09-13
#> 3          alta_80_o_mas          CAR-CMPE2011-P65-0121    2026-09-13
#>   puente_2005_aplicado carrera_cmpe2011 campo_arm8_cmpe2011
#> 1                 TRUE              531                   5
#> 2                FALSE              335                   3
#> 3                FALSE              334                   3
#>   campo_arm8_horizontal   fuente_arm8_horizontal perfil_armonizacion_carrera
#> 1                     5     puente_2005_cmpe2011              panel_validado
#> 2                     3          cmpe2011_nativo              panel_validado
#> 3                     3 cmpe2016_nativo_agregado              panel_validado
#>   carrera_asistida_por_ocupacion en_serie_historica_horizontal
#> 1                          FALSE                         FALSE
#> 2                          FALSE                         FALSE
#> 3                          FALSE                          TRUE
#>                  segmento_serie_horizontal fuente_armonizacion_carrera
#> 1                    fuera_serie_principal              panel_validado
#> 2                    fuera_serie_principal           oficial_observada
#> 3 cmpe2016_cambio_clasificador_2021III_mas              panel_validado
#>   nivel_resultado_carrera apta_carrera_para_desajuste_horizontal
#> 1      cmpe2011_detallado                                  FALSE
#> 2      cmpe2011_detallado                                  FALSE
#> 3      cmpe2011_detallado                                   TRUE
#>                                                             uso_desajuste_horizontal
#> 1                           Sólo sensibilidad: fuera de la serie principal 2014-III+
#> 2                           Sólo sensibilidad: fuera de la serie principal 2014-III+
#> 3 Continuidad ARM8 con marca de cambio CMPE 2016; validar la comparabilidad temporal
#>   carrera_catalogo_origen carrera_catalogo_destino carrera_codigo_original
#> 1           Carreras 2005                CMPE 2011                    3111
#> 2               CMPE 2011                CMPE 2011                    5335
#> 3               CMPE 2016                CMPE 2011                   41400
#>   carrera_codigo_armonizado_3d carrera_codigo_armonizado_arm8
#> 1                          531                              5
#> 2                          335                              3
#> 3                          334                              3
#>   carrera_nivel_maximo_sustentado carrera_decision_status
#> 1              cmpe2011_detallado               preferred
#> 2              cmpe2011_detallado                  unique
#> 3              cmpe2011_detallado               preferred
#>   carrera_decision_phase carrera_evidence_level      carrera_regla_id
#> 1               accepted                  panel CAR-CMPE2011-P65-0019
#> 2             integrated               observed     CMPE2011_OBSERVED
#> 3               accepted                  panel CAR-CMPE2011-P65-0121
#>   carrera_decision_layer carrera_auxiliares_usados carrera_destinos_plausibles
#> 1        panel_crosswalk     ninguno_en_aplicacion                         531
#> 2        native_observed     ninguno_en_aplicacion                         335
#> 3        panel_crosswalk     ninguno_en_aplicacion                         334
#>   carrera_motivo_pendiente       calidad_armonizacion
#> 1                     <NA> Exacta por clave detallada
#> 2                     <NA>      Exacta a nivel amplio
#> 3                     <NA>      Exacta a nivel amplio
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
armonizar_carreras_enoe(datos, perfil = "experimental")
#> Warning: Cobertura del perfil 'experimental' para la serie 2014-III+:
#> 2021: ARM8 100.00% clasificado (0.00% sin clasificar)
#> Advertencia de circularidad: no use SINCO observado o imputado para completar carreras que después se compararán con SINCO. El lookup integrado de carreras no usa ocupación ni SINCO.
#>   anio trim cs_p13_1 cs_p14_c clasificador_carrera cs_p14_c_original
#> 1 2012   t2        7     3111        Carreras 2005              3111
#> 2 2012   t3        7     5335            CMPE 2011              5335
#> 3 2021   t3        7    41400            CMPE 2016             41400
#>   cs_p14_c_canonica nivel_observacion_carrera nivel_carrera grupo_2005
#> 1              3111   Carreras 2005 detallado             7         31
#> 2              5335       CMPE 2011 detallado             7       <NA>
#> 3            041400       CMPE 2016 detallado             7       <NA>
#>   campo_cmpe2011 campo_cmpe2016 campo_arm8 campo_arm10 descripcion_2005_arm
#> 1           <NA>           <NA>          5          07         Arquitectura
#> 2            335           <NA>          3          04                 <NA>
#> 3           <NA>         041400          3          04                 <NA>
#>   destinos_cmpe2011_posibles estado_correspondencia_cmpe2011
#> 1                        531                       preferred
#> 2                        335                          unique
#> 3                        334                       preferred
#>   nivel_evidencia_cmpe2011 fase_correspondencia_cmpe2011 soporte_codigo_n
#> 1                    panel                      accepted             1000
#> 2           official_exact                    integrated               NA
#> 3                    panel                      accepted             4609
#>   soporte_codigo_poblacion proporcion_modal_codigo_registros
#> 1                   255424                         0.9930000
#> 2                       NA                         1.0000000
#> 3                  1235719                         0.9659362
#>   proporcion_modal_codigo_poblacion paneles_soporte_codigo
#> 1                         0.9979642                      4
#> 2                         1.0000000                     NA
#> 3                         0.9680728                      4
#>   nivel_cobertura_codigo regla_correspondencia_cmpe2011 version_regla
#> 1          alta_80_o_mas          CAR-CMPE2011-P65-0019    2026-09-13
#> 2        nativa_cmpe2011            CAR-CMPE2011-ID-335    2026-09-13
#> 3          alta_80_o_mas          CAR-CMPE2011-P65-0121    2026-09-13
#>   puente_2005_aplicado carrera_cmpe2011 campo_arm8_cmpe2011
#> 1                FALSE             <NA>                <NA>
#> 2                FALSE              335                   3
#> 3                FALSE              334                   3
#>   campo_arm8_horizontal   fuente_arm8_horizontal perfil_armonizacion_carrera
#> 1                  <NA>           sin_clasificar                experimental
#> 2                     3          cmpe2011_nativo                experimental
#> 3                     3 cmpe2016_nativo_agregado                experimental
#>   carrera_asistida_por_ocupacion en_serie_historica_horizontal
#> 1                          FALSE                         FALSE
#> 2                          FALSE                         FALSE
#> 3                          FALSE                          TRUE
#>                  segmento_serie_horizontal fuente_armonizacion_carrera
#> 1                    fuera_serie_principal         sin_regla_integrada
#> 2                    fuera_serie_principal           oficial_observada
#> 3 cmpe2016_cambio_clasificador_2021III_mas              panel_validado
#>   nivel_resultado_carrera apta_carrera_para_desajuste_horizontal
#> 1          sin_clasificar                                  FALSE
#> 2      cmpe2011_detallado                                  FALSE
#> 3      cmpe2011_detallado                                   TRUE
#>                                                             uso_desajuste_horizontal
#> 1                           Sólo sensibilidad: fuera de la serie principal 2014-III+
#> 2                           Sólo sensibilidad: fuera de la serie principal 2014-III+
#> 3 Continuidad ARM8 con marca de cambio CMPE 2016; validar la comparabilidad temporal
#>   carrera_catalogo_origen carrera_catalogo_destino carrera_codigo_original
#> 1           Carreras 2005                CMPE 2011                    3111
#> 2               CMPE 2011                CMPE 2011                    5335
#> 3               CMPE 2016                CMPE 2011                   41400
#>   carrera_codigo_armonizado_3d carrera_codigo_armonizado_arm8
#> 1                         <NA>                           <NA>
#> 2                          335                              3
#> 3                          334                              3
#>   carrera_nivel_maximo_sustentado carrera_decision_status
#> 1                  sin_clasificar               preferred
#> 2              cmpe2011_detallado                  unique
#> 3              cmpe2011_detallado               preferred
#>   carrera_decision_phase carrera_evidence_level      carrera_regla_id
#> 1               accepted                  panel CAR-CMPE2011-P65-0019
#> 2             integrated               observed     CMPE2011_OBSERVED
#> 3               accepted                  panel CAR-CMPE2011-P65-0121
#>   carrera_decision_layer carrera_auxiliares_usados carrera_destinos_plausibles
#> 1             unresolved     ninguno_en_aplicacion                         531
#> 2        native_observed     ninguno_en_aplicacion                         335
#> 3        panel_crosswalk     ninguno_en_aplicacion                         334
#>                carrera_motivo_pendiente       calidad_armonizacion
#> 1 destinos_multiples_sin_regla_aceptada Exacta por clave detallada
#> 2                                  <NA>      Exacta a nivel amplio
#> 3                                  <NA>      Exacta a nivel amplio
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
