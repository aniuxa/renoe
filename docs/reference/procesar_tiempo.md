# Procesar variables de tiempo en actividades del hogar y cuidado

Calcula duraciones semanales a partir de las baterías `p11_*` y `p9_*`
de ENOE. Conserva sin cambios los campos fuente y distingue duración
observada, actividad realizada con duración desconocida (98),
realización desconocida (99), reactivo no seleccionado y batería no
medible.

## Usage

``` r
procesar_tiempo(
  data,
  anio,
  trimestre,
  tratamiento_faltantes = c("distinguir", "historico_cero")
)
```

## Arguments

- data:

  Data frame fusionado por
  [`fusion_enoe()`](https://aniuxa.github.io/renoe/reference/fusion_enoe.md)
  o cargado directamente.

- anio:

  Año del trimestre, usado si falta `anio` en `data`.

- trimestre:

  Trimestre 1–4, usado si faltan metadatos en `data`.

- tratamiento_faltantes:

  Contrato de las variables principales: `"distinguir"` conserva los
  estados y `NA`; `"historico_cero"` reproduce la conversión histórica a
  cero. En ambos casos se crean columnas legacy.

## Value

El mismo data frame, en el mismo orden, con duraciones, estados de
medición, totales completos y parciales, y resultados históricos.

## Details

El orden de las actividades cambió en 2011. Hasta 2010, los reactivos 3
a 6 corresponden a construcción, reparación, quehaceres y servicios a la
comunidad. Desde 2011, los reactivos 3 y 4 corresponden a compras y
traslados, y las cuatro actividades anteriores pasan a los reactivos 5 a
8.

Las variables específicas `t_*` se expresan en horas. `t_total`,
`t_total0` y sus versiones parciales se expresan en minutos; los sufijos
`_hrs` son sus equivalentes en horas. Un total completo es `NA` cuando
contiene una actividad con duración desconocida, realización
desconocida, información incompleta o inválida. El total parcial suma
sólo las duraciones observadas y los ceros de reactivos no
seleccionados.

Las columnas `*_legacy` reproducen el contrato histórico, que convertía
a cero los códigos 98, 99 y todos los faltantes. El argumento
`tratamiento_faltantes = "historico_cero"` permite mantener
temporalmente ese resultado en los nombres principales.

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
[`procesar_variables_enoe()`](https://aniuxa.github.io/renoe/reference/procesar_variables_enoe.md),
[`procesar_vars_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_vars_hogar.md),
[`procesar_vars_laborales()`](https://aniuxa.github.io/renoe/reference/procesar_vars_laborales.md),
[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md),
[`sinco2019_to_sinco2011()`](https://aniuxa.github.io/renoe/reference/sinco2019_to_sinco2011.md)
