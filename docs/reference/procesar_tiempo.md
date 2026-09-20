# Procesar variables de tiempo en actividades del hogar y cuidado

Calcula duraciones semanales a partir de las baterias `p11_*`
(cuestionario ampliado) y `p9_*` (cuestionario basico) de la ENOE.
Conserva los campos fuente y distingue duracion observada, actividad
realizada con duracion desconocida (98), realizacion desconocida (99),
reactivo no seleccionado y reactivo que no existe en la version del
instrumento.

## Usage

``` r
procesar_tiempo(data, anio, trimestre)
```

## Arguments

- data:

  Data frame fusionado por
  [`fusion_enoe()`](https://aniuxa.github.io/renoe/reference/fusion_enoe.md)
  o cargado directamente.

- anio:

  Ano del trimestre, usado si falta `anio` en `data`.

- trimestre:

  Trimestre 1-4, usado si faltan metadatos en `data`.

## Value

El mismo data frame, en el mismo orden, con duraciones, estados de
medicion, version del instrumento y agregados conceptuales.

## Details

La bateria cambio en 2013. Hasta 2012 contiene seis actividades y el
reactivo de cuidado incluye los traslados. Desde 2013 contiene ocho:
separa traslados del cuidado y agrega compras, cuentas, tramites y
seguridad del hogar.

Todas las duraciones derivadas se expresan en horas. `t_cuidado_directo`
solo es identificable desde 2013. `t_cuidado_amplio` armoniza el
contenido anterior sumando cuidado y traslado desde 2013.
`t_trabajo_hogar_indirecto_armonizado` usa construccion, reparacion y
quehaceres. `t_trabajo_hogar_armonizado` suma cuidado amplio y trabajo
indirecto realizado para el propio hogar. Los servicios comunitarios se
conservan en `t_comun`, pero no integran estas sumas. La armonizacion no
elimina la ruptura de medicion observada en 2013; `t_total_instrumento`
suma todos los reactivos no educativos disponibles en cada version y,
por ello, no debe usarse como serie homogenea.

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
[`procesar_libro1()`](https://aniuxa.github.io/renoe/reference/procesar_libro1.md),
[`procesar_variables_enoe()`](https://aniuxa.github.io/renoe/reference/procesar_variables_enoe.md),
[`procesar_vars_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_vars_hogar.md),
[`procesar_vars_laborales()`](https://aniuxa.github.io/renoe/reference/procesar_vars_laborales.md),
[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md),
[`reglas_enoe()`](https://aniuxa.github.io/renoe/reference/reglas_enoe.md),
[`sinco2019_to_sinco2011()`](https://aniuxa.github.io/renoe/reference/sinco2019_to_sinco2011.md)
