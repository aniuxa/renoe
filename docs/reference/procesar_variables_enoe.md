# Procesar variables clave de la ENOE en una sola funcion

Ruta canonica que aplica en un orden fijo las transformaciones
utilizadas por los productos del libro. Conserva filas y orden y valida
la llave de persona al terminar.

## Usage

``` r
procesar_variables_enoe(
  data,
  anio,
  trimestre,
  semilla = 1234,
  perfil_carreras = c("panel_validado", "oficial", "experimental"),
  usar_puente_2005 = FALSE,
  escenario_clasificadores = c("integrated_accepted", "official_strict",
    "analysis_legacy")
)
```

## Arguments

- data:

  Un data frame con las tablas fusionadas de la ENOE (por ejemplo,
  salida de
  [`fusion_enoe()`](https://aniuxa.github.io/renoe/reference/fusion_enoe.md)).

- anio:

  Año del trimestre (numerico).

- trimestre:

  Trimestre numerico (1-4).

- semilla:

  Semilla de la imputacion de ingreso.

- perfil_carreras:

  Perfil de evidencia para armonizar carreras.

- usar_puente_2005:

  Si se permite el puente experimental de carreras de 2005. Por defecto
  es `FALSE`.

- escenario_clasificadores:

  Escenario explicito para SINCO y sus consumidores.

## Value

Un data frame con variables sociodemograficas, estructura del hogar, uso
del tiempo, IPC y variables imputadas.

## Details

Aplica automaticamente las funciones:

- [`drop_tri()`](https://aniuxa.github.io/renoe/reference/drop_tri.md):
  Cuando se trata de la ENOEN, renombra automaticamente variables
  terminadas en `_tri` a su forma base (por ejemplo, `fac_tri` ? `fac`).

- [`crear_folios()`](https://aniuxa.github.io/renoe/reference/crear_folios.md):
  Genera identificadores unicos de vivienda, hogar y persona.

- [`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md):
  Crea variables de edad, sexo y grupos etarios.

- [`procesar_vars_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_vars_hogar.md):
  Clasifica hogares por tipo, tamano y dependencia.

- [`armonizar_carreras()`](https://aniuxa.github.io/renoe/reference/armonizar_carreras_enoe.md),
  [`procesar_vars_laborales()`](https://aniuxa.github.io/renoe/reference/procesar_vars_laborales.md),
  [`calcular_desajuste_estadistico()`](https://aniuxa.github.io/renoe/reference/calcular_desajuste_estadistico.md)
  y
  [`calcular_desajuste_horizontal()`](https://aniuxa.github.io/renoe/reference/calcular_desajuste_horizontal.md):
  armonizan educacion y trabajo.

- [`procesar_tiempo()`](https://aniuxa.github.io/renoe/reference/procesar_tiempo.md):
  Calcula horas en actividades del hogar y cuidado, con corte
  instrumental explicito en 2013.

- [`ipc_enoe()`](https://aniuxa.github.io/renoe/reference/ipc_enoe.md):
  Anade una variable con el IPC nacional del trimestre correspondiente.

- [`imputa_ingocup()`](https://aniuxa.github.io/renoe/reference/imputa_ingocup.md):
  Imputa el ingreso ocupacional con `mice` para personas ocupadas.

- [`procesar_contribucion_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_contribucion_hogar.md)
  y
  [`procesar_cuidado_extra()`](https://aniuxa.github.io/renoe/reference/procesar_cuidado_extra.md):
  agregan recursos, tiempos y capacidad del hogar.

- [`procesar_estudio_trabajo()`](https://aniuxa.github.io/renoe/reference/procesar_estudio_trabajo.md),
  [`procesar_libro1()`](https://aniuxa.github.io/renoe/reference/procesar_libro1.md)
  y
  [`procesar_clasificaciones_reproducibles()`](https://aniuxa.github.io/renoe/reference/procesar_clasificaciones_reproducibles.md):
  construyen las salidas finales.

## See also

[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md),
[`procesar_vars_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_vars_hogar.md),
[`ipc_enoe()`](https://aniuxa.github.io/renoe/reference/ipc_enoe.md),
[`imputa_ingocup()`](https://aniuxa.github.io/renoe/reference/imputa_ingocup.md)

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
[`procesar_tiempo()`](https://aniuxa.github.io/renoe/reference/procesar_tiempo.md),
[`procesar_vars_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_vars_hogar.md),
[`procesar_vars_laborales()`](https://aniuxa.github.io/renoe/reference/procesar_vars_laborales.md),
[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md),
[`reglas_enoe()`](https://aniuxa.github.io/renoe/reference/reglas_enoe.md),
[`sinco2019_to_sinco2011()`](https://aniuxa.github.io/renoe/reference/sinco2019_to_sinco2011.md)

## Examples

``` r
if (FALSE) { # \dontrun{
datos <- fusion_enoe(2022, 1)
datos_proc <- procesar_variables_enoe(datos, 2022, 1)
dplyr::glimpse(datos_proc)
table(datos_proc$tipo_hog_lab, useNA = "always")
} # }
```
