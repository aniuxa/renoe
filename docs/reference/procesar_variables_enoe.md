# Procesar variables clave de la ENOE en una sola función

Función envolvente (`wrapper`) que aplica en cadena varias funciones de
procesamiento sobre los microdatos de la ENOE. Crea variables
sociodemográficas, estructura del hogar, uso del tiempo, imputación de
ingresos e información del IPC.

## Usage

``` r
procesar_variables_enoe(data, anio, trimestre)
```

## Arguments

- data:

  Un data frame con las tablas fusionadas de la ENOE (por ejemplo,
  salida de
  [`fusion_enoe()`](https://aniuxa.github.io/renoe/reference/fusion_enoe.md)).

- anio:

  Año del trimestre (numérico).

- trimestre:

  Trimestre numérico (1–4).

## Value

Un data frame con variables sociodemográficas, estructura del hogar, uso
del tiempo, IPC y variables imputadas.

## Details

Aplica automáticamente las funciones:

- [`crear_folios()`](https://aniuxa.github.io/renoe/reference/crear_folios.md):
  Genera identificadores únicos de vivienda, hogar y persona.

- [`drop_tri()`](https://aniuxa.github.io/renoe/reference/drop_tri.md):
  Cuando se trata de la ENOEN, renombra automáticamente variables
  terminadas en `_tri` a su forma base (por ejemplo, `fac_tri` → `fac`).

- [`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md):
  Crea variables de edad, sexo y grupos etarios.

- [`procesar_vars_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_vars_hogar.md):
  Clasifica hogares por tipo, tamaño y dependencia.

- [`procesar_tiempo()`](https://aniuxa.github.io/renoe/reference/procesar_tiempo.md):
  Calcula minutos y horas en actividades del hogar y cuidado.

- [`ipc_enoe()`](https://aniuxa.github.io/renoe/reference/ipc_enoe.md):
  Añade una variable con el IPC nacional del trimestre correspondiente.

- [`imputa_ingocup()`](https://aniuxa.github.io/renoe/reference/imputa_ingocup.md):
  Imputa el ingreso ocupacional con `mice` para personas ocupadas.

- ` procesar_contribucion_hogar`: IProcesar contribución económica y de
  trabajo no remunerado al hogar.

## See also

[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md),
[`procesar_vars_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_vars_hogar.md),
[`ipc_enoe()`](https://aniuxa.github.io/renoe/reference/ipc_enoe.md),
[`imputa_ingocup()`](https://aniuxa.github.io/renoe/reference/imputa_ingocup.md)

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
[`procesar_vars_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_vars_hogar.md),
[`procesar_vars_laborales()`](https://aniuxa.github.io/renoe/reference/procesar_vars_laborales.md),
[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md),
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
