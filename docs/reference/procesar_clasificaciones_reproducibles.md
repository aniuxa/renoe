# Procesar la cascada de clasificaciones reproducibles

Ejecuta separadamente, para cada clasificacion consumidora de ocupacion,
la precedencia acordada: oficial, panel, auxiliares ENOE/SCIAN, consenso
y reglas de la autora o el autor. Una capa solo completa valores
faltantes y nunca reescribe una asignacion de una capa anterior.

## Usage

``` r
procesar_clasificaciones_reproducibles(
  data,
  correspondencia_damian = NULL,
  puente_2019 = NULL,
  reglas_consenso = NULL,
  escenario = c("integrated_accepted", "official_strict", "analysis_legacy")
)
```

## Arguments

- data:

  Microdatos que ya contienen la salida detallada de
  [`armonizar_sinco()`](https://aniuxa.github.io/renoe/reference/armonizar_sinco.md).

- correspondencia_damian:

  Tabla opcional SINCO 2011-ISCO88.

- puente_2019:

  Tabla oficial larga SINCO 2019-SINCO 2011.

- reglas_consenso:

  Tabla aceptada de consenso por clasificacion para el corte CMO-SINCO
  2011.

- escenario:

  Contrato publico de decision. `official_strict` conserva solo
  resultados oficiales; `integrated_accepted` aplica la cascada
  aceptada; `analysis_legacy` habilita ademas rescates historicos.

## Value

El data frame con las clasificaciones y, para cada salida, columnas
terminadas en `_capa`, `_regla_id` y `_nivel_digitos`.

## Details

Teletrabajo y cuidados se calculan con el catalogo observado
correspondiente al periodo. Las salidas de Damian se calculan primero
con el SINCO 2011 armonizado. Para las correspondencias multiples de
SINCO 2019 se acepta el resultado de una salida cuando todos los
destinos oficiales producen la misma categoria; el remanente pasa
finalmente por las reglas de Damian sobre el codigo observado. Este
proceso no inventa ni rellena SINCO 2011 canonico.
