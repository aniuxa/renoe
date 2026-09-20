# Procesar variables adicionales de cuidado y capacidad del hogar

Construye indicadores individuales auxiliares, los agrega por hogar y
une los resultados nuevamente a cada integrante. Debe ejecutarse despues
de
[`procesar_tiempo()`](https://aniuxa.github.io/renoe/reference/procesar_tiempo.md),
[`imputa_ingocup()`](https://aniuxa.github.io/renoe/reference/imputa_ingocup.md)
y
[`procesar_contribucion_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_contribucion_hogar.md).

## Usage

``` r
procesar_cuidado_extra(
  data,
  edad_adulta = 18,
  edad_adolescente_min = 13,
  edad_adolescente_max = 17,
  umbral_jornada_alta = 40,
  umbral_jornada_muy_alta = 48
)
```

## Arguments

- data:

  Data frame individual de ENOE ya procesado.

- edad_adulta:

  Edad minima para considerar a una persona adulta.

- edad_adolescente_min:

  Edad minima del grupo adolescente.

- edad_adolescente_max:

  Edad maxima del grupo adolescente.

- umbral_jornada_alta:

  Horas semanales que definen jornada mayor a 40.

- umbral_jornada_muy_alta:

  Horas semanales que definen jornada mayor a 48.

## Value

El mismo data frame individual con variables del hogar anadidas.

## References

Escoto, Ana (2026, 4 de junio). *Transversalidad del derecho al cuidado:
tensiones y desafios* (ponencia). Mesa 1 del conversatorio *?Una cancha
pareja? Escuela, cuidado y fragmentacion de derechos*, El Colegio de
Mexico. Transmision oficial:
<https://www.youtube.com/watch?v=fdpzAe6IYBc>

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
if (FALSE) { # \dontrun{
datos <- datos |>
  procesar_cuidado_extra()
} # }
```
