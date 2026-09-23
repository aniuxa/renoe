# Aplicar reglas declarativas y trazables a variables ENOE

Aplica una tabla de reglas en formato largo a una sola variable de
destino. Las condiciones de una misma `regla_id` se combinan con AND y
las reglas se evaluan por prioridad ascendente. No evalua expresiones R
arbitrarias.

## Usage

``` r
reglas_enoe(
  data,
  reglas,
  consumidor = "general",
  perfil = c("oficial", "panel_validado", "experimental"),
  modo = c("principal", "sensibilidad"),
  sobrescribir = FALSE,
  advertir = TRUE
)
```

## Arguments

- data:

  Data frame que contiene las variables auxiliares.

- reglas:

  Data frame largo con las columnas `regla_id`, `variable_destino`,
  `valor_destino`, `prioridad`, `perfil_regla`, `fase_decision`,
  `nivel_evidencia`, `variable_auxiliar`, `operador`, `valor_condicion`,
  `rol_auxiliar` y `origen_auxiliar`.

- consumidor:

  Producto que usara el resultado. `"desajuste_horizontal"` activa el
  control especifico de circularidad entre carrera y ocupacion.

- perfil:

  Maximo nivel de evidencia habilitado: `"oficial"`, `"panel_validado"`
  o `"experimental"`.

- modo:

  `"principal"` excluye reglas de sensibilidad; `"sensibilidad"` las
  permite. Las reglas prohibidas nunca se aplican.

- sobrescribir:

  Si es `FALSE`, solo completa valores faltantes. Si es `TRUE`, la
  primera regla aplicable puede reemplazar el valor recibido.

- advertir:

  Si es `TRUE`, informa reglas omitidas por fase, perfil o circularidad.

## Value

El mismo data frame, sin cambiar filas, con la variable de destino y las
columnas `regla_enoe_id`, `regla_enoe_perfil`, `regla_enoe_fase`,
`regla_enoe_evidencia`, `regla_enoe_auxiliares`,
`regla_enoe_circularidad` y `regla_enoe_aplicada`.

## Details

Operadores disponibles: `==`, `!=`, `in`, `between`, `is_na` y `not_na`.
Para `in`, los valores se separan con `|`; para `between`, se
proporcionan dos limites numericos separados con `|`.

En el consumidor `desajuste_horizontal`, una regla que complete una
carrera con ocupacion observada se clasifica como sensibilidad. Si la
ocupacion fue armonizada o imputada, la regla se prohibe. Un auxiliar
con rol `resultado` tambien se prohibe. Esta restriccion evita usar el
propio resultado para fabricar uno de sus componentes.

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
[`procesar_tiempo()`](https://aniuxa.github.io/renoe/reference/procesar_tiempo.md),
[`procesar_variables_enoe()`](https://aniuxa.github.io/renoe/reference/procesar_variables_enoe.md),
[`procesar_vars_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_vars_hogar.md),
[`procesar_vars_laborales()`](https://aniuxa.github.io/renoe/reference/procesar_vars_laborales.md),
[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md),
[`sinco2019_to_sinco2011()`](https://aniuxa.github.io/renoe/reference/sinco2019_to_sinco2011.md)

## Examples

``` r
datos <- data.frame(nivel = c(7, 6), carrera = c(NA, NA))
reglas <- data.frame(
  regla_id = "NIVEL_7", variable_destino = "carrera",
  valor_destino = "A", prioridad = 1, perfil_regla = "panel_validado",
  fase_decision = "accepted", nivel_evidencia = "auxiliary",
  variable_auxiliar = "nivel", operador = "==", valor_condicion = "7",
  rol_auxiliar = "educacion", origen_auxiliar = "observado"
)
reglas_enoe(datos, reglas, perfil = "panel_validado")
#>   nivel carrera regla_enoe_id regla_enoe_perfil regla_enoe_fase
#> 1     7      NA       NIVEL_7    panel_validado        accepted
#> 2     6      NA          <NA>              <NA>            <NA>
#>   regla_enoe_evidencia regla_enoe_auxiliares regla_enoe_circularidad
#> 1            auxiliary                 nivel               principal
#> 2                 <NA>                  <NA>                    <NA>
#>   regla_enoe_aplicada
#> 1                TRUE
#> 2               FALSE
```
