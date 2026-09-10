# Convertir SINCO 2019 a SINCO 2011

Aplica la tabla de equivalencia oficial incluida en el anexo del [SINCO
2019](https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=702825198411).
La tabla conserva todas las relaciones uno-a-varios. Por defecto, la
función no elige arbitrariamente un destino cuando el código SINCO 2019
tiene más de una equivalencia en SINCO 2011.

## Usage

``` r
sinco2019_to_sinco2011(
  data,
  variable_sinco = "sinco4d",
  correspondencia = NULL,
  resolver_multiples = c("na", "primero"),
  sobrescribir = TRUE
)
```

## Arguments

- data:

  Data frame que contiene el código SINCO 2019.

- variable_sinco:

  Nombre de la variable con el código SINCO 2019.

- correspondencia:

  Tabla opcional en formato largo con las columnas `sinco2019` y
  `sinco2011`. Si se omite, se utiliza la tabla oficial distribuida con
  el paquete.

- resolver_multiples:

  Tratamiento de correspondencias uno-a-varios: `"na"` (recomendado) las
  conserva como ambiguas y deja `sinco2011` en `NA`; `"primero"`
  selecciona el primer destino únicamente para reproducir un análisis
  que documente expresamente esa decisión.

- sobrescribir:

  Si es `FALSE`, detiene la ejecución cuando alguna variable de salida
  ya existe.

## Value

El mismo data frame con `sinco2019_original`, `sinco2011`,
`sinco2011_n_destinos` y `sinco2011_calidad`.

## References

INEGI (2020). *Sistema Nacional de Clasificación de Ocupaciones 2019*.
Anexo: Tabla de equivalencia SINCO 2011-2019.

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
[`procesar_tiempo()`](https://aniuxa.github.io/renoe/reference/procesar_tiempo.md),
[`procesar_variables_enoe()`](https://aniuxa.github.io/renoe/reference/procesar_variables_enoe.md),
[`procesar_vars_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_vars_hogar.md),
[`procesar_vars_laborales()`](https://aniuxa.github.io/renoe/reference/procesar_vars_laborales.md),
[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md)

## Examples

``` r
datos <- data.frame(ocupacion = c(2433, 2423, 2429, NA))
sinco2019_to_sinco2011(datos, variable_sinco = "ocupacion")
#>   ocupacion sinco2019_original sinco2011 sinco2011_n_destinos
#> 1      2433               2433      2423                    1
#> 2      2423               2423      2412                    1
#> 3      2429               2429        NA                    2
#> 4        NA                 NA        NA                   NA
#>                                    sinco2011_calidad
#> 1                       Equivalencia oficial directa
#> 2                       Equivalencia oficial directa
#> 3 Equivalencia oficial m<U+00FA>ltiple: sin resolver
#> 4                                SINCO 2019 faltante
```
