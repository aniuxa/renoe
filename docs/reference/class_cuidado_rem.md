# Clasificar el trabajo de cuidado de mercado

Reproduce la clasificacion utilizada en el articulo a partir de la
ocupacion armonizada a tres digitos y de la rama de actividad. Distingue
ocupaciones de cuidado, industria del cuidado y la posicion resultante
del trabajador en la economia del cuidado. La pertenencia a este
conjunto no implica por si sola que se observe remuneracion positiva.

## Usage

``` r
class_cuidado_rem(
  data,
  variable_ocupacion = "sinco3d",
  variable_actividad = "p4a",
  variable_ocupado = "clase2",
  valor_ocupado = 1,
  variable_cmo = "p3coe",
  aplicar_puente_cmo = TRUE,
  puente_cmo_precalculado = FALSE
)
```

## Arguments

- data:

  Data frame individual de ENOE ya procesado.

- variable_ocupacion:

  Nombre de la variable SINCO a tres digitos.

- variable_actividad:

  Nombre de la variable SCIAN-Hogares (`p4a`).

- variable_ocupado:

  Nombre de la variable de condicion de ocupacion.

- valor_ocupado:

  Valor que identifica a la poblacion ocupada.

- variable_cmo:

  Nombre de la ocupacion CMO en los periodos hasta 2012-II; normalmente
  `p3coe`.

- aplicar_puente_cmo:

  Si es TRUE, aplica automaticamente el puente de cuidado a las
  observaciones de 2005-I a 2012-II.

- puente_cmo_precalculado:

  Si es TRUE, el llamador ya resolvio el remanente CMO antes de entrar y
  no se emite la advertencia de omision.

## Value

El mismo data frame con `class_ocu`, `isco_care`, `care_industry`,
`care_w`, `trabajo_cuidado_mercado`, `cuida_1d` y banderas de medicion.

## Details

La funcion detecta automaticamente el clasificador por periodo. Entre
2005-I y 2012-II aplica el puente analitico
[`cmo_to_sinco11_care()`](https://aniuxa.github.io/renoe/reference/cmo_to_sinco11_care.md);
desde 2012-III utiliza SINCO. La concordancia CMO-SINCO no es biunivoca,
por lo que conserva sus banderas de calidad y no debe interpretarse como
una conversion oficial exacta para otros usos.

## See also

Other cuidado_remunerado:
[`cmo_to_sinco11_care()`](https://aniuxa.github.io/renoe/reference/cmo_to_sinco11_care.md),
[`procesar_cuidado_remunerado()`](https://aniuxa.github.io/renoe/reference/procesar_cuidado_remunerado.md)

## Examples

``` r
if (FALSE) { # \dontrun{
datos <- datos |>
  class_cuidado_rem()
} # }
```
