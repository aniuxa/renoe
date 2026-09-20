# Crear un puente CMO-SINCO 2011 para estudiar trabajo de cuidado

Funcion auxiliar construida para armonizar las ocupaciones necesarias en
el analisis longitudinal del trabajo de cuidado de mercado. Se basa en
el do-file `cmo a sinco11.do` preparado por Damian (2026).

## Usage

``` r
cmo_to_sinco11_care(data, variable_cmo = "p3coe", sobrescribir = TRUE)
```

## Arguments

- data:

  Data frame que contiene la ocupacion codificada en CMO.

- variable_cmo:

  Nombre de la variable CMO; en las bases fusionadas suele ser `p3coe`.

- sobrescribir:

  Si es FALSE, detiene la ejecucion cuando ya existen las variables de
  salida.

## Value

El data frame con `cmo_original`, `sinco11`, `sinco3d`,
`sinco11_n_destinos` y `sinco11_calidad`.

## Details

No es una conversion oficial, general ni biunivoca entre CMO y SINCO
2011. No debe utilizarse para afirmar que todas las personas recibieron
una ocupacion SINCO exacta ni para estudiar ocupaciones ajenas al
objetivo de cuidados sin revisar antes la concordancia correspondiente.

Para reproducir el do-file, cuando un CMO tiene varios destinos conserva
en `sinco11` el destino de la primera regla, ya que el codigo Stata solo
reemplaza mientras `sinco11 == -1`. Esa eleccion es mecanica: no
resuelve la ambiguedad sustantiva. Por ello la funcion tambien informa
el numero de destinos posibles y la calidad de la correspondencia. La
clasificacion final de cuidados debe resolver los casos multiples en
categorias amplias: si los destinos coinciden en la misma categoria de
cuidado puede utilizarse esa categoria; si discrepan, se requiere una
regla analitica explicita.

## See also

Other cuidado_remunerado:
[`class_cuidado_rem()`](https://aniuxa.github.io/renoe/reference/class_cuidado_rem.md),
[`procesar_cuidado_remunerado()`](https://aniuxa.github.io/renoe/reference/procesar_cuidado_remunerado.md)

## Examples

``` r
if (FALSE) { # \dontrun{
datos <- datos |>
  cmo_to_sinco11_care(variable_cmo = "p3coe")
} # }
```
