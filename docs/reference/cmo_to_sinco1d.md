# Convertir códigos CMO a clasificación SINCO 1d (nivel agregado)

Aplica mapeo manual desde 2005-I hasta el segundo trimestre de 2012, con
base en `p3_coe`, usando reglas por `cmo2d`, `cmo3d` y códigos
completos.

## Usage

``` r
cmo_to_sinco1d(data)
```

## Arguments

- data:

  Un data frame con variables `p3_coe`, `anio`, `trim` y preferentemente
  `pos_ocu`, `tue2`.

## Value

El data frame original con la variable `sinco1d` agregada.
