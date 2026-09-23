# Armonizar SCIAN-Hogares antes de la ocupacion

El periodo se infiere de `anio` y `trim` cuando no se declara
expresamente. Conserva `p4a` observado y toda la procedencia del
adaptador SCIAN.

## Usage

``` r
armonizar_scian(datos, ...)
```

## Arguments

- datos:

  Microdatos ENOE con `anio`, `trim` y `p4a`.

- ...:

  Argumentos adicionales para el adaptador SCIAN-Hogares interno.
