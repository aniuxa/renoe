# Descargar cuestionarios técnicos de la ENOE

Descarga automáticamente los cuestionarios en PDF y los descriptores de
archivos (FD) correspondientes a un trimestre específico de la ENOE,
basándose en la información de versiones. Los archivos se almacenan en
una carpeta única con nombre `docs_{anio}_{trim}` dentro del directorio
especificado.

## Usage

``` r
descargar_cuestionarios(
  anio,
  trimestre,
  destino = "cuestionarios",
  sobrescribir = FALSE
)
```

## Arguments

- anio:

  Año del trimestre (2005-2024). Debe ser un valor numérico entre 2005 y
  2024.

- trimestre:

  Número del trimestre (1-4). Donde 1 = ENE-MAR, 2 = ABR-JUN, 3 =
  JUL-SEP, 4 = OCT-DIC.

- destino:

  Directorio base donde se guardarán los archivos. Por defecto,
  "cuestionarios".

- sobrescribir:

  Lógico. ¿Deben sobrescribirse los archivos existentes? (FALSE por
  defecto).

## Value

Vector invisible con las rutas de los archivos descargados.

## See also

Other descarga_documenta_enoe:
[`carga_enoe()`](https://aniuxa.github.io/renoe/reference/carga_enoe.md),
[`descarga_enoe()`](https://aniuxa.github.io/renoe/reference/descarga_enoe.md),
[`fusion_enoe()`](https://aniuxa.github.io/renoe/reference/fusion_enoe.md),
[`info_trimestre()`](https://aniuxa.github.io/renoe/reference/info_trimestre.md)

## Examples

``` r
if (FALSE) { # \dontrun{
descargar_cuestionarios(2020, 1)
descargar_cuestionarios(2019, 2, sobrescribir = TRUE)
descargar_cuestionarios(2018, 3, destino = "documentacion_enoe")
} # }
```
