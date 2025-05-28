
<!-- README.md es generado por R. Para actualizarlo, ejecuta rmarkdown::render("README.Rmd") -->

# renoe <img src="man/figures/logo.png" align="right" height="120" />

Herramientas para descargar, procesar y analizar los microdatos de la
Encuesta Nacional de Ocupación y Empleo (ENOE) del INEGI, México.

Este paquete permite automatizar la descarga, lectura, etiquetado y
fusión de los componentes de la ENOE desde 2005 hasta 2024, en distintos
formatos de salida (CSV, Parquet, DTA, RDS).

## Instalación

Puedes instalar la versión en desarrollo directamente desde GitHub:

``` r
# install.packages("remotes")
remotes::install_github("aniuxa/renoe")
```

## Uso básico

``` r
library(renoe)

# Fusionar tablas para el primer trimestre de 2023
datos <- fusion_enoe(2023, 1)

# Guardar en formato Parquet
fusion_enoe(2023, 1, formato = "parquet", guardar = TRUE)
```

## Funciones principales

| Función | Descripción |
|----|----|
| `descarga_enoe()` | Descarga el archivo .zip correspondiente a un trimestre |
| `carga_enoe()` | Lee y etiqueta los archivos de un trimestre |
| `fusion_enoe()` | Fusiona viv, hog, sdem y coe1/coe2 en un solo data frame |
| `descargar_cuestionarios()` | Descarga los cuestionarios PDF desde el sitio de INEGI |
| `info_trimestre()` | Señala si el trimestre tiene cuestionario ampliado o básico y la versión |

## Formatos soportados

- `.parquet`: Compacto y rápido para análisis en R y Python.
- `.dta`: Compatible con Stata.
- `.rds`: Eficiente para uso interno en R.

## Estructura esperada

Los archivos descargados se guardan en las siguientes carpetas:

    renoe/
    ├── zip/      # Contiene los .zip y carpetas extraídas por año-trimestre
    ├── datos/    # Contiene los archivos fusionados guardados por la persona usuaria

## Créditos

Este paquete fue desarrollado para facilitar el trabajo con los
microdatos de la ENOE en proyectos académicos y de investigación social.

Proyecto **PAPIIT IN305925 Retos de la Inserción laboral en México**

**Nota:** Este paquete no está afiliado al INEGI. Los datos son
propiedad pública y pueden ser consultados en
<https://www.inegi.org.mx>.
