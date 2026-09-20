# Correspondencias ocupacionales

La ruta canónica del paquete es:

```text
CMO -> SINCO 2011 <- SINCO 2019
```

`armonizar_sinco()` conserva el catálogo y el código observados y produce una
base común SINCO 2011. Ninguna clasificación sustantiva debe elegir por su
cuenta un destino cuando una correspondencia tiene varios posibles. La columna
`version_sinco_destino` deja marcado `SINCO 2011` como destino predeterminado.

## Insumos activos

- `puente_cmo_sinco2011_oficial.csv`: puente oficial CMO a SINCO 2011. Puede sustentar una
  equivalencia a cuatro o sólo a tres dígitos; la granularidad queda explícita
  en la salida de `armonizar_sinco()`.
- `puente_sinco2019_sinco2011.csv`: tabla oficial utilizada para llevar SINCO
  2019 a SINCO 2011. Las correspondencias múltiples permanecen pendientes para
  una resolución posterior con evidencia de panel y, sólo como control grueso,
  actividad SCIAN.
- `correspondencia_sinco2011_isco88_damian.csv`: clasificación derivada que se
  aplica después de construir la base canónica SINCO 2011.

## Insumo legado retenido temporalmente

- cmo_sinco_total.csv: puente general heredado. Se conserva para reproducir
  resultados históricos, pero no es la fuente canónica de nuevas reglas.

- `concordancia_cmo_sinco_cuidado.csv` y
  `README_concordancia_cmo_sinco_cuidado.md`: puente específico del módulo de
  cuidados. No es una correspondencia general y no debe alimentar nuevas
  clasificaciones. Se conserva para reproducir salidas históricas y permitir
  la migración de bases ya publicadas.

La eliminación física del insumo legado requiere primero retirar su uso de
`cmo_to_sinco11_care()` y verificar que ningún flujo compatible dependa de él;
por ello corresponde a una versión con ruptura de compatibilidad. Hasta
entonces se considera deprecado conceptualmente, no un insumo canónico.

En esa versión, las rutas históricas se expondrán sólo mediante una función
separada y explícita. La salida legada se representará preferentemente mediante
identificadores estables de regla y versión; las tablas completas permanecerán
como recursos versionados de esa función, no como columnas redundantes de la
salida ordinaria.

## Criterio de incorporación de evidencia adicional

Los casos múltiples deben resolverse mediante un ciclo auditable por nivel:
consenso catalogal en la granularidad vigente; panel global entre los destinos
todavía múltiples; y panel condicionado por SCIAN grueso entre los remanentes.
Sólo entonces se baja un dígito y se reinicia el ciclo. Para SINCO la secuencia
es 4d → 3d → 2d → 1d. Cada decisión futura debe conservar nivel, regla, fuente,
versión y número de destinos candidatos.

El consenso de una clasificación derivada entre todos los destinos oficiales
se evalúa después de esas capas y no selecciona un código SINCO. Mientras las
reglas de panel sigan en fase `candidate`, cualquier cobertura calculada con
ese consenso es un escenario provisional que debe volver a estimarse sobre el
remanente posterior a las reglas de panel aceptadas.
## Arquitectura futura en renoe

La arquitectura prevista mantiene separadas las familias de clasificadores y
ordena su armonización en tres etapas:

1. `armonizar_carreras()`
2. `armonizar_scian()`
3. `armonizar_sinco()`

Este orden expresa una secuencia de procesamiento, no la fusión de los
clasificadores ni una equivalencia entre sus categorías. `armonizar_sinco()`
podrá consumir el SCIAN previamente armonizado como evidencia auxiliar para
resolver remanentes ocupacionales. Esa evidencia deberá conservar su
procedencia y no sustituirá las reglas oficiales ni modificará la salida propia
de SCIAN. Las entradas, reglas y salidas de carreras, actividad económica y
ocupación permanecerán separadas para evitar dependencias circulares y permitir
auditorías y análisis de sensibilidad.

Cada función deberá registrar, como mínimo: catálogo y versión de origen y
destino; niveles de granularidad disponibles y utilizados; reglas oficiales;
evidencia de panel y variables auxiliares; estado de cada regla o decisión
(`candidate`, `accepted` o `integrated`); cobertura alcanzada; y casos
pendientes. Una regla auxiliar aceptada para ENOE no se considerará
automáticamente una correspondencia estable para otras fuentes.

Las funciones legacy se conservarán durante esta transición. Su deprecación y
eventual retiro serán una decisión posterior y separada, con documentación
propia y verificación de compatibilidad.
