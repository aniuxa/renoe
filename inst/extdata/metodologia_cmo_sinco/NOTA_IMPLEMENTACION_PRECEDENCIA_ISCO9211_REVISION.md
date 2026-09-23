# Nota de implementacion: precedencia ISCO 9211

Fecha de certificacion: 2026-09-16.

## Alcance

Se corrigio exclusivamente la copia candidata aislada
`staging_candidato_armonizacion_20260915`. No se reconstruyeron los 85
trimestres ni se modificaron el staging vigente, el libro trimestral o el
checkout principal.

## Cambio

En `procesar_clases_damian()` se excluye de forma explicita ISCO 9211 de las
reglas generales ISCO 9111-9333 que asignan EGP10/11. Dos predicados nombrados
documentan las reglas especificas y permiten que se asignen efectivamente:

- ISCO 9211, `posocup_damian == 3` y `tam_est_damian < 4`: EGP12.
- ISCO 9211 y `posocup_damian == 4`: EGP13.

Los demas codigos del intervalo 9111-9333 conservan las asignaciones EGP10/11.
El cambio no amplia cobertura: reclasifica observaciones que ya tenian EGP.

## Pruebas y contraste longitudinal

La prueba dirigida cubre todos los tamanos codificados 1, 2, 3, 4 y 9 que son
pertinentes para la precedencia; verifica EGP13, su agregado EGP7, la macroclase
EGP4, los codigos vecinos y la invariancia de cobertura.

La funcion corregida se contrasta con una reconstruccion exacta del bloque
anterior sobre los archivos procesados, y con
`control/auditoria_precedencia_isco9211_casos_REVISION.csv` y
`control/sensibilidad_precedencia_isco9211_trimestres_REVISION.csv`.
Los conteos certificados son: 2012-T2 = 0, 2012-T3 = 6,943 y 2012-T4 = 6,796.
En los tres periodos se conserva el numero y el factor expandido de casos con
EGP valido.
