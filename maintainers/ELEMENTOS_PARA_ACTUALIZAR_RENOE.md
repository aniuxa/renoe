# Elementos para actualizar renoe

Esta guia registra el procedimiento de mantenimiento de descargas y procesamiento de ENOE. El ultimo trimestre habilitado y validado es **2026-T2**. 2026-T3 y T4 permanecen deshabilitados; una fecha en el calendario no demuestra que sus microdatos esten publicados ni sean compatibles.

## 1. Comprobar la publicacion

- Consultar el [programa ENOE del INEGI](https://www.inegi.org.mx/programas/enoe/15ymas/) y su seccion de microdatos. Registrar fecha de consulta, trimestre, pagina oficial, URL final del ZIP y fecha de publicacion cuando conste.
- Descargar en una carpeta aislada, conservar la URL, fecha, tamano y SHA-256 del original, y comprobar que es un ZIP legible. Revisar que incluya vivienda, hogar, SDEM, COE1 y COE2 y los documentos pertinentes. Una respuesta HTTP 200 o un nombre de archivo previsible no prueban que el contenido sea el publicado.
- No habilitar un periodo si faltan los archivos, no son legibles o solo existe un anuncio. Mantener separadas la disponibilidad de microdatos y la disponibilidad de insumos para procesarlos.

## 2. Actualizar la disponibilidad y metadatos

- El limite unico de trimestres habilitados se declara en `.ultimo_periodo_enoe`, en `R/disponibilidad_enoe.R`. Actualizarlo solo despues de completar las validaciones de este documento. La excepcion 2020-T2 se mantiene explicitamente.
- Revisar el patron de URL en `.construir_url_enoe()` y cualquier estructura nueva de carpetas, nombres o identificadores. `verificar_url = TRUE` solo activa, a partir del segundo intento, la alternativa historica conocida de 2018-T2; no descubre formatos futuros.
- Incorporar en `inst/extdata/trimestre_info.csv` el tipo y las versiones de cuestionario y descriptor respaldados por INEGI. No inferirlos por el trimestre anterior.
- Actualizar `inst/extdata/ipc.rds` con el IPC trimestral comprobado, documentando fuente, calculo y base comparable. Sin IPC, la descarga puede existir pero la cadena que deflacta ingresos no esta lista. No extrapolar valores.
- Revisar los diccionarios, clasificaciones y cambios de variables, particularmente identificadores geograficos y llaves; documentar cualquier ruptura y su impacto.

## 3. Validar sin alterar datos de trabajo

1. Trabajar en una carpeta aislada. Ejecutar `descarga_enoe(anio, trimestre, formato = "rds", cache = FALSE)` y verificar las cinco tablas, su lectura, nombres y conteos. Repetir con `cache = TRUE`; comprobar que una cache corrupta se rechace.
2. Ejecutar `carga_enoe(..., list = TRUE, rapida = TRUE)` y `fusion_enoe()`. Comparar el numero de filas fusionadas con SDEM despues de los filtros `r_def == 0` y `c_res != 2`; revisar llaves duplicadas y perdidas. Verificar que las tablas y variables esperadas existan.
3. Ejecutar `info_trimestre()`, `ipc_enoe()` y, si corresponde, la cadena `procesar_variables_enoe()` con una semilla registrada. Revisar resultados, advertencias y codificacion. Comparar contra cifras o documentacion oficial pertinentes sin confundir controles tecnicos con validacion estadistica integral.
4. Agregar pruebas sin red para el nuevo periodo, sus metadatos, URL y cambios de estructura; conservar las pruebas que bloquean periodos aun no validados. Ejecutar la suite completa y `R CMD check`, incluidas vinietas cuando el entorno lo permita.
5. Actualizar descripcion, ayuda, README, NEWS y este documento; registrar la evidencia y SHA-256. No afirmar que se validaron formatos, escenarios o trimestres que no se probaron.

## Comportamiento de descarga

Se aceptan `parquet`, `rds` y `dta`; un formato distinto produce error. Los reintentos revisan el codigo de descarga y la legibilidad estructural del ZIP; si ninguno resulta valido, la funcion informa el fallo y no reemplaza el ZIP anterior. La cache de tablas convertidas se reutiliza solo si las cinco existen, son legibles y tienen filas y columnas. La cache CSV exige las cinco tablas con encabezado y al menos una fila; despues de leerlas, `carga_enoe()` exige las cinco tablas no vacias.

Una comprobacion estructural no sustituye la validacion de contenido ni verifica que los archivos pertenezcan al trimestre esperado. El guardado prepara archivos temporales antes de reemplazar salidas; un error de escritura durante la copia final puede requerir repetir la descarga.
