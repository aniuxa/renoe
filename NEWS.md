# renoe 0.1.3

## Nuevas funciones y mejoras

- Se incorporó una clasificación de parentesco (`relative`) robusta y compatible con todas las versiones de ENOE desde 2005. Esta clasificación se ajusta dinámicamente según el año (`anio`) y trimestre (`trimestre`) de la encuesta, utilizando los archivos externos `par_c1.csv` y `par_c2.csv` incluidos en `extdata/`.
- Se actualizó la función `procesar_vars_hogar()` para utilizar esta nueva clasificación, mejorando la identificación de hogares extensos, compuestos y otras estructuras familiares, en especial para los periodos anteriores a 2013.
- Se asegura que todos los códigos `par_c` sean clasificados. Si un código no se encuentra en los catálogos correspondientes, se asigna automáticamente a la categoría de "otro sin parentesco" (`relative = 6`).

## Cambios en el etiquetado

- La función `.procesar_etiquetas_enoe()` excluye ahora la variable `par_c` del etiquetado automático de catálogos, debido a errores en los datos abiertos del INEGI que podían asignar descripciones incorrectas.

Este release mejora la confiabilidad de los análisis sobre estructura familiar en los microdatos de ENOE, con mayor precisión histórica y menor dependencia de catálogos inconsistentes.


# renoe 0.1.2 (23 de julio de 2025)

## Cambios en esta versión

- Se añadió la función `procesar_vars_laborales()` para calcular desajuste educativo, condiciones contractuales y experiencia laboral previa, utilizando códigos SINCO armonizados y clasificadores de nivel educativo.
- Se incorporó la función `imputa_ingocup()` para **imputar ingresos ocupacionales** usando métodos de imputación múltiple (`mice`) entre personas ocupadas, facilitando análisis más robustos. Esta mejora representa un avance importante en el procesamiento analítico.
- Las funciones `procesar_vars_*()` ahora comparten la etiqueta `@family procesamiento_enoe` para facilitar su consulta conjunta en la documentación.
- Se actualizaron los tests automatizados con `testthat` para validar etiquetas, clases y contenido de variables generadas.
- Se mejoró el manejo condicional de variables que difieren entre el cuestionario ampliado y básico, evitando errores en `mutate()` cuando ciertas columnas no existen.
- Se corrigieron advertencias de clase y etiquetas faltantes en las variables derivadas.

## Notas

- Las funciones `procesar_tiempo()` y `procesar_vars_laborales()` ahora discriminan internamente el tipo de cuestionario (`coe_tipo`) para evitar errores cuando hay campos incompatibles.
- El paquete mantiene compatibilidad con versiones previas (≥ R 4.1.0) y continúa usando operadores modernos (`|>`, funciones lambda `\(x)`).


# renoe 0.1.1 (20 de julio de 2025)

## Cambios en esta versión

- Se agregó soporte para descargar y procesar archivos del **año 2025**.
- Se corrigió el caso especial del trimestre **2022 T1**, reemplazando todos los archivos defectuosos por versiones estables provenientes del portal de microdatos de INEGI.
- Se incorporó la opción de **fusión robusta** (`fusion_robusta = TRUE`) en `fusion_enoe()`, para manejar variaciones en las claves de fusión entre años.
- Se añadió validación del número de filas esperadas tras el filtrado (`r_def == 0 & c_res != 2`), y se emiten advertencias si el número de filas fusionadas difiere.
- Se actualizó el manejo de claves para empatar las tablas. Ahora `fusion_enoe()` llama internamente a `crear_folios()` para generar los identificadores únicos `folio`, `folio2` y `folio3` de forma consistente.
- Mejora en documentación y ejemplos (`README.md`, `DESCRIPTION`, ayuda por función).

## Notas

- El paquete ahora requiere R >= 4.1.0 debido al uso del operador `|>` y funciones abreviadas `\(x)` introducidas en esa versión.
- Algunas dependencias actualizadas: `arrow`, `haven`, `dplyr`, `readr`, entre otras.


# renoe 0.1.0 (28 de mayo de 2025)

## Lanzamiento inicial del paquete

- Primer conjunto de funciones para descargar, descomprimir, fusionar y cargar datos de la ENOE.
- Soporte para etiquetado automático, manejo de codificaciones y procesamiento sociodemográfico básico.
- Funciones auxiliares internas (`.leer_datos_enoe`, `.descargar_zip_enoe`, `.procesar_etiquetas_enoe`, entre otras).
