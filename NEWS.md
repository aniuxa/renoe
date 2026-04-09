# renoe 0.1.4 (8 de abril de 2026)

## Nuevas funciones y mejoras

- Se incorporó la función `procesar_contribucion_hogar()` para generar ingreso ocupacional individual deflactado, agregados del hogar, indicadores per cápita y quintiles ponderados de ingreso y trabajo no remunerado.
- Se actualizaron las funciones `procesar_vars_sociodemo()`, `procesar_tiempo()`, `procesar_vars_hogar()` y `procesar_vars_laborales()` para estandarizar el uso de etiquetas mediante `sjlabelled::var_labels()` y `sjlabelled::val_labels()`.
- Se amplió la documentación de `procesar_tiempo()` para dejar explícito que las variables específicas de uso del tiempo se convierten a horas y que los valores faltantes se recodifican a cero para facilitar agregados y análisis descriptivos.
- Se actualizó `procesar_vars_laborales()` para reflejar cambios en la variable de experiencia previa de trabajo, usando `p2h4` en la construcción de `nunca_trabajo`.
- Se mejoró la claridad semántica del procesamiento de aportes del hogar, renombrando y etiquetando los quintiles derivados de ingreso y trabajo no remunerado con nombres más explícitos.
- Se actualizaron la vignette, el `README.md` y otros materiales introductorios del paquete para reflejar el flujo recomendado actual de procesamiento.
- Se preparó la infraestructura de citación del paquete mediante archivos de citación y metadatos actualizados para la versión `0.1.4`.

## Cambios en fusión y compatibilidad

- Se reforzó la estrategia de fusión robusta en `fusion_enoe()` para manejar cambios recientes en nombres de variables clave entre trimestres.
- Se incorporó la homologación de nombres de entidad federativa cuando las bases utilizan `cve_ent` en lugar de `ent`, facilitando la compatibilidad con trimestres recientes.
- Se mejoró la detección de claves de unión y se evitó el uso de todas las columnas compartidas como variables de empalme en la fusión, reduciendo errores por incompatibilidades de tipo.

## Cambios en documentación y mantenimiento interno

- Se actualizó la lista de `utils::globalVariables()` para incluir las nuevas variables derivadas creadas en funciones recientes y retirar nombres obsoletos.
- Se revisaron los imports del paquete para reflejar el uso actual de `sjlabelled::var_labels()`, `sjlabelled::val_labels()` y `dineq::ntiles.wtd()`.
- Se actualizaron ejemplos y documentación del flujo de procesamiento para incorporar `procesar_contribucion_hogar()` y la nueva secuencia sugerida de análisis.

## Notas

- Esta versión fortalece la consistencia interna del paquete, especialmente en el etiquetado de variables y en la compatibilidad entre distintos periodos de la ENOE.
- Se mantiene la compatibilidad con versiones previas del paquete y con el enfoque de análisis reproducible en R.

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
