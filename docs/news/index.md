# Changelog

## renoe 0.3.2 (hotfix documental, 23 de septiembre de 2026)

- Sincroniza `DESCRIPTION`, `CITATION.cff`, `inst/CITATION`, README y
  pkgdown con la versión pública 0.3.2.
- Publica la guía “Migración de renoe 0.2.0 a renoe 0.3.1” y precisa la
  guía histórica “Migración de renoe 0.1.4 a renoe 0.2.0”.
- Reorganiza la referencia en siete grupos funcionales, corrige
  etiquetas visibles `año`/`años` y evita duplicar nombres en el pie de
  página.
- Añade controles fail-closed de versión, UTF-8, enlaces, reconstrucción
  limpia de pkgdown y verificación de la URL pública antes de cerrar un
  Release.
- No cambia la API ni los resultados analíticos de renoe 0.3.1. \# renoe
  0.3.1 (23 de septiembre de 2026)

0.3.1 es la versión pública que sustituye a 0.2.0. La versión 0.3.0 fue
una candidata interna y no se publicó. La guía [Migración de renoe 0.2.0
a renoe
0.3.1](https://aniuxa.github.io/renoe/articles/novedades-migracion-0.3.1.md)
explica los cambios para personas usuarias; esta sección conserva el
detalle técnico.

### Corrección ENOE 2020-T1

- Usa conjuntamente las cinco tablas de la publicación vigente de
  microdatos de INEGI. Esto recupera los códigos `P3` presentes en COE1
  y evita mezclar ediciones de VIV, HOG, SDEM, COE1 y COE2.
- Invalida cachés de 2020-T1 que no contienen las cinco tablas y obliga
  a regenerar la fusión antes de sus derivados transversales y paneles.
- Excluye `ur` de la llave SDEM–COE sólo en 2020-T1: en 2,851 registros
  el ámbito difiere entre ambas tablas aunque la identidad personal
  coincide. `ur` se conserva como variable descriptiva y en las uniones
  de vivienda y hogar donde corresponde.
- La anomalía se detectó con una tabulación transversal de
  `clase_alt6_damian`: 3,011 personas ocupadas aparecían sin
  clasificación. Tras corregir la unión quedan 162 faltantes legítimos
  (factor 41,820): 133 con código oficial `9999` y 29 sin una regla EGP
  aplicable.
- El cambio invalida el transversal o PINI de 2020-T1, los paneles 57–61
  y sus derivados. Otros trimestres conservan la llave con `ur` y no se
  regeneran sin una dependencia comprobada.

### Ruta reproducible de clasificadores

- Publica los escenarios `official_strict`, `integrated_accepted` y
  `analysis_legacy`. `integrated_accepted` es el predeterminado para
  análisis generales; `analysis_legacy` debe solicitarse expresamente.
- Fija el orden SCIAN → SINCO → carreras → consumidores.
- Conserva código observado, versión, destinos posibles, regla,
  evidencia, nivel sustentado y estado de decisión.
- Mantiene `9999` como código especial no comparable.
- Añade
  [`procesar_productos_academicos()`](https://aniuxa.github.io/renoe/reference/procesar_productos_academicos.md)
  para reproducir productos académicos sin duplicar reglas.
- Conserva
  [`armoniza_sinco()`](https://aniuxa.github.io/renoe/reference/armoniza_sinco.md)
  únicamente como alias deprecado de
  [`armonizar_sinco()`](https://aniuxa.github.io/renoe/reference/armonizar_sinco.md).
- Hace que la ruta general se detenga si cambia el número de filas o si
  `folio3` no termina completa y única.

### Cambios acumulados desde 0.2.0

- Corrige HOG de 2022-T1 combinando los componentes oficiales urbano y
  rural, armoniza sus meses y usa SDEM como ancla.
- Corrige los quintiles del hogar para usar una fila y un factor por
  hogar y conserva como `NA` los ingresos completamente desconocidos.
- Separa cuidado adolescente amplio y directo, corrige el indicador de
  cuidado adolescente y armoniza el trabajo no remunerado dentro del
  hogar.
- Documenta las rupturas de 2013, 2020 y 2023 y audita 85 trimestres
  entre 2005-T1 y 2026-T2.
- Añade clasificaciones reproducibles de origen, parentesco, hogares,
  educación, situación laboral, cuidados y región socioeconómica.

## renoe 0.3.0 (candidata final local, 20 de septiembre de 2026)

- Reconcilia la rama de publicación con los escenarios explícitos
  `official_strict`, `integrated_accepted` y `analysis_legacy`.
- Fija el orden canónico `SCIAN → SINCO → carreras → consumidores`.
- Conserva el descenso oficial por dígitos, separa los rescates
  históricos y mantiene `9999` como código no comparable.
- Añade
  [`procesar_productos_academicos()`](https://aniuxa.github.io/renoe/reference/procesar_productos_academicos.md)
  como wrapper sin reglas duplicadas y conserva
  [`armoniza_sinco()`](https://aniuxa.github.io/renoe/reference/armoniza_sinco.md)
  únicamente como alias deprecado de la API ya publicada antes de 0.3.0.

### Cambios acumulados desde 0.2.0

- Establece una ruta canonica reproducible desde la fusion ENOE hasta
  las clasificaciones finales, sin alias ni opciones legacy no
  publicadas.
- Incorpora la cascada ocupacional oficial, panel, reglas ENOE/SCIAN,
  consenso y reglas de autor, con procedencia y nivel de digitos.
- Corrige los quintiles del hogar para usar una fila y un factor por
  hogar y conserva como `NA` los ingresos completamente desconocidos.
- Separa cuidado adolescente amplio y directo, corrige el indicador de
  cuidado adolescente y armoniza el trabajo no remunerado dentro del
  hogar.
- Documenta las rupturas de 2013, 2020 y 2023 y audita 85 trimestres
  entre 2005-T1 y 2026-T2.
- Anade clasificaciones reproducibles de origen, parentesco, hogares,
  educacion, situacion laboral, cuidados y region socioeconomica.
- Corrige HOG de 2022-T1 combinando los componentes oficiales urbano y
  rural, armoniza sus meses, incorpora `ur` a las llaves de union y usa
  SDEM como ancla para impedir que una ausencia auxiliar elimine
  personas.

## renoe 0.2.0 (10 de septiembre de 2026)

- Se formalizó a Ana Escoto como única autora y mantenedora (`aut`,
  `cre`) y a Gerardo Damián Hernández y Gabriela Cervantes como
  colaboradores de código (`ctb`), sin incorporarlos a la cita
  bibliográfica del paquete.
- Nuevo modulo de cuidado de mercado con wrapper, trazabilidad y pruebas
  sinteticas; `trabajo_cuidado_rem` queda como alias transitorio.
- Concordancia analitica CMO extraida a extdata, conservando las 447
  reglas anteriores.
- El wrapper utiliza ocupacion observada para no confundir SINCO 2019
  con la armonizacion general a 2011.
- La clasificacion de cuidado conserva NA para no ocupados, condicion
  desconocida y codigos no aplicables; respeta la columna ocupacional
  personalizada en CMO.
- [`procesar_vars_laborales()`](https://aniuxa.github.io/renoe/reference/procesar_vars_laborales.md)
  normaliza de forma controlada `cs_p13_1` y `cs_p15`, corrige la
  agrupación SINCO y usa las categorías sobreeducación, ajuste y
  subeducación.
- [`calcular_desajuste_estadistico()`](https://aniuxa.github.io/renoe/reference/calcular_desajuste_estadistico.md)
  usa una referencia transversal trimestral por defecto. La referencia
  anual exige trimestres acumulados, mantiene la unidad
  persona-trimestre y conserva la definición histórica en
  `mismatch2_legacy` cuando corresponde.
- Los diccionarios CSV son la fuente portable de etiquetas; los
  atributos de `sjlabelled` se conservan para el trabajo en R y la
  exportación a Stata.
- Se corrigió la fusión de 2022-T1 para conservar la población rural:
  `ur` ya no actúa como llave implícita y se toma de SDEM. La función
  obliga a usar la vía robusta en ese trimestre y detiene el guardado si
  no conserva las filas esperadas.
- [`procesar_tiempo()`](https://aniuxa.github.io/renoe/reference/procesar_tiempo.md)
  usa el corte documental de 2011 y distingue 98, 99, no selección y
  batería no medible. Expone totales parciales, banderas de
  incompletitud y columnas `*_legacy` para la transición.
- [`ipc_enoe()`](https://aniuxa.github.io/renoe/reference/ipc_enoe.md)
  valida argumentos, esquema, dominio y unicidad del recurso, y falla de
  forma explícita cuando falta un trimestre.
- Se habilitaron los metadatos de 2026-T1 y 2026-T2 después de comparar
  esquemas y verificar llaves de los ZIP oficiales. La estandarización
  incorpora `cve_ent`, `cve_mun`, `cve_loc` y `cve_ageb` y conserva
  `cvegeo`.

## renoe 0.1.4 (8 de abril de 2026)

### Nuevas funciones y mejoras

- Se incorporó
  [`aplicar_etiquetas_enoe()`](https://aniuxa.github.io/renoe/reference/aplicar_etiquetas_enoe.md)
  para restaurar, antes de exportar a RDS o Stata, etiquetas de
  variables y valores cuya portabilidad no está garantizada entre
  distintos lectores de Parquet. La función usa catálogos explícitos,
  mantiene los códigos numéricos y evita convertir las variables
  analíticas en factores.
- Se incorporó
  [`sinco2019_to_sinco2011()`](https://aniuxa.github.io/renoe/reference/sinco2019_to_sinco2011.md)
  y la tabla de equivalencia oficial de INEGI para armonizar el cambio
  de clasificador aplicado por la ENOE desde 2021-III.
  [`armoniza_sinco()`](https://aniuxa.github.io/renoe/reference/armoniza_sinco.md)
  ahora distingue CMO, SINCO 2011 y SINCO 2019, conserva el código
  original y no selecciona arbitrariamente las correspondencias
  uno-a-varios.
- Se incorporó
  [`armonizar_carreras_enoe()`](https://aniuxa.github.io/renoe/reference/armonizar_carreras_enoe.md)
  para identificar el clasificador de carreras vigente, conservar los
  códigos originales y canónicos, y producir campos armonizados ARM8 y
  ARM10 a lo largo de la serie. La función documenta explícitamente las
  correspondencias detalladas ambiguas del clasificador de 2005.
- Se corrigió `mujer_universitaria` en
  [`procesar_libro1()`](https://aniuxa.github.io/renoe/reference/procesar_libro1.md):
  ahora identifica exclusivamente a mujeres con licenciatura o
  profesional, maestría o doctorado (`cs_p13_1` entre 7 y 9), excluyendo
  estudios normales y técnicos. También se generan
  `nivel_educativo_codigo` y `educacion_universitaria` para hacer
  transparente la regla.
- Se incorporó la función
  [`procesar_contribucion_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_contribucion_hogar.md)
  para generar ingreso ocupacional individual deflactado, agregados del
  hogar, indicadores per cápita y quintiles ponderados de ingreso y
  trabajo no remunerado.
- Se actualizaron las funciones
  [`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md),
  [`procesar_tiempo()`](https://aniuxa.github.io/renoe/reference/procesar_tiempo.md),
  [`procesar_vars_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_vars_hogar.md)
  y
  [`procesar_vars_laborales()`](https://aniuxa.github.io/renoe/reference/procesar_vars_laborales.md)
  para estandarizar el uso de etiquetas mediante
  [`sjlabelled::var_labels()`](https://strengejacke.github.io/sjlabelled/reference/set_label.html)
  y
  [`sjlabelled::val_labels()`](https://strengejacke.github.io/sjlabelled/reference/set_labels.html).
- En esa versión,
  [`procesar_tiempo()`](https://aniuxa.github.io/renoe/reference/procesar_tiempo.md)
  recodificaba faltantes a cero; la versión 0.2.0 conserva este
  comportamiento sólo en las columnas `*_legacy` y mediante
  `tratamiento_faltantes = "historico_cero"`.
- Se actualizó
  [`procesar_vars_laborales()`](https://aniuxa.github.io/renoe/reference/procesar_vars_laborales.md)
  para reflejar cambios en la variable de experiencia previa de trabajo,
  usando `p2h4` en la construcción de `nunca_trabajo`.
- Se mejoró la claridad semántica del procesamiento de aportes del
  hogar, renombrando y etiquetando los quintiles derivados de ingreso y
  trabajo no remunerado con nombres más explícitos.
- Se actualizaron la vignette, el `README.md` y otros materiales
  introductorios del paquete para reflejar el flujo recomendado actual
  de procesamiento.
- Se preparó la infraestructura de citación del paquete mediante
  archivos de citación y metadatos actualizados para la versión `0.1.4`.

### Cambios en fusión y compatibilidad

- Se reforzó la estrategia de fusión robusta en
  [`fusion_enoe()`](https://aniuxa.github.io/renoe/reference/fusion_enoe.md)
  para manejar cambios recientes en nombres de variables clave entre
  trimestres.
- Se incorporó la homologación de nombres de entidad federativa cuando
  las bases utilizan `cve_ent` en lugar de `ent`, facilitando la
  compatibilidad con trimestres recientes.
- Se mejoró la detección de claves de unión y se evitó el uso de todas
  las columnas compartidas como variables de empalme en la fusión,
  reduciendo errores por incompatibilidades de tipo.

### Cambios en documentación y mantenimiento interno

- Se actualizó la lista de
  [`utils::globalVariables()`](https://rdrr.io/r/utils/globalVariables.html)
  para incluir las nuevas variables derivadas creadas en funciones
  recientes y retirar nombres obsoletos.
- Se revisaron los imports del paquete para reflejar el uso actual de
  [`sjlabelled::var_labels()`](https://strengejacke.github.io/sjlabelled/reference/set_label.html),
  [`sjlabelled::val_labels()`](https://strengejacke.github.io/sjlabelled/reference/set_labels.html)
  y
  [`dineq::ntiles.wtd()`](https://rdrr.io/pkg/dineq/man/ntiles.wtd.html).
- Se actualizaron ejemplos y documentación del flujo de procesamiento
  para incorporar
  [`procesar_contribucion_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_contribucion_hogar.md)
  y la nueva secuencia sugerida de análisis.

### Notas

- Esta versión fortalece la consistencia interna del paquete,
  especialmente en el etiquetado de variables y en la compatibilidad
  entre distintos periodos de la ENOE.
- Se mantiene la compatibilidad con versiones previas del paquete y con
  el enfoque de análisis reproducible en R.

## renoe 0.1.3

### Nuevas funciones y mejoras

- Se incorporó una clasificación de parentesco (`relative`) robusta y
  compatible con todas las versiones de ENOE desde 2005. Esta
  clasificación se ajusta dinámicamente según el año (`anio`) y
  trimestre (`trimestre`) de la encuesta, utilizando los archivos
  externos `par_c1.csv` y `par_c2.csv` incluidos en `extdata/`.
- Se actualizó la función
  [`procesar_vars_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_vars_hogar.md)
  para utilizar esta nueva clasificación, mejorando la identificación de
  hogares extensos, compuestos y otras estructuras familiares, en
  especial para los periodos anteriores a 2013.
- Se asegura que todos los códigos `par_c` sean clasificados. Si un
  código no se encuentra en los catálogos correspondientes, se asigna
  automáticamente a la categoría de “otro sin parentesco”
  (`relative = 6`).

### Cambios en el etiquetado

- La función
  [`.procesar_etiquetas_enoe()`](https://aniuxa.github.io/renoe/reference/dot-procesar_etiquetas_enoe.md)
  excluye ahora la variable `par_c` del etiquetado automático de
  catálogos, debido a errores en los datos abiertos del INEGI que podían
  asignar descripciones incorrectas.

Este release mejora la confiabilidad de los análisis sobre estructura
familiar en los microdatos de ENOE, con mayor precisión histórica y
menor dependencia de catálogos inconsistentes.

## renoe 0.1.2 (23 de julio de 2025)

### Cambios en esta versión

- Se añadió la función
  [`procesar_vars_laborales()`](https://aniuxa.github.io/renoe/reference/procesar_vars_laborales.md)
  para calcular desajuste educativo, condiciones contractuales y
  experiencia laboral previa, utilizando códigos SINCO armonizados y
  clasificadores de nivel educativo.
- Se incorporó la función
  [`imputa_ingocup()`](https://aniuxa.github.io/renoe/reference/imputa_ingocup.md)
  para **imputar ingresos ocupacionales** usando métodos de imputación
  múltiple (`mice`) entre personas ocupadas, facilitando análisis más
  robustos. Esta mejora representa un avance importante en el
  procesamiento analítico.
- Las funciones `procesar_vars_*()` ahora comparten la etiqueta
  `@family procesamiento_enoe` para facilitar su consulta conjunta en la
  documentación.
- Se actualizaron los tests automatizados con `testthat` para validar
  etiquetas, clases y contenido de variables generadas.
- Se mejoró el manejo condicional de variables que difieren entre el
  cuestionario ampliado y básico, evitando errores en `mutate()` cuando
  ciertas columnas no existen.
- Se corrigieron advertencias de clase y etiquetas faltantes en las
  variables derivadas.

### Notas

- Las funciones
  [`procesar_tiempo()`](https://aniuxa.github.io/renoe/reference/procesar_tiempo.md)
  y
  [`procesar_vars_laborales()`](https://aniuxa.github.io/renoe/reference/procesar_vars_laborales.md)
  ahora discriminan internamente el tipo de cuestionario (`coe_tipo`)
  para evitar errores cuando hay campos incompatibles.
- El paquete mantiene compatibilidad con versiones previas (≥ R 4.1.0) y
  continúa usando operadores modernos (`|>`, funciones lambda `\(x)`).

## renoe 0.1.1 (20 de julio de 2025)

### Cambios en esta versión

- Se agregó soporte para descargar y procesar archivos del **año 2025**.
- Se corrigió el caso especial del trimestre **2022 T1**, reemplazando
  todos los archivos defectuosos por versiones estables provenientes del
  portal de microdatos de INEGI.
- Se incorporó la opción de **fusión robusta** (`fusion_robusta = TRUE`)
  en
  [`fusion_enoe()`](https://aniuxa.github.io/renoe/reference/fusion_enoe.md),
  para manejar variaciones en las claves de fusión entre años.
- Se añadió validación del número de filas esperadas tras el filtrado
  (`r_def == 0 & c_res != 2`), y se emiten advertencias si el número de
  filas fusionadas difiere.
- Se actualizó el manejo de claves para empatar las tablas. Ahora
  [`fusion_enoe()`](https://aniuxa.github.io/renoe/reference/fusion_enoe.md)
  llama internamente a
  [`crear_folios()`](https://aniuxa.github.io/renoe/reference/crear_folios.md)
  para generar los identificadores únicos `folio`, `folio2` y `folio3`
  de forma consistente.
- Mejora en documentación y ejemplos (`README.md`, `DESCRIPTION`, ayuda
  por función).

### Notas

- El paquete ahora requiere R \>= 4.1.0 debido al uso del operador `|>`
  y funciones abreviadas `\(x)` introducidas en esa versión.
- Algunas dependencias actualizadas: `arrow`, `haven`, `dplyr`, `readr`,
  entre otras.

## renoe 0.1.0 (28 de mayo de 2025)

### Lanzamiento inicial del paquete

- Primer conjunto de funciones para descargar, descomprimir, fusionar y
  cargar datos de la ENOE.
- Soporte para etiquetado automático, manejo de codificaciones y
  procesamiento sociodemográfico básico.
- Funciones auxiliares internas (`.leer_datos_enoe`,
  `.descargar_zip_enoe`, `.procesar_etiquetas_enoe`, entre otras).
