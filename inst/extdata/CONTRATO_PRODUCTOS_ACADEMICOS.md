# Contrato de productos académicos reproducibles

La ruta autorizada para preparar activos destinados a la Shiny es
`procesar_productos_academicos(..., escenario = "analysis_legacy")`.

El resultado debe conservar simultáneamente:

- `sinco_escenario == "analysis_legacy"`;
- `clasificaciones_escenario == "analysis_legacy"`;
- `productos_academicos_escenario == "analysis_legacy"`;
- `perfil_publicacion == "academic_reproducible"`.

La función delega en la armonización y los consumidores canónicos. No mantiene
reglas paralelas. Su equivalencia se prueba contra
`procesar_clasificaciones_reproducibles()` y contra el golden snapshot
`tests/testthat/golden/productos_academicos_analysis_legacy.csv`.

Este contrato no convierte la salida en un bundle publicable. La fase upstream
debe congelar cada producto y aportar `run_id`, versión y SHA-256 del paquete,
escenario, productor, SHA-256 del activo y evidencia de equivalencia. No pueden
mezclarse productos `integrated_accepted` ni sustituirse activos certificados.
