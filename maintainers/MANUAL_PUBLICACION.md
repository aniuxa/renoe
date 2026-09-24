# Manual de publicación de renoe

## Índice

1. [Regla de cierre](#1-regla-de-cierre)
2. [Alcance y glosario](#2-alcance-y-glosario)
3. [Responsables y fuentes canónicas](#3-responsables-y-fuentes-canónicas)
4. [Preparación de una versión candidata](#4-preparación-de-una-versión-candidata)
5. [Secuencia de publicación fail-closed](#5-secuencia-de-publicación-fail-closed)
6. [Shiny y productos de datos](#6-shiny-y-productos-de-datos)
7. [Checklist copiable](#7-checklist-copiable)
8. [Hotfix, rollback y release retrospectivo](#8-hotfix-rollback-y-release-retrospectivo)
9. [Controles automáticos](#9-controles-automáticos)
10. [Postmortem 0.2.0 y 0.3.1](#10-postmortem-020-y-031)

## 1. Regla de cierre

**No se publica ni se cierra un GitHub Release hasta verificar dos resultados producidos desde una instalación limpia del mismo commit etiquetado:**

- la URL pública muestra la versión nueva en la página principal, la cita y el índice de artículos;
- `citation("renoe")` devuelve esa misma versión y el título con su codificación correcta.

Una ejecución verde de R CMD check o de GitHub Pages no sustituye estas comprobaciones. Ante cualquier discrepancia, el proceso se detiene; no se corrige directamente sobre el sitio vivo.

## 2. Alcance y glosario

- **Versión del paquete:** valor `Version` de `DESCRIPTION`; es la fuente primaria del número de versión.
- **Commit:** instantánea Git inmutable que contiene código, metadatos, viñetas y salida pkgdown coherentes.
- **Tag:** referencia inmutable `vX.Y.Z` que debe señalar exactamente el commit aprobado.
- **GitHub Release:** ficha pública asociada al tag. Se prepara como borrador y se publica sólo al terminar las verificaciones.
- **Tarball:** archivo `renoe_X.Y.Z.tar.gz` generado con `R CMD build` desde el commit etiquetado.
- **pkgdown:** sitio estático generado desde ese mismo commit. `docs/` es salida generada, no fuente editorial.
- **Shiny:** aplicación desplegable con ciclo y rollback propios; no forma parte del cierre del paquete.
- **Producto de datos:** CSV, Parquet, RDS u otro activo publicable, con versión, procedencia y hash propios.
- **Release candidate (RC):** commit congelado que pasó controles locales y de CI, pero aún no es un release cerrado.

## 3. Responsables y fuentes canónicas

La mantenedora del release designa antes de iniciar: responsable de versión, revisor/a técnico/a y responsable de publicación. Una misma persona puede ocupar más de un rol, pero cada casilla del checklist debe tener nombre y evidencia.

| Materia | Fuente canónica | Derivados que deben concordar |
|---|---|---|
| Número de versión | `DESCRIPTION` | `NEWS.md`, `CITATION.cff`, `inst/CITATION`, README, viñetas, pkgdown, tag y Release |
| Cambios publicados | primer encabezado de `NEWS.md` | notas del GitHub Release |
| Cita en R | `inst/CITATION` | `citation("renoe")`, `authors.html`, cita del inicio |
| Metadatos de forja | `CITATION.cff` | ficha de GitHub y archivado externo, si aplica |
| Narrativa de uso | `README.Rmd` y viñetas | `README.md` y artículos pkgdown |
| Sitio público | fuentes anteriores + `_pkgdown.yml` | `docs/`, que debe reconstruirse y no editarse a mano |
| Artefacto instalable | commit etiquetado | tarball y SHA-256 adjuntos al Release |

## 4. Preparación de una versión candidata

1. Crear una rama candidata desde la rama principal actualizada y registrar el commit base.
2. Fijar `Version` en `DESCRIPTION` y abrir el encabezado correspondiente en `NEWS.md`.
3. Sincronizar la misma versión en `CITATION.cff`, `inst/CITATION`, `README.Rmd`, `README.md`, viñetas de migración y borrador de notas del Release.
4. Regenerar `README.md` desde `README.Rmd`; no mantenerlos manualmente con cifras distintas.
5. Ejecutar documentación y pruebas:

   ```sh
   Rscript tools/development/verificar_release.R
   R CMD build .
   R CMD check renoe_X.Y.Z.tar.gz --no-manual
   ```

6. Instalar el tarball en una biblioteca vacía y comprobar `packageVersion("renoe")`, ejemplos pertinentes y `citation("renoe")`.
7. Ejecutar la prueba Unicode y revisar que no existan U+FFFD, mojibake ni `ano`/`anos` donde corresponde `año`/`años`.
8. Construir pkgdown desde la RC, comprobar enlaces y confirmar que `docs/` coincide exactamente con la reconstrucción:

   ```sh
   Rscript -e "pkgdown::build_site(new_process = FALSE, install = TRUE)"
   Rscript tools/development/verificar_release.R --docs=docs
   Rscript tools/development/verificar_pkgdown_local.R --docs=docs
   git diff --exit-code -- docs
   ```

9. Revisar ejemplos ejecutables, títulos de migración, índice de referencia, cita, créditos y navegación en HTML.
10. Abrir la propuesta de fusión. La RC es el commit que obtiene R CMD check y `pkgdown-release-gate` en verde, sin excepciones manuales silenciosas.

## 5. Secuencia de publicación fail-closed

La secuencia es deliberadamente estricta:

1. **Fusionar la candidata** aprobada en la rama principal. No añadir cambios después de la RC sin repetir todos los controles.
2. **Etiquetar el commit exacto**: `vX.Y.Z` debe apuntar al SHA fusionado y aprobado.
3. **Crear el GitHub Release como borrador**, asociado a ese tag; adjuntar el tarball construido desde el mismo SHA y su archivo SHA-256. Verificar el hash después de descargar el adjunto.
4. **Desplegar pkgdown desde ese mismo commit/tag**. En la configuración actual, `docs/` debe estar versionado y Pages lo publica desde el commit de `master`; por ello el tag y `master` deben coincidir en el SHA de cierre.
5. Esperar el despliegue de Pages y ejecutar manualmente `pkgdown-release-gate` con `verify_public=true`.
6. Comprobar en navegador la página principal, `authors.html`, artículos y referencia. Instalar de nuevo el tarball descargado del borrador y ejecutar `citation("renoe")`.
7. **Sólo entonces publicar/cerrar el GitHub Release.** Registrar los run IDs, SHA, tag, hashes, URLs y hora en el checklist.

Si cualquier paso falla, el Release permanece en borrador y el tag no se mueve. Una corrección posterior usa un nuevo commit y, si el tag ya salió del ámbito local, una nueva versión; no se reescribe historia pública.

### Reconstrucción limpia y despliegue exacto

El sitio se reconstruye como árbol completo; no se copian páginas sueltas ni se reutiliza una carpeta anterior:

```sh
Rscript -e "pkgdown::build_site(new_process = FALSE, install = TRUE, clean = TRUE)"
Rscript tools/development/verificar_release.R --docs=docs
Rscript tools/development/verificar_pkgdown_local.R --docs=docs
git diff --exit-code -- docs
test -z "$(git status --porcelain -- docs)"
```

El candidato [`pkgdown-release-gate`](../.github/workflows/pkgdown.yaml) ejecuta esos controles desde un checkout limpio. El despliegue vigente lo realiza el workflow administrado por GitHub `pages-build-deployment`: ante un cambio en `master`, hace checkout, construye con Jekyll, sube un artefacto y lo despliega. El run ID debe corresponder al mismo SHA etiquetado. No existe un comando local autorizado que publique; cualquier migración a otro mecanismo de Pages requiere una propuesta separada.

Un build incremental puede dejar páginas heterogéneas porque actualiza archivos alcanzados pero no necesariamente elimina páginas obsoletas. Pages puede desplegar correctamente ese árbol mixto: el “éxito” significa que Jekyll publicó el contenido recibido, no que pkgdown haya reconstruido toda la documentación ni que sus versiones sean coherentes. Por eso la compuerta exige `clean = TRUE`, árbol Git sin diferencias ni archivos no versionados y verificación posterior de cinco páginas públicas.

## 6. Shiny y productos de datos

Shiny y los productos de datos tienen despliegues separados. No deben incorporarse como cambios oportunistas al cierre del paquete.

Para cada activo se registra: repositorio o fuente, versión, commit, fecha, tamaño, SHA-256, URL, responsable, prueba de lectura y plan de rollback. El paquete sólo referencia una versión de datos ya publicada y verificada. Una aplicación Shiny registra además configuración, secretos fuera de Git, health check y revisión de logs.

El fallo de Shiny o de un producto de datos no se resuelve sustituyendo el tarball o moviendo el tag del paquete. Se revierte el activo a su versión previa y se abre una incidencia independiente.

## 7. Checklist copiable

```text
Versión candidata:
Responsable de versión:
Revisión técnica:
Responsable de publicación:
Commit base:
Commit RC aprobado:
Tag previsto:

[ ] DESCRIPTION =
[ ] Primer encabezado de NEWS.md =
[ ] CITATION.cff =
[ ] inst/CITATION / citation("renoe") =
[ ] README.Rmd y README.md =
[ ] Viñetas y notas de Release =
[ ] R CMD check: run_id/URL =
[ ] pkgdown-release-gate: run_id/URL =
[ ] Pruebas y ejemplos relevantes =
[ ] UTF-8/mojibake =
[ ] Enlaces pkgdown =
[ ] git diff --exit-code -- docs =

Tarball:
SHA-256 local:
SHA-256 del adjunto descargado:
URL del borrador de Release:
Commit al que apunta el tag:
URL pública pkgdown:
Versión visible en portada:
Versión visible en la cita:
Resultado de citation("renoe") desde instalación limpia:
Run ID de verificación pública:
Fecha/hora y zona de verificación:

Shiny (si aplica; ticket separado):
Producto(s) de datos (si aplica; ticket separado):
Rollback preparado:

[ ] Tag, commit, tarball y hash son concordantes.
[ ] La web pública fue comprobada después del despliegue.
[ ] citation("renoe") fue comprobada desde el tarball descargado.
[ ] El Release puede dejar de ser borrador.
```

## 8. Hotfix, rollback y release retrospectivo

- **Hotfix:** crear `X.Y.Z+1` o la siguiente versión patch desde el tag afectado, aplicar sólo la corrección, repetir el proceso completo y documentar el alcance. No reemplazar artefactos de un Release ya publicado.
- **Rollback de pkgdown:** restaurar en una nueva revisión el `docs/` del último tag sano y dejar trazabilidad del incidente. No mover el tag sano ni editar HTML directamente en producción.
- **Rollback de paquete:** retirar enlaces promocionales si es necesario, marcar el Release afectado con una advertencia y publicar una versión correctiva. Los tarballs y hashes anteriores permanecen inmutables.
- **Release retrospectivo:** usar sólo cuando una versión o tag existente careció de GitHub Release. Verificar primero el SHA histórico, reconstruir o identificar el tarball sin fingir una fecha contemporánea, declarar expresamente que la formalización es retrospectiva y no mover el tag. Si no puede reproducirse el artefacto exacto, documentar la limitación.

## 9. Controles automáticos

El candidato [`workflows/pkgdown.yaml`](../.github/workflows/pkgdown.yaml) usa [`verificar_release.R`](../tools/development/verificar_release.R) y falla si:

- discrepan `DESCRIPTION`, `NEWS.md`, `CITATION.cff`, `inst/CITATION`, README, HTML pkgdown o el tag;
- falla la prueba Unicode;
- el tarball no instala limpiamente o `citation("renoe")` no refleja la versión;
- pkgdown no construye, hay enlaces locales rotos o `docs/` difiere de la reconstrucción;
- en la ejecución manual previa al cierre, la URL pública todavía muestra otra versión.

Debe marcarse `R-CMD-check` y `pkgdown-release-gate` como comprobaciones obligatorias de la rama principal. El workflow tiene sólo permiso de lectura: valida y conserva artefactos de la RC, pero no publica, mueve tags ni cierra Releases.

## 10. Postmortem 0.2.0 y 0.3.1

### Hechos verificados: 0.2.0

- El código y los metadatos públicos para 0.2.0 se prepararon el 10 de septiembre de 2026; el tag anotado `v0.2.0` apunta al commit `293779a3abc5a19a8ca04af7cb174276c8e046bb`.
- El objeto GitHub Release que existe actualmente fue creado el 10 de septiembre de 2026 a las 22:30:06 UTC y publicado a las 23:04:29 UTC, asociado al mismo commit.
- La evidencia histórica parcial aportada por la usuaria mostraba, en una consulta anterior, que la página de Releases todavía terminaba en 0.1.4. Esa captura no permite datar por sí sola cuánto tiempo estuvo 0.2.0 sin Release.

**Inferencia cautelosa:** 0.2.0 parece haber recibido una formalización retrospectiva respecto de su disponibilidad o preparación previa, no un cierre contemporáneo completo y verificable. No debe decirse que el Release 0.2.0 “nunca existió”: existe hoy. Este antecedente muestra que tag, versión, documentación y objeto Release pueden quedar desacoplados si el checklist no es obligatorio.

### Hechos verificados: 0.3.1

- El commit de release `1d44a63d4bff5135f539e8c8f091e682356e9ebd` y el tag `v0.3.1` declaraban 0.3.1 en `DESCRIPTION` y `CITATION.cff`.
- En ese mismo commit, `inst/CITATION` y `README.md` aún declaraban 0.3.0 en la cita. `docs/index.html` y `docs/authors.html` reprodujeron 0.3.0; el índice de artículos sólo mostraba “Migración a renoe 0.2.0”.
- La rama luego fusionada contenía el commit `6fe013a9cb919e3b90748724ff9d76a413d874d3` (“Rebuild pkgdown site for 0.3.1”), pero esa reconstrucción heredó la cita fuente 0.3.0.
- R-CMD-check y Pages terminaron correctamente para el SHA de release. Pages construyó con Jekyll y desplegó el contenido disponible; no ejecutó una reconstrucción pkgdown ni comparó versiones semánticas.
- El GitHub Release 0.3.1 fue publicado a las 22:54:59 UTC, después de que Pages informara éxito, pero sin una comprobación de la versión realmente visible.

### Evidencia pública antes/después

Una consulta sin caché el 23 de septiembre de 2026 obtuvo `Last-Modified: Wed, 23 Sep 2026 22:51:04 GMT` en las cinco páginas públicas. El estado vivo y la reconstrucción limpia aislada fueron:

| Página | Sitio público antes | Reconstrucción limpia candidata |
|---|---|---|
| `index.html` | cita 0.3.0 | cita 0.3.1; 0.2.0/0.3.0 sólo en explicación histórica |
| `authors.html` | cita 0.3.0 | cita 0.3.1 y “Ocupación” correctamente acentuada |
| `news/index.html` | historial 0.1.0–0.3.1 | historial completo; versiones antiguas explícitamente históricas |
| `articles/index.html` | sólo guía de migración 0.2.0 | guías 0.1.4→0.2.0 y 0.2.0→0.3.1 |
| `reference/index.html` | agrupación antigua | siete grupos funcionales y versión 0.3.1 |

La observación de interfaz que mostraba 0.2.0 en portada y 0.1.3 en autores no se reprodujo al consultar las URLs sin caché: ambas devolvieron cita 0.3.0. Se conserva como indicio de caché o estado previo, no como hecho actual. Esta discrepancia refuerza que la evidencia de cierre debe registrar respuesta sin caché, ETag/Last-Modified, run ID y hora.

### Causa raíz, factores y efecto

**Causa raíz:** el proceso aceptaba como éxito la presencia y el despliegue de archivos generados, pero no exigía que pkgdown se reconstruyera desde las fuentes finales ni que todas las representaciones de versión coincidieran antes de cerrar el Release.

**Factores contribuyentes:** varias fuentes manuales de cita; README fuente y derivado no sincronizados; un workflow de R CMD check sin prueba de frescura de `docs/`; Pages limitado a construir/desplegar el sitio estático; ausencia de una puerta obligatoria posterior al despliegue; y el precedente organizacional de 0.2.0, donde la formalización del Release no quedó integrada a un cierre único.

**Consecuencia observable:** la navegación mostraba 0.3.1, mientras la cita pública decía 0.3.0; faltaba la guía de migración 0.3.1, persistían textos `ano` y el pie duplicaba nombres. El despliegue fue técnicamente exitoso pero editorialmente inconsistente.

La prevención es el orden definido arriba: una RC inmutable, controles de coherencia y reconstrucción, despliegue desde el mismo SHA, verificación pública y sólo después cierre del Release.
