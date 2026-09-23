# Cobertura maxima de decisiones SINCO

Estado: `integrated`. Incluye todas las decisiones aceptadas actualmente; excluye candidatos experimentales.
Cobertura significa salida no faltante, no exactitud de la correspondencia.

| Clasificacion | CMO registros | CMO ponderada | 2019 registros | 2019 ponderada | Riesgo CMO / 2019 |
|---|---:|---:|---:|---:|---|
| SINCO 2011 a 4 digitos | 86.28% | 87.35% | 98.87% | 98.90% | alto / bajo_medio |
| SINCO 2011 a 3 digitos | 92.27% | 93.17% | 98.87% | 98.90% | medio / bajo_medio |
| SINCO 2011 a 1 digito | 100.00% | 100.00% | 98.87% | 98.90% | medio / bajo_medio |
| Correspondencia ISCO-88 | 88.40% | 89.50% | 98.84% | 98.87% | alto / medio |
| Supervision | 99.22% | 99.29% | 98.87% | 98.90% | medio / bajo_medio |
| EGP13 | 99.86% | 99.87% | 98.85% | 98.88% | alto / medio |
| EGP7 | 97.52% | 97.59% | 98.85% | 98.88% | medio / bajo_medio |
| Macro EGP4 | 98.22% | 98.28% | 98.85% | 98.88% | medio / bajo_medio |
| Clase ocupacional Damian | 100.00% | 100.00% | 98.87% | 98.90% | bajo_medio / bajo_medio |
| Clasificacion de cuidado | 99.85% | 99.84% | 99.88% | 99.89% | medio / medio |
| Trabajo de cuidado de mercado | 99.85% | 99.84% | 99.88% | 99.89% | medio / medio |
| Desajuste educativo normativo | 99.95% | 99.95% | 98.75% | 98.80% | medio_alto / medio |
| Desajuste educativo estadistico | 99.85% | 99.83% | 98.68% | 98.74% | medio_alto / medio |
| Susceptibilidad de teletrabajo | No aplicable | No aplicable | 100.00% | 100.00% | no_aplicable / medio |

## Reglas de lectura

- `alto`: exige estratificar por evidencia y presentar sensibilidad.
- `medio` o `medio_alto`: utilizable si se conserva calidad, regla y auxiliares.
- `bajo_medio`: apropiado para resultados agregados; no implica equivalencia oficial.
- El consenso se aplica por clasificacion y no rellena automaticamente SINCO4d.
- En 2019 el consenso longitudinal no esta activado; los pendientes proceden del puente oficial.

Reproducir despues de la auditoria principal con:
`Rscript data-raw/cmo_sinco/documentar_cobertura_maxima.R`.
