# Cobertura efectiva con todas las capas y familias activas

Evaluacion de personas ocupadas con ponderador finito no negativo.
CMO: ENOE 2012-T2; SINCO 2019: ENOE 2021-T3. Son cortes de referencia, no promedios de toda la serie.
Las funciones se ejecutan con oficial, panel, enoe y consenso activos y todas las familias seleccionadas.
La referencia de mismatch2 se calcula efectivamente por trimestre, usando fac.

| Salida | CMO registros % | CMO ponderado % | 2019 registros % | 2019 ponderado % |
|---|---:|---:|---:|---:|
| sinco4d | 86.28 | 87.35 | 98.87 | 98.90 |
| sinco3d | 92.27 | 93.17 | 98.87 | 98.90 |
| sinco2d | 92.27 | 93.17 | 98.87 | 98.90 |
| sinco1d | 100.00 | 100.00 | 98.87 | 98.90 |
| skill_level | 100.00 | 100.00 | 98.87 | 98.90 |
| skill_actual | 99.95 | 99.95 | 99.88 | 99.89 |
| mismatch | 99.95 | 99.95 | 98.75 | 98.80 |
| esco_ref | 99.85 | 99.83 | 98.68 | 98.74 |
| mismatch2 | 99.85 | 99.83 | 98.68 | 98.74 |
| grupo_ocu9_damian | 100.00 | 100.00 | 98.87 | 98.90 |
| isco88_damian | 88.40 | 89.50 | 98.84 | 98.87 |
| clase_ocu_damian | 100.00 | 100.00 | 98.87 | 98.90 |
| calificada_damian | 100.00 | 100.00 | 98.87 | 98.90 |
| manual_damian | 100.00 | 100.00 | 98.87 | 98.90 |
| supervisa_damian | 99.22 | 99.29 | 98.87 | 98.90 |
| clase_egp13_damian | 96.52 | 96.75 | 98.85 | 98.88 |
| clase_egp7_damian | 97.52 | 97.59 | 98.85 | 98.88 |
| clase_alt6_damian | 97.57 | 97.59 | 98.85 | 98.88 |
| macro_egp4_damian | 98.22 | 98.28 | 98.85 | 98.88 |
| macro_solis4_damian | 97.83 | 97.88 | 98.85 | 98.88 |
| egp3_damian | 98.31 | 98.35 | 98.85 | 98.88 |
| baja_damian | 99.63 | 99.66 | 98.85 | 98.88 |
| alta_damian | 98.78 | 98.81 | 98.85 | 98.88 |
| class_ocu | 99.86 | 99.86 | 99.94 | 99.95 |
| isco_care | 99.86 | 99.86 | 99.94 | 99.95 |
| care_industry | 99.99 | 99.97 | 99.92 | 99.93 |
| care_w | 99.85 | 99.84 | 99.88 | 99.89 |
| cuida_total | 99.85 | 99.84 | 99.88 | 99.89 |
| cuida_1d | 99.85 | 99.84 | 99.88 | 99.89 |
| trabajo_cuidado_mercado | 99.85 | 99.84 | 99.88 | 99.89 |
| cuidado_posicion_remunerada | 99.85 | 99.84 | 99.88 | 99.89 |
| cuidado_sin_pago | 99.85 | 99.84 | 99.88 | 99.89 |
| susceptible_teletrabajo | No aplicable | No aplicable | 100.00 | 100.00 |

## Interpretacion

- Cobertura es disponibilidad de salida, no exactitud ni concordancia.
- Las reglas ENOE de EGP recuperan 571 casos de CMO7201 y 188 de CMO1204 en 2012T2 sin rellenar SINCO. La segunda tiene categoria panel_semantico por muestra limitada.
- skill_actual se incluye como insumo de contraste educativo: no depende de SINCO.
- En CMO el puente manual permite 100% a 1d; no significa equivalencia oficial completa.
- El consenso actual solo recupera clasificaciones del periodo CMO; no se aplica a SINCO 2019.
- El puente 2019 no recupera convergencias de destinos multiples a niveles menores: sus pendientes 4d quedan tambien pendientes 3d/2d/1d.
- Care usa puente propio en CMO y version observada en 2019, junto con p4a. Su respaldo de panel no es su porcentaje de clasificacion.
- Teletrabajo recibe p3coe observado y periodo; no es comparable antes de 2012-T3. Su cobertura computacional cuenta los codigos no vacios, incluso 9999 si aparecen: la funcion no valida exhaustivamente el catalogo.
- Grupo/clase/calificada/manual pueden utilizar respaldo por gran grupo; EGP e ISCO tienen requisitos mas finos.
- La capa consenso completa cada salida independientemente; no se ha agregado propagacion posterior de ISCO recuperado hacia EGP.
- diagnosticos_cobertura_dos_puentes.csv separa el estado determinado del ingreso entre personas identificadas como cuidadoras.

Reproducir: Rscript data-raw/cmo_sinco/evaluar_cobertura_dos_puentes.R RUTA_ENOE
