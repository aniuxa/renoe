# Correspondencia para desajuste horizontal

## Archivos

- `correspondencia_arm8_isco08_montt.csv`: reproducción estructurada de la
  tabla normativa internacional del Anexo 2 de Montt (2015). Sus códigos de
  ocupación pertenecen a ISCO-08 y no deben aplicarse directamente a SINCO.
- `correspondencia_campo_arm8_sinco3d.csv`: adaptación mexicana preliminar,
  normalizada a una fila por combinación `campo_arm8`–`sinco3d`.

## Estado de la matriz mexicana

La versión 0.1.0 es una propuesta para revisión. Distingue relaciones directas
y transversales, pero ninguna fila debe considerarse validada mientras
`estado_revision` conserve el valor `propuesta`. La ausencia de una combinación
no equivale todavía a desajuste: las ocupaciones no relacionadas se incorporarán
explícitamente después de la revisión sustantiva.

La matriz se construyó mediante la traducción conceptual de la tabla ISCO-08 de
Montt (2015) a las descripciones y criterios de especialización del SINCO 2011.
No existe identidad numérica entre los códigos ISCO-08 y SINCO.

## Citación provisional

Escoto, Ana, y equipo del proyecto IN305925 (2026). *Correspondencia propuesta
entre campos de formación armonizados de la ENOE y ocupaciones SINCO a tres
dígitos*, versión 0.1.0 [archivo de datos]. `renoe`.

La autoría deberá completarse con los nombres y apellidos de quienes participen
en la validación final.

## Referencias

- Montt, G. (2015). *The causes and consequences of field-of-study mismatch:
  An analysis using PIAAC*. OECD Social, Employment and Migration Working
  Papers, No. 167. https://doi.org/10.1787/5jrxm4dhv9r2-en
- Wolbers, M. H. J. (2003). Job mismatches and their labour-market effects
  among school-leavers in Europe. *European Sociological Review*, 19(3),
  249–266. https://doi.org/10.1093/esr/19.3.249
- International Labour Organization. *Education and Mismatch Indicators*.
  https://ilostat.ilo.org/methods/concepts-and-definitions/description-education-and-mismatch-indicators/
- Somers, M. A., Cabus, S. J., Groot, W., and van den Brink, H. M. (2019).
  Horizontal mismatch between employment and field of education: Evidence
  from a systematic literature review. *Journal of Economic Surveys*, 33(2),
  567–603. https://doi.org/10.1111/joes.12271
