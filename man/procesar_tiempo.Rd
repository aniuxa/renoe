% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/procesar_tiempo.R
\name{procesar_tiempo}
\alias{procesar_tiempo}
\title{Procesar variables de tiempo en actividades del hogar y cuidado}
\usage{
procesar_tiempo(data, anio, trimestre)
}
\arguments{
\item{data}{Un data frame fusionado por \code{fusion_enoe()} o cargado directamente, que contenga variables de tiempo (\verb{p11_*} o \verb{p9_*}).}

\item{anio}{Año del trimestre (numérico).}

\item{trimestre}{Trimestre numérico (1–4).}
}
\value{
Un data frame con variables de tiempo en minutos y horas, y etiquetas descriptivas.
}
\description{
Calcula el tiempo (en minutos y horas) dedicado a diversas actividades como estudiar, cuidar, quehaceres, construcción, reparación, comunidad, compras y traslados.
Soporta tanto el cuestionario ampliado como el básico, ajustando el mapeo de variables por año. Si no existen las variables sociodemográficas requeridas (\code{anio}, \code{coe_tipo}), las crea con \code{procesar_vars_sociodemo()}.
}
\examples{
\dontrun{
datos <- fusion_enoe(2022, 1)
datos <- procesar_tiempo(datos, anio = 2022, trimestre = 1)
summary(datos$t_cuidado)
}
}
\seealso{
Other procesamiento_enoe: 
\code{\link{armoniza_sinco}()},
\code{\link{crear_folios}()},
\code{\link{imputa_ingocup}()},
\code{\link{ipc_enoe}()},
\code{\link{procesar_variables_enoe}()},
\code{\link{procesar_vars_hogar}()},
\code{\link{procesar_vars_laborales}()},
\code{\link{procesar_vars_sociodemo}()}
}
\concept{procesamiento_enoe}
