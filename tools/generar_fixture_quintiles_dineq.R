suppressPackageStartupMessages(library(dplyr))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop('Uso: Rscript generar_fixture_quintiles_dineq.R BASE_032 DIRECTORIO_SALIDA')
pkgload::load_all(args[1], quiet=TRUE)
if (as.character(packageVersion('renoe')) != '0.3.2') stop('Se requiere el codigo original de renoe 0.3.2')
if (as.character(packageVersion('dineq')) != '0.1.0') stop('Se requiere dineq 0.1.0')
dir.create(args[2], recursive=TRUE, showWarnings=FALSE)
capturar <- function(f) {
 avisos <- character()
 valor <- tryCatch(withCallingHandlers(f(),warning=function(w) {
  avisos <<- c(avisos,conditionMessage(w)); invokeRestart('muffleWarning')
 }),error=function(e) structure(conditionMessage(e),class='error_cuantiles'))
 list(valor=valor,avisos=avisos)
}
casos <- list(
 ordinario=list(x=1:10,w=rep(1,10)), empates=list(x=c(rep(0,5),1:5),w=rep(1,10)),
 pesos_desiguales=list(x=1:5,w=c(1,1,6,1,1)),
 pesos_fraccionarios=list(x=c(-2,0,0,5,9),w=c(.1,.2,.3,.9,.7)),
 ceros_y_faltantes=list(x=c(1,2,NA,4,5,6),w=c(1,NA,1,0,1,1)),
 mismos_valores=list(x=rep(3,5),w=rep(1,5)), un_valor=list(x=3,w=1),
 sin_datos=list(x=numeric(),w=numeric()), todos_x_na=list(x=rep(NA_real_,5),w=rep(1,5)),
 todos_w_na=list(x=1:5,w=rep(NA_real_,5)), cero_total=list(x=1:5,w=rep(0,5)),
 negativo=list(x=1:5,w=c(1,1,-1,1,1)),
 infinito_en_x=list(x=c(-Inf,0,1,2,Inf),w=rep(1,5)),
 infinito_en_w=list(x=1:5,w=c(1,1,Inf,1,1)),
 nan=list(x=c(1,NaN,3,4,5),w=c(1,1,NaN,1,1)),
 decimales_cercanos=list(x=c(805.81183981210165,805.81183981210199,1000,2000,3000),w=rep(1,5))
)
casos <- lapply(casos,function(z) {z$esperado<-capturar(function() dineq::ntiles.wtd(z$x,5,z$w));z})
set.seed(20261112)
simulados <- lapply(seq_len(100),function(i) {
 x<-sample(c(NA_real_,-1:8),100,replace=TRUE); w<-sample(c(NA_real_,0,.1,.5,1,10,1000),100,replace=TRUE)
 list(x=x,w=w,esperado=dineq::ntiles.wtd(x,5,w))
})
datos <- data.frame(anio=rep(c(2023L,2024L),each=10),trim=1L,
 folio2=rep(rep(paste0('hogar_',1:5),each=2),2),
 ingocup_imp=c(0,10,15,NA,20,20,30,40,50,60,2,4,6,8,10,NA,12,14,16,18),
 ipc=100,hrsocup=40,t_trabajo_hogar_armonizado=rep(c(0,1,2,NA,4),4),
 t_total_instrumento=rep(c(0,2,4,NA,8),4),tam_hog=2,fac=rep(rep(c(1,2,3,4,5),each=2),2))
salida <- procesar_contribucion_hogar(datos)
quints <- c('quintil_ing_ind','quintil_ing_hog_pc','quintil_norem_pc_armonizado','quintil_ing_hog_pc_sego')
flujo <- list(datos=datos,esperado=lapply(salida[quints],as.numeric),version=as.character(packageVersion('renoe')))
fixture <- list(metadata=list(dineq=as.character(packageVersion('dineq')),Hmisc=as.character(packageVersion('Hmisc')),R=as.character(getRversion()),semilla=20261112),casos=casos,simulados=simulados)
destinos <- file.path(args[2],c('quintiles_dineq_0.1.0.rds','flujo_contribucion_032.rds'))
if (any(file.exists(destinos))) stop('Los archivos de salida ya existen; no se sobrescriben')
saveRDS(fixture,destinos[1],version=2)
saveRDS(flujo,destinos[2],version=2)
cat('Referencias de renoe 0.3.2 guardadas en',args[2],'\n')