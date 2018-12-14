setwd("/home/fou/Desktop/Ecobici/DataSubset/")
library(readr)
library(lubridate)
library(hms)
lista <- dir()
tabla1 <- read_csv(file=lista[1])
for (i in lista[2:22])
{
  mes <- read_csv(file=i)
  #mes$Fecha_Retiro <- ymd(mes$Fecha_Retiro)
  #mes$Fecha_Arribo <- ymd(mes$Fecha_Arribo)
  #mes$Hora_Retiro <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , mes$Hora_Retiro) ))
  #mes$Hora_Arribo <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , mes$Hora_Arribo) ))
  tabla1 <- rbind(tabla1, mes)
  print(i)
  gc()
  Sys.sleep(1)
}
tabla1 <- na.omit(tabla1)
library(dplyr)
names(tabla1)
#########################
class(tabla1)
estaciones.muestral <- unique(c(unique(tabla1$Ciclo_Estacion_Retiro), unique(tabla1$Ciclo_Estacion_Arribo)))
library(jsonlite)
library(dplyr)
fromJSON('/home/fou/Desktop/Ecobici/estaciones.json') %>% as.data.frame -> estaciones
estaciones.id <- unique(estaciones$stations.id)
sum( estaciones.id %in% estaciones.muestral )
tabla2 <- tabla1[ tabla1$Ciclo_Estacion_Retiro %in% estaciones.id,  ]
tabla2 <- tabla2[ tabla2$Ciclo_Estacion_Arribo %in% estaciones.id,  ]
save(tabla2, file='tablaFiltro.Rdata')
load( file='tablaFiltro.Rdata')

