setwd("~/Desktop/Ecobici/Code")
morado <- rgb(t(col2rgb('#5C4788')/255), alpha= 0.5)
#########3librerias ##########
library(dtwclust)
library(rgeos)
library(dplyr)
library(lubridate)
load('Cluster.Rdata') # es la clusterizacion de las series por dia que hizo Danie
setwd("~/Desktop/Ecobici/DataSubset")
load('tablaFiltro.Rdata')
names(tabla2)
tabla2 %>% group_by(Ciclo_Estacion_Retiro, Fecha_Retiro) %>% summarise(viajes=n()) -> series.diarias
names(series.diarias)
library(ggplot2)
series.diarias$Fecha_Retiro <- dmy(series.diarias$Fecha_Retiro)
series.diarias <- as.data.frame(series.diarias) 
#class(series.diarias)
a <- Salida_3_geodesic@cluster
index <- match( series.diarias$Ciclo_Estacion_Retiro, as.numeric(names(a)))
series.diarias$kluster <- a[index]
set.seed(0)
p <- ggplot(series.diarias, aes(Fecha_Retiro, viajes, color=factor(Ciclo_Estacion_Retiro),
                           alpha=.001)) + theme_minimal() +
  geom_line() + ggtitle('Cluster temporal') + facet_grid(factor(kluster)~.) + 
  xlab('Fecha') + ylab('Viajes') +theme(legend.position="none") + xlim(c(dmy('01-01-2017'), dmy('30-08-2018')))
library(plotly)
Cluster_temporal <- ggplotly(p)
save(Cluster_temporal, file='Cluster_temporal.Rdata')
gc()
remove(tabla2)
gc()

