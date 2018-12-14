morado <- rgb(t(col2rgb('purple')/255), alpha= 0.5)
naranja <- rgb(t(col2rgb('orange')/255), alpha= 0.5)
chido <- rgb(t(col2rgb('#0089E0')/255), alpha=0.5)
color1 <- rgb(t(col2rgb('#FA00A4')/255), alpha=0.5)
color2 <- rgb(t(col2rgb('#27D6C2')/255), alpha=0.5)
color3 <- rgb(t(col2rgb('#442CBC')/255), alpha=0.5)
setwd("/home/fou/Desktop/Ecobici/DataSubset/")
library(readr)
library(lubridate)
library(dplyr)
library(hms)
load( file='tablaFiltro.Rdata')
library(ggplot2)
library(plotly)
names(tabla2)
tabla2%>% select(Genero_Usuario, Edad_Usuario ) %>% group_by(Genero_Usuario, Edad_Usuario) %>%
  summarise(Porcentaje =n()) -> x
x %>% group_by(Genero_Usuario)%>% summarise(Porcentaje=sum(Porcentaje)) -> x
x$Porcentaje <- x$Porcentaje/sum(x$Porcentaje)
names(x) <- c('Genero', 'Porcentaje')
class(x) <- 'data.frame'
p <- ggplot(x, aes(x=Genero, y=Porcentaje, fill=Genero)) + geom_bar(stat = 'identity')  +
  scale_fill_manual(values=c('#604B89', 'orange')) + theme_minimal() +xlab('') +ylab('') +
  ggtitle('Distribución del generó')
p <- ggplotly(p)
tabla2 %>% group_by(Fecha_Retiro) %>% summarise(viajes=n()) -> viajes.salida
p
genero <- p
save(genero, file='genero.Rdata')
#############################################
names(viajes.salida)
sapply(viajes.salida, class)
p <- ggplot(viajes.salida, aes(x=dmy(Fecha_Retiro), y=viajes)) + geom_line(colour='#604B89') + 
  xlim(c(ymd('2017-01-01'), ymd('2018-10-31'))) + theme_minimal() + xlab('Fecha') + ylab('Viajes por día') + 
  ggtitle('Viajes en ECOBICI') 
p
ggplotly(p)
hora <- p
###############################
# En que horarios hay mayor afluencia ?
##############################
tabla2 %>% select(Hora_Retiro) %>%  group_by(Hora_Retiro) %>% summarise(viajes=n()) -> horarios
horarios$Hora_Retiro <- as.hms(horarios$Hora_Retiro)
p <- ggplot(horarios, aes(x=Hora_Retiro , y=viajes)) + geom_line(colour='#604B89') + 
  theme_minimal() + xlab('Horario') + ylab('Viajes') + 
  ggtitle('Viajes en ECOBICI, por segundo') 
p <- ggplotly(p)
p
###############################
# En que estaciones hay mayor afluencia 
##############################
hora <- p 
save(hora, file='hora.Rdata')
###############################
# En que estaciones hay mayor afluencia vamos el el general luego la serie de tiempo
##############################
library(dplyr)
library(lubridate)
setwd("~/Desktop/Ecobici/DataSubset")
load('tablaFiltro.Rdata')
tabla2$Fecha_Arribo <- dmy(tabla2$Fecha_Arribo) 
tabla2$Fecha_Retiro <- dmy(tabla2$Fecha_Retiro)
dia <- unique(c(tabla2$Fecha_Retiro, tabla2$Fecha_Arribo))
frame <- subset(tabla2, Fecha_Retiro <= dia[1] )
frame %>% select(Ciclo_Estacion_Retiro, Ciclo_Estacion_Arribo, Fecha_Retiro) %>%
  group_by(Ciclo_Estacion_Retiro, Ciclo_Estacion_Arribo, Fecha_Retiro) %>% summarise(out=n())  -> flujo
names(flujo)
gc()
library(jsonlite)
fromJSON('/home/fou/Desktop/Ecobici/Mapa/estaciones.json') %>% as.data.frame -> estaciones
s <- estaciones[, -c(1:6,8, 10 )]
flujo <- merge(flujo, s, all.x=TRUE, by.x = 'Ciclo_Estacion_Retiro', by.y = 'stations.id' )
flujo$out.lat <- flujo$stations.location$lat
flujo$out.long <- flujo$stations.location$lon
names(flujo)
flujo$stations.location <- NULL
gc()
names(flujo)
flujo <- merge(flujo, s, all.x=TRUE, by.x = 'Ciclo_Estacion_Arribo', by.y = 'stations.id' )
flujo$in.lat <- flujo$stations.location$lat
flujo$in.long <- flujo$stations.location$lon
names(flujo)
flujo$stations.location <- NULL
gc()
gc()
library(leaflet)
library(magrittr)
library(dplyr)
tabla2 %>% select(Ciclo_Estacion_Retiro, Ciclo_Estacion_Arribo) %>% 
  group_by(Ciclo_Estacion_Retiro, Ciclo_Estacion_Arribo) %>% summarise(i =n())-> puntos
gc()
puntos$i <- NULL
remove(tabla2)
gc()
out <- merge(puntos, s, all.x=TRUE, by.x = 'Ciclo_Estacion_Retiro', by.y = 'stations.id' )
out$out.lat <- out$stations.location$lat
out$out.long <- out$stations.location$lon
names(out)
out$stations.location <- NULL
gc()
########
out <- merge(out, s, all.x=TRUE, by.x = 'Ciclo_Estacion_Arribo', by.y = 'stations.id' )
out$in.lat <- out$stations.location$lat
out$in.long <- out$stations.location$lon
names(out)
names(flujo)
gc()
remove(puntos)
gc()
###################
library(leaflet)
library(leaflet.extras)
df.1 <- out [, c('out.long', 'out.lat' )]
df.1 <- unique(df.1)
df <- sp::SpatialPointsDataFrame(
  df.1, data.frame(x=rep(1,dim(df.1)[1])  ))
names(df)
mydf <- data.frame(Observation = c("S"),
                   InitialLat = flujo$out.lat,
                   InitialLong = flujo$out.long,
                   NewLat = flujo$in.lat,
                   NewLong = flujo$in.long,
                   stringsAsFactors = FALSE)
mydf2 <- data.frame(group = c("S"),
                    lat = c(mydf$InitialLat, mydf$NewLat),
                    long = c(mydf$InitialLong, mydf$NewLong))
remove(flujo, mydf, df.1)
gc()
remove(out, s, estaciones)
names(mydf2)
gc()
leaflet(df) %>% addTiles() %>%
  addCircleMarkers(
    radius = 3,
    color = I('purple'),
    stroke = FALSE, fillOpacity = 0.8
  ) %>% addPolylines(data = mydf2, lng = ~long, lat = ~lat)
names(mydf2)
