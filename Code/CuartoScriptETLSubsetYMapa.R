###################
# subset de datos
#############
setwd("C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos") # fijamos directoria de descarga de todos los datos del portal
lista <- dir()
library(readr)
library(lubridate)
load('tabla4.Rdata')
tablaFiltro <- tabla4
#save(tablaFiltro, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\DataSubset\\tablaFiltro.rdata')
###############################
# Mapa
##############################
setwd("C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos") # fijamos directoria de descarga de todos los datos del portal
library(readr)
library(lubridate)
dir()
load('tabla4.Rdata')
morado <- rgb(t(col2rgb('purple')/255), alpha= 0.5)
naranja <- rgb(t(col2rgb('orange')/255), alpha= 0.5)
chido <- rgb(t(col2rgb('#0089E0')/255), alpha=0.5)
color1 <- rgb(t(col2rgb('#FA00A4')/255), alpha=0.5)
color2 <- rgb(t(col2rgb('#27D6C2')/255), alpha=0.5)
color3 <- rgb(t(col2rgb('#442CBC')/255), alpha=0.5)
library(jsonlite)
library(dplyr)
fromJSON('C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Mapa\\estaciones.json') %>% as.data.frame -> estaciones
names(tabla4)
tabla4 %>% group_by(Ciclo_Estacion_Retiro) %>%
    summarise(viajes=n()) -> flujo.ext
flujo.ext <- flujo.ext[ flujo.ext$Ciclo_Estacion_Retiro %in% estaciones$stations.id,]
flujo.ext <- merge(flujo.ext, estaciones, all.x = TRUE, by.x = 'Ciclo_Estacion_Retiro', by.y='stations.id')
colnames(flujo.ext)
flujo.ext <- flujo.ext[, c('Ciclo_Estacion_Retiro', 'viajes', 'stations.location')]
flujo.ext <- cbind(flujo.ext, flujo.ext$stations.location)
puntos <- flujo.ext
str(puntos)
puntos$stations.location <-NULL
names(puntos) <- c('id', 'viajes',  'lat', 'long')
library(leaflet)
pal <- colorNumeric(
    palette = "YlGnBu",
    domain = puntos$viajes)
puntos.mapa <- leaflet(data = puntos) %>% addTiles() %>%
    addCircles(~long, ~lat,  ~viajes/400,
               label = ~as.character(id), opacity = .7, fillOpacity = 0.7, color='purple')
save(puntos.mapa, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\mapa.rdata')
save(puntos, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\puntos.rdata')
