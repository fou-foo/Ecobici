# Script para descargar los datos de la pagina web
# en vista de que la api dejo de tener mantenimiento

setwd("C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code") # fijamos directoria de descarga de todos los datos del portal
                                         # https://www.ecobici.cdmx.gob.mx/es/informacion-del-servicio/open-data
library(readr) # lectura rapida
direccion <- 'https://www.ecobici.cdmx.gob.mx/sites/default/files/data/usages/'
file <- '201' #la descarga consiste en una serie de consultas GET
library(lubridate) #para un comodo manejo de fechas
library(hms)       # comodo manejo de hrs
#descarga de anio 2010
for(j in 0)
{
    for(i in 2:9) #para el anio 2010 el servicio comenzo en febrero
    {
        #descargamos uno por uno el historial
        s <- paste0(direccion, file,  as.character(j), '-0', as.character(i), '.csv' )
        print(s)
        mes <- read.csv( s) #la funcion read.csv del kernel base, hace solicitudes GET
        write.csv(mes, file=paste0('C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\RawData\\',
                                   file, as.character(j), '-0', as.character(i), '.csv'), row.names = FALSE) #lo escribimos en disco
        Sys.sleep(2) # esperamos unos segundos para no saturar el servidor con solicitudes de la misma IP
    }
}
# descarga de meses octubre, novimbre diciembre
for(j in 0)
{
    for(i in 0:2) #para el anio 2010 el servicio comenzo en febrero
    {
        #descargamos uno por uno el historial
        s <- paste0(direccion, file,  as.character(j), '-1', as.character(i), '.csv' )
        print(s)
        mes <- read.csv( s) #la funcion read.csv del kernel base, hace solicitudes GET
        write.csv(mes, file=paste0('C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\RawData\\',
                                   file, as.character(j), '-1', as.character(i), '.csv'), row.names = FALSE) #lo escribimos en disco
        Sys.sleep(2) # esperamos unos segundos para no saturar el servidor con solicitudes de la misma IP
    }
}
# descarga de anios 2011-2018
for(j in 1:8)
{
    for(i in 1:9)
    {
        #descargamos uno por uno el historial
        s <- paste0(direccion, file,  as.character(j), '-0', as.character(i), '.csv' )
        print(s)
        mes <- read.csv( s) #la funcion read.csv del kernel base, hace solicitudes GET
        write.csv(mes, file=paste0('C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\RawData\\',
                                   file, as.character(j), '-0', as.character(i), '.csv'), row.names = FALSE) #lo escribimos en disco
        Sys.sleep(2) # esperamos unos segundos para no saturar el servidor con solicitudes de la misma IP
    }
}
# descarga de meses octubre, novimbre diciembre
for(j in 1:8)
{
    for(i in 0:2)
    {
        #descargamos uno por uno el historial
        s <- paste0(direccion, file,  as.character(j), '-1', as.character(i), '.csv' )
        print(s)
        mes <- read.csv( s) #la funcion read.csv del kernel base, hace solicitudes GET
        write.csv(mes, file=paste0('C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\RawData\\',
                                   file, as.character(j), '-1', as.character(i), '.csv'), row.names = FALSE) #lo escribimos en disco
        Sys.sleep(2) # esperamos unos segundos para no saturar el servidor con solicitudes de la misma IP
    }
}


# Despues de que se descargaron todos los archivos intentamos homologar las fechas y las hrs, junto con los headers
# como hay varios archivos, se unen en diferentes objetos para el preprocesamiento.

setwd('C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\RawData\\')
library(readr) # lectura rapida
library(lubridate) #para un comodo manejo de fechas
#library(hms)       # comodo manejo de hrs
lista <- dir() # lista con los archivos decargados
tabla1 <- read.csv(file=lista[1], stringsAsFactors = FALSE)
tabla1$Fecha_Retiro <- ymd(tabla1$Fecha_Retiro)
tabla1$Fecha_Arribo <- ymd(tabla1$Fecha_Arribo)
tabla1$Hora_Retiro <- hms( tabla1$Hora_Retiro)
tabla1$Hora_Arribo <- hms(  tabla1$Hora_Arribo)
for (i in 2:50) #hay que cambiar el numero de archivos que se descarguen
{
    mes <- read_csv(file=lista[i])
    mes$Fecha_Retiro <- ymd(mes$Fecha_Retiro)
    mes$Fecha_Arribo <- ymd(mes$Fecha_Arribo)
    mes$Hora_Retiro <- hms(   mes$Hora_Retiro)
    mes$Hora_Arribo <- hms( mes$Hora_Arribo)
    tabla1 <- rbind(tabla1, mes)
    print(i)
    gc()
    #Sys.sleep(1) # el garbage collector de R tarda apro. 3's en ejecutar su trabajo
}
tabla1 <- na.omit(tabla1) # eliminamos registro que no presentan en el mes de octubre de 2017
library(dplyr)

tabla1 %>% select(Fecha_Retiro) %>% group_by(Fecha_Retiro)%>% summarise(viajes=n()) -> temp1
write.csv(temp1, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\temp1.csv')
save(temp1, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\temp1.Rdata')
remove(temp1, mes)
gc()
Sys.sleep(10)
#write.csv(tabla1, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\tabla1.csv')
save(tabla1, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\tabla1.Rdata')
remove(tabla1)
gc()
Sys.sleep(30)

#################################
tabla2 <- read.csv(file=lista[50], stringsAsFactors = FALSE)
tabla2$Fecha_Retiro <- ymd(tabla2$Fecha_Retiro)
tabla2$Fecha_Arribo <- ymd(tabla2$Fecha_Arribo)
tabla2$Hora_Retiro <- hms(  tabla2$Hora_Retiro)
tabla2$Hora_Arribo <- hms(   tabla2$Hora_Arribo)
for (i in 51:80)
{
  mes <- read_csv(file=lista[i])
  mes$Fecha_Retiro <- ymd(mes$Fecha_Retiro)
  mes$Fecha_Arribo <- ymd(mes$Fecha_Arribo)
  #mes$Hora_Retiro <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , mes$Hora_Retiro) ))
  #mes$Hora_Arribo <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , mes$Hora_Arribo) ))
  mes$Hora_Retiro <- hms(  mes$Hora_Retiro)
  mes$Hora_Arribo <- hms(   mes$Hora_Arribo)

  tabla2 <- rbind(tabla2, mes)
  print(i)
  gc()
  #Sys.sleep(4)
}
tabla2 <- na.omit(tabla2)
library(dplyr)
names(tabla2)

tabla2 %>% select(Fecha_Retiro) %>% group_by(Fecha_Retiro)%>% summarise(viajes=n()) -> temp2
write.csv(temp2, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\temp2.csv')
save(temp2, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\temp2.Rdata')
remove(temp2)
gc()
Sys.sleep(4)
#write.csv(tabla2, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\temp2.csv')
save(tabla2, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\tabla2.Rdata')
gc()

remove(tabla2)
Sys.sleep(30)

#################################
lista <- dir()
tabla3 <- read_csv(file=lista[72])
tabla3$Fecha_Retiro <- ymd(tabla3$Fecha_Retiro)
tabla3$Fecha_Arribo <- ymd(tabla3$Fecha_Arribo)
#tabla3$Hora_Retiro <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , tabla3$Hora_Retiro) ))
#tabla3$Hora_Arribo <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , tabla3$Hora_Arribo) ))
tabla3$Hora_Retiro <- hms(   tabla3$Hora_Retiro)
tabla3$Hora_Arribo <- hms(   tabla3$Hora_Arribo)

for (i in 73:77)
{
  mes <- read_csv(file=lista[i])
  mes$Fecha_Retiro <- ymd(mes$Fecha_Retiro)
  mes$Fecha_Arribo <- ymd(mes$Fecha_Arribo)
  #mes$Hora_Retiro <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , mes$Hora_Retiro) ))
  #mes$Hora_Arribo <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , mes$Hora_Arribo) ))
  mes$Hora_Retiro <- hms(  mes$Hora_Retiro)
  mes$Hora_Arribo <- hms(   mes$Hora_Arribo)
  tabla3 <- rbind(tabla3, mes)
  print(i)
  gc()
  #Sys.sleep(4)
}
tabla3 <- na.omit(tabla3)
library(dplyr)
names(tabla3)
tabla3 %>% select(Fecha_Retiro) %>% group_by(Fecha_Retiro)%>% summarise(viajes=n()) -> temp3
write.csv(temp3, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\temp3.csv')
save(temp3, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\temp3.Rdata') # los ultimos dos meses se fueron
names(temp3)
remove(temp3)
gc()
Sys.sleep(10)
#write.csv(tabla3, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\temp3.csv')
save(tabla3, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\tabla3.Rdata') # los ultimos dos meses se fueron
gc()
Sys.sleep(30)




##########################################
# al actualizar los datos se agrego una tabla-compendio extra
lista <- dir()
tabla4 <- read_csv(file=lista[78])
tabla4$Fecha_Retiro <- mdy(tabla4$Fecha_Retiro)
tabla4$Fecha_Arribo <- mdy(tabla4$Fecha_Arribo)
#tabla3$Hora_Retiro <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , tabla3$Hora_Retiro) ))
#tabla3$Hora_Arribo <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , tabla3$Hora_Arribo) ))
tabla4$Hora_Retiro <- hms(   tabla4$Hora_Retiro)
tabla4$Hora_Arribo <- hms(   tabla4$Hora_Arribo)

# hizo falta cambiar el formato de las fechas en un ntervalo del 2016-2017
for (i in 79:107)
{
    mes <- read_csv(file=lista[i])
    mes$Fecha_Retiro <- dmy(mes$Fecha_Retiro)
    mes$Fecha_Arribo <- dmy(mes$Fecha_Arribo)
    #mes$Hora_Retiro <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , mes$Hora_Retiro) ))
    #mes$Hora_Arribo <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , mes$Hora_Arribo) ))
    mes$Hora_Retiro <- hms(  mes$Hora_Retiro)
    mes$Hora_Arribo <- hms(   mes$Hora_Arribo)
    tabla4 <- rbind(tabla4, mes)
    print(i)
    gc()
    #Sys.sleep(4)
}
gc()
remove(tabla4.1)
gc()
tabla4 <- na.omit(tabla4)
library(dplyr)
names(tabla4)

tabla4 %>% select(Fecha_Retiro) %>% group_by(Fecha_Retiro)%>% summarise(viajes=n()) -> temp4
write.csv(temp4, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\temp4.csv')
save(temp4, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\temp4.Rdata') # los ultimos dos meses se fueron
names(temp4)
remove(temp4)
gc()
Sys.sleep(10)
#write.csv(tabla3, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\temp3.csv')
save(tabla4, file='C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\tabla4.Rdata') # los ultimos dos meses se fueron
gc()
Sys.sleep(30)

##############
# se termina integración de archivos y 'homologacion'
