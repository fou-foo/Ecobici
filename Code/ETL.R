setwd("/home/fou/Desktop/Ecobici/Data/") # fijamos directoria de descarga de todos los datos del portal
                                         # https://www.ecobici.cdmx.gob.mx/es/informacion-del-servicio/open-data
library(readr) # lectura rapida
direccion <- 'https://www.ecobici.cdmx.gob.mx/sites/default/files/data/usages/'
file <- '201' #la descarga consiste en una serie de consultas GET
library(lubridate) #para un comodo manejo de fechas
library(hms)       # comodo manejo de hrs
for(j in 1:8)
{
    for(i in 0:2)
    {
        #descargamos uno por uno el historial
        s <- paste0(direccion, file,  as.character(j), '-1', as.character(i), '.csv' )
        mes <- read.csv( s) #la funcion read.csv del kernel base, hace solicitudes GET
        write.csv(mes, file=paste0(file, as.character(j), '-1', as.character(i), '.csv'), row.names = FALSE) #lo escribimos en disco
        Sys.sleep(2) # esperamos unos segundos para no saturar el servidor con solicitudes de la misma IP
    }
}

  # Despues de que se descargaron todos los archivos intentamos homologar las fechas y las hrs, junto con los headers
lista <- dir()
tabla1 <- read.csv(file=lista[1], stringsAsFactors = FALSE)
tabla1$Fecha_Retiro <- ymd(tabla1$Fecha_Retiro)
tabla1$Fecha_Arribo <- ymd(tabla1$Fecha_Arribo)
tabla1$Hora_Retiro <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , tabla1$Hora_Retiro) ))
tabla1$Hora_Arribo <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , tabla1$Hora_Arribo) ))
for (i in 2:50)
{
    mes <- read_csv(file=lista[i])
    mes$Fecha_Retiro <- ymd(mes$Fecha_Retiro)
    mes$Fecha_Arribo <- ymd(mes$Fecha_Arribo)
    mes$Hora_Retiro <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , mes$Hora_Retiro) ))
    mes$Hora_Arribo <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , mes$Hora_Arribo) ))
    tabla1 <- rbind(tabla1, mes)
    print(i)
    gc()
    Sys.sleep(4) # el garbage collector de R tarda apro. 3's en ejecutar su trabajo
}
tabla1 <- na.omit(tabla1) # eliminamos registro que no presentan en el mes de octubre de 2017
library(dplyr)
names(tabla1)

tabla1 %>% select(Fecha_Retiro) %>% group_by(Fecha_Retiro)%>% summarise(viajes=n()) -> temp1
write.csv(temp1, file='temp1.csv')
save(temp1, file='temp1.Rdata')
#################################
lista <- dir()
tabla2 <- read.csv(file=lista[50], stringsAsFactors = FALSE)
tabla2$Fecha_Retiro <- ymd(tabla2$Fecha_Retiro)
tabla2$Fecha_Arribo <- ymd(tabla2$Fecha_Arribo)
tabla2$Hora_Retiro <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , tabla2$Hora_Retiro) ))
tabla2$Hora_Arribo <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , tabla2$Hora_Arribo) ))
for (i in 51:80)
{
  mes <- read_csv(file=lista[i])
  #mes$Fecha_Retiro <- ymd(mes$Fecha_Retiro)
  #mes$Fecha_Arribo <- ymd(mes$Fecha_Arribo)
  #mes$Hora_Retiro <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , mes$Hora_Retiro) ))
  #mes$Hora_Arribo <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , mes$Hora_Arribo) ))
  
  tabla2 <- rbind(tabla2, mes)
  print(i)
  gc()
  Sys.sleep(4)
}
tabla2 <- na.omit(tabla2)
library(dplyr)
names(tabla2)

tabla2 %>% select(Fecha_Retiro) %>% group_by(Fecha_Retiro)%>% summarise(viajes=n()) -> temp2
write.csv(temp2, file='temp2.csv')
save(temp2, file='temp2.Rdata')
#################################
lista <- dir()
tabla3 <- read_csv(file=lista[81])
tabla3$Fecha_Retiro <- ymd(tabla3$Fecha_Retiro)
tabla3$Fecha_Arribo <- ymd(tabla3$Fecha_Arribo)
tabla3$Hora_Retiro <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , tabla3$Hora_Retiro) ))
tabla3$Hora_Arribo <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , tabla3$Hora_Arribo) ))
for (i in 82:108)
{
  mes <- read_csv(file=lista[i])
  #mes$Fecha_Retiro <- ymd(mes$Fecha_Retiro)
  #mes$Fecha_Arribo <- ymd(mes$Fecha_Arribo)
  #mes$Hora_Retiro <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , mes$Hora_Retiro) ))
  #mes$Hora_Arribo <- hms(unlist(lapply(FUN=function(x) as.hms(  unlist(strsplit(x, split='\\.'))[1])  , mes$Hora_Arribo) ))
  
  tabla3 <- rbind(tabla3, mes)
  print(i)
  gc()
  Sys.sleep(4)
}
tabla3 <- na.omit(tabla3)
library(dplyr)
names(tabla3)

tabla3 %>% select(Fecha_Retiro) %>% group_by(Fecha_Retiro)%>% summarise(viajes=n()) -> temp3
write.csv(temp3, file='temp3.csv')
save(temp3, file='temp3.Rdata') # los ultimos dos meses se fueron 
names(temp3)



##########################################
library(ggplot2)
dmy(temp3$Fecha_Retiro)
ggplot(temp3, aes(x=dmy(Fecha_Retiro), y=viajes)) + geom_line()
save(temp3, file='temp3.Rdata') # 
names(temp3)
