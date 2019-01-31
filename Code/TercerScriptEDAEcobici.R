# analisis exploratorio de todos los viajes de ECOBICI
library(readr)
library(lubridate)
library(dplyr)
setwd('C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\')
load(file= 'tabla1.rdata')
tabla1 %>% select(Genero_Usuario, Edad_Usuario ) -> tabla1
gc()
load( file='tabla2.rdata')
tabla2 %>% select(Genero_Usuario, Edad_Usuario ) -> tabla2
gc()
Sys.sleep(4)
tabla2 <- rbind(tabla1, tabla2)
remove(tabla1)
gc()
Sys.sleep(4)
load(file= 'tabla3.rdata')
tabla3 %>% select(Genero_Usuario, Edad_Usuario ) -> tabla3
gc()
Sys.sleep(4)
tabla2 <- rbind(tabla2, tabla3)
remove(tabla3)
gc()
Sys.sleep(4)
gc()
load(file= 'tabla4.rdata')
gc()
Sys.sleep(10)
tabla4 %>% select(Genero_Usuario, Edad_Usuario ) -> tabla4
gc()
Sys.sleep(10)
tabla2 <- rbind(tabla2, tabla4)
remove(tabla4)
gc()
Sys.sleep(10)
# 53,731,199 registros
######################################
# definicion de colores para los graficos
#######################################
morado <- rgb(t(col2rgb('purple')/255), alpha= 0.5)
naranja <- rgb(t(col2rgb('orange')/255), alpha= 0.5)
chido <- rgb(t(col2rgb('#0089E0')/255), alpha=0.5)
color1 <- rgb(t(col2rgb('#FA00A4')/255), alpha=0.5)
color2 <- rgb(t(col2rgb('#27D6C2')/255), alpha=0.5)
color3 <- rgb(t(col2rgb('#442CBC')/255), alpha=0.5)
setwd("C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos")
library(ggplot2)
library(plotly)
tabla2 %>% select(Genero_Usuario, Edad_Usuario ) %>% group_by(Genero_Usuario, Edad_Usuario) %>%
    summarise(Porcentaje =n()) -> x
x %>% group_by(Genero_Usuario)%>% summarise(Porcentaje=sum(Porcentaje)) -> x
x$Porcentaje <- x$Porcentaje/sum(x$Porcentaje)
names(x) <- c('Género', 'Porcentaje')
class(x) <- 'data.frame'
p <- ggplot(x, aes(x=Género, y=Porcentaje, fill=Género)) + geom_bar(stat = 'identity')  +
    scale_fill_manual(values=c('#604B89', 'orange')) + theme_minimal() +xlab('') +ylab('') +
    ggtitle('Distribución del género')
p <- ggplotly(p)
p
genero <- p
save(genero, file='genero.Rdata')
remove(genero)
remove(p,x)
gc()
Sys.sleep(4)
###################################
# visualización de viajes por fecha ZOOM
#############################################
load(file= 'tabla3.rdata')
tabla3 %>% select(Fecha_Retiro ) -> tabla3
gc()
Sys.sleep(4)
gc()
load(file= 'tabla4.rdata')
gc()
Sys.sleep(10)
tabla4 %>% select(Fecha_Retiro ) -> tabla4
gc()
Sys.sleep(10)
#tabla3 <- rbind(tabla3, tabla4)
tabla3 <-  tabla4
remove(tabla4)
gc()
Sys.sleep(10)
tabla3 %>% group_by(Fecha_Retiro) %>% summarise(viajes=n()) -> viajes.salida
names(viajes.salida)
sapply(viajes.salida, class)
p <- ggplot(viajes.salida, aes(x=(Fecha_Retiro), y=viajes)) +
    geom_line(colour='#604B89') +
    xlim(c(ymd('2017-01-01'), ymd('2018-08-31'))) +
    theme_minimal() + xlab('Fecha') + ylab('Viajes por día') +
    ggtitle('Viajes en ECOBICI')
p
viajes.salida <- ggplotly(p)
save(viajes.salida,file='viajes_salida.rdata' )
###############################
# En que horarios hay mayor afluencia ?
##############################
gc()
setwd('C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos')
load(file= 'tabla4.rdata')
class(tabla4$Hora_Retiro)
tabla4 %>% select(Hora_Retiro)-> s
s %>% mutate(hora=paste0(as.character(hour(Hora_Retiro)),':',
                         as.character(minute(Hora_Retiro)),':',
                         second(Hora_Retiro)))->s
library(hms)
as.hms(s$hora[1:6])
s %>% select(hora) %>%  group_by(as.hms(hora)) %>% summarise(viajes=n()) -> s2
gc()
Sys.sleep(2)
names(s2) <- c('hora', 'viajes')
sapply(s2, class)
p <- ggplot(s2, aes(x=hora , y=viajes)) + geom_line(colour='#604B89') +
    theme_minimal() + xlab('Horario') + ylab('Viajes') +
    ggtitle('Viajes en ECOBICI, por segundo')
p
hora <- ggplotly(p)
save(hora, file='hora.rdata')
