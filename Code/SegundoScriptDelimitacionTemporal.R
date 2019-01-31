# Visualización de todos los datos de viajes de Ecobici
setwd("C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos")
library(ggplot2)
library(plotly)
library(lubridate)
library(dplyr)
load(file='temp1.Rdata')
load(file='temp2.Rdata')
load(file='temp3.Rdata')
load(file='temp4.Rdata')
tiempo <- rbind(temp1, temp2, temp3, temp4)
s <- tiempo
class(s$Fecha_Retiro)
tiempo %>% filter(ymd(Fecha_Retiro) >= ymd('2010-02-01'), ymd(Fecha_Retiro)<=ymd('2018-12-31') ) -> s
class(s) <- 'data.frame'
p <- ggplot(s, aes(x=Fecha_Retiro, y=viajes)) + geom_line(colour='#604B89') +
  xlim(c(ymd('2010-02-01'), ymd('2019-01-01'))) + theme_minimal() + xlab('Fecha') + ylab('Viajes por día') +
  ggtitle('Histórico de viajes en ECOBICI') + geom_vline(xintercept = ymd('2017-01-01'), colour='red', size=2)
p
ggplotly(p)
p <- ggplotly(p)
serie_diaria <- p
save(serie_diaria, file ='serie_diaria.Rdata')

save(tiempo, file='tablaFiltro.Rdata')
library(tseries)
t.s <- ts(s$viajes, start = c(2010,2), frequency=365)
adf.test(t.s)
adf.test(diff(t.s))
ts.plot((t.s))
Busetti.Harvey(t.s, k=1, posicion = 2561)
Busetti.Harvey(t.s, k=1, posicion = 2560)

tiempo %>% filter(Fecha_Retiro >=ymd('2017-01-01'))->s #filtramos las observaciones a partor del 2017
            #debido a cambios estructurales udanso el test de Busetti-Harvey
save(s, file='tablaFiltro.Rdata')

########################################################
# Test de Bussetti-Harvey
########################################################
Busetti.Harvey <- function(y, option='both', k = k,
                           posicion=posicion,
                           p=.95)
{
  # y (numeric): vector con la serie de tiempo
  # K (int): numero de posibles saltos estructurales 1<=k<=4
  # posiciones (int): vector con los indices en donde la serie se sospecha que
  #presenta cambios estructurales
  # p (double): confianza a la que se requiere el test
  if(k >4) stop()
  serie <- y
  posicion <- posicion
  k <- k
  #creamos un dataframe con las posiciones para particionar la serie
  particion <- data.frame(start = c(1, posicion+1),
                          stop = c(posicion, length(serie)))
  e <- serie
  # a continuacion particionamos la serie con el data.frame 'particion'
  muestras <- lapply(X=1:dim(particion)[1],
                     function(x)
                     {
                       return(e[particion$start[x]:particion$stop[x]])

                     })
  estadistico.particion <- function(parte)
  {
    #calculo del numerados del estadistico dado por la ecuacion (4.5) del paper
    media.parte <- mean(unlist(parte), na.rm=TRUE )
    e.s <- sum((cumsum( parte- media.parte))**2)
    return(e.s/(length(parte)**2))
  }
  errores <- lapply(muestras, estadistico.particion)
  sigma <- var(serie) #para ambos casos la varianza se calcula igual
  # se termina calculo del estadistico de la ecuacion (4.5)
  estadistico <- sum(unlist(errores))/sigma
  #tabla de valores de los valores criticos para el modelo de la forma 1
  tabla1 <- data.frame(k = 1:4,
                       p0.9 =c(0.347, 0.607, 0.841, 1.063),
                       p0.95= c(0.461, 0.748, 1.000, 1.237  ),
                       p0.99 = c(0.743, 1.074, 1.359, 1.623 ))

  #tabla de valores de los valores criticos para el modelo de la forma 2
  tabla2 <- data.frame(k=1:4,
                       p0.9= c(0.119, 0.211, 0.296, 0.377),
                       p0.95= c(0.149, 0.247, 0.332, 0.423 ),
                       p0.99 = c(0.218, 0.329, 0.428, 0.521  ) )
  if(option == 'c') #determinacion del valor critico
  {
    valor.critico <- tabla1[ k , paste0('p',p)]
  } else {
    valor.critico <- tabla2[ k , paste0('p',p)]
  }
  a <- ifelse(estadistico >= valor.critico, 'Se rechaza H0, ie sÃ??? hay cambio estructural',
              'No se rechaza H0, ie no hay cambio estructural' )
  a <- paste0(a, ' en las posiciones: ', posicion, ' con confianza de: ', p)
  return(a)}  #regresamos un mensaje imformativo
####################################################################################
