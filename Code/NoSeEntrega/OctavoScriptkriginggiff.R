#################
# lectura de datos
##########
# --------------------------------
# Librerias y datos para el arranque
{
    library(dplyr)
    library(jsonlite)
    library(lubridate) #manejo sencillo de fechas
    #library(hms)
    # --------------------------------
    # Los registros preparados
    load("C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos\\tabla4.Rdata") # fijamos directoria de descarga de todos los datos del portal
    # --------------------------------
    # Constantes
    morado <- rgb(t(col2rgb('#5C4788')/255), alpha= 0.5)

    # --------------------------------
    #Carga de las estaciones: "estaciones"
    fromJSON('C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Mapa\\estaciones.json') %>% as.data.frame -> estaciones
    estacionesBase <- as.data.frame(estaciones$stations.id)
    colnames(estacionesBase) <- c("stations.id")
}
## Ajustando los formatos de fechas
#tabla4$Fecha_Retiro <- ymd(tabla4$Fecha_Retiro)
#gc()
#Sys.sleep(4)
#tabla4$Fecha_Arribo <- ymd(tabla4$Fecha_Arribo)
#gc()
#Sys.sleep(4)

# Muestra: Semana 2 del 2017
inicio <- dmy('09-01-2017')
fin    <- dmy('30-08-2018')
class(tabla4) <- 'data.frame'
tabla4 <- tabla4[tabla4$Fecha_Retiro>= inicio,]
semana <- tabla4[tabla4$Fecha_Retiro<=fin,]
gc()
remove(tabla4)
gc()
# Conteos de salida en la semana
semana %>% group_by(Ciclo_Estacion_Retiro, Fecha_Retiro) %>% summarise(viajes=n()) -> semana2
semana$Hora_Retiro <- NULL
semana$Hora_Arribo <- NULL
# Funciones
{
    CambiaDia <- function( mes, anio) {
        dmy(paste0('01/', as.character(mes),"/", as.character(anio)))
    }
    Filtra <- function( mes, anio) {
        semana %>% filter(Fecha_Retiro <= CambiaDia( mes, anio)) -> aux
        (aux %>% group_by(Ciclo_Estacion_Retiro)) %>% summarise(viajes=n()) -> aux
        aux <- merge(estaciones, aux, all.x=TRUE, by.x='stations.id', by.y='Ciclo_Estacion_Retiro')
        aux <- na.omit(aux)
        return(aux)
    }
    # Conteos de salidas por dia y hora de cada estacion
    FiltraH <- function( mes, anio) {
        a <- Filtra( mes, anio)
        a
    }
}
########################
# grid fino
##############
# Grid, construccion de grid fino
library(sp)
library(rgeos)
library(rgdal)
mex <- readOGR(dsn="C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Mapa\\df\\df_municipal.shp")
CDMX <- mex[as.character(mex@data$NOMBRE) %in% c('Miguel Hidalgo','Cuauhtémoc','Benito Juárez'),]
plot(CDMX)
class(CDMX)
polys.1 <- CDMX@polygons[[1]]@Polygons[[1]]@coords
class(polys.1)
plot(polys.1)
puntos <- polys.1
Region <- gUnionCascaded(CDMX)
plot(Region)
estaciones$lat <- estaciones$stations.location$lat
estaciones$lon <- estaciones$stations.location$lon
estaciones$stations.location <- NULL
k <-0
for( mes in c(2:12))
{
    for (anio in c(2017))
    {
        xx <- FiltraH( mes, anio)
        # Lectura de coordenadas
        names(xx)
        table(xx$viajes)
        names(estaciones)
        foo <- merge(xx, estaciones, all.x=TRUE )
        names(foo)
        foo$stations.nearbyStations<-NULL
        foo[, c(1,3,4,5,6,7,8,9,10)] <- NULL
        names(foo)
        library(sp)
        y <- SpatialPointsDataFrame(coords = foo[, c('lon', 'lat')],
                                    data = data.frame(viajes=foo$viajes), proj4string = CRS("+proj=longlat +datum=WGS84"))
        points(y@coords, col=morado, pch=20, cex= y@data$viajes/30000)
        Region -> CDMX
        plot(Region)
        library(geoR)
        grid <- pred_grid(Region@polygons[[1]]@Polygons[[1]]@coords,by=.002)
        plot(grid)
        plot(Region)
        points(grid, pch="+")
        grid.fino <- locations.inside(grid, Region@polygons[[1]]@Polygons[[1]]@coords)
        plot(grid.fino)
        grid <- SpatialPoints(grid.fino, proj4string = y@proj4string)
        plot(grid.fino)
        plot(grid.fino)
        points(y@coords, col=morado, pch=20, cex= y@data$viajes /sd(y@data$viajes))
        # #
        library(automap)
        y <- SpatialPointsDataFrame(coords = foo[, c('lon', 'lat')],
                                    data = data.frame(viajes=foo$viajes), proj4string = CRS("+proj=longlat +datum=WGS84"))

        xxx <- as.data.frame(cbind(y@data, y@coords))
        names(xxx)
        class(xxx)
        coordinates(xxx) =~ lon+lat
        grid.nuevo <- as.data.frame(grid@coords)
        head(grid.nuevo)
        gridded(grid.nuevo) =~ Var1+Var2
        names(xxx)
        class(xxx)
        kriging_result = autoKrige(log(viajes)~1, xxx, grid.nuevo)
        plot(kriging_result)
        ###################
        prediction = kriging_result$krige_output
        jpeg(paste0('/home/fou/Desktop/Ecobici/Kriging_mes/',letters[k+1],'.jpg'))
        plot(prediction, main=paste0('Viajes acumulados hasta mes ',as.character(mes) ,' año ', as.character(anio)))
        points(y, pch=4, col='blue', cex= 1)
        dev.off()
        k <- k+1
    }
}
##################3
for( mes in c(1:8))
{
    for (anio in c(2018))
    {
        xx <- FiltraH( mes, anio)
        # Lectura de coordenadas
        names(xx)
        table(xx$viajes)
        names(estaciones)
        foo <- merge(xx, estaciones, all.x=TRUE )
        names(foo)
        foo$stations.nearbyStations<-NULL
        foo[, c(1,3,4,5,6,7,8,9,10)] <- NULL
        names(foo)
        library(sp)
        y <- SpatialPointsDataFrame(coords = foo[, c('lon', 'lat')],
                                    data = data.frame(viajes=foo$viajes), proj4string = CRS("+proj=longlat +datum=WGS84"))
        points(y@coords, col=morado, pch=20, cex= y@data$viajes/30000)
        Region -> CDMX
        plot(Region)
        library(geoR)
        grid <- pred_grid(Region@polygons[[1]]@Polygons[[1]]@coords,by=.002)
        plot(grid)
        plot(Region)
        points(grid, pch="+")
        grid.fino <- locations.inside(grid, Region@polygons[[1]]@Polygons[[1]]@coords)
        plot(grid.fino)
        grid <- SpatialPoints(grid.fino, proj4string = y@proj4string)
        plot(grid.fino)
        class(grid.fino)
        plot(grid.fino)
        points(y@coords, col=morado, pch=20, cex= y@data$viajes /sd(y@data$viajes))
        # #
        library(automap)
        y <- SpatialPointsDataFrame(coords = foo[, c('lon', 'lat')],
                                    data = data.frame(viajes=foo$viajes), proj4string = CRS("+proj=longlat +datum=WGS84"))

        xxx <- as.data.frame(cbind(y@data, y@coords))
        names(xxx)
        class(xxx)
        coordinates(xxx) =~ lon+lat
        grid.nuevo <- as.data.frame(grid@coords)
        head(grid.nuevo)
        gridded(grid.nuevo) =~ Var1+Var2
        names(xxx)
        class(xxx)
        kriging_result = autoKrige(log(viajes)~1, xxx, grid.nuevo)
        plot(kriging_result)
        ###################
        prediction = kriging_result$krige_output
        jpeg(paste0('/home/fou/Desktop/Ecobici/Kriging_mes/', letters[k+1],'.jpg'))
        plot(prediction, main=paste0('Viajes acumulados hasta mes ',as.character(mes) ,' año ', as.character(anio)))
        points(y, pch=4, col='blue', cex= 1)
        dev.off()
        k <- k+1
    }
}
setwd('/home/fou/Desktop/Ecobici/Kriging_mes')
dir()
library(imager)

png(file="krigind_por_mes.png", width=200, height=200)
plot.new()
for (i in sort(dir())){
    a <- load.image(i)
    plot(a)
}
dev.off()

# convert the .png files to one .gif file using ImageMagick.
# The system() function executes the command as if it was done
# in the terminal. the -delay flag sets the time between showing
# the frames, i.e. the speed of the animation.
system("convert -delay 300 *.jpg example_1.gif")

# to not leave the directory with the single jpeg files
# I remove them.
file.remove(list.files(pattern=".png"))