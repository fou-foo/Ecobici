setwd("C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos") # fijamos directoria de descarga de todos los datos del portal
load('puntos.Rdata')
morado <- rgb(t(col2rgb('purple')/255), alpha= 0.5)
naranja <- rgb(t(col2rgb('orange')/255), alpha= 0.5)
chido <- rgb(t(col2rgb('#0089E0')/255), alpha=0.5)
color1 <- rgb(t(col2rgb('#FA00A4')/255), alpha=0.5)
color2 <- rgb(t(col2rgb('#27D6C2')/255), alpha=0.5)
color3 <- rgb(t(col2rgb('#442CBC')/255), alpha=0.5)
# Eduardo Euresti calculo las distancias de las estaciones a los establecimientos que determinamos como m�s importantes en
# las tres delegaciones de 5 actividades economicas
#C:\Users\fou-f\Documents\GitHub\Ecobici\denue_scian
act.econ <- read.csv('C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\proximidad.csv')
names(puntos)
names(act.econ)
s <- merge(act.econ, puntos, all.x=TRUE, by.x='stations.id' , by.y='id')
names(s)
s$lat.y <- s$lon.y <- NULL
s$id <- s$stations.location <- NULL
s$stations.id <- NULL
s$long <- NULL
names(s)
names(s) <- c('lat', 'lon', 'x1', 'x2', 'x3', 'x4', 'x5', 'viajes')
row.names(s) <- NULL
library(geoR)
library(sp)
library(rgdal)
class(s)
names(s)
y <- SpatialPointsDataFrame(coords = s[, c('lon', 'lat')],
                            data = data.frame(s[,3:8] ), proj4string = CRS("+proj=longlat +datum=WGS84"))
class(y)
names(y)
mex <- readOGR(dsn="C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Mapa\\df\\df_municipal.shp")
CDMX <- mex[as.character(mex@data$NOMBRE) %in% c('Miguel Hidalgo','Cuauhtémoc','Benito Juárez'),]
plot(CDMX)
library(rgeos)
Region <- gUnionCascaded(CDMX)
plot(Region)
points(y@coords, col=morado, pch=20, cex= y@data$viajes/30000)
Region -> CDMX
library(sp)
library(rgeos)
library(rgdal)
plot(CDMX)
class(CDMX)
polys.1 <- CDMX@polygons[[1]]@Polygons[[1]]@coords
class(polys.1)
plot(polys.1)
puntos <- polys.1
puntos <- unique(puntos)
grid <- pred_grid(puntos,by=.002)
plot(grid)
points(grid, pch="+")
grid.fino <- locations.inside(grid, polys.1)
plot(grid.fino)
grid <- SpatialPoints(grid.fino, proj4string = y@proj4string)
plot(grid)
class(grid)
plot(grid)
points(y@coords, col=morado, pch=20, cex= y@data$data/sd(y@data$data))
#########
# #

library(automap)
xxx <- as.data.frame(cbind(y@data, y@coords))
names(xxx)
class(xxx)
coordinates(xxx) =~ lon+lat
grid.nuevo <- as.data.frame(grid@coords)
head(grid.nuevo)
gridded(grid.nuevo) =~ Var1+Var2
names(xxx)
# Ordinary kriging, no new_data object
kriging_result = autoKrige(log(viajes)+x1+x2+~1, xxx)
plot(kriging_result)

# la ultima imagen se guarda manualmente en 'www' dentro de 'shinyapp'