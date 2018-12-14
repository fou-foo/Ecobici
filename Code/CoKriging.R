setwd("~/Desktop/Ecobici/DataSubset")
load('tablaFiltro.Rdata')
morado <- rgb(t(col2rgb('purple')/255), alpha= 0.5)
naranja <- rgb(t(col2rgb('orange')/255), alpha= 0.5)
chido <- rgb(t(col2rgb('#0089E0')/255), alpha=0.5)
color1 <- rgb(t(col2rgb('#FA00A4')/255), alpha=0.5)
color2 <- rgb(t(col2rgb('#27D6C2')/255), alpha=0.5)
color3 <- rgb(t(col2rgb('#442CBC')/255), alpha=0.5)
library(jsonlite)
library(dplyr)
fromJSON('~/Desktop/Ecobici/Mapa/estaciones.json') %>% as.data.frame -> estaciones
names(tabla2)
tabla2 %>% group_by(Ciclo_Estacion_Retiro) %>%
  summarise(viajes=n()) -> flujo.ext
flujo.ext <- merge(flujo.ext, estaciones, all.x = TRUE, by.x = 'Ciclo_Estacion_Retiro', by.y='stations.id')
colnames(flujo.ext)
flujo.ext <- flujo.ext[, c('Ciclo_Estacion_Retiro', 'viajes', 'stations.location')]
flujo.ext <- cbind(flujo.ext, flujo.ext$stations.location)
x <- flujo.ext
str(x)
xx <- x
act.econ <- read.csv('/home/fou/Desktop/Ecobici/Code/proximidad.csv')
names(xx)
names(act.econ)
names(xx)
s <- merge(act.econ, xx, all.x=TRUE, by.x='stations.id' , by.y='Ciclo_Estacion_Retiro')
names(s)
s$lat.y <- s$lon.y <- NULL
s$id <- s$stations.location <- NULL
s$stations.id <- NULL
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
mex <- readOGR(dsn="~/Desktop/Ecobici/Mapa/df/df_municipal.shp")
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
kriging_result = autoKrige(log(viajes)~1, xxx)
plot(kriging_result)

# Ordinary kriging
kriging_result = autoKrige(log(viajes)+ x1~1, xxx, grid.nuevo)
plot(kriging_result)

# Fixing the nugget to 0.2
kriging_result = autoKrige(log(viajes)+x1~1, xxx, 
                           grid.nuevo,
                           fix.values = c(0.1,NA,NA))
plot(kriging_result)
hist(kriging_result$krige_output$var1.stdev)
class(kriging_result)
kr <- kriging_result
prediction = kr$krige_output
sample_variogram = kr$exp_var
variogram_model = kr$var_model
plot(prediction)
plot(sample_variogram)
plot(variogram_model)
## End(Not run)