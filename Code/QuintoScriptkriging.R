setwd("C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Code\\Objetos") # fijamos directoria de descarga de todos los datos del portal
load('tablaFiltro.Rdata')
load('puntos.Rdata')
################## definicion de colores
###############
morado <- rgb(t(col2rgb('purple')/255), alpha= 0.5)
naranja <- rgb(t(col2rgb('orange')/255), alpha= 0.5)
chido <- rgb(t(col2rgb('#0089E0')/255), alpha=0.5)
color1 <- rgb(t(col2rgb('#FA00A4')/255), alpha=0.5)
color2 <- rgb(t(col2rgb('#27D6C2')/255), alpha=0.5)
color3 <- rgb(t(col2rgb('#442CBC')/255), alpha=0.5)
names(puntos)
row.names(puntos) <- NULL
library(geoR)
library(sp)
library(rgdal)
class(puntos)
names(puntos)
y <- SpatialPointsDataFrame(coords = puntos[, c('long', 'lat')],
                            data = data.frame(puntos[,2] ), proj4string = CRS("+proj=longlat +datum=WGS84"))
class(y)
names(y) <- c('viajes')
# el mapa de CDMX fue facilitado por el Dr. Victor
mex <- readOGR(dsn="C:\\Users\\fou-f\\Documents\\GitHub\\Ecobici\\Mapa\\df\\df_municipal.shp")
CDMX <- mex[as.character(mex@data$NOMBRE) %in% c('Miguel Hidalgo','CuauhtÃ©moc','Benito JuÃ¡rez'),]
plot(CDMX)
library(rgeos)
Region <- gUnionCascaded(CDMX)
plot(Region)
points(y@coords, col=morado, pch=20, cex= y@data$viajes/30000)
Region -> CDMX
#######################################################################
# grid de Kriging
# Grid, construccion de grid fino
library(sp)
library(rgeos)
library(rgdal)
plot(CDMX)
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
class(y)
#########
# #
library(ggplot2)
plot(CDMX)
points(y@coords, col=morado, pch=20, cex= y@data$data/sd(y@data$data))
names(y@data) <- 'Salidas'
p1 <- ggplot(y@data, aes(x = log(Salidas))) +geom_density(color='purple')+
  theme_minimal() + ggtitle('Distribuciónn del logaritmo de los viajes de salida')+
  xlab('número')+ ylab('')
p1
coo <- as.data.frame(y@coords)
names(coo) <- c('x', 'y')
coo <- cbind(coo, y@data)
names(coo)
p2 <- ggplot(coo, aes(x, log(Salidas))) +geom_point(color=I(chido))+
  theme_minimal() + ggtitle('')+
  xlab('longitud')+ ylab('')+
  stat_smooth(aes(x = x, y = log(Salidas), color=chido),
              se=FALSE)+theme(legend.position="none")
p2
p3 <- ggplot(coo, aes(y, log(Salidas))) +geom_point(color=I(chido))+
  theme_minimal() + ggtitle('')+
  xlab('latitud')+ ylab('')+
  stat_smooth(aes(x = y, y = log(Salidas), color=I(morado)),
              method='auto', se=FALSE) +theme(legend.position="none")
p3
library(easyGgplot2)
library(plotly)
ggplotly(ggplot2.multiplot(p1, p2, p3, cols=3))
#la figura anterior se guarda manualmente
# en la carpeta 'www' contenida en carpeta 'shinyapp'
library(automap)
xxx <- as.data.frame(cbind(y@data, y@coords))
names(xxx)
class(xxx)
coordinates(xxx) =~ long+lat
class(xxx)
grid.nuevo <- as.data.frame(grid@coords)
head(grid.nuevo)
class(grid.nuevo)
names(grid.nuevo)
gridded(grid.nuevo) =~ Var1+Var2
#names(xxx)
#class(xxx)

kriging_result = autoKrige(log(Salidas)~1, xxx)
plot(kriging_result)

# Ordinary kriging
kriging_result = autoKrige(log(Salidas)~1, xxx, grid.nuevo)
plot(kriging_result)
# la imagen anteriortambien se guarda localmente dentro de 'www' dentro de 'shinyapp'
# Fixing the nugget to 0.2
