####################################
#####    J. Antonio Garcia #########
#####   jose.ramirez@cimat.mx ######
####     Eduardo Uresti Charre     #
####     Luis Daniel Mendoza       #
####################################
library(plotly)
library(MASS)
library(shiny)
library(knitr)
library(rmarkdown) 
library(ggplot2)
library(lubridate)
library(leaflet)
library(shinyLP)
library(gapminder)
library(ggplot2)
library(shiny)

theme_set(theme_bw())
#########################################
# Construccion del backend              # 
#########################################
#lectura de datos simulados previamente
n <- 20
load('datos.RData')
load('serie_diaria.Rdata')
load('DelimitacionTiempo.Rdata')
load('genero.Rdata')
load('hora.Rdata')
load('Cluster_temporal.Rdata')
load( file='xx.Rdata')
library(plotly)
Cluster_temporal
####################################
server <- function(input, output) {
 
  #seleccion de datos simulados
  a <- reactive({
    d <- input$d #seleccion de datos
    I <- diag(rep(1, d))
    pos <- pos[, 1:d]
    neg <- neg[, 1:d]
    stack <- rbind(pos,neg)
    pos.mean <- apply(pos, 2, mean)
    neg.mean <- apply(neg, 2, mean)
    w <- ginv(I) %*% (pos.mean - neg.mean)
    w <- w/sum(w**2)**.5 #normalizamos el vector MDP
    X <- ginv(cov(pos)) %*% (pos.mean - neg.mean)
    X <- X/sum(X**2)**.5 #normalizamos el vector que define al frontera de Bayes
    #acos(sum(X*w))*360/(2*pi)
    stack$label <- 1
    stack$label[(n+1):(2*n)] <- -1 
    Y <- X
    Y[-c(1,2)] <- 0
    Y[1] <- X[2]
    Y[2] <- -X[1] #elegimos un vector perpenticular a X
    Y <- Y/sum(Y**2)**.5
    sum(Y*w)
    M <- cbind(X, Y,  w)
    pos.proyec <- as.matrix(pos)%*%M
    neg.proyec <- as.matrix(neg)%*%M
    pos.proyec <- as.data.frame(pos.proyec)
    neg.proyec <- as.data.frame(neg.proyec)
    pos.proyec$label <- '+1'
    neg.proyec$label <- '-1'
    proyec <- rbind(pos.proyec, neg.proyec)
    proyec$w <- proyec$V2 * (w[1]/w[2])
    b <- list(proyec, w )
    return(b)
  })
  
  # mapa
  output$mymap <- renderLeaflet({
    #names(xx)
    xx$stations.location <-NULL
    names(xx) <- c('id', 'viajes',  'lat', 'long')
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = xx$viajes)
    #xx$viajes <- (xx$viajes -min(xx$viajes))/(max(xx$viajes)-min(xx$viajes)) 
    # Show first 20 rows from the `quakes` dataset
    leaflet(data = xx) %>% addTiles() %>%
      addCircles(~long, ~lat,  ~viajes/400, 
                 label = ~as.character(id), opacity = .7, fillOpacity = 0.7, color='purple')
  })
 
  
  #construccion serie total
  output$serie_diaria <- renderPlotly({
    serie_diaria
  })
  # carga de limite temporal
  output$DelimitacionTiempo <- renderPlotly({
    DelimitacionTiempo
  })
  # carga genero
  output$genero <- renderPlotly({
    genero
  })
  # carga hora
  output$hora <- renderPlotly({
    hora
  })
  # cluster temporal
  output$Cluster_temporal <- renderPlotly({
    Cluster_temporal
  })
 
  # giff
  output$plot1 <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- imager::load.image('example_1.gif')
    
    # now make the animation
    play(outfile)
  })
  
  
  
  #construccion de densidades del tab DWD
  output$Bayes <- renderPlotly({
    a <- a()
    proyec <- a[[1]]
    p2 <- ggplot(data = proyec, aes(x=V3, fill=label, colour=label))+geom_density()+
      geom_rug(sides="b")+ggtitle('Distribución en la dirección de Bayes') + theme_minimal()+
      xlab('Bayes') + ylab('') + 
      scale_fill_manual(  labels = c('-1', '+1'), values = c("purple", "orange"))+
      scale_color_manual(  labels = c('-1', '+1'), values = c("purple", "orange"))+
      theme(legend.title = element_blank())
    p2 <- ggplotly(p2) #distro en bayes
    p2
  })
  
  #construccion de proyecciones del tab DWD
  output$DWD <- renderPlotly({
    a <- a()
    proyec <- a[[1]]
    x <- subset(proyec, label == '+1')
    y <- subset(proyec, label == '-1')
    y$V3 <- abs(y$V3)#distancia al plano separador
    p <- plot_ly(y = ~x$V3, type = "box",
                 line = list(color = 'rgb(255,165,0)'),
                 name = "+1") %>%
      add_trace(y = ~y$V3,
                line = list(color = 'rgb(160,32,240)'),
                name = "-1"
                ) %>%
      layout(title = "Distancia a hiperplano MDP",
             xaxis = list(title = ' '),
                          yaxis = list(title = ''))
    
  })
}
