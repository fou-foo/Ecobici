####################################
#####    J. Antonio Garcia #########
#####   jose.ramirez@cimat.mx ######
####     Eduardo Uresti Charre     #
####     Luis Daniel Mendoza       #
####################################
library(shinydashboard)
library(shiny)
library(plotly)
library(knitr)
library(rmarkdown)
library(leaflet)
library(shinyLP)
library(gapminder)
library(ggplot2)
library(shiny)

theme_set(theme_bw())
#########################################
# Construccion de la UI                 #
#########################################
sidebar <- dashboardSidebar(
  #comenzamos con el menu
  sidebarMenu(
    menuItem("ECOBICI", tabName = "ECOBICI", icon = icon("bicycle")),
    menuItem("Data", icon = icon("database"), tabName = "Data"),
    menuItem("EDA", icon = icon("bicycle"), tabName = "EDA"),
    menuItem("Kriging", icon = icon("th"), tabName = "Kriging"),
    menuItem("coKriging", icon = icon("th"), tabName = "coKriging"),
    menuItem("CTemporal", icon = icon("clock"), tabName = "CTemporal")
  )
)
#cramos varias tabs
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "ECOBICI",
            img(src='ecobicilogo.png', align = "left"), hr(), hr(), hr(), hr(), hr(), hr(),
            box(   width=8,leafletOutput("mymap"))
    ),
    tabItem(tabName = "Data",
            box(   width=12,  plotlyOutput("serie_diaria", height = 500, width = "100%")
            ),
            box(   width=12,  plotlyOutput("DelimitacionTiempo", height = 500, width = "100%")
            )  ),

    tabItem(tabName = "EDA",
            box(   width=12,  plotlyOutput("genero", height = 500, width = "100%")
            ),
            box(   width=12,  plotlyOutput("hora", height = 500, width = "100%")
            )
    ),    #la tab de la derivacion
     tabItem(tabName = "Kriging",
             img(src='eda.png', align = "center"), hr(),hr(),hr(),hr(),hr(),hr(),
             img(src='kriging.png', align = "center")
             ),
    tabItem(tabName = "coKriging",
            img(src='cokriging.png', align = "center")
    ),
    tabItem(tabName = "CTemporal",
            box(   width=12,  plotlyOutput("Cluster_temporal", height = 500, width = "100%")#,
                  # hr(),  imageOutput("plot1"))
                  ))



    )

  )

# Put them together into a dashboardPage
dashboardPage(skin = "green",
  dashboardHeader(title = "CIMAT Monterrey"),
  sidebar,
  body
)