library(shiny)
library(leaflet)
library(bs4Dash)

ui <- fluidPage(
  fluidRow(
    column(3,  # Imagen alineada a la izquierda
           tags$img(src = "Logo IMPLAN.png", height = "80px", style = "display: block; margin: auto;")
    ),
    column(7,  # Texto a la derecha de la imagen
           tags$h2("Seguimiento de tráfico:", style = "margin-left: 10px; display: inline-block;"),
           tags$h2("Ciudad de Cancún", style = "font-weight: bold; color: #4B4F54; display: inline-block; margin-left: 10px;")
    )
  ),
  
  tags$head(
    tags$style(HTML("
      #tabla_resultados table {
        width: 100%;
        border-collapse: collapse;
        border-radius: 10px;
        overflow: hidden;
        box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
      }
      #tabla_resultados th {
        background-color: #4B4F54;
        color: white;
        padding: 10px;
        text-align: center;
      }
      #tabla_resultados td {
        padding: 10px;
        border-bottom: 1px solid #ccc;
        text-align: center;
      }
      #tabla_resultados tr:nth-child(even) {
        background-color: #f8f9fa;
      }
      #tabla_resultados tr:hover {
        background-color: #d6d8db;
      }
    "))
  ),

#)
  #titlePanel("Mapa y Cálculo de Distancias"),
  #dashboardBody(

 # )
  # Primera fila de botones (toda la parte superior)
  fluidRow(
    column(12, 
           #actionButton("mostrar_Kukulcan", "Av. Kukulcan"),
           actionButton("mostrar_Portillo", "Av. Lopez Portillo"),
           actionButton("mostrar_Kabah", "Av. Kabah"),
           actionButton("mostrar_Chac", "Av. Chac Mool"),
           actionButton("mostrar_Bonampak", "Av. Bonampak"),
           actionButton("mostrar_Nichupte", "Av. Nichupté"),
           actionButton("mostrar_Tulum", "Av. Tulum"),
           actionButton("mostrar_Quintana", "Av. Andrés Quintana Roo"),
           actionButton("mostrar_Xcaret", "Av. Xcaret"),
           actionButton("mostrar_Coba", "Av. Cobá"),
           actionButton("mostrar_Ida", "Trayectos de ida"),
           actionButton("mostrar_Regreso", "Trayectos de regreso"),
           actionButton("mostrar_Todos", "Mostrar Todos")#,

    )
  ),
 
  tags$br(),
  # Distribución principal: Tabla (4 columnas) y Mapa (8 columnas)
  fluidRow(
    column(4, 
           tableOutput("tabla_resultados")
    ),
    #fluidRow(
      # Tarjeta 1: Suma del Tiempo
    column(2, bs4Card(
      width = 8,
      
      tags$div(
        style = "font-weight: bold; font-size: 18px; display: flex; align-items: center;",
        icon("clock", class = "fa-2x", style = "margin-right: 10px;"),
        "Objetivo"
      ),
      
      div(
        style = "background-color: #D60057; color: white; border-radius: 15px; padding: 10px; 
             text-align: center; font-size: 18px; font-weight: bold; margin-top: 8px;",
        textOutput("objetivo")
      )
    )),
      
      # Tarjeta 2: Suma de la Distancia
    column(2, bs4Card(
      width = 8,
      
      tags$div(
        style = "font-weight: bold; font-size: 18px; display: flex; align-items: center;",
        icon("clock", class = "fa-2x", style = "margin-right: 10px;"),
        "Tiempo"
      ),
      
      div(
        style = "background-color: #8BDC65; color: white; border-radius: 15px; padding: 10px; 
             text-align: center; font-size: 18px; font-weight: bold; margin-top: 8px;",
        textOutput("Tiempo")
      )
    )),
      
      # Tarjeta 3: Promedio del Tiempo
    column(2, bs4Card(
      width = 8,
      
      tags$div(
        style = "font-weight: bold; font-size: 18px; display: flex; align-items: center;",
        icon("road", class = "fa-2x", style = "margin-right: 10px;"),
        "Distancia"
      ),
      
      div(
        style = "background-color: #00AFAA; color: white; border-radius: 15px; padding: 10px; 
             text-align: center; font-size: 18px; font-weight: bold; margin-top: 8px;",
        textOutput("suma_distancia")
      )
    )),
    
    column(2, bs4Card(
      width = 8,
      
      tags$div(
        style = "font-weight: bold; font-size: 18px; display: flex; align-items: center;",
        icon("road", class = "fa-2x", style = "margin-right: 10px;"),
        "Km / Hora"
      ),
      
      div(
        style = "background-color: #9A3CBB; color: white; border-radius: 15px; padding: 10px; 
             text-align: center; font-size: 18px; font-weight: bold; margin-top: 8px;",
        textOutput("Km/H")
      )
    )),

    column(8, leafletOutput("mapa")),
    column(4, uiOutput("gif_mostrar_ida")),
    column(4, uiOutput("gif_mostrar_vuelta")),
    column(1, actionButton("generar", "Calcular ETA")),
    column(1, downloadButton("descargar", "Descargar Resultados"))
  ),
  #tags$head(tags$style(".card-status { display: none !important; }"))

  tags$head(tags$style(".card-header { display: none !important; }"))

)