library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("Visualización de Rutas y Tiempos de Tráfico"),
  
  fluidRow(
    column(5,  # Ocupa 4 de 12 columnas disponibles
           actionButton("generar", "Calcular ETA"),
           downloadButton("descargar", "Descargar Resultados"),
           tableOutput("tabla_resultados")
    ),
    column(7,  # Ocupa el espacio restante (8 de 12 columnas)
           leafletOutput("mapa", height = "600px")
    )
  )
)