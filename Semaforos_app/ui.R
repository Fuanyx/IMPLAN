library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("Mapa y Cálculo de Distancias"),
  
  fluidRow(
    column(8, offset = 4,  # Colocamos los botones alineados a la derecha
           actionButton("generar", "Calcular ETA"),
           downloadButton("descargar", "Descargar Resultados")
    )
  ),
  
  # Primera fila de botones (toda la parte superior)
  fluidRow(
    column(12, 
           actionButton("mostrar_Kukulcan", "Av. Kukulcan"),
           actionButton("mostrar_Portillo", "Av. Lopez Portillo"),
           actionButton("mostrar_Kabah", "Av. Kabah"),
           actionButton("mostrar_Chac", "Av. Chac Mool"),
           actionButton("mostrar_Bonampak", "Av. Bonampak"),
           actionButton("mostrar_Nichupte", "Av. Nichupté"),
           actionButton("mostrar_Tulum", "Av. Tulum"),
           actionButton("mostrar_Quintana", "Av. Andrés Quintana Roo"),
           actionButton("mostrar_Xcaret", "Av. Xcaret"),
           actionButton("mostrar_Coba", "Av. Cobá"),
           actionButton("mostrar_Todos", "Mostrar Todos")
    )
  ),
  
  # Segunda fila de botones (trayectos de ida y regreso)
  fluidRow(
    column(12, 
           actionButton("mostrar_Ida", "Trayectos de ida"),
           actionButton("mostrar_Regreso", "Trayectos de regreso")
    )
  ),
  
  # Distribución principal: Tabla (4 columnas) y Mapa (8 columnas)
  fluidRow(
    column(4, 
           tableOutput("tabla_resultados")
    ),
    column(8, 
           leafletOutput("mapa"),
           column(6, plotOutput("grafico1")),
           column(6, plotOutput("grafico2"))
    )
  ),
  
)