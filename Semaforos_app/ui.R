library(shiny)
library(leaflet)
library(bs4Dash)

ui <- fluidPage(
  fluidRow(
    column(3,  # Imagen alineada a la izquierda
           tags$img(src = "Logo IMPLAN.png", height = "80px", style = "display: block; margin: auto;")
    ),
    column(4,  # Texto a la derecha de la imagen
           tags$h2("Seguimiento de tráfico:", style = "margin-left: 10px; display: inline-block;"),
           tags$h2("Ciudad de Cancún", style = "font-weight: bold; color: #4B4F54; display: inline-block; margin-left: 10px;")
    ),
    
    column(2, wellPanel( 
      selectInput("avenida", "Elige una avenida:",
                  choices = c("Av.Kabah", "Av. Andres Quintana Roo", "Av. López Portillo",
                              "Av. Xcaret", "Av. Cobá", "Av. Bonampak",
                              "Av. Chac Mool", "Av. Tulum", "Av. Nichupté","Todas")),
    )),
    
    column(2, wellPanel( # Opcional, solo para dar estilo a la selección
      selectInput("ruta", "Elige un sentido:",
                  choices = c("Ida", "Regreso")),
    )),
    
    #column(1, wellPanel( # Opcional, solo para dar estilo a la selección
    #  selectInput("hora", "Hora:",
    #              choices = c("0","1","2","3","4","5","6",
    #                          "7","8","9","10","11","12","13",
    #                          "14","15","16","17","18","19","20",
    #                          "21","22","23")),
    #)),
    
    
  ),
  tags$script(HTML("
  Shiny.addCustomMessageHandler('fadeMarkers', function(group) {
    setTimeout(function() {
      $('.leaflet-marker-icon, .leaflet-interactive').fadeOut(3000);
    }, 100);
  });
"))
  
  
  
  ,
  
  
  tags$head(
    tags$style(HTML("
    /* Contenedor de la tabla */
    #tabla_resultados {
      width: 100%;
      min-height: 300px; /* Altura reservada */
      display: flex;
      justify-content: center;
      align-items: center;
    }

    /* Ocultar la tabla cuando no hay datos, pero reservando el espacio */
    #tabla_resultados table:empty {
      visibility: hidden;
    }

    /* Estilos generales */
    #tabla_resultados table {
      width: 100%;
      border-collapse: separate;
      border-spacing: 0;
      border-radius: 12px;
      overflow: hidden;
      box-shadow: 2px 4px 8px rgba(0, 0, 0, 0.15);
      min-height: 200px;
    }

    /* Encabezado */
    #tabla_resultados th {
      background-color: #4B4F54;
      color: white;
      padding: 12px;
      text-align: center;
      font-weight: bold;
      border-top-left-radius: 12px;
      border-top-right-radius: 12px;
    }

    /* Celdas */
    #tabla_resultados td {
      padding: 12px;
      border-bottom: 1px solid #ccc;
      text-align: center;
    }

    /* Filas pares */
    #tabla_resultados tr:nth-child(even) {
      background-color: #f1f3f5;
    }

    /* Filas impares */
    #tabla_resultados tr:nth-child(odd) {
      background-color: #ffffff;
    }

    /* Efecto hover */
    #tabla_resultados tr:hover {
      background-color: #e2e6ea;
      transition: background-color 0.3s ease-in-out;
    }
  "))
  ),
  
  
  tags$head(
    tags$style(HTML("
    /* Contenedor de la tabla */
    #tabla_metrica {
      width: 100%;
      min-height: 400px; /* Altura reservada */
      display: flex;
      justify-content: center;
      align-items: center;
    }

    /* Ocultar la tabla cuando no hay datos, pero reservando el espacio */
    #tabla_metrica table:empty {
      visibility: hidden;
    }

    /* Estilos generales */
    #tabla_metrica table {
      width: 100%;
      border-collapse: separate;
      border-spacing: 0;
      border-radius: 12px;
      overflow: hidden;
      box-shadow: 2px 4px 8px rgba(0, 0, 0, 0.15);
      min-height: 200px;
    }

    /* Encabezado */
    #tabla_metrica th {
      background-color: #4B4F54;
      color: white;
      padding: 12px;
      text-align: center;
      font-weight: bold;
      border-top-left-radius: 12px;
      border-top-right-radius: 12px;
    }

    /* Celdas */
    #tabla_metrica td {
      padding: 12px;
      border-bottom: 1px solid #ccc;
      text-align: center;
    }

    /* Filas pares */
    #tabla_metrica tr:nth-child(even) {
      background-color: #f1f3f5;
    }

    /* Filas impares */
    #tabla_metrica tr:nth-child(odd) {
      background-color: #ffffff;
    }

    /* Efecto hover */
    #tabla_metrica tr:hover {
      background-color: #e2e6ea;
      transition: background-color 0.3s ease-in-out;
    }
  "))
  ),
  
  
  
 
  tags$br(),
  # Distribución principal: Tabla (4 columnas) y Mapa (8 columnas)

  fluidRow(
    column(3, bs4Card(
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
    column(3, bs4Card(
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
    column(3, bs4Card(
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
    
    column(3, bs4Card(
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
    tags$head(tags$style(".card-header { display: none !important; }"))
    
  ),
  
  
  fluidRow(
    column(4, uiOutput("gif_trayecto_mostrar")),
    column(8, leafletOutput("mapa",  width = "100%", height = "400px")),
  ),

fluidRow(
  
  div(class = "tabla-container",
      column(4,
             tableOutput("tabla_metrica")
      )),
  column(4, uiOutput("gif_mostrar_ida")),
  column(4, uiOutput("gif_mostrar_vuelta")),
  column(1, actionButton("generar", "Calcular ETA")),
  column(1, downloadButton("descargar", "Descargar Resultados"))
  
),
  



)
