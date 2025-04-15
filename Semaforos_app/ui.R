library(shiny)
library(leaflet)
library(bs4Dash)

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
    body {
      background-image: url('www/Mapa.png');  /* Cambia por el nombre de tu imagen */
      background-size: cover;
      background-position: center;
      background-repeat: no-repeat;
    }

    /* Capa para difuminado */
    .blur-overlay {
      position: fixed;
      top: 0; left: 0;
      width: 100%;
      height: 100%;
      backdrop-filter: blur(6px); /* Ajusta la cantidad de difuminado */
      z-index: -1; /* Envía al fondo */
    }
  "))
  ),
  
  div(class = "blur-overlay"),
  
  fluidRow(
    column(3, 
           tags$img(src = "Logo IMPLAN.png", height = "80px", style = "display: block; margin: auto;")
    ),
    column(4,
           tags$h2("Seguimiento de tráfico:", style = "margin-left: 10px; display: inline-block;"),
           tags$h2("Ciudad de Cancún", style = "font-weight: bold; color: #4B4F54; display: inline-block; margin-left: 10px;")
    ),
    
    column(2, wellPanel( 
      selectInput("avenida", "Elige una avenida:",
                  choices = c("Av.Kabah", "Av. Andres Quintana Roo", "Av. López Portillo",
                              "Av. Xcaret", "Av. Cobá", "Av. Bonampak",
                              "Av. Chac Mool", "Av. Tulum", "Av. Nichupté","Todas")),
    )),
    
    column(2, wellPanel(
      selectInput("ruta", "Elige un sentido:",
                  choices = c("Ida", "Regreso")),
    )),
    
    
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
    /* Contenedor principal para centrar la tabla */
    #tabla_metrica {
      display: flex;
      justify-content: center;
      align-items: center;
      min-height: 100px;
    }

    /* Estilo de la tabla */
    #tabla_metrica table {
      width: 100%;
      max-width: 600px;
      border-collapse: separate;
      border-spacing: 0;
      border-radius: 12px;
      overflow: hidden;
      box-shadow: 2px 4px 8px rgba(0, 0, 0, 0.15);
    }

    /* Encabezado */
    #tabla_metrica th {
      background-color: #4B4F54;
      color: white;
      padding: 12px;
      text-align: center;
      font-weight: bold;
    }

    /* Celdas */
    #tabla_metrica td {
      padding: 12px;
      border-bottom: 1px solid #ccc;
      text-align: center;
    }

    /* Filas pares e impares */
    #tabla_metrica tr:nth-child(even) {
      background-color: #f1f3f5;
    }

    #tabla_metrica tr:nth-child(odd) {
      background-color: #ffffff;
    }

    /* Efecto hover */
    #tabla_metrica tr:hover {
      background-color: #e2e6ea;
      transition: background-color 0.3s ease-in-out;
    }
  "))
  )
  ,
  
  
  
 
  tags$br(),
  # Distribución principal: Tabla (4 columnas) y Mapa (8 columnas)

  fluidRow(
    column(3, bs4Card(
      width = 8,
      
      tags$div(
        style = "font-weight: bold; font-size: 18px; display: flex; align-items: center;",
        icon("clock", class = "fa-2x", style = "margin-right: 10px;"),
        "Velocidad actual"
      ),
      
      div(
        style = "background-color: #D60057; color: white; border-radius: 15px; padding: 10px; 
             text-align: center; font-size: 18px; font-weight: bold; margin-top: 8px;",
        textOutput("km_best")
      )
    )),
      
      # Tarjeta 2: Suma de la Distancia
    column(3, bs4Card(
      width = 8,
      
      tags$div(
        style = "font-weight: bold; font-size: 18px; display: flex; align-items: center;",
        icon("clock", class = "fa-2x", style = "margin-right: 10px;"),
        "Tiempo actual"
      ),
      
      div(
        style = "background-color: #8BDC65; color: white; border-radius: 15px; padding: 10px; 
             text-align: center; font-size: 18px; font-weight: bold; margin-top: 8px;",
        textOutput("Tiempo_best")
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
        textOutput("distancia_best")
      )
    )),
    
    column(3, bs4Card(
      width = 8,
      
      tags$div(
        style = "font-weight: bold; font-size: 18px; display: flex; align-items: center;",
        icon("road", class = "fa-2x", style = "margin-right: 10px;"),
        "Variación"
      ),
      
      div(
        style = "background-color: #9A3CBB; color: white; border-radius: 15px; padding: 10px; 
             text-align: center; font-size: 18px; font-weight: bold; margin-top: 8px;",
        textOutput("variacion_tiempo")
      )
    )),
    tags$head(tags$style(".card-header { display: none !important; }"))
    
  ),
  
  
  
  

  
  
  fluidRow(
    

    
    column(4, uiOutput("gif_trayecto_mostrar")),
    column(8, leafletOutput("mapa",  width = "100%", height = "400px")),
  ),
  
  
  
  fluidRow(
    fluidRow(
    column(2, bs4Card(
      width = 8,
      div(
        style = "background-color: #D60057; color: white; border-radius: 15px; padding: 10px; 
             text-align: center; font-size: 18px; font-weight: bold; margin-top: 8px;",
        textOutput("km_obj")
      )
    )),
    
    column(2, bs4Card(
      width = 8,
      div(
        style = "background-color: #D60057; color: white; border-radius: 15px; padding: 10px; 
             text-align: center; font-size: 18px; font-weight: bold; margin-top: 8px;",
        textOutput("Tiempo_obj")
      )
    )),
    ),
    column(4,
           div(id = "tabla_metrica",
               tableOutput("tabla_metrica")
           )
    ),
    
    column(4, uiOutput("gif_mostrar_ida")),
    column(4, uiOutput("gif_mostrar_vuelta")),
    
    # 🔽 Esto fuerza que lo siguiente se ponga debajo, no a la derecha
    div(style = "clear: both;"),
    
    #fluidRow(

    #)
    #,
    
    column(1, actionButton("generar", "Calcular ETA")),
    column(1, downloadButton("descargar", "Descargar Resultados"))
    
  )
  
)

