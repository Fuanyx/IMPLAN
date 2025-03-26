library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)

# API Key de HERE Maps (Reemplázala si es necesario)
API_KEY <- "zMvsNweQ4jp7FKQIV4WCeIeaT0rWtbM1GPzofanYGLk"

# Lista de rutas con identificador, origen y destino
#rutas_inicial <- data.frame(
#  id = 1:24,
#  origen = c("21.166578,-86.855914", "21.160493,-86.860974", "21.164213,-86.853738", "21.162237,-86.851633", 
#             "21.164010,-86.853810", "21.159546,-86.856096", "21.164543,-86.846202", "21.159930,-86.849424", 
#             "21.161586,-86.851451", "21.167457,-86.833764", "21.158810,-86.848344", "21.159769,-86.849423", 
#             "21.156457,-86.856116", "21.153547,-86.842967", "21.158649,-86.848385", "21.152603,-86.841975", 
#             "21.154680,-86.839498", "21.153109,-86.842730", "21.150730,-86.847411", "21.150932,-86.840287", 
#             "21.152241,-86.841823", "21.149525,-86.838766", "21.143057,-86.846546", "21.153467,-86.838689"),
#  destino = c("21.164187,-86.853997", "21.163996,-86.853891", "21.167180,-86.847722", "21.164060,-86.853681", 
#              "21.161980,-86.851847", "21.161641,-86.851695", "21.162150,-86.851315", "21.161792,-86.851227", 
#              "21.159878,-86.849540", "21.159894,-86.849274", "21.159821,-86.849323", "21.158764,-86.848521", 
#             "21.158681,-86.848519", "21.158711,-86.848243", "21.153404,-86.843026", "21.153273,-86.842643", 
#              "21.153361,-86.842593", "21.152554,-86.842146", "21.152432,-86.842161", "21.152347,-86.841729", 
#              "21.150846,-86.840380", "21.150670,-86.840026", "21.150647,-86.840234", "21.150929,-86.840131")
#)

rutas_inicial <- data.frame(
  id = c(4, 2, 8, 9, 10, 11, 14, 17, 22, 23, 12, 20, 21, 19, 24, 1, 18, 5, 6, 7, 3, 13, 15, 16),
  Principal = c("Av. Kabah", "Francisco I madero", "Av. Kabah", "Av. Kabah", "Chichen", "Av. Kabah",
                  "Av. Kabah", "Coba", "Av. Kabah", "La luna", "Av. Kabah", "Av. Kabah", "Av. Kabah", 
                  "Xcaret", "La luna", "Av. Kabah", "Av. Kabah", "Av. Kabah", "Portillo", "Portillo",
                  "Francisco I madero", "Chichen", "Av. Kabah", "Av. Kabah"),
  De = c("Av. José López Portillo", "Calle 103", "Av. Chichen-Itza", "Av. José López Portillo", "Uxmal",
               "Av. Chichen-Itza 2", "Av. Cobá", "La costa", "Av. Mayapan", "Kohunlich", "Av. Chichen-Itza",
               "La costa", "Av. Xcaret", "Del sol", "Xcaret", "Av. Miguel Hidalgo", "Av. Cobá",
               "Av. Francisco I. Madero", "XUENCAL", "Calle 71", "Calle 71", "XUENCAL", "Av. Chichen-Itza 2",
               "Av. Xcaret"),
  A = c("Av. Francisco I. Madero", "AV kabah", "Av. José López Portillo", "Av. Chichen-Itza", "AV kabah",
              "Av. Chichen-Itza", "Av. Chichen-Itza", "AV kabah", "La costa", "AV kabah", "Av. Chichen-Itza 2",
              "Av. Xcaret", "La costa", "AV kabah", "La costa", "Av. Francisco I. Madero", "Av. Xcaret",
              "Av. José López Portillo", "AV kabah", "AV kabah", "AV kabah", "AV kabah", "Av. Cobá", "Av. Cobá"),
  origen = c("21.162237,-86.851633", "21.160493,-86.860974", "21.159930,-86.849424", "21.161586,-86.851451",
             "21.167457,-86.833764", "21.158810,-86.848344", "21.153547,-86.842967", "21.154680,-86.839498",
             "21.149525,-86.838766", "21.143057,-86.846546", "21.159769,-86.849423", "21.150932,-86.840287",
             "21.152241,-86.841823", "21.150730,-86.847411", "21.153467,-86.838689", "21.166578,-86.855914",
             "21.153109,-86.842730", "21.164010,-86.853810", "21.159546,-86.856096", "21.164543,-86.846202",
             "21.164213,-86.853738", "21.156457,-86.856116", "21.158649,-86.848385", "21.152603,-86.841975"),
  destino = c("21.164060,-86.853681", "21.163996,-86.853891", "21.161792,-86.851227", "21.159878,-86.849540",
              "21.159894,-86.849274", "21.159821,-86.849323", "21.158711,-86.848243", "21.153361,-86.842593",
              "21.150670,-86.840026", "21.150647,-86.840234", "21.158764,-86.848521", "21.152347,-86.841729",
              "21.150846,-86.840380", "21.152432,-86.842161", "21.150929,-86.840131", "21.164187,-86.853997",
              "21.152554,-86.842146", "21.161980,-86.851847", "21.161641,-86.851695", "21.162150,-86.851315",
              "21.167180,-86.847722", "21.158681,-86.848519", "21.153404,-86.843026", "21.153273,-86.842643"),
  Tipo = c("Vuelta", NA, "Vuelta", "Ida", NA, "Vuelta", "Vuelta", NA, "Vuelta", NA, "Ida", "Vuelta", "Ida", NA, NA, 
           "Ida", "Ida", "Ida", NA, NA, NA, "Ida", "Ida", "Vuelta"),
  "Tiempo" = c(0, 0, 0, 0,
                 0, 0, 0, 0,
                 0, 0, 0, 0,
                 0, 0, 0, 0,
                 0, 0, 0, 0,
                 0, 0, 0, 0),
  distancia = c("300 m", "850 m", "280 m", "280 m", "1800 m", "150 m",
                 "800 m", "350 m", "180 m","1100 m", "150 m", "220 m",
                 "220 m", "600 m", "350 m", "350 m", "86 m", "300 m",
                 "500 m", "600 m", "700 m", "850 m", "800 m", "100 m") )

rutas_inicial <- subset(rutas_inicial, rutas_inicial$Principal == "Av. Kabah")

# Función para obtener el ETA desde HERE Maps
obtener_eta <- function(origen, destino) {
  url <- paste0("https://router.hereapi.com/v8/routes?transportMode=car&origin=", origen, "&destination=", destino, "&return=summary&apikey=", API_KEY)
  
  respuesta <- GET(url)
  Sys.sleep(1) # Pequeña pausa para evitar bloqueo por la API
  
  if (status_code(respuesta) == 200) {
    datos <- content(respuesta, as = "parsed", type = "application/json")
    
    if (!is.null(datos$routes)) {
      duracion_seg <- datos$routes[[1]]$sections[[1]]$summary$duration
      return(round(duracion_seg / 60, 2))
    }
  }
  return(NA)
}

server <- function(input, output, session) {
  
  rutas_reactivas <- reactiveVal(rutas_inicial)
  
  observeEvent(input$generar, {
    nuevas_rutas <- rutas_inicial %>% mutate("Tiempo" = mapply(obtener_eta, origen, destino))
    rutas_reactivas(nuevas_rutas)
  })
  
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = rutas_inicial, 
                       lat = as.numeric(sapply(strsplit(rutas_inicial$origen, ","), `[[`, 1)), 
                       lng = as.numeric(sapply(strsplit(rutas_inicial$origen, ","), `[[`, 2)), 
                       color = "blue", label = ~paste("Origen", id)) %>%
      addCircleMarkers(data = rutas_inicial, 
                       lat = as.numeric(sapply(strsplit(rutas_inicial$destino, ","), `[[`, 1)), 
                       lng = as.numeric(sapply(strsplit(rutas_inicial$destino, ","), `[[`, 2)), 
                       color = "red", label = ~paste("Destino", id))
  })
  
  observe({
    nuevas_rutas <- rutas_reactivas()
    
    leafletProxy("mapa") %>%
      clearMarkers() %>%
      addCircleMarkers(data = nuevas_rutas, 
                       lat = as.numeric(sapply(strsplit(nuevas_rutas$origen, ","), `[[`, 1)), 
                       lng = as.numeric(sapply(strsplit(nuevas_rutas$origen, ","), `[[`, 2)), 
                       color = "blue", label = ~paste("Origen", id)) %>%
      addCircleMarkers(data = nuevas_rutas, 
                       lat = as.numeric(sapply(strsplit(nuevas_rutas$destino, ","), `[[`, 1)), 
                       lng = as.numeric(sapply(strsplit(nuevas_rutas$destino, ","), `[[`, 2)), 
                       color = "red", label = ~paste("Destino", id))
  })
  
  output$tabla_resultados <- renderTable({
    rutas_reactivas()[, c("De","A","distancia","Tipo","Tiempo"), drop = FALSE]
  })
  
  output$descargar <- downloadHandler(
    filename = function() { "ETA_Resultados.csv" },
    content = function(file) {
      write.csv(rutas_reactivas(), file, row.names = FALSE)
    })
}

