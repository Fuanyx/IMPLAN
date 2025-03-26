library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)
library(RPostgres)

# API Key de HERE Maps (Reemplázala si es necesario)
API_KEY <- "zMvsNweQ4jp7FKQIV4WCeIeaT0rWtbM1GPzofanYGLk"

# Conexión a la base de datos
con <- dbConnect(
  Postgres(),
  dbname = "siginplan",
  host = "45.132.241.118",     
  port = 5432,         
  user = "juanyam",
  password = "eJnNPmklNznIkZ1EJ8JB4B=="
)

semaforos <- dbGetQuery(con, 'SELECT * FROM "99_Evaluaciones"."Coordenadas_Semaforos_Sistema"')
semaforos <- semaforos[,c(2,4,5)]
colnames(semaforos) <- c("nombre","lat","lng")


trayectos <- dbGetQuery(con, 'SELECT * FROM "99_Evaluaciones"."Tiempo_traslado_semaforos"')
trayectos$key <- paste(trayectos$Principal, trayectos$De)
trayectos$Hora <- as.numeric(trayectos$Hora)
#trayectos$Hora <- as.character(trayectos$Hora)
trayectos$Principal <- gsub(" ", "",trayectos$Principal)
trayectos$De <- gsub(" ", "",trayectos$De)
trayectos <- trayectos[,c(1,2,7,3,8,9)]
ida <- subset(trayectos, trayectos$Tipo == "Ida")
vuelta <- subset(trayectos, trayectos$Tipo == "Regreso")
ida$Tipo <- NULL
vuelta$Tipo <- NULL


coordenadas <- dbGetQuery(con, 'SELECT * FROM "99_Evaluaciones"."Trayectos_traslado_semaforos"')
coordenadas$distancia <- paste(coordenadas$distancia, "m")
coordenadas <- coordenadas[,c(1:7,9,8)]

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
  
  semaforos_reactivos <- reactiveVal(semaforos)
  coordenadas_reactivas <- reactiveVal(coordenadas)
  
  ida_reactivas <- reactiveVal(ida)
  vuelta_reactivas <- reactiveVal(vuelta)
  
  
  coordenadas_reactivas_ida <- reactiveVal(coordenadas_reactivas)
  coordenadas_reactivas_vuelta <- reactiveVal(coordenadas_reactivas)
  
  grafico_actual <- reactiveVal("barras")
  
  
  observeEvent(input$generar, {
    nuevas_rutas <- coordenadas_reactivas() %>% 
      mutate("Tiempo" = mapply(obtener_eta, origen, destino))  
    
    coordenadas_reactivas(nuevas_rutas)  
  })

  observeEvent(input$mostrar_Kabah, {
    semaforos_filtrados <- filter(semaforos, nombre == "1. Av Kabah")
    coordenadas_filtradas <- filter(coordenadas, Sobre == "Av.Kabah")
    ida_filtradas <- filter(ida, Principal == "Av.Kabah")
    vuelta_filtradas <- filter(vuelta, Principal == "Av.Kabah")
    
    semaforos_reactivos(semaforos_filtrados)
    coordenadas_reactivas(coordenadas_filtradas)
    ida_reactivas(ida_filtradas)
    vuelta_reactivas(vuelta_filtradas)
  })
  
  observeEvent(input$mostrar_Xcaret, {
    semaforos_filtrados <- filter(semaforos, nombre == "6. Av. Xcaret")
    coordenadas_filtradas <- filter(coordenadas, Sobre == "Av. Xcaret")
    semaforos_reactivos(semaforos_filtrados)
    coordenadas_reactivas(coordenadas_filtradas)
  })
  
  
  observeEvent(input$mostrar_Quintana, {
    semaforos_filtrados <- filter(semaforos, nombre == "5. Av. Andrés Quintana Roo")
    coordenadas_filtradas <- filter(coordenadas, Sobre == "Av. Andres Quintana Roo")
    ida_filtradas <- filter(ida, Principal == "Av. Andres Quintana Roo")
    vuelta_filtradas <- filter(vuelta, Principal == "Av. Andres Quintana Roo")
    semaforos_reactivos(semaforos_filtrados)
    coordenadas_reactivas(coordenadas_filtradas)
    ida_reactivas(ida_filtradas)
    vuelta_reactivas(vuelta_filtradas)
  })
  
  observeEvent(input$mostrar_Kukulcan, {
    semaforos_filtrados <- filter(semaforos, nombre == "2. Bld. Kukulcan")
    coordenadas_filtradas <- filter(coordenadas, Sobre == "2. Bld. Kukulcan")
    semaforos_reactivos(semaforos_filtrados)
    coordenadas_reactivas(coordenadas_filtradas)
  })
  observeEvent(input$mostrar_Portillo, {
    semaforos_filtrados <- filter(semaforos, nombre == "3. Av. Lopez Portillo")
    coordenadas_filtradas <- filter(coordenadas, Sobre == "Av. López Portillo")
    semaforos_reactivos(semaforos_filtrados)
    coordenadas_reactivas(coordenadas_filtradas)
  })
  
  observeEvent(input$mostrar_Chac, {
    semaforos_filtrados <- filter(semaforos, nombre == "10. Av. Chac Mool")
    coordenadas_filtradas <- filter(coordenadas, Sobre == "Av. Chac Mool")
    semaforos_reactivos(semaforos_filtrados)
    coordenadas_reactivas(coordenadas_filtradas)
  })
  
  observeEvent(input$mostrar_Bonampak, {
    semaforos_filtrados <- filter(semaforos, nombre == "8. Av. Bonampak")
    coordenadas_filtradas <- filter(coordenadas, Sobre == "Av. Bonampak")
    semaforos_reactivos(semaforos_filtrados)
    coordenadas_reactivas(coordenadas_filtradas)
  })
  
  observeEvent(input$mostrar_Nichupte, {
    semaforos_filtrados <- filter(semaforos, nombre == "4. Av. Nichupté")
    coordenadas_filtradas <- filter(coordenadas, Sobre == "Av. Nichupté")
    semaforos_reactivos(semaforos_filtrados)
    coordenadas_reactivas(coordenadas_filtradas)
  })
  
  observeEvent(input$mostrar_Tulum, {
    semaforos_filtrados <- filter(semaforos, nombre == "9. Av. Tulum")
    coordenadas_filtradas <- filter(coordenadas, Sobre == "Av. Tulum")
    semaforos_reactivos(semaforos_filtrados)
    coordenadas_reactivas(coordenadas_filtradas)
  })
  
  
  observeEvent(input$mostrar_Coba, {
    semaforos_filtrados <- filter(semaforos, nombre == "7. Av. Cobá")
    coordenadas_filtradas <- filter(coordenadas, Sobre == "Av. Cobá")
    semaforos_reactivos(semaforos_filtrados)
    coordenadas_reactivas(coordenadas_filtradas)
  })
  
  observeEvent(input$mostrar_Ida, {
    coordenadas_filtradas <- filter(coordenadas_reactivas(), Trayecto == "Ida")
    coordenadas_reactivas(coordenadas_filtradas)
  })
  
  observeEvent(input$mostrar_Regreso, {
    coordenadas_filtradas <- filter(coordenadas_reactivas(), Trayecto == "Regreso")
    coordenadas_reactivas(coordenadas_filtradas)
  })

  # Botón para mostrar todos
  observeEvent(input$mostrar_Todos, {
    semaforos_reactivos(semaforos)
    coordenadas_reactivas(coordenadas)
  })
  
  # Renderizar el mapa
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  observe({
    leafletProxy("mapa") %>%
      clearMarkers() %>%
      setView(lng = mean(semaforos_reactivos()$lng), 
              lat = mean(semaforos_reactivos()$lat), 
              zoom = 13) %>%  
      addCircleMarkers(
        data = semaforos_reactivos(),
        lat = ~lat, lng = ~lng,
        color = "blue",
        label = ~nombre
      )
  }) 
  
  output$tabla_resultados <- renderTable({
    coordenadas_reactivas()[, c("De","a","distancia","Trayecto","Tiempo"), drop = FALSE]
  })
  colnames(coordenadas)
  
  output$descargar <- downloadHandler(
    filename = function() { "ETA_Resultados.csv" },
    content = function(file) {
      write.csv(coordenadas_reactivas(), file, row.names = FALSE)
    })
  
  observeEvent(input$btn1, { grafico_actual("barras") })
  
  output$grafico1 <- renderPlot({
    if (grafico_actual() == "barras") {
      ggplot(ida_reactivas(), aes(x = Hora, y = `Tiempo promedio`, fill = reorder(De, ID))) +
        geom_bar(stat = "identity") +
        labs(title = "Tiempo en trayecto Ida",
             x = "Hora", y = "Tiempo (minutos)", fill = "Av principal") +
        theme_minimal()
    } 
  })
  
  output$grafico2 <- renderPlot({
    if (grafico_actual() == "barras") {
      ggplot(vuelta_reactivas(), aes(x = Hora, y = `Tiempo promedio`, fill = reorder(De, ID))) +
        geom_bar(stat = "identity") +
        labs(title = "Tiempo en trayecto Regreso",
             x = "Hora", y = "Tiempo (minutos)", fill = "Av principal") +
        theme_minimal()
    } 
  })
  
}
