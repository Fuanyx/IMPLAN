library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)
library(RPostgres)
library(mapdeck)
library(ggplot2)
library(stringr)


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


datos <- read.csv("C:/Users/yamli/Documents/IMPLAN/Semaforos/Pruebas/puntos_interpolados2.csv")
datos$"elevation" <- (datos$M.por.minuto - min(datos$M.por.minuto)) / (max(datos$M.por.minuto) - min(datos$M.por.minuto))
datos$elevation <- datos$elevation * 100
datos <- datos[,c(10,5,6,10)]
datos$elevation <- "1. Av Kabah"
colnames(datos) <- c("nombre","lat","lng","elevation")

semaforos$elevation <- 150

semaforos <- rbind(semaforos,datos)

semaforos$color <- if_else(semaforos$elevation < 50, "yellow",
                       if_else(semaforos$elevation < 80, "orange",
                               if_else(semaforos$elevation < 101, "red", "blue")))

#colnames(semaforos)

# API Key de Mapbox
key <- "pk.eyJ1IjoiZnVhbnl4IiwiYSI6ImNtOHJqajl0ZDBvdXEya3B1NDRqdGFrbWkifQ.WDEktBp9M8nUmdzf6BxdYg"

### Fin de datos para el mapa nuevo ###


### Datos de tarjetas visuales ###

temp1  <- trayectos %>%
  group_by(Principal,Tipo,Hora) %>%
  mutate("Tiempo_total" = sum(`Tiempo promedio`)) %>%
  ungroup()
temp1$key <- paste(temp1$Principal, temp1$Hora, temp1$Tipo)
temp1 <- temp1[!duplicated(temp1$key),]
temp2  <- coordenadas %>%
  group_by(Sobre) %>%
  mutate("Distancia_total" = sum(as.numeric(str_split_fixed(distancia," ",2)[,1]))) %>%
  ungroup()
temp2 <- temp2[duplicated(temp2$Sobre),]
temp1 <- temp1[,c(2,3,6,7)]
temp2 <- temp2[,c(2,10)]
colnames(temp2)[1] <- "Principal"
temp2 <- temp2[!duplicated(temp2$Principal),]
temp2$Principal <- gsub(" ","",temp2$Principal)
tarjetas <- full_join(temp1,temp2)
tarjetas$"Km/H" <- (tarjetas$Distancia_total/1000) / (tarjetas$Tiempo_total / 60)
tarjetas$Distancia_total <- tarjetas$Distancia_total/1000
tarjetas$Tiempo_total <- tarjetas$Tiempo_total / 60


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


gif_list <- c("Av kabah ida.gif", "kabah_vuelta.gif")  

server <- function(input, output, session) {
  
  
  
  
  semaforos_reactivos <- reactiveVal(semaforos)
  coordenadas_reactivas <- reactiveVal(coordenadas)
  
  ida_reactivas <- reactiveVal(ida)
  vuelta_reactivas <- reactiveVal(vuelta)
  
  
  coordenadas_reactivas_ida <- reactiveVal(coordenadas_reactivas)
  coordenadas_reactivas_vuelta <- reactiveVal(coordenadas_reactivas)
  
  #grafico_actual <- reactiveVal("barras")
  
  
  gif_ida <- reactiveVal(gif_list[1])
  gif_vuelta <- reactiveVal(gif_list[2])
  
    #observeEvent(input[[paste0("gif", i)]], {
    #  gif_actual(gif_list[i])  # Cambia el GIF mostrado
    #})
  
  
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
    
    gif_ida(gif_list[1])
    gif_vuelta(gif_list[2])
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
  
  #output$mapa <- renderMapdeck({
  #  mapdeck(token = key, style = mapdeck_style("dark"),
  #          pitch = 80, zoom = 14) %>%
  #    add_column(
  #      data = data,
  #      lon = "lon",
  #      lat = "lat",
  #      elevation = "elevation",
  #      fill_colour = "elevation",
  #      radius = 20,
  #      elevation_scale = 2,
  #      layer_id = "column_layer",
  #      tooltip = "elevation"
  #    )
  #})
  
  observe({
    leafletProxy("mapa") %>%
      clearShapes() %>%  # Limpia los círculos previos
      clearMarkers() %>%
      setView(lng = mean(semaforos_reactivos()$lng), 
              lat = mean(semaforos_reactivos()$lat), 
              zoom = 13) %>%  
      addCircleMarkers(
        data = semaforos_reactivos(),
        lat = ~lat, lng = ~lng,
        color = ~color,       # Usa la columna color dinámicamente
        fillColor = ~color,   # También aplica el color al relleno
        fillOpacity = 0.9,    # Ajusta la opacidad del círculo
        radius = .05,           # Tamaño fijo en píxeles
        label = ~nombre
      )
  })
  
  
  
  output$tabla_resultados <- renderTable({
    coordenadas_reactivas()[, c("De","a","distancia","Trayecto","Tiempo"), drop = FALSE]
  })

  
  output$descargar <- downloadHandler(
    filename = function() { "ETA_Resultados.csv" },
    content = function(file) {
      write.csv(coordenadas_reactivas(), file, row.names = FALSE)
    })
  
#  observeEvent(input$btn1, { grafico_actual("barras") })
  
#  output$grafico1 <- renderPlot({
#    if (grafico_actual() == "barras") {
#      ggplot(ida_reactivas(), aes(x = Hora, y = `Tiempo promedio`, fill = reorder(De, ID))) +
#        geom_bar(stat = "identity") +
#        labs(title = "Tiempo en trayecto Ida",
#             x = "Hora", y = "Tiempo (minutos)", fill = "Av principal") +
#        theme_minimal()
#    } 
#  })
  
#  output$grafico2 <- renderPlot({
#    if (grafico_actual() == "barras") {
#      ggplot(vuelta_reactivas(), aes(x = Hora, y = `Tiempo promedio`, fill = reorder(De, ID))) +
#        geom_bar(stat = "identity") +
#        labs(title = "Tiempo en trayecto Regreso",
#             x = "Hora", y = "Tiempo (minutos)", fill = "Av principal") +
#        theme_minimal()
#    } 
 # })
  
  
  output$gif_mostrar_ida <- renderUI({
    tags$img(src = gif_ida(), height = "400px")  # Muestra el GIF seleccionado
  })
  
  output$gif_mostrar_vuelta <- renderUI({
    tags$img(src = gif_vuelta(), height = "400px")  # Muestra el GIF seleccionado
  })
  
  output$objetivo <- renderText({
    paste(round(min(tarjetas$Tiempo_total, na.rm = TRUE), 2), "min")
  })
  
  output$Tiempo <- renderText({
    paste(round(sum(tarjetas$Tiempo_total, na.rm = TRUE), 2), "min")
  })
  
  output$suma_distancia <- renderText({
    paste(round(sum(tarjetas$Distancia_total, na.rm = TRUE), 2), "km")
  })
  
  output$"Km/H" <- renderText({
    paste(round(mean(tarjetas$`Km/H`, na.rm = TRUE), 2), "Km/H")
  })
  
  
}
