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
coordenadas$`Origen ` <- gsub(" ","",coordenadas$`Origen `)
coordenadas$Destino <- gsub(" ","",coordenadas$Destino)

#?gsub()


datos <- read.csv("./www/puntos_interpolados2.csv")
datos$Tiempo <- as.numeric(datos$Tiempo)
datos <- subset(datos, !is.na(datos$Tiempo))
datos$"elevation" <- (datos$Tiempo - min(datos$Tiempo)) / (max(datos$Tiempo) - min(datos$Tiempo))
datos$elevation <- datos$elevation * 100
datos <- datos[,c(1,5,6,10,7,9)]
colnames(datos) <- c("nombre","lat","lng","elevation","Hora","Trayecto")
datos$nombre <- gsub("1. Av Kabah","Av.Kabah",datos$nombre)
datos$Hora <- as.character(datos$Hora)


datos$color <- if_else(datos$elevation < 50, "yellow",
                       if_else(datos$elevation < 80, "orange",
                               if_else(datos$elevation < 101, "red", "blue")))

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
#tarjetas$Tiempo_total <- tarjetas$Tiempo_total / 60
tarjetas$Principal <- gsub("Av.Kabah","Av.Kabah",tarjetas$Principal)
tarjetas$Principal <- gsub("Av.AndresQuintanaRoo","Av. Andres Quintana Roo",tarjetas$Principal)




# Función para obtener el ETA desde HERE Maps
obtener_eta <- function(origen, destino) {
  url <- paste0("https://router.hereapi.com/v8/routes?transportMode=car&origin=", origen, "&destination=", destino, "&return=summary&apikey=", API_KEY)
  # <- paste0("https://router.hereapi.com/v8/routes?transportMode=car&origin=", df$`Origen `[1], "&destination=", df$Destino[1], "&return=summary&apikey=", API_KEY)
  
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
  
  # Variable reactiva para almacenar los datos calculados al presionar el botón
  datos_actualizados <- reactiveVal(NULL)
  
  # Filtrar datos al seleccionar la avenida
  datos_hora <- reactive({
    req(input$hora)
    list(
      datos = filter(datos, Hora == input$hora),
      tarjetas = filter(tarjetas, Hora == input$hora )
    )
  })
  
  datos_filtrados <- reactive({
    req(input$avenida)
    df_datos <- datos_hora()$datos
    df_tarjetas <- datos_hora()$tarjetas
    
    list(
      df_tarjetas = filter(df_tarjetas, Principal == input$avenida ),
      df_datos = filter(df_datos, nombre == input$avenida),
      coordenadas = filter(coordenadas, Sobre == input$avenida)
    )
  })
  
  # Filtrar datos al seleccionar la ruta, pero sin calcular "Tiempo"
  datos_sentido <- reactive({
    req(input$ruta)

    df_coordenadas <- datos_filtrados()$coordenadas
    df_datos = datos_filtrados()$df_datos
    list(
        df_datos = filter(df_datos, Trayecto == input$ruta),
        df = filter(df_coordenadas, Trayecto == input$ruta)  # Retorna un dataframe, no lista
    )
  })
  
  # Mostrar tabla con datos filtrados o calculados
  output$tabla_resultados <- renderTable({
    df_filtrado <- datos_sentido()$df  
    df_calculado <- datos_actualizados()
    df <- if (!is.null(df_calculado)) df_calculado else df_filtrado 
    if (nrow(df) == 0) return(NULL)
    df[, c("De", "a", "distancia", "Trayecto", "Tiempo"), drop = FALSE]
  })

  # Evento que se activa solo cuando el usuario presiona el botón "Generar"
  observeEvent(input$generar, {
    df <- datos_sentido()$df
    if (nrow(df) > 0) {
      df <- df %>% 
        mutate(Tiempo = mapply(obtener_eta, `Origen ` , Destino))
      datos_actualizados(df)
    }
  })

  output$descargar <- downloadHandler(
    filename = function() { "ETA_Resultados.csv" },
    content = function(file) {
      write.csv(datos_sentido()$df, file, row.names = FALSE)
    })
  
  observe({
    leafletProxy("mapa") %>%
      clearShapes() %>%
      clearMarkers() %>%
      setView(lng = mean(semaforos$lng), 
              lat = mean(semaforos$lat), 
              zoom = 13) %>%  
      addCircleMarkers(
        data = semaforos,
        lat = ~lat, lng = ~lng,
        color = "blue",
        fillColor = "blue",
        fillOpacity = 0.9,
        radius = 5,  # Ajustado para mayor visibilidad
        label = ~nombre
      ) %>%
      addCircleMarkers(
        data = datos_sentido()$df_datos,  # Segunda fuente de datos
        lat = ~lat, lng = ~lng,
        color = ~color,  # Otro color para diferenciar
        fillColor = ~color,
        fillOpacity = 0.7,
        radius = 2
        #label = ~descripcion
      )
  })
  
  # Renderizar el mapa
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  

  
  gif_ida <- reactiveVal(gif_list[1])
  gif_vuelta <- reactiveVal(gif_list[2])
  
  output$gif_mostrar_ida <- renderUI({
    tags$img(src = gif_ida(), height = "300px")  # Muestra el GIF seleccionado
  })
  
  output$gif_mostrar_vuelta <- renderUI({
    tags$img(src = gif_vuelta(), height = "300px")  # Muestra el GIF seleccionado
  })
  
  output$objetivo <- renderText({
    paste(round(min(datos_filtrados()$df_tarjetas$Tiempo_total, na.rm = TRUE), 2), "min")
  })
  
  output$Tiempo <- renderText({
    paste(round(sum(datos_filtrados()$df_tarjetas$Tiempo_total, na.rm = TRUE), 2), "min")
  })
  
  output$suma_distancia <- renderText({
    paste(round(sum(datos_filtrados()$df_tarjetas$Distancia_total, na.rm = TRUE), 2), "km")
  })
  
  output$"Km/H" <- renderText({
    paste(round(mean(datos_filtrados()$df_tarjetas$`Km/H`, na.rm = TRUE), 2), "Km/H")
  })
  
  
}

         