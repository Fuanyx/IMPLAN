library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(dplyr)
library(RPostgres)
library(mapdeck)
library(ggplot2)
library(stringr)
library(dplyr)

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
semaforos <- subset(semaforos, semaforos$nombre != "2. Bld. Kukulcan")

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




metrica <- read.csv("./www/Analisis calles.csv",encoding = "Latin1")
colnames(metrica) <- c("Calle", "Trayecto", "Tiempo", "p1", "p2", "trafico", "Horas", "Km/H","Distancia")
metrica$Distancia <- paste(metrica$Distancia, "km")
metrica$trafico <- gsub("Poco trafico", "Objetivo", metrica$trafico)
unique(metrica$Calle)
#metrica$Calle<- gsub("Av. Cob�", "Av. Cobá", metrica$Calle)
#metrica$Calle<- gsub("Av. L\xf3pez Portillo", "Av. López Portillo", metrica$Calle)
#metrica$Calle<- gsub("Av. Nichupt\xe9", "Av. Nichupté", metrica$Calle)
#metrica$Calle<- gsub("Av. Cob\xe1", "Av. Cobá", metrica$Calle)

unique(metrica$Calle)

datos <- read.csv("./www/puntos_interpolados2.csv")

datos <- datos[,c(1,2,3,4,5,10,11,8,9)]
colnames(datos) <- c("Sobre", "De", "a", "distancia", "Trayecto", "Hora", "Tiempo","Latitud", "Longitud")

colnames(datos)
datos$Tiempo <- as.numeric(datos$Tiempo)
datos <- subset(datos, !is.na(datos$Tiempo))

datos$distancia <- as.numeric(datos$distancia)
datos$metrosminuto <- datos$distancia / datos$Tiempo

#datos$"elevation" <- (datos$metrosminuto - min(datos$metrosminuto)) / (max(datos$metrosminuto) - min(datos$metrosminuto))
#datos$"elevation" <- 100
#datos$elevation <- datos$elevation * 100
datos <- datos[,c("Sobre","Latitud","Longitud","metrosminuto","Hora","Trayecto","De")]
colnames(datos) <- c("nombre","lat","lng","elevation","Hora","Trayecto","Secundaria")
#datos$nombre <- "Todas"

#datos$nombre <- "Todas"
#unique(datos$nombre)

#unique(datos$)

#datos <- datos %>%
#  group_by(nombre,Trayecto) %>%
#  mutate(acum_Hora = rep(0:23, each = 1))

datos <- datos %>%
  group_by(nombre, Trayecto, Secundaria) %>%
  mutate("p1" = quantile(`elevation`, probs = 0.33) ) %>%
  mutate("p2" = quantile(`elevation`, probs = 0.66) ) %>%
  ungroup()

#p1 <- quantile(datos$elevation, probs = 0.33)  # Primer tercil
#p2 <- quantile(datos$elevation, probs = 0.66)  # Segundo tercil

datos$color <- if_else(datos$elevation < datos$p1, "red",
                       if_else(datos$elevation < datos$p2, "orange", "yellow"))

#                               if_else(datos$elevation < 101, "red", "blue")))
#
#tiempos$trafico <- if_else(tiempos$`Tiempo total` < tiempos$p1, "Sin trafico",
#                           if_else(tiempos$`Tiempo total` < tiempos$p2, "Poco trafico", "Mucho trafico"))

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

trayecto_gif <- c("Ida.gif", "Regreso.gif")  

horas_unicas <- sort(unique(datos$Hora))  


server <- function(input, output, session) {

  
  # Variable reactiva para almacenar los datos calculados al presionar el botón
  datos_actualizados <- reactiveVal(NULL)
  indice <- reactiveVal(1)

  
  timer <- reactiveTimer(3000)
  
  # Estado reactivo para la hora actual
  hora_actual <- reactiveVal(horas_unicas[1])  # Iniciar con la primera hora
  
  # Cambiar la hora automáticamente
  observeEvent(timer(), {
    # Obtener el índice de la hora actual
    idx <- match(hora_actual(), horas_unicas)
    
    # Determinar la siguiente hora (cíclica)
    nueva_hora <- if (idx == length(horas_unicas)) horas_unicas[1] else horas_unicas[idx + 1]
    
    # Actualizar el valor reactivo
    hora_actual(nueva_hora)
  })
  
  
  
  
  # Filtrar datos al seleccionar la avenida
  datos_hora <- reactive({
    req(hora_actual)
    list(
      datos = filter(datos, Hora == hora_actual()),
      tarjetas = filter(tarjetas, Hora == hora_actual() )
    )
  })
  
  datos_filtrados <- reactive({
    req(input$avenida)
    df_datos <- datos_hora()$datos
    df_tarjetas <- datos_hora()$tarjetas
    
    list(
      df_tarjetas = filter(df_tarjetas, Principal == input$avenida ),
      df_datos = filter(df_datos, nombre == input$avenida),
      coordenadas = filter(coordenadas, Sobre == input$avenida),
      metrica = filter(metrica, Calle == input$avenida)
    )
  })
  
  # Filtrar datos al seleccionar la ruta, pero sin calcular "Tiempo"
  datos_sentido <- reactive({
    req(input$ruta)

    df_coordenadas <- datos_filtrados()$coordenadas
    df_datos = datos_filtrados()$df_datos
    metrica = datos_filtrados()$metrica
    
    list(
        df_datos = filter(df_datos, Trayecto == input$ruta),
        df = filter(df_coordenadas, Trayecto == input$ruta),  # Retorna un dataframe, no lista
        metrica = filter(metrica, Trayecto== input$ruta)
        
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
  
  
  
  output$tabla_metrica <- renderTable({
    metri <- datos_sentido()$metrica
    if (nrow(metri) == 0) return(NULL)
    metri[, c("Calle", "trafico", "Distancia","Tiempo","Horas"), drop = FALSE]
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
  
  icono_semaforo <- makeIcon(
    iconUrl = "www/semaforo.png",
    iconWidth = 15, iconHeight = 15
  )
  
  icono_carro <- makeIcon(
    iconUrl = "https://cdn-icons-png.flaticon.com/512/741/741407.png",
    iconWidth = 30, iconHeight = 30
  )
  
  observe({
    # Definir el ícono

    
      req(datos_sentido()$df_datos) 
      leafletProxy("mapa") %>%
        clearShapes() %>%
        clearMarkers() %>%
        setView(lng = mean(semaforos$lng), 
                lat = mean(semaforos$lat), 
                zoom = 12.5) %>%  
        addMarkers(
          data = semaforos,
          lat = ~lat, lng = ~lng,
          icon = icono_semaforo,
          label = ~nombre
        )
      df_movimiento <- datos_sentido()$df_datos
          leafletProxy("mapa") %>%
          addCircleMarkers(
            lat = df_movimiento$lat,
            lng = df_movimiento$lng,
            color = df_movimiento$color,
            fillColor = df_movimiento$color,#
            fillOpacity = 0.7,
            radius = 2,
          )
          
          #session$sendCustomMessage('fadeMarkers', "movimiento")
          
    })
  
  
  # Renderizar el mapa
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
# ICONO DESCARGADO
  
  gif_ida <- reactiveVal(gif_list[1])
  gif_vuelta <- reactiveVal(gif_list[2])
  
  output$gif_mostrar_ida <- renderUI({
    tags$img(src = gif_ida(), height = "350px")  # Muestra el GIF seleccionado
  })
  
  output$gif_mostrar_vuelta <- renderUI({
    tags$img(src = gif_vuelta(), height = "350px")  # Muestra el GIF seleccionado
  })
  
  
  gif_trayecto <- reactiveVal(trayecto_gif[1])
  
  output$gif_trayecto_mostrar <- renderUI({
    tags$img(src = gif_trayecto(), height = "400px",)  # Muestra el GIF seleccionado
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
  
  output$"Km/H" <- renderText({ #hora_actual()
    paste(round(mean(datos_filtrados()$df_tarjetas$`Km/H`, na.rm = TRUE), 2), "Km/H")
  })
  

}


         