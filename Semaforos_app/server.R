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
library(readxl)

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
semaforos$id <- str_split_fixed(semaforos$Calle," ",2)[,1]
semaforos <- semaforos[,c(2,4,5,8)]
colnames(semaforos) <- c("nombre","lat","lng","id")
semaforos <- subset(semaforos, semaforos$nombre != "2. Bld. Kukulcan")




trayectos <- dbGetQuery(con, 'SELECT * FROM "99_Evaluaciones"."Tiempo_traslado_semaforos"')
trayectos$key <- paste(trayectos$Principal, trayectos$De)
trayectos$Hora <- as.numeric(trayectos$Hora)
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
colnames(coordenadas)[5] <- "origen"
colnames(coordenadas)[6] <- "destino"
#unique(metrica$Sobre)

metrica <- read.csv("./www/Analisis calles.csv",fileEncoding = "latin1")
colnames(metrica) <- c("Calle", "Trayecto", "Tiempo", "p1", "p2", "trafico", "Horas", "Km/H","Distancia")
metrica$Distancia <- paste(metrica$Distancia, "km")
metrica$trafico <- gsub("Poco trafico", "Fluido 🚗🚗", metrica$trafico)
metrica$trafico <- gsub("Sin trafico", "Libre 🚗", metrica$trafico)
metrica$trafico <- gsub("Mucho trafico", "Atascado 🚗🚗🚗", metrica$trafico)

metrica <- metrica[order(metrica$trafico, decreasing = F),]

metrica2 <- subset(metrica, metrica$trafico == "Libre 🚗")
metrica2$Vel_obj <- "60 km/h"
metrica2$Tiempo_obj <- as.numeric(str_split_fixed(metrica2$Distancia," ",2)[,1] )
metrica2$`Km/H` <- paste(round(metrica2$`Km/H`, 2),"km/h")

# Baches
#baches <- read_excel("./www/baches.xlsx")



# Baches
#choque <- read_excel("./www/accidentes.xlsx")
#choque$latitud <- as.numeric(choque$latitud)
#choque$longitud <- as.numeric(choque$longitud)
#choque$label_html <- paste0(
#  "Likes: ", choque$Likes, "<br/>",
#  "Último reporte ", choque$fecha_reporte, "<br/>"
#)


datos <- read.csv("./www/puntos_interpolados2.csv")
datos <- datos[,c(1,2,3,4,5,10,11,8,9)]
colnames(datos) <- c("Sobre", "De", "a", "distancia", "Trayecto", "Hora", "Tiempo","Latitud", "Longitud")
datos$Tiempo <- as.numeric(datos$Tiempo)
datos <- subset(datos, !is.na(datos$Tiempo))

datos$distancia <- as.numeric(datos$distancia)
datos$metrosminuto <- datos$distancia / datos$Tiempo

datos <- datos[,c("Sobre","Latitud","Longitud","metrosminuto","Hora","Trayecto","De")]
colnames(datos) <- c("nombre","lat","lng","elevation","Hora","Trayecto","Secundaria")

datos <- datos %>%
  group_by(nombre, Trayecto, Secundaria) %>%
  mutate("p1" = quantile(`elevation`, probs = 0.33) ) %>%
  mutate("p2" = quantile(`elevation`, probs = 0.66) ) %>%
  ungroup()

datos$color <- if_else(datos$elevation < datos$p1, "red",
                       if_else(datos$elevation < datos$p2, "orange", "yellow"))


###

#colores avenida
colores <- datos
colores$key <- paste0(colores$nombre," - " ,colores$Trayecto)
colores <- colores[!duplicated(colores$key),]
colores <- colores[,c(11,8,9)]

## Flechas de direccion

flechas <- read_excel("./www/flechas.xlsx")
colnames(flechas) <- c("nombre","lat","lng")
#flechas$icono <- "icono_semaforo"

flechas1 <- subset(flechas, flechas$nombre == "arriba")
flechas3 <- subset(flechas, flechas$nombre == "abajo")

flechas2 <- subset(flechas, flechas$nombre == "izquierda" )
flechas4 <- subset(flechas, flechas$nombre == "derecha" )

flechas1$ruta <- if_else(flechas1$nombre == "arriba", "Regreso", "Ida")
flechas3$ruta <- if_else(flechas3$nombre == "abajo", "Ida", "Regreso")


flechas2$ruta <- if_else(flechas2$nombre == "izquierda", "Regreso", "Ida")
flechas4$ruta <- if_else(flechas4$nombre == "derecha", "Ida", "Regreso")


# Función para obtener el ETA desde HERE Maps
obtener_eta <- function(origen, destino) {
  url <- paste0("https://router.hereapi.com/v8/routes?transportMode=car&origin=", origen, "&destination=", destino, "&return=summary&apikey=", API_KEY)
  respuesta <- GET(url)
  Sys.sleep(.5) # Pequeña pausa para evitar bloqueo por la API
  
  if (status_code(respuesta) == 200) {
    datos <- content(respuesta, as = "parsed", type = "application/json")
    
    if (!is.null(datos$routes)) {
      duracion_seg <- datos$routes[[1]]$sections[[1]]$summary$duration
      return(round(duracion_seg / 60, 2))
    }
  }
  return(NA)
}

nuevas_rutas <- subset(coordenadas, coordenadas$Sobre == "Av.Kabah" & coordenadas$Trayecto == "Ida")
nuevas_rutas$Tiempo <- 0
nuevas_rutas$distancia <- str_split_fixed(nuevas_rutas$distancia," ",2)[,1]
nuevas_rutas$Tiempo <- as.numeric(nuevas_rutas$Tiempo)
nuevas_rutas$distancia <- as.numeric(nuevas_rutas$distancia)
nuevas_rutas$elevation <-   nuevas_rutas$distancia / nuevas_rutas$Tiempo
nuevas_rutas$key <-  paste0(nuevas_rutas$Sobre," - " ,nuevas_rutas$Trayecto)
nuevas_rutas <- left_join(nuevas_rutas, colores)
nuevas_rutas$color <- if_else(nuevas_rutas$elevation < nuevas_rutas$p1, "red",
                              if_else(nuevas_rutas$elevation < nuevas_rutas$p2, "orange", "yellow"))
nuevas_rutas$key <- paste0(nuevas_rutas$Sobre," - ", nuevas_rutas$De," - ", nuevas_rutas$Trayecto)
nuevas_rutas <- nuevas_rutas[,c("key","color")]



ida_gif <- c("Av.Kabah" = "kabah_ida.gif","Av. Andres Quintana Roo" = "andres_ida.gif", 
             "Av. López Portillo" = "portillo_ida.gif", "Av. Xcaret" = "xcaret_ida.gif","Av. Cobá" = "coba_regreso.gif",
             "Av. Chac Mool" = "chacmol_ida.gif", "Av. Tulum" = "tulum_ida.gif", "Av. Nichupté" = "nichupte_ida.gif")

ida_png <- c("Av.Kabah" = "kabah_ida.png","Av. Andres Quintana Roo" = "andres_ida.png", 
             "Av. López Portillo" = "portillo_ida.png", "Av. Xcaret" = "xcaret_ida.png","Av. Cobá" = "coba_regreso.png",
             "Av. Chac Mool" = "chacmol_ida.png", "Av. Tulum" = "tulum_ida.png", "Av. Nichupté" = "nichupte_ida.png")

vuelta_gif <- c("Av.Kabah" = "kabah_regreso.gif","Av. Andres Quintana Roo" = "andres_regreso.gif",
              "Av. López Portillo" = "portillo_regreso.gif","Av. Xcaret" = "xcaret_ida.gif", "Av. Cobá" = "coba_regreso.gif",
              "Av. Chac Mool" = "chacmol_regreso.gif", "Av. Tulum" = "tulum_regreso.gif", "Av. Nichupté" = "nichupte_regreso.gif")

vuelta_png <- c("Av.Kabah" = "kabah_regreso.png","Av. Andres Quintana Roo" = "andres_regreso.png",
                "Av. López Portillo" = "portillo_regreso.png","Av. Xcaret" = "xcaret_ida.png", "Av. Cobá" = "coba_regreso.png",
                "Av. Chac Mool" = "chacmol_regreso.png", "Av. Tulum" = "tulum_regreso.png", "Av. Nichupté" = "nichupte_regreso.png")


trayecto_gif <- c("Ida" = "Ida.gif", "Regreso" = "Regreso.gif")


gif_trayecto <- reactiveVal(trayecto_gif[1])

gif_ida <- reactiveVal(ida_gif[1])
gif_vuelta <- reactiveVal(vuelta_gif[1])

png_ida <- reactiveVal(ida_png[1])
png_vuelta <- reactiveVal(vuelta_png[1])
horas_unicas <- sort(unique(datos$Hora))  


server <- function(input, output, session) {

  
    
  baches <- reactive({
    df <- dbGetQuery(con, 'SELECT * FROM "datos_trafico"."baches"')
    df$latitud <- as.numeric(df$latitud)
    df$longitud <- as.numeric(df$longitud)
    df$label_html <- paste0(
      "Likes: ", df$Likes, "<br/>",
      "Último reporte ", df$fecha_reporte, "<br/>"
    )
    df
  })
  
  
  choque <- reactive({
    df <- dbGetQuery(con, 'SELECT * FROM "datos_trafico"."accidentes"')
    df$latitud <- as.numeric(df$latitud)
    df$longitud <- as.numeric(df$longitud)
    df$label_html <- paste0(
      "Likes: ", df$Likes, "<br/>",
      "Último reporte ", df$fecha_reporte, "<br/>"
    )
    df
  })
  

  
  
  
  # Variable reactiva para almacenar los datos calculados al presionar el botón
  datos_actualizados <- reactiveVal(NULL)
  indice <- reactiveVal(1)

  
  timer <- reactiveTimer(3000)
  
  # Estado reactivo para la hora actual
  hora_actual <- reactiveVal(horas_unicas[1])  # Iniciar con la primera hora
  
  rutas_reactivas <- reactiveVal(nuevas_rutas)

  
  datos_filtrados <- reactive({
    req(input$avenida)
    
    list(
      df_datos = filter(datos, nombre == input$avenida),
      coordenadas = filter(coordenadas, Sobre == input$avenida),
      metrica = filter(metrica, Calle == input$avenida),
      metrica2 = filter(metrica2, Calle == input$avenida)
      
                                                                                            
    )
  })
  
  # Filtrar datos al seleccionar la ruta, pero sin calcular "Tiempo"
  datos_sentido <- reactive({
    req(input$ruta)

    df_coordenadas <- datos_filtrados()$coordenadas
    df_datos = datos_filtrados()$df_datos
    metrica = datos_filtrados()$metrica
    metrica2 = datos_filtrados()$metrica2
    
    
    list(
        df_datos = filter(df_datos, Trayecto == input$ruta),
        df = filter(df_coordenadas, Trayecto == input$ruta),  # Retorna un dataframe, no lista
        metrica = filter(metrica, Trayecto== input$ruta),
        metrica2 = filter(metrica2, Trayecto== input$ruta),
        
        flechas1 = filter(flechas1, ruta == input$ruta),
        flechas2 = filter(flechas2, ruta == input$ruta),
        flechas3 = filter(flechas3, ruta == input$ruta),
        flechas4 = filter(flechas4, ruta == input$ruta),
        
        direccion = input$ruta 
        
    )
  })
  
  
  
  observeEvent(input$generar, {
    
    nuevas_rutas <- datos_sentido()$df %>% mutate("Tiempo" = mapply(obtener_eta, origen, destino))
    nuevas_rutas$distancia <- str_split_fixed(nuevas_rutas$distancia," ",2)[,1]
    nuevas_rutas$Tiempo <- as.numeric(nuevas_rutas$Tiempo)
    nuevas_rutas$distancia <- as.numeric(nuevas_rutas$distancia)
    nuevas_rutas$elevation <-   nuevas_rutas$distancia / nuevas_rutas$Tiempo
    nuevas_rutas$key <-  paste0(nuevas_rutas$Sobre," - " ,nuevas_rutas$Trayecto)
    nuevas_rutas <- left_join(nuevas_rutas, colores)
    nuevas_rutas$color <- if_else(nuevas_rutas$elevation < nuevas_rutas$p1, "red",
                                  if_else(nuevas_rutas$elevation < nuevas_rutas$p2, "orange", "yellow"))
    nuevas_rutas$key <- paste0(nuevas_rutas$Sobre," - ", nuevas_rutas$De," - ", nuevas_rutas$Trayecto)
    nuevas_rutas <- nuevas_rutas[,c("key","color")]
    rutas_reactivas(nuevas_rutas)
    
  })
  
  # Evita que se pueda seleccionar relaciones que no tienen sentido 
  observeEvent(input$avenida, {
    if (input$avenida == "Av. Xcaret") {
      updateSelectInput(inputId = "ruta",
                        choices = c("Ida"),
                        selected = "Ida")
    } else if (input$avenida == "Av. Cobá") {
      updateSelectInput(inputId = "ruta",
                        choices = c("Regreso"),
                        selected = "Regreso")
    } else {
      updateSelectInput(inputId = "ruta",
                        choices = c("Ida", "Regreso"),
                        selected = NULL)
    }
  })
  
  
  
  observeEvent(input$ruta, {
    req(input$ruta)
    
    # Verifica que el valor de input$ruta exista en los nombres del vector
    if (input$ruta %in% names(trayecto_gif)) {
      gif_trayecto(trayecto_gif[[input$ruta]])
    }
  })
  
  # Variable reactiva para almacenar el src de la imagen que se debe mostrar
  imagen_ida <- reactiveVal(ida_gif[1])
  imagen_vuelta <- reactiveVal(vuelta_gif[1])
  
  # Observador para cambiar la imagen según la avenida y si se pausa
  observeEvent({ input$avenida; input$pausar_gif_ida }, {
    req(input$avenida)
    
    if (input$avenida %in% names(ida_gif)) {
      if (input$pausar_gif_ida) {
        imagen_ida(ida_png[[input$avenida]])
      } else {
        imagen_ida(ida_gif[[input$avenida]])
      }
    }
  })
  
  
  # Observador para cambiar la imagen según la avenida y si se pausa
  observeEvent({ input$avenida; input$pausar_gif_ida }, {
    req(input$avenida)
    
    if (input$avenida %in% names(vuelta_gif)) {
      if (input$pausar_gif_ida) {
        imagen_vuelta(vuelta_png[[input$avenida]])
      } else {
        imagen_vuelta(vuelta_gif[[input$avenida]])
      }
    }
  })
  
  output$gif_mostrar_ida<- renderUI({
    tags$img(src = imagen_ida(), height = "400px")  # Muestra el GIF seleccionado
  })
  
  
  output$gif_mostrar_vuelta <- renderUI({
    tags$img(src = imagen_vuelta(), height = "400px")  # Muestra el GIF seleccionado
  })
  
  
  output$tabla_metrica <- renderTable({
    metri <- datos_sentido()$metrica
    if (nrow(metri) == 0) return(NULL)
    metri[, c("Calle", "trafico", "Distancia","Tiempo","Horas"), drop = FALSE]
  })
  
  
  observeEvent(input$generar, {
    df <- datos_sentido()$df
    if (nrow(df) > 0) {
      df <- df %>% 
        mutate(Tiempo = mapply(obtener_eta, origen , destino))
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
  
  icono_bache <- makeIcon(
    iconUrl = "./www/bache.png",  # Cambia el ícono aquí
    iconWidth = 20, iconHeight = 20
  )
  
  
  arriba <- makeIcon(
    iconUrl = "./www/arriba.png",
    iconWidth = 25, iconHeight = 25
  )
  
  
  abajo <- makeIcon(
    iconUrl = "./www/abajo.png",
    iconWidth = 25, iconHeight = 25
  )
  
  izquierda <- makeIcon(
    iconUrl = "./www/izquierda.png",
    iconWidth = 25, iconHeight = 25
  )
  
  derecha <- makeIcon(
    iconUrl = "./www/derecha.png",
    iconWidth = 25, iconHeight = 25
  )
  
  accidente <- makeIcon(
    iconUrl = "./www/choque.png",
    iconWidth = 25, iconHeight = 25
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
          label = ~id
        ) %>%
        addMarkers(
          data = baches(),
          lng = ~longitud, lat = ~latitud,
          icon = icono_bache,
          label = lapply(baches()$label_html, HTML)
        ) %>%
        addMarkers(
          data = choque(),
          lng = ~longitud, lat = ~latitud,
          icon = accidente,
          label = lapply(choque()$label_html, HTML)
        ) %>%

        addMarkers(
          data = datos_sentido()$flechas1,
          lng = ~lng, lat = ~lat,
          icon = arriba
        ) %>%
        addMarkers(
          data = datos_sentido()$flechas3,
          lng = ~lng, lat = ~lat,
          icon = abajo
        ) %>%
        addMarkers(
          data = datos_sentido()$flechas2,
          lng = ~lng, lat = ~lat,
          icon = izquierda
        ) %>%
        addMarkers(
          data = datos_sentido()$flechas4,
          lng = ~lng, lat = ~lat,
          icon = derecha
        )
      
      df_movimiento <- datos_sentido()$df_datos
      df_movimiento$key <- paste0(df_movimiento$nombre," - ", df_movimiento$Secundaria, " - ",df_movimiento$Trayecto) 
      df_movimiento$color <- NULL
      df_movimiento <- left_join(df_movimiento, rutas_reactivas())

          leafletProxy("mapa") %>%
          addCircleMarkers(
            lat = df_movimiento$lat,
            lng = df_movimiento$lng,
            color = df_movimiento$color,
            fillColor = df_movimiento$color,#
            fillOpacity = 0.7,
            radius = 2,
          )
          
          
    })
  
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  output$gif_trayecto_mostrar <- renderUI({
    tags$img(src = gif_trayecto(), height = "400px",)  # Muestra el GIF seleccionado
  })
  
  output$km_obj <- renderText({
    datos_sentido()$metrica2$Vel_obj
  })
  
  
  output$Tiempo_obj <- renderText({
    paste(datos_sentido()$metrica2$Tiempo_obj, "min")
  })
  
  output$km_best <- renderText({
    datos_sentido()$metrica2$`Km/H`
  })
  
  
  output$Tiempo_best <- renderText({
    paste(round(datos_sentido()$metrica2$Tiempo,2), "min")
  })
  
  output$distancia_best <- renderText({
    datos_sentido()$metrica2$Distancia
  })
  
  
  output$variacion_tiempo <- renderText({
    paste(round(datos_sentido()$metrica2$Tiempo_obj - datos_sentido()$metrica2$Tiempo,2), "min")
  })

}

