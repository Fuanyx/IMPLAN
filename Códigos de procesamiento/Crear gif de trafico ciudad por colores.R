library(leaflet)
library(dplyr)
library(htmlwidgets)
library(webshot2)
library(magick)

# Semáforos ya definidos
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

library(readxl)
flechas <- read_excel("C:/Users/yamli/Documents/IMPLAN/Semaforos/Coordenadas de av principales/flechas.xlsx")
colnames(flechas) <- c("nombre","lat","lng")
#flechas$icono <- "icono_semaforo"

flechas1 <- subset(flechas, flechas$nombre == "arriba")
flechas2 <- subset(flechas, flechas$nombre == "izquierda" )


arriba <- makeIcon(
  iconUrl = "C:/Users/yamli/Documents/IMPLAN/Semaforos/Coordenadas de av principales/arriba.png",
  iconWidth = 25, iconHeight = 25
)


abajo <- makeIcon(
  iconUrl = "C:/Users/yamli/Documents/IMPLAN/Semaforos/Coordenadas de av principales/abajo.png",
  iconWidth = 25, iconHeight = 25
)

izquierda <- makeIcon(
  iconUrl = "C:/Users/yamli/Documents/IMPLAN/Semaforos/Coordenadas de av principales/izquierda.png",
  iconWidth = 25, iconHeight = 25
)

derecha <- makeIcon(
  iconUrl = "C:/Users/yamli/Documents/IMPLAN/Semaforos/Coordenadas de av principales/derecha.png",
  iconWidth = 25, iconHeight = 25
)



#flechas$icono <- "icono_semaforo"
datos <- read.csv("C:/Users/yamli/Documents/GitHub/IMPLAN/Semaforos_app/www/puntos_interpolados2.csv")
datos <- datos[,c(1,2,3,4,5,10,11,8,9)]
colnames(datos) <- c("Sobre", "De", "a", "distancia", "Trayecto", "Hora", "Tiempo","Latitud", "Longitud")
datos$Tiempo <- as.numeric(datos$Tiempo)
datos <- subset(datos, !is.na(datos$Tiempo))
datos$distancia <- as.numeric(datos$distancia)
datos$metrosminuto <- datos$distancia / datos$Tiempo
datos <- datos[,c("Sobre","Latitud","Longitud","metrosminuto","Hora","Trayecto","De")]
colnames(datos) <- c("nombre","lat","lng","elevation","Hora","Trayecto","Secundaria")
datos$nombre <- "Todas"

datos <- datos %>%
  group_by(nombre, Trayecto, Secundaria) %>%
  mutate("p1" = quantile(elevation, probs = 0.33) ) %>%
  mutate("p2" = quantile(elevation, probs = 0.66) ) %>%
  ungroup()

datos$color <- if_else(datos$elevation < datos$p1, "red",
                       if_else(datos$elevation < datos$p2, "orange", "yellow"))

datos <- subset(datos, datos$Trayecto == "Ida")


# Crear carpeta temporal
dir.create("frames", showWarnings = FALSE)

# Crear ícono
icono_semaforo <- makeIcon(
  iconUrl = "C:/Users/yamli/Documents/GitHub/IMPLAN/Semaforos_app/www/semaforo.png",
  iconWidth = 16, iconHeight = 16
)

# Loop de horas con espera adicional
for (h in 0:23) {
  datos_hora <- subset(datos, Hora == h)
  
  map <- leaflet() %>%
    addTiles() %>%
    addMarkers(
      data = semaforos,
      lat = ~lat, lng = ~lng,
      icon = icono_semaforo
    ) %>%
    addMarkers(
      data = flechas1,
      lat = ~lat, lng = ~lng,
      icon = abajo
    ) %>%
    addMarkers(
      data = flechas2,
      lat = ~lat, lng = ~lng,
      icon = derecha
    ) %>%
    addCircleMarkers(
      data = datos_hora,
      lat = ~lat,
      lng = ~lng,
      color = ~color,
      fillColor = ~color,
      fillOpacity = 0.7,
      radius = 2
    ) %>%
    addControl(
      html = paste0("<div style='font-size:24px; font-weight:bold; color:black; background:white; padding:5px; border-radius:5px;'>Hora: ", h, ":00</div>"),
      position = "topright"
    ) 
  
  # Guardar mapa como HTML y luego como imagen con delay
  html_file <- sprintf("frames/map_%02d.html", h)
  png_file <- sprintf("frames/map_%02d.png", h)
  
  saveWidget(map, file = html_file, selfcontained = TRUE)
  
  #Sys.sleep(10)  # Pausa para asegurar que el mapa se guarde correctamente
  
  webshot(html_file, file = png_file, vwidth = 800, vheight = 600, delay = 5)  # Esperar 5 segundos antes de capturar
}

# Crear GIF más lento
imgs <- image_read(list.files("frames", pattern = "*.png", full.names = TRUE))
animation <- image_animate(imgs, fps = 1)  # Reducir FPS para que el GIF sea más lento
image_write(animation, "mapa_animado.gif")
