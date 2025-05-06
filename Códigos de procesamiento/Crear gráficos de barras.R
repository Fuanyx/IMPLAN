library(ggplot2)
library(gganimate)
library(dplyr)
library(shiny)
library(RPostgres)
library(readxl)
library(gifski)
library(av)
library(magick)

# Conexión a la base de datos
con <- dbConnect(
  Postgres(),
  dbname = "siginplan",
  host = "45.132.241.118",     
  port = 5432,         
  user = "juanyam",
  password = "eJnNPmklNznIkZ1EJ8JB4B=="
)
# Datos de ejemplo (reemplaza con tu propio dataset)
datos <- data.frame(
  Hora = rep(seq(1, 24), each = 5),  # Hora de 1 a 24 (se repite para 5 calles)
  Tiempo = runif(120, min = 5, max = 15),  # Tiempos aleatorios
  Calle = rep(letters[1:5], times = 24),  # 5 calles
  ID = rep(1:5, times = 24)  # ID de las calles
)


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

ida <- vuelta
ida <- subset(ida, ida$Principal == "Av.Kabah")
ida <- ida[,c(3,5,4,1)]


datos <- read_excel("D:/IMPLAN/Semaforos/Tiempos en el trafico/Tiempos de traslado.xlsx")

datos$key <- paste(datos$Sobre, datos$De, datos$a, datos$Trayecto, datos$Hora)

datos <- datos %>%
  group_by(key) %>%
  mutate(Tiempo = mean(`Tiempo ETA`)) %>%
  ungroup()

datos <- datos[!duplicated(datos$key),]

#[1] "Av.Kabah"                "Av. Andres Quintana Roo" "Av. López Portillo"      "Av. Nichupté"           
#[5] "Av. Xcaret"              "Av. Cobá"                "Av. Chac Mool"           "Av. Tulum"   

#"Ida"     "Regreso"

datos$ID <- seq_len(nrow(datos))

ida <- subset(datos, datos$Sobre =="Av. Tulum" & datos$Trayecto == "Regreso")

if (ida$Trayecto[1] == "Regreso") {
  ida <- ida[order(ida$ID, decreasing = T),]
}


ida <- ida[,c(7,12,2)]
ida$ID <- seq_len(nrow(ida))

colnames(ida) <- c("Hora", "Tiempo", "Calle", "ID")

#datos <- ida


anim <- ggplot(ida, aes(x = Hora, y = Tiempo, fill = reorder(Calle, ID))) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +  # Más anchas
  labs(
    title = "Regreso",
    x = "Hora",
    y = "Tiempo (minutos)",
    fill = "Avenida"
  ) +
  theme_minimal(base_size = 16) +  # Aumenta tamaño de fuente general
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 18)
  ) +
  transition_manual(Hora, cumulative = TRUE) +
  ease_aes('linear') + theme_light(base_size = 16)



# Ruta de guardado (asegúrate que exista la carpeta)
ruta_gif <- "C:/Users/IMPLAN/Documents/tulum_Regreso.gif"

# Crear y guardar directamente el GIF con gifski_renderer
gganimate::animate(
  anim,
  duration = 20,
  fps = 30,
  width = 800,
  height = 600,
  renderer = gifski_renderer(ruta_gif)
)

# Mostrar ruta para confirmar
ruta_gif
