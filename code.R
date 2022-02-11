rm(list = ls())
setwd("/home/creambbq/code/bairesBikes")  

##librerias 
library("dplyr")
library("ggplot2")

#datos
datos <-  read.table("recorridos-realizados-2017.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

##Limpieza de datos y creación de nuevas variables.
nRow <- nrow(datos)
nCol <- ncol(datos)
#print(class(datos$bici_Fecha_hora_retiro))
datos$bici_Fecha_hora_retiro <- as.POSIXct(datos$bici_Fecha_hora_retiro, format="%Y-%m-%d %H:%M:%OS")
datos$fecha <- as.Date(datos$bici_Fecha_hora_retiro, tz = "America/Argentina/Buenos_Aires" )
datos$bici_tiempo_uso_sec <- datos$bici_tiempo_uso * 60
datos$bici_Fecha_hora_devolucion <- datos$bici_Fecha_hora_retiro + datos$bici_tiempo_uso_sec
datos$dia <-  strftime(datos$bici_Fecha_hora_retiro, format = "%w")
datos <- datos %>% mutate(hour = strftime(bici_Fecha_hora_retiro, format = "%H"),
                          hour = as.numeric(hour))
datos <- datos %>% mutate(fin_de_semana = case_when(dia %in% c(6,0) ~ "Fin de semana", dia %in% c(1,2,3,4,5) ~ "Día de semana"), 
                          fin_de_semana = as.factor(fin_de_semana))
datos <- datos %>% mutate(misma_estacion = case_when((bici_estacion_origen == bici_estacion_destino) == TRUE ~ "Misma estación",
                                                     (bici_estacion_origen == bici_estacion_destino) == FALSE ~ "Distinta estación"),
                          misma_estacion = as.factor(misma_estacion))

##Gráficos 
#Minutos de viaje dependiendo de si es fin de semana o no. 
ggplot(datos, 
       aes(x=bici_tiempo_uso, fill = fin_de_semana, ..scaled..)) + 
  geom_density(alpha = 0.66) + 
  coord_cartesian(xlim = c(0, 200)) + 
  labs(x="Duracion de los viajes en minutos", y = "Densidad") +
  scale_fill_manual(values = c("#004c69","#72737e")) +
  scale_x_continuous(breaks = seq(0,200,15)) + 
  theme(legend.title = element_blank(), legend.position = "top")
#sum(is.na(datos_bicis$bici_tiempo_uso) == TRUE)
#Hora de extracción distinguiendo por si es fin de semana o no
datos %>% group_by(fecha, fin_de_semana, hour) %>% summarise(ext = n()) %>% 
  group_by(fin_de_semana, hour) %>% summarise(media = mean(ext)) %>% 
  ggplot(aes(x=hour, y = media, fill = fin_de_semana)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(x="Hora del dia", y= "Extracciones promedio") +
  scale_fill_manual(values = c("#004c69","#72737e")) +
  scale_x_continuous(breaks = seq(0,23,2)) +
  scale_y_continuous(breaks = seq(0,500,50)) + 
  theme(legend.title = element_blank())
#Figura con facets que nos permite ver la densidad de extracciones en el eje 
#vertical, la duración del viaje en el horizontal y distingue en cuatro paneles
#con las combinaciones posibles de la varibale "fin_de_semana" y "misma_estación"
datos %>% group_by(bici_nombre_estacion_origen) %>% 
  filter(n()>999, is.na(misma_estacion) == FALSE, bici_sexo!="NO INFORMADO") %>%
  ggplot(aes(x= bici_tiempo_uso, fill = bici_sexo, ..scaled..)) + 
  geom_density(alpha = 0.75) + 
  coord_cartesian(xlim = c(0, 150)) + 
  scale_fill_manual(values =c("#66c880","#eb872a"), labels = c("Mujeres", "Hombres")) +
  labs(x="Duracion de los viajes en minutos", y = "Densidad de extracción") + 
  facet_grid( misma_estacion ~ fin_de_semana ) +
  theme_light() +
  theme(legend.title = element_blank())

#Evolución anual de la extracción de bicicletas distinguiendo por sexo
datos %>% filter(bici_sexo != "NO INFORMADO") %>%  group_by(fecha, bici_sexo) %>% 
  summarise(cant = n()) %>% 
  ggplot(aes(x=fecha, y=cant, colour = bici_sexo)) +
  geom_point(alpha = .7) + 
  geom_smooth() +
  scale_color_manual(values = c("#66c880","#eb872a"), 
                     labels = c("Mujeres", "Hombres")) + 
  labs(y = "Cantidad de extracciones", x = "Fecha") +
  theme_gray() + 
  theme(legend.title = element_blank())

#20 estaciones con mayor cantidad de extracciones 
datos %>% group_by(bici_nombre_estacion_origen) %>% summarise(cant = n()) %>% 
  arrange(desc(cant)) %>% top_n(20) %>% 
  ggplot(aes(x=cant, y = reorder(bici_nombre_estacion_origen, -cant))) + 
  geom_bar(stat = "identity", fill = "#f4e04d") +
  theme_dark() +
  labs(x = "Cantidad de extracciones", y = " ") 


