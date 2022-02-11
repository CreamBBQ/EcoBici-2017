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



