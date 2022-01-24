rm(list = ls())
setwd("/home/creambbq/bairesBikes")  

#librerias 
library("dplyr")
library("ggplot2")

#datos
datos <-  read.table("recorridos-realizados-2017.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

#script
fil <- nrow(datos)
col <- ncol(datos)

