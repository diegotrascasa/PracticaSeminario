#PRACTICA_SEMINARIO

#Ajustar tablas para que coincidan las columnas:

library(dplyr)

datos_aire_20220b <- rename(.data = datos_aire_20220, Comunidad_Autonoma = Estación)
colnames(X02001) <- c("Sexo","Diagnóstico principal","Total Nacional","Comunidad_Autonoma","Provincia","Total")
View(X02001)
datos_aire_20220C <- mutate(.data = datos_aire_20220b, Comunidad_Autonoma = " Castilla y León")
View(datos_aire_20220C)
