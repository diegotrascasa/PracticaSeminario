#PRACTICA_SEMINARIO

#Ajustar tablas para que coincidan las columnas:

library(dplyr)
DW1 <-
select(.data = calidadFinal,PROVINCIA:ARSENICO)%>%
  group_by(PROVINCIA)%>%
  summarise(across(c(ARSENICO), ~ mean(.x, na.rm = TRUE)))
  print(DW1)
  

