#PRACTICA_SEMINARIO

#Calcular la media de los 5 años de cada provincia

library(dplyr)
DW1 <-
  select(.data = calidadFinal,PROVINCIA:N_CCAA)%>%
  group_by(PROVINCIA)%>%
  summarise(across(c(ARSENICO:PM_25), ~ mean(.x, na.rm = TRUE)))
view(DW1)
