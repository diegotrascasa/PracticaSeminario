#IMPORTAR DATOS DEL AIRE
library(readr)
As_DD_2021 <- read_delim("input/data/calidad del aire/2020/As_DD_2021.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(As_DD_2021)

library(readr)
BaP_DD_2021 <- read_delim("input/data/calidad del aire/2020/BaP_DD_2021.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(BaP_DD_2021)

library(readr)
Cd_DD_2021 <- read_delim("input/data/calidad del aire/2020/Cd_DD_2021.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Cd_DD_2021)

library(readr)
Ni_DD_2021 <- read_delim("input/data/calidad del aire/2020/Ni_DD_2021.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Ni_DD_2021)

library(readr)
Pb_DD_2021 <- read_delim("input/data/calidad del aire/2020/Pb_DD_2021.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Pb_DD_2021)

library(readr)
PM10_DD_2021 <- read_delim("input/data/calidad del aire/2020/PM10_DD_2021.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(PM10_DD_2021)

library(readr)
PM25_DD_2021 <- read_delim("input/data/calidad del aire/2020/PM25_DD_2021.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(PM25_DD_2021)





###Arsenico

tabla <- select(.data = As_DD_2021, PROVINCIA, ANNO, D01:D31 )

tabla <-rename(.data = tabla, AÑO = ANNO)

tab <- tabla %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tab$ARSENICO <- apply(tab[ ,c(3:33)], 1, mean, na.rm = TRUE)

tab <- select(.data = tab, PROVINCIA, AÑO, ARSENICO)
  
##Benzopireno
tabla2 <- select(.data = BaP_DD_2021, PROVINCIA, ANNO, D01:D31)

tabla2 <-rename(.data = tabla2, AÑO = ANNO)

tab2 <- tabla2 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tab2$BENZOPIRENO <- apply(tab2[ ,c(3:33)], 1, mean, na.rm = TRUE)

tab2 <- select(.data = tab2, PROVINCIA, AÑO, BENZOPIRENO)




