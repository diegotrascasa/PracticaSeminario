#IMPORTAR DATOS DEL AIRE
library(readr)
As_DD_2021 <- read_delim("input/data/calidad del aire/2020/As_DD_2021.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(As_DD_2021)


BaP_DD_2021 <- read_delim("input/data/calidad del aire/2020/BaP_DD_2021.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(BaP_DD_2021)


Cd_DD_2021 <- read_delim("input/data/calidad del aire/2020/Cd_DD_2021.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Cd_DD_2021)


Ni_DD_2021 <- read_delim("input/data/calidad del aire/2020/Ni_DD_2021.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Ni_DD_2021)


Pb_DD_2021 <- read_delim("input/data/calidad del aire/2020/Pb_DD_2021.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Pb_DD_2021)


PM10_DD_2021 <- read_delim("input/data/calidad del aire/2020/PM10_DD_2021.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(PM10_DD_2021)


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


#2019

As_DD_2019 <- read_delim("input/data/calidad del aire/2019/As_DD_2019.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(As_DD_2019)


BaP_DD_2019 <- read_delim("input/data/calidad del aire/2019/BaP_DD_2019.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(BaP_DD_2019)


C6H6_DD_2019 <- read_delim("input/data/calidad del aire/2019/C6H6_DD_2019.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(C6H6_DD_2019)


Cd_DD_2019 <- read_delim("input/data/calidad del aire/2019/Cd_DD_2019.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Cd_DD_2019)


Ni_DD_2019 <- read_delim("input/data/calidad del aire/2019/Ni_DD_2019.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Ni_DD_2019)


Pb_DD_2019 <- read_delim("input/data/calidad del aire/2019/Pb_DD_2019.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Pb_DD_2019)


PM10_DD_2019 <- read_delim("input/data/calidad del aire/2019/PM10_DD_2019.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(PM10_DD_2019)


PM25_DD_2019 <- read_delim("input/data/calidad del aire/2019/PM25_DD_2019.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(PM25_DD_2019)


##Arsenico 2019

tablaAS_2019 <- select(.data = As_DD_2019, PROVINCIA, ANNO, D01:D31 )

tablaAS_2019 <-rename(.data = tablaAS_2019, AÑO = ANNO)

tabAS_2019 <- tablaAS_2019 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabAS_2019$ARSENICO <- apply(tabAS_2019[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabAS_2019 <- select(.data = tabAS_2019, PROVINCIA, AÑO, ARSENICO)

print (tabAS_2019)

##Benzopireno 2019

tablaBaP_2019 <- select(.data = BaP_DD_2019, PROVINCIA, ANNO, D01:D31)

tablaBaP_2019 <-rename(.data = tablaBaP_2019, AÑO = ANNO)

tabBaP_2019 <- tablaBaP_2019 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabBaP_2019$BENZOPIRENO <- apply(tabBaP_2019[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabBaP_2019 <- select(.data = tabBaP_2019, PROVINCIA, AÑO, BENZOPIRENO)
print(tabBaP_2019)

##Benceno 2019

tablaC6H6_2019 <- select(.data = C6H6_DD_2019, PROVINCIA, ANNO, D01:D31)

tablaC6H6_2019 <-rename(.data = tablaC6H6_2019, AÑO = ANNO)

tabC6H6_2019 <- tablaC6H6_2019 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabC6H6_2019$BENCENO <- apply(tabC6H6_2019[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabC6H6_2019 <- select(.data = tabC6H6_2019, PROVINCIA, AÑO, BENCENO)
print(tabC6H6_2019)

##Cadmio 2019

tablaCd_2019 <- select(.data = Cd_DD_2019, PROVINCIA, ANNO, D01:D31)

tablaCd_2019 <-rename(.data = tablaCd_2019, AÑO = ANNO)

tabCd_2019 <- tablaCd_2019 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabCd_2019$CADMIO <- apply(tabCd_2019[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabCd_2019 <- select(.data = tabCd_2019, PROVINCIA, AÑO, CADMIO)
print(tabCd_2019)

##Niquel 2019

tablaNi_2019 <- select(.data = Ni_DD_2019, PROVINCIA, ANNO, D01:D31)

tablaNi_2019 <-rename(.data = tablaNi_2019, AÑO = ANNO)

tabNi_2019 <- tablaNi_2019 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabNi_2019$NIQUEL <- apply(tabNi_2019[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabNi_2019 <- select(.data = tabNi_2019, PROVINCIA, AÑO, NIQUEL)
print(tabNi_2019)

##Plomo 2019

tablaPb_2019 <- select(.data = Pb_DD_2019, PROVINCIA, ANNO, D01:D31)

tablaPb_2019 <-rename(.data = tablaPb_2019, AÑO = ANNO)

tabPb_2019 <- tablaPb_2019 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPb_2019$PLOMO <- apply(tabPb_2019[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPb_2019 <- select(.data = tabPb_2019, PROVINCIA, AÑO, PLOMO)
print(tabPb_2019)

##PM10 2019

tablaPM10_2019 <- select(.data = PM10_DD_2019, PROVINCIA, ANNO, D01:D31)

tablaPM10_2019 <-rename(.data = tablaPM10_2019, AÑO = ANNO)

tabPM10_2019 <- tablaPM10_2019 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPM10_2019$PM_10 <- apply(tabPM10_2019[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPM10_2019 <- select(.data = tabPM10_2019, PROVINCIA, AÑO, PM_10)
print(tabPM10_2019)

##PM25 2019

tablaPM25_2019 <- select(.data = PM25_DD_2019, PROVINCIA, ANNO, D01:D31)

tablaPM25_2019 <-rename(.data = tablaPM25_2019, AÑO = ANNO)

tabPM25_2019 <- tablaPM25_2019 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPM25_2019$PM_25 <- apply(tabPM25_2019[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPM25_2019 <- select(.data = tabPM25_2019, PROVINCIA, AÑO, PM_25)
print(tabPM25_2019)

#2018