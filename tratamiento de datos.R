#IMPORTAR DATOS DEL AIRE

library(readr)
library(dplyr)
library (tidyverse)
library(tidyselect)

#2020 ---------------------------------------------------------------------------------------------------
As_DD_2020 <- read_delim("input/data/calidad del aire/2020/As_DD_2020.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(As_DD_2020)

BaP_DD_2020 <- read_delim("input/data/calidad del aire/2020/BaP_DD_2020.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

Cd_DD_2020 <- read_delim("input/data/calidad del aire/2020/Cd_DD_2020.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

Ni_DD_2020 <- read_delim("input/data/calidad del aire/2020/Ni_DD_2020.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

Pb_DD_2020 <- read_delim("input/data/calidad del aire/2020/Pb_DD_2020.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

PM10_DD_2020 <- read_delim("input/data/calidad del aire/2020/PM10_DD_2020.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

PM25_DD_2020 <- read_delim("input/data/calidad del aire/2020/PM25_DD_2020.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)


##Arsenico 2020
tablaAS_2020 <- select(.data = As_DD_2020, PROVINCIA, ANNO, D01:D31 )

tablaAS_2020 <- mutate(tablaAS_2020, AÑO = 2020)

tabAS_2020 <- tablaAS_2020 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabAS_2020$ARSENICO <- apply(tabAS_2020[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabAS_2020 <- select(.data = tabAS_2020, PROVINCIA, AÑO, ARSENICO)

##Benzopireno 2020
tablaBaP_2020 <- select(.data = BaP_DD_2020, PROVINCIA, ANNO, D01:D31)

tablaBaP_2020 <- mutate(tablaBaP_2020, AÑO = 2020)

tabBaP_2020 <- tablaBaP_2020 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabBaP_2020$BENZOPIRENO <- apply(tabBaP_2020[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabBaP_2020 <- select(.data = tabBaP_2020, PROVINCIA, AÑO, BENZOPIRENO)

##Cadmio 2020

tablaCd_2020 <- select(.data = Cd_DD_2020, PROVINCIA, ANNO, D01:D31)

tablaCd_2020 <- mutate(tablaCd_2020, AÑO = 2020)

tabCd_2020 <- tablaCd_2020 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabCd_2020$CADMIO <- apply(tabCd_2020[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabCd_2020 <- select(.data = tabCd_2020, PROVINCIA, AÑO, CADMIO)

##Niquel 2020

tablaNi_2020 <- select(.data = Ni_DD_2020, PROVINCIA, ANNO, D01:D31)

tablaNi_2020 <- mutate(tablaNi_2020, AÑO = 2020)

tabNi_2020 <- tablaNi_2020 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabNi_2020$NIQUEL <- apply(tabNi_2020[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabNi_2020 <- select(.data = tabNi_2020, PROVINCIA, AÑO, NIQUEL)

##Plomo 2020

tablaPb_2020 <- select(.data = Pb_DD_2020, PROVINCIA, ANNO, D01:D31)

tablaPb_2020 <- mutate(tablaPb_2020, AÑO = 2020)

tabPb_2020 <- tablaPb_2020 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPb_2020$PLOMO <- apply(tabPb_2020[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPb_2020 <- select(.data = tabPb_2020, PROVINCIA, AÑO, PLOMO)


##PM10 2020

tablaPM10_2020 <- select(.data = PM10_DD_2020, PROVINCIA, ANNO, D01:D31)

tablaPM10_2020 <- mutate(tablaPM10_2020, AÑO = 2020)

tabPM10_2020 <- tablaPM10_2020 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPM10_2020$PM_10 <- apply(tabPM10_2020[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPM10_2020 <- select(.data = tabPM10_2020, PROVINCIA, AÑO, PM_10)

##PM25 2020

tablaPM25_2020 <- select(.data = PM25_DD_2020, PROVINCIA, ANNO, D01:D31)

tablaPM25_2020 <- mutate(tablaPM25_2020, AÑO = 2020)

tabPM25_2020 <- tablaPM25_2020 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPM25_2020$PM_25 <- apply(tabPM25_2020[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPM25_2020 <- select(.data = tabPM25_2020, PROVINCIA, AÑO, PM_25)

#union de los datos
join2020.1 <- full_join (x=tabAS_2020,y=tabBaP_2020, by=c("PROVINCIA","AÑO"))

join2020.2 <- full_join (x=join2020.1,y=tabCd_2020, by=c("PROVINCIA","AÑO"))

join2020.3 <- full_join (x=join2020.2,y=tabNi_2020, by=c("PROVINCIA","AÑO"))

join2020.4 <- full_join (x=join2020.3,y=tabPb_2020, by=c("PROVINCIA","AÑO"))

join2020.5 <- full_join (x=join2020.4,y=tabPM10_2020, by=c("PROVINCIA","AÑO"))

join2020.6 <- full_join (x=join2020.5,y=tabPM25_2020, by=c("PROVINCIA","AÑO"))
print (join2020.6)

#2019 ---------------------------------------------------------------------------------------------

As_DD_2019 <- read_delim("input/data/calidad del aire/2019/As_DD_2019.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

BaP_DD_2019 <- read_delim("input/data/calidad del aire/2019/BaP_DD_2019.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

C6H6_DD_2019 <- read_delim("input/data/calidad del aire/2019/C6H6_DD_2019.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

Cd_DD_2019 <- read_delim("input/data/calidad del aire/2019/Cd_DD_2019.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

Ni_DD_2019 <- read_delim("input/data/calidad del aire/2019/Ni_DD_2019.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

Pb_DD_2019 <- read_delim("input/data/calidad del aire/2019/Pb_DD_2019.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

PM10_DD_2019 <- read_delim("input/data/calidad del aire/2019/PM10_DD_2019.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

PM25_DD_2019 <- read_delim("input/data/calidad del aire/2019/PM25_DD_2019.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)


##Arsenico 2019

tablaAS_2019 <- select(.data = As_DD_2019, PROVINCIA, ANNO, D01:D31 )

tablaAS_2019 <-rename(.data = tablaAS_2019, AÑO = ANNO)

tabAS_2019 <- tablaAS_2019 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabAS_2019$ARSENICO <- apply(tabAS_2019[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabAS_2019 <- select(.data = tabAS_2019, PROVINCIA, AÑO, ARSENICO)


##Benzopireno 2019

tablaBaP_2019 <- select(.data = BaP_DD_2019, PROVINCIA, ANNO, D01:D31)

tablaBaP_2019 <-rename(.data = tablaBaP_2019, AÑO = ANNO)

tabBaP_2019 <- tablaBaP_2019 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabBaP_2019$BENZOPIRENO <- apply(tabBaP_2019[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabBaP_2019 <- select(.data = tabBaP_2019, PROVINCIA, AÑO, BENZOPIRENO)

##Benceno 2019

tablaC6H6_2019 <- select(.data = C6H6_DD_2019, PROVINCIA, ANNO, D01:D31)

tablaC6H6_2019 <-rename(.data = tablaC6H6_2019, AÑO = ANNO)

tabC6H6_2019 <- tablaC6H6_2019 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabC6H6_2019$BENCENO <- apply(tabC6H6_2019[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabC6H6_2019 <- select(.data = tabC6H6_2019, PROVINCIA, AÑO, BENCENO)

##Cadmio 2019

tablaCd_2019 <- select(.data = Cd_DD_2019, PROVINCIA, ANNO, D01:D31)

tablaCd_2019 <-rename(.data = tablaCd_2019, AÑO = ANNO)

tabCd_2019 <- tablaCd_2019 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabCd_2019$CADMIO <- apply(tabCd_2019[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabCd_2019 <- select(.data = tabCd_2019, PROVINCIA, AÑO, CADMIO)

##Niquel 2019

tablaNi_2019 <- select(.data = Ni_DD_2019, PROVINCIA, ANNO, D01:D31)

tablaNi_2019 <-rename(.data = tablaNi_2019, AÑO = ANNO)

tabNi_2019 <- tablaNi_2019 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabNi_2019$NIQUEL <- apply(tabNi_2019[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabNi_2019 <- select(.data = tabNi_2019, PROVINCIA, AÑO, NIQUEL)

##Plomo 2019

tablaPb_2019 <- select(.data = Pb_DD_2019, PROVINCIA, ANNO, D01:D31)

tablaPb_2019 <-rename(.data = tablaPb_2019, AÑO = ANNO)

tabPb_2019 <- tablaPb_2019 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPb_2019$PLOMO <- apply(tabPb_2019[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPb_2019 <- select(.data = tabPb_2019, PROVINCIA, AÑO, PLOMO)

##PM10 2019

tablaPM10_2019 <- select(.data = PM10_DD_2019, PROVINCIA, ANNO, D01:D31)

tablaPM10_2019 <-rename(.data = tablaPM10_2019, AÑO = ANNO)

tabPM10_2019 <- tablaPM10_2019 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPM10_2019$PM_10 <- apply(tabPM10_2019[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPM10_2019 <- select(.data = tabPM10_2019, PROVINCIA, AÑO, PM_10)

##PM25 2019

tablaPM25_2019 <- select(.data = PM25_DD_2019, PROVINCIA, ANNO, D01:D31)

tablaPM25_2019 <-rename(.data = tablaPM25_2019, AÑO = ANNO)

tabPM25_2019 <- tablaPM25_2019 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPM25_2019$PM_25 <- apply(tabPM25_2019[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPM25_2019 <- select(.data = tabPM25_2019, PROVINCIA, AÑO, PM_25)



join2019.1 <- full_join (x=tabAS_2019,y=tabBaP_2019, by=c("PROVINCIA","AÑO"))

join2019.2 <- full_join (x=join2019.1,y=tabCd_2019, by=c("PROVINCIA","AÑO"))

join2019.3 <- full_join (x=join2019.2,y=tabNi_2019, by=c("PROVINCIA","AÑO"))

join2019.4 <- full_join (x=join2019.3,y=tabPb_2019, by=c("PROVINCIA","AÑO"))

join2019.5 <- full_join (x=join2019.4,y=tabPM10_2019, by=c("PROVINCIA","AÑO"))

join2019.6 <- full_join (x=join2019.5,y=tabPM25_2019, by=c("PROVINCIA","AÑO"))
print (join2019.6)


#2018 ------------------------------------------------------------------------------------- 

As_DD_2018 <- read_delim("input/data/calidad del aire/2018/As_DD_2018.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

B_a_P_DD_2018 <- read_delim("input/data/calidad del aire/2018/B(a)P_DD_2018.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

Cd_DD_2018 <- read_delim("input/data/calidad del aire/2018/Cd_DD_2018.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

Ni_DD_2018 <- read_delim("input/data/calidad del aire/2018/Ni_DD_2018.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

Pb_DD_2018 <- read_delim("input/data/calidad del aire/2018/Pb_DD_2018.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

PM2_5_DD_2018 <- read_delim("input/data/calidad del aire/2018/PM2.5_DD_2018.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

PM10_DD_2018 <- read_delim("input/data/calidad del aire/2018/PM10_DD_2018.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)


##Arsenico 2018

tablaAS_2018 <- select(.data = As_DD_2018, PROVINCIA, ANNO, D01:D31 )

tablaAS_2018 <-rename(.data = tablaAS_2018, AÑO = ANNO)

tabAS_2018 <- tablaAS_2018 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabAS_2018$ARSENICO <- apply(tabAS_2018[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabAS_2018 <- select(.data = tabAS_2018, PROVINCIA, AÑO, ARSENICO)

print (tabAS_2018)

##Benzopireno 2018

tablaBaP_2018 <- select(.data = B_a_P_DD_2018, PROVINCIA, ANNO, D01:D31)

tablaBaP_2018 <-rename(.data = tablaBaP_2018, AÑO = ANNO)

tabBaP_2018 <- tablaBaP_2018 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabBaP_2018$BENZOPIRENO <- apply(tabBaP_2018[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabBaP_2018 <- select(.data = tabBaP_2018, PROVINCIA, AÑO, BENZOPIRENO)
print(tabBaP_2018)

##Cadmio 2018

tablaCd_2018 <- select(.data = Cd_DD_2018, PROVINCIA, ANNO, D01:D31)

tablaCd_2018 <-rename(.data = tablaCd_2018, AÑO = ANNO)

tabCd_2018 <- tablaCd_2018 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabCd_2018$CADMIO <- apply(tabCd_2018[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabCd_2018 <- select(.data = tabCd_2018, PROVINCIA, AÑO, CADMIO)
print(tabCd_2018)

##Niquel 2018

tablaNi_2018 <- select(.data = Ni_DD_2018, PROVINCIA, ANNO, D01:D31)

tablaNi_2018 <-rename(.data = tablaNi_2018, AÑO = ANNO)

tabNi_2018 <- tablaNi_2018 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabNi_2018$NIQUEL <- apply(tabNi_2018[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabNi_2018 <- select(.data = tabNi_2018, PROVINCIA, AÑO, NIQUEL)
print(tabNi_2018)

##Plomo 2018

tablaPb_2018 <- select(.data = Pb_DD_2018, PROVINCIA, ANNO, D01:D31)

tablaPb_2018 <-rename(.data = tablaPb_2018, AÑO = ANNO)

tabPb_2018 <- tablaPb_2018 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPb_2018$PLOMO <- apply(tabPb_2018[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPb_2018 <- select(.data = tabPb_2018, PROVINCIA, AÑO, PLOMO)
print(tabPb_2018)

##PM10 2018

tablaPM10_2018 <- select(.data = PM10_DD_2018, PROVINCIA, ANNO, D01:D31)

tablaPM10_2018 <-rename(.data = tablaPM10_2018, AÑO = ANNO)

tabPM10_2018 <- tablaPM10_2018 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPM10_2018$PM_10 <- apply(tabPM10_2018[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPM10_2018 <- select(.data = tabPM10_2018, PROVINCIA, AÑO, PM_10)
print(tabPM10_2018)

##PM25 2018

tablaPM25_2018 <- select(.data = PM2_5_DD_2018, PROVINCIA, ANNO, D01:D31)

tablaPM25_2018 <-rename(.data = tablaPM25_2018, AÑO = ANNO)

tabPM25_2018 <- tablaPM25_2018 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPM25_2018$PM_25 <- apply(tabPM25_2018[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPM25_2018 <- select(.data = tabPM25_2018, PROVINCIA, AÑO, PM_25)
print(tabPM25_2018)

join2018.1 <- full_join (x=tabAS_2018,y=tabBaP_2018, by=c("PROVINCIA","AÑO"))

join2018.2 <- full_join (x=join2018.1,y=tabCd_2018, by=c("PROVINCIA","AÑO"))

join2018.3 <- full_join (x=join2018.2,y=tabNi_2018, by=c("PROVINCIA","AÑO"))

join2018.4 <- full_join (x=join2018.3,y=tabPb_2018, by=c("PROVINCIA","AÑO"))

join2018.5 <- full_join (x=join2018.4,y=tabPM10_2018, by=c("PROVINCIA","AÑO"))

join2018.6 <- full_join (x=join2018.5,y=tabPM25_2018, by=c("PROVINCIA","AÑO"))

print (join2018.6)

#2017 ----------------------------------------------------------------------------

As_DD_2017 <- read_delim("input/data/calidad del aire/2017/As_DD_2017.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

B_a_P_DD_2017 <- read_delim("input/data/calidad del aire/2017/B(a)P_DD_2017.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

Cd_DD_2017 <- read_delim("input/data/calidad del aire/2017/Cd_DD_2017.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

Ni_DD_2017 <- read_delim("input/data/calidad del aire/2017/Ni_DD_2017.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

Pb_DD_2017 <- read_delim("input/data/calidad del aire/2017/Pb_DD_2017.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

PM2_5_DD_2017 <- read_delim("input/data/calidad del aire/2017/PM2.5_DD_2017.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

PM10_DD_2017 <- read_delim("input/data/calidad del aire/2017/PM10_DD_2017.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)


##Arsenico 2017

tablaAS_2017 <- select(.data = As_DD_2017, PROVINCIA, ANNO, D01:D31 )

tablaAS_2017 <-rename(.data = tablaAS_2017, AÑO = ANNO)

tabAS_2017 <- tablaAS_2017 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabAS_2017$ARSENICO <- apply(tabAS_2017[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabAS_2017 <- select(.data = tabAS_2017, PROVINCIA, AÑO, ARSENICO)

print (tabAS_2017)

##Benzopireno 2017

tablaBaP_2017 <- select(.data = B_a_P_DD_2017, PROVINCIA, ANNO, D01:D31)

tablaBaP_2017 <-rename(.data = tablaBaP_2017, AÑO = ANNO)

tabBaP_2017 <- tablaBaP_2017 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabBaP_2017$BENZOPIRENO <- apply(tabBaP_2017[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabBaP_2017 <- select(.data = tabBaP_2017, PROVINCIA, AÑO, BENZOPIRENO)
print(tabBaP_2017)

##Cadmio 2017

tablaCd_2017 <- select(.data = Cd_DD_2017, PROVINCIA, ANNO, D01:D31)

tablaCd_2017 <-rename(.data = tablaCd_2017, AÑO = ANNO)

tabCd_2017 <- tablaCd_2017 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabCd_2017$CADMIO <- apply(tabCd_2017[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabCd_2017 <- select(.data = tabCd_2017, PROVINCIA, AÑO, CADMIO)
print(tabCd_2017)

##Niquel 2017

tablaNi_2017 <- select(.data = Ni_DD_2017, PROVINCIA, ANNO, D01:D31)

tablaNi_2017 <-rename(.data = tablaNi_2017, AÑO = ANNO)

tabNi_2017 <- tablaNi_2017 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabNi_2017$NIQUEL <- apply(tabNi_2017[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabNi_2017 <- select(.data = tabNi_2017, PROVINCIA, AÑO, NIQUEL)
print(tabNi_2017)

##Plomo 2017

tablaPb_2017 <- select(.data = Pb_DD_2017, PROVINCIA, ANNO, D01:D31)

tablaPb_2017 <-rename(.data = tablaPb_2017, AÑO = ANNO)

tabPb_2017 <- tablaPb_2017 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPb_2017$PLOMO <- apply(tabPb_2017[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPb_2017 <- select(.data = tabPb_2017, PROVINCIA, AÑO, PLOMO)
print(tabPb_2017)

##PM10 2017

tablaPM10_2017 <- select(.data = PM10_DD_2017, PROVINCIA, ANNO, D01:D31)

tablaPM10_2017 <-rename(.data = tablaPM10_2017, AÑO = ANNO)

tabPM10_2017 <- tablaPM10_2017 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPM10_2017$PM_10 <- apply(tabPM10_2017[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPM10_2017 <- select(.data = tabPM10_2017, PROVINCIA, AÑO, PM_10)
print(tabPM10_2017)

##PM25 2017

tablaPM25_2017 <- select(.data = PM2_5_DD_2017, PROVINCIA, ANNO, D01:D31)

tablaPM25_2017 <-rename(.data = tablaPM25_2017, AÑO = ANNO)

tabPM25_2017 <- tablaPM25_2017 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPM25_2017$PM_25 <- apply(tabPM25_2017[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPM25_2017 <- select(.data = tabPM25_2017, PROVINCIA, AÑO, PM_25)
print(tabPM25_2017)

join2017.1 <- full_join (x=tabAS_2017,y=tabBaP_2017, by=c("PROVINCIA","AÑO"))

join2017.2 <- full_join (x=join2017.1,y=tabCd_2017, by=c("PROVINCIA","AÑO"))

join2017.3 <- full_join (x=join2017.2,y=tabNi_2017, by=c("PROVINCIA","AÑO"))

join2017.4 <- full_join (x=join2017.3,y=tabPb_2017, by=c("PROVINCIA","AÑO"))

join2017.5 <- full_join (x=join2017.4,y=tabPM10_2017, by=c("PROVINCIA","AÑO"))

join2017.6 <- full_join (x=join2017.5,y=tabPM25_2017, by=c("PROVINCIA","AÑO"))

print (join2017.6)
#2016

library(readxl)
As_DD_2016 <- read_excel("input/data/calidad del aire/2016/As_DD_2016.xlsx")

B_a_P_DD_2016 <- read_excel("input/data/calidad del aire/2016/B(a)P_DD_2016.xlsx")

Cd_DD_2016 <- read_excel("input/data/calidad del aire/2016/Cd_DD_2016.xlsx")

Ni_DD_2016 <- read_excel("input/data/calidad del aire/2016/Ni_DD_2016.xlsx")

Pb_DD_2016 <- read_excel("input/data/calidad del aire/2016/Pb_DD_2016.xlsx")

PM10_DD_2016 <- read_excel("input/data/calidad del aire/2016/PM10_DD_2016.xlsx")

PM25_DD_2016 <- read_excel("input/data/calidad del aire/2016/PM25_DD_2016.xlsx")

##Arsenico 2016

tablaAS_2016<- select(.data = As_DD_2016,PROVINCIA, ANNO, D01:D31 )

tablaAS_2016<-rename(.data = tablaAS_2016, AÑO = ANNO)

tabAS_2016<- tablaAS_2016 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabAS_2016$ARSENICO <- apply(tabAS_2016[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabAS_2016 <- select(.data = tabAS_2016, PROVINCIA, AÑO, ARSENICO)
print(tabAS_2016)

##Benzopireno 2016

tablaBaP_2016<- select(.data = B_a_P_DD_2016,PROVINCIA, ANNO, D01:D31)

tablaBaP_2016 <-rename(.data = tablaBaP_2016, AÑO = ANNO)

tabBaP_2016<- tablaBaP_2016 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabBaP_2016$BENZOPIRENO <- apply(tabBaP_2016[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabBaP_2016 <- select(.data = tabBaP_2016, PROVINCIA, AÑO, BENZOPIRENO)
print(tabBaP_2016)

##Cadmio 2016

tablaCd_2016<- select(.data = Cd_DD_2016,PROVINCIA, ANNO, D01:D31)

tablaCd_2016 <-rename(.data = tablaCd_2016, AÑO = ANNO)

tabCd_2016<- tablaCd_2016 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabCd_2016$CADMIO <- apply(tabCd_2016[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabCd_2016 <- select(.data = tabCd_2016, PROVINCIA, AÑO, CADMIO)
print(tabCd_2016)

##Niquel 2016

tablaNi_2016<- select(.data = Ni_DD_2016,PROVINCIA, ANNO, D01:D31)

tablaNi_2016 <-rename(.data = tablaNi_2016, AÑO = ANNO)

tabNi_2016<- tablaNi_2016 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabNi_2016$NIQUEL <- apply(tabNi_2016[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabNi_2016 <- select(.data = tabNi_2016, PROVINCIA, AÑO, NIQUEL)
print(tabNi_2016)

##Plomo 2016

tablaPb_2016<- select(.data = Pb_DD_2016,PROVINCIA, ANNO, D01:D31)

tablaPb_2016 <-rename(.data = tablaPb_2016, AÑO = ANNO)

tabPb_2016<- tablaPb_2016 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPb_2016$PLOMO <- apply(tabPb_2016[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPb_2016 <- select(.data = tabPb_2016, PROVINCIA, AÑO, PLOMO)
print(tabPb_2016)

##PM10 2016

tablaPM10_2016<- select(.data = PM10_DD_2016,PROVINCIA, ANNO, D01:D31)

tablaPM10_2016 <-rename(.data = tablaPM10_2016, AÑO = ANNO)

tabPM10_2016<- tablaPM10_2016 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPM10_2016$PM_10 <- apply(tabPM10_2016[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPM10_2016 <- select(.data = tabPM10_2016, PROVINCIA, AÑO, PM_10)
print(tabPM10_2016)

##PM25 2016

tablaPM25_2016<- select(.data = PM25_DD_2016,PROVINCIA, ANNO, D01:D31)

tablaPM25_2016 <-rename(.data = tablaPM25_2016, AÑO = ANNO)

tabPM25_2016<- tablaPM25_2016 %>%
  group_by(PROVINCIA,AÑO) %>%
  summarise(across(c(D01:D31), ~ mean(.x, na.rm = TRUE)))

tabPM25_2016$PM_25 <- apply(tabPM25_2016[ ,c(3:33)], 1, mean, na.rm = TRUE)

tabPM25_2016 <- select(.data = tabPM25_2016, PROVINCIA, AÑO, PM_25)
print(tabPM25_2016)

join2016.1 <- full_join (x=tabAS_2016,y=tabBaP_2016, by=c("PROVINCIA","AÑO"))

join2016.2 <- full_join (x=join2016.1,y=tabCd_2016, by=c("PROVINCIA","AÑO"))

join2016.3 <- full_join (x=join2016.2,y=tabNi_2016, by=c("PROVINCIA","AÑO"))

join2016.4 <- full_join (x=join2016.3,y=tabPb_2016, by=c("PROVINCIA","AÑO"))

join2016.5 <- full_join (x=join2016.4,y=tabPM10_2016, by=c("PROVINCIA","AÑO"))

join2016.6 <- full_join (x=join2016.5,y=tabPM25_2016, by=c("PROVINCIA","AÑO"))

print (join2016.6)


#union de todas las tablas creadas anteriormente

Calidad1 <- union_all(join2020.6,join2019.6)

Calidad2 <- union_all(Calidad1,join2018.6)

Calidad3 <- union_all(Calidad2,join2017.6)

Calidad4 <- union_all(Calidad3,join2016.6)


#cambiar el número de la provincia por el nombre de la provincia y la comunidad.

library(readxl)
provincia_con_numero <- read_excel("input/data/calidad del aire/provincia con numero.xlsx", 
                                   sheet = "Estaciones evaluación 2019")
print(provincia_con_numero)

calidadFinal = full_join (x=Calidad4, y=provincia_con_numero, by=c("PROVINCIA"))

view(calidadFinal)
#Importar datos de las enfermedades
library(readr)
X49971 <- read_delim("input/data/Enfermedades/49971.csv", 
                     delim = "\t", escape_double = FALSE, 
                     trim_ws = TRUE)


#Camniar nombre de las columnas de la base de datos de las enfermedades para su mas facil acceso
colnames(X49971)<- c('Nacional','CCAA','Causa_muerte','Sexo','Lugar','Año','Total')
Enfermedades <- X49971

Enfermedades <-
  Enfermedades %>% 
  mutate(N_CCAA = case_when(CCAA == "Andalucía"  ~ "ANDALUCÍA",
                            CCAA == "Aragón"  ~ "ARAGÓN",
                            CCAA == "Asturias, Principado de" ~ "ASTURIAS (PRINCIPADO DE)",
                            CCAA == "Balears, Illes"  ~ "BALEARES (ISLAS)",
                            CCAA == "Canarias"  ~ "CANARIAS",
                            CCAA == "Cantabria"  ~ "CANTABRIA",
                            CCAA == "Castilla y León" ~ "CASTILLA Y LEÓN",
                            CCAA == "Castilla-La Mancha"  ~ "CASTILLA-LA MANCHA",
                            CCAA == "Cataluña"  ~ "CATALUÑA",
                            CCAA == "Comunitat Valenciana"  ~ "COMUNIDAD VALENCIANA",
                            CCAA == "Extremadura"  ~ "EXTREMADURA",
                            CCAA == "Galicia"  ~ "GALICIA",
                            CCAA == "Madrid, Comunidad de"  ~ "MADRID",
                            CCAA == "Murcia, Región de"  ~ "MURCIA (REGIÓN DE)",
                            CCAA == "Navarra, Comunidad Foral de"  ~ "NAVARRA (COMUNIDAD FORAL)",
                            CCAA == "País Vasco"  ~ "PAÍS VASCO",
                            CCAA == "Rioja, La"  ~ "RIOJA (LA)",
                            CCAA == "Ceuta"  ~ "CEUTA",
                            CCAA == "Melilla"  ~ "MELILLA"))

#Filtrado de la base de datos para tener datos mas manejables y los que necesitamos
#Filtramos para tener datos de ambos sexos en conjunto
#Filtrado por Total de datos
#Filtrado de las enfermedades que vamos a  tratar
Enfermedades %>%
  filter(Lugar == "Total") %>% 
  filter(Sexo == "Total") %>%
  filter(Causa_muerte=="062-067 X.Enfermedades del sistema respiratorio")-> Enf1

Enfermedades %>%
  filter(Lugar == "Total") %>% 
  filter(Sexo == "Total") %>%
  filter(Causa_muerte=="062 Influenza (gripe) (incluye gripe por virus de la influenza pandémica o zoonótica identificados)")-> Gripe

Enfermedades %>%
  filter(Lugar == "Total") %>% 
  filter(Sexo == "Total") %>%
  filter(Causa_muerte=="063 Neumonía")-> Neumonia

Enfermedades %>%
  filter(Lugar == "Total") %>% 
  filter(Sexo == "Total") %>%
  filter(Causa_muerte=="064 Enfermedades crónicas de las vías respiratorias inferiores (excepto asma)")-> EnfViasInf

Enfermedades %>%
  filter(Lugar == "Total") %>% 
  filter(Sexo == "Total") %>%
  filter(Causa_muerte=="065 Asma")-> Asma

Enfermedades %>%
  filter(Lugar == "Total") %>% 
  filter(Sexo == "Total") %>%
  filter(Causa_muerte=="066 Insuficiencia respiratoria")-> Insuficiencia_Respiratoria










Enfermedades %>%
  filter(Lugar == "Total") %>%
  filter(Sexo == "Total") %>%
  filter(Causa_muerte=="009-041 II.Tumores")-> Enf2

Enfermedades %>%
  filter(Lugar == "Total") %>%
  filter(Sexo == "Total") %>%
  filter(Causa_muerte=="053-061 IX.Enfermedades del sistema circulatorio")-> Enf3

EnfTotal1 <- union_all(Enf1,Enf2)
EnfTotal <- union_all (EnfTotal1,Enf3)

View(EnfTotal)




  
  