library(readxl)
datos_aire_20220 <- read_excel("input/data/datos_aire_20220.xlsx")
View(datos_aire_20220)


library(readr)
X02001 <- read_delim("input/data/02001.csv", 
                     delim = "\t", escape_double = FALSE, 
                     trim_ws = TRUE)
View(X02001)