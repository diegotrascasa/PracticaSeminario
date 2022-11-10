library(readr)
X46687 <- read_delim("46687.csv", delim = "\t", 
                     escape_double = FALSE, trim_ws = TRUE)
View(X46687)


library(readxl)
datos_aire_20220 <- read_excel("~/Downloads/datos_aire_20220.xlsx")
View(datos_aire_20220)
