library(rstudioapi)
filename = "taller6.r"
filepath = rstudioapi::getActiveDocumentContext()$path
dir = substr(filepath, 1, nchar(filepath)-nchar(filename))
setwd(dir)
rm(filename, filepath,dir)

data <- read.table(file="taller06_datos.txt", header = T, sep=";")

###########
# PUNTO 1 #
###########
#  a - 3  #
#  b - 1  #
#  c - 4  #
#  d - 2  # 
###########

############
#  PUNTO 2 #
############

x <- c(1.4, 2.4, 4.0, 4.9, 5.7, 6.3,   7.8,  9.0,  9.3, 11.0)
y <- c(2.3, 3.7, 5.7, 9.9, 6.9, 15.8, 15.4, 36.9, 34.6, 53.2)

# Dispersograma
plot(x,y)

# Coeficiente de correlación
# cov(temp, peso)/(sd(x)*sd(y))
cor(x,y)










