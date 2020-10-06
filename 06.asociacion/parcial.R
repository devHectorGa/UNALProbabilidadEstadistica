library(rstudioapi)
filename = "parcial.r"
filepath = rstudioapi::getActiveDocumentContext()$path
dir = substr(filepath, 1, nchar(filepath)-nchar(filename))
setwd(dir)
rm(filename, filepath,dir)

# Punto 1
datos <- read.table(file = "datos_brutos.csv", header = TRUE, sep = ";")
summary(datos$x)
cv <- sd(datos$x)/mean(datos$x)
d <- c(datos$x)
q <- quantile(d)
q[4]-q[2]
summary(d)
cv <- sd(d)/mean(d)
Hj <- cumsum(table(datos$x)/50)
cv
quantile(d, probs = 0.8)
R <- max(datos$x) - min(datos$x)


# Punto 3
tabla <- cbind(c(230, 57), c(91,13))
n <- 391

colnames(tabla) <- c("Cigarrillos Si", "Cigarrillo No")
rownames(tabla) <- c("Alcohol Sí", "Alcohol No")

pf <- 100*prop.table(x = tabla, margin=1)
pc <- 100*prop.table(x = tabla, margin=2)


round(addmargins(A = 100*prop.table(x = tabla), margin = c(1,2)), digits = 2)
# PF
round(addmargins(A = pf, margin = 2), digits = 2)
# PC
round(addmargins(A = pc, margin = 1), digits = 2)

# Punto 5

nj <- c(1,1,1,3,15,46,63,83,40,1)
n <- sum(nj)
hj <- nj / n

Nj <- cumsum(nj)
Hj <- cumsum(hj)

#Marca de clase
li <- c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5)
ls <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
x <- (li+ls)/2

xbar = sum(x*nj)/n

sum(hj[9:10])*100

xbar

xbar * n - 1 * n

s2 <- sum(nj*((x-xbar)^2))/n
s = sqrt(s2)
cv <- 100*s/xbar
cv
