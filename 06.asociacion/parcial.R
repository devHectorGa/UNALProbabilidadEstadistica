tabla <- cbind(c(223, 53), c(91,11))
n <- 378

colnames(tabla) <- c("Cigarrillos Si", "Cigarrillo No")
rownames(tabla) <- c("Alcohol Sí", "Alcohol No")

pf <- 100*prop.table(x = tabla, margin=1)
pc <- 100*prop.table(x = tabla, margin=2)


round(addmargins(A = 100*prop.table(x = tabla), margin = c(1,2)), digits = 2)
# PF
round(addmargins(A = pf, margin = 2), digits = 2)
# PC
round(addmargins(A = pc, margin = 1), digits = 2)



### OTRO PUNTO

nj <- c(1,1,1,2,16,43,76,72,41,1)
n <- sum(nj)
hj <- nj / n

Nj <- cumsum(nj)
Hj <- cumsum(hj)

#Marca de clase
li <- c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5)
ls <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5 )
x <- (li+ls)/2

xbar = sum(x*nj)/n

sum(hj[9:10])*100

xbar

xbar * n - 1 * n
