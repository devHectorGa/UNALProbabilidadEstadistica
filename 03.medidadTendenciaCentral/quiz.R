n = 100
xbar = 1800
s_x = 150
ybar = 36
s_y = 3

cv_x = s_x / xbar
cv_y = s_y / ybar
cv_x
cv_y

###########
# 2 PUNTO #
###########

xbar = 44
n = 130

#############
#  3 PUNTO  #
#############

data <- c(19.29,21.14,23.24,17.63,19.55,19.89,17.45,19.18,16.65,16.95)
summary(data, digits = 4)
xbar <- mean(data)
n <- length (data)

sqrt(sum(data-xbar)^2/(n-1))
