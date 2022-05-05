#!/usr/bin/env Rscript

library(MASS)
library(pracma)

x_fact <- 4
y_fact <- 3

# Q1
k <- 2/(x_fact+y_fact)

print("Q1:")
print(fractions(k))

# Q2
val_x <- 0.9
val_y <- 0.29

f_XY <- function(x,y) { ifelse((0<=x && x<=1) && (0<=y && y<=1), k*(x_fact*x+y_fact*y), 0) }
f_X <- function(x) { ifelse(0<=x && x<=1, k*(x_fact*x+y_fact/2), 0) }
f_Y <- function(y) { ifelse(0<=y && y<=1, k*(x_fact/2+y_fact*y), 0) }

q2 <- c(integrate(Vectorize(f_X), 0, val_x)$value, integrate(Vectorize(f_Y), 0, val_y)$value)

print("Q2:")
print(q2)

# Q3
offset <- 0.333

to_integrate <- function(x,y) { Vectorize(f_XY)(x,y)*Vectorize(f_X)(x)*Vectorize(f_Y)(y) }

q3 <- integral2(to_integrate, 0, 1, 0, 1)$Q + offset

print("Q3:")
print(q3)

# [1] "Q1:"
# [1] 2/7
# [1] "Q2:"
# [1] 0.8485714 0.2017571
# [1] "Q3:"
# [1] 1.503068
