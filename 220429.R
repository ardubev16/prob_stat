#!/usr/bin/env Rscript

# Q1
c <- 1/36

print("Q1:")
print(c)

# Q2
E_t2 <- 10.8

print("Q2:")
print(E_t2)

# Q3
input <- c(-0.1, 4.29, 2.88, 4.22, 3.26, 4.33, 6)

F_T <- function(t) { ifelse(t > 0, ifelse(t < 6, 1-1/36*(1/2*t*(6-t)^2+1/6*(6-t)^3), 1), 0) }

print("Q3:")
print(F_T(input))
