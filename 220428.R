#!/usr/bin/env Rscript

n <- 19
p <- 0.123

# Q1
q1 <- n*p

print("Q1:")
print(q1)

# Q2
lambda <- n*p
va_pois <- lambda
q2 <- 17*va_pois + 18.745

print("Q2:")
print(q2)

# Q3
Y <- 4

q3 <- dpois(Y, lambda)

print("Q3:")
print(q3)
