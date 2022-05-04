#!/usr/bin/env Rscript

library(MASS)

var.data <- c(9, 8, 3, 4, 6, 7, 5, 1, 2)
X <- c(-0.05, 0.5, 0.75)
Y <- c(-2.37, -0.66, 1.41)
var <- matrix(var.data, nrow=3, ncol=3, byrow=TRUE)

# Q1
k <- 1/sum(var)
var <- var*k

print("Q1:")
print(k)
print(fractions(k))

# Q2
p_X <- rowSums(var)
q2 <- p_X[match(-0.05, X)]

print("Q2:")
print(q2)
print(fractions(q2))

# Q3
p_Y <- colSums(var)
q3 <- p_Y[match(1.41, Y)]

print("Q3:")
print(q3)
print(fractions(q3))

# Q4
E_X <- sum(X*p_X)

print("Q4:")
print(E_X)

# Q5
E_Y <- sum(Y*p_Y)
E_Y.2 <- sum(Y^2*p_Y)

Var_Y <- E_Y.2 - E_Y^2

print("Q5:")
print(Var_Y)
