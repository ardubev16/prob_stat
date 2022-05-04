#!/usr/bin/env Rscript

library(MASS)

var.data <- c(2, 3, 4, 6, 9, 1, 7, 5, 10, 12, 11, 8)
X <- c(1.28, 2.32, 3.22)
Y <- c(-1.32, -0.25, 0.76, 1.47)
var <- matrix(var.data, nrow=3, ncol=4, byrow=TRUE)

# Q1
k <- 1/sum(var)
var <- var*k

print("Q1:")
print(k)
print(fractions(k))

# Q2
p_X <- rowSums(var)
q2 <- sum(p_X[1:match(2.32, X)])

print("Q2:")
print(q2)
print(fractions(q2))

# Q3
p_Y <- colSums(var)
q3 <- 1 - sum(p_Y[1:match(-0.25, Y)])

print("Q3:")
print(q3)
print(fractions(q3))

# Q5
E_X <- sum(X*p_X)
E_X.2 <- sum(X^2*p_X)

Var_X <- E_X.2 - E_X^2

print("Q5:")
print(Var_X)

# Q4
E_Y <- sum(Y*p_Y)

print("Q4:")
print(E_Y)
print(fractions(E_Y))
