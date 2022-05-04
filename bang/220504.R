#!/usr/bin/env Rscript

var.data <- c(9, 5, 8, 3, 4, 6, 1, 2, 7)
X <- c(-3.14, -1.95, -1)
Y <- c(0.32, 0.99, 2.07)
var <- matrix(var.data, nrow=3, ncol=3, byrow=TRUE)

# Q1
k <- 1/sum(var)
var <- var*k

print("Q1:")
print(k)

# Q2
p_X <- rowSums(var)
q2 <- p_X[match(-1.95, X)]

print("Q2:")
print(q2)

# Q3
p_Y <- colSums(var)
q3 <- p_Y[match(0.99, Y)]

print("Q3:")
print(q3)

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
