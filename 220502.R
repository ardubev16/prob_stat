#!/usr/bin/env Rscript

lambda <- 1/115

# Q1
gts <- 123.1

q1 <- 1 - pexp(gts, lambda)

print("Q1:")
print(q1)

# Q2
prob <- 0.91

q2 <- qexp(prob, lambda)

print("Q2:")
print(q2)

# Q3
more_s <- 46

q3 <- pexp(more_s, lambda)

print("Q3:")
print(q3)

# Q4
print("Q4:")
function(x) { ifelse(x>0, 3*x^2*(1/115)*exp(-(1/115)*x^3), 0) }
