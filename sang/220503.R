#!/usr/bin/env Rscript

lambda <- 11

# Q1
q1 <- dpois(0, lambda)

print("Q1:")
print(q1)

# Q2
q2 <- dpois(3, lambda)

print("Q2:")
print(q2)

# Q3
upper <- 10
lower <- 6

q3 <- pnorm(upper, lambda, sqrt(lambda)) - pnorm(lower, lambda, sqrt(lambda))

print("Q3:")
print(q3)
