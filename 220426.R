#!/usr/bin/env Rscript

dado <- c(0.23, 0.06, 0.03, 0.12, 0.18, 0.09, 0.18, 0.11)
win <- dado[4]+dado[1]+dado[5]

# assertion
if (sum(dado) != 1) stop("Probabilita' sbagliate!")

# Q1
n <- 20
succ <- 8

tmp <- c(1:succ-1)
q1 <- sum(dbinom(tmp, n, win))

print("Q1:")
print(q1)

# Q2
m <- 4

q2 <- dgeom(m-1, win)

print("Q2:")
print(q2)

# Q3
m <- 4

q3 <- pgeom(m-2, win)

print("Q3:")
print(q3)
