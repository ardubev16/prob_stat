#!/usr/bin/env Rscript

# Biglie
ba <- 25
bb <- 26
bc <- 31

# Q1
pc <- bc/(ba+bb+bc)

print("Q1:")
print(pc)

# Q2
N <- 6
q2 <- dbinom(1, N, pc)

print("Q2:")
print(q2)

# Q3
pa <- ba/(ba+bb+bc)
M <- 11
at_least <- 5
estraz <- c(at_least:M)
q3 <- sum(dbinom(estraz, M, pa))

print("Q3:")
print(q3)
