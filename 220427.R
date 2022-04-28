#!/usr/bin/env Rscript

max_wrong <- 5
p_right <- 0.354

# Q1
q1 <- (1-p_right)^max_wrong

print("Q1:")
print(q1)

# Q2
ans_right <- 14

q2 <- dbinom(max_wrong-1, ans_right-1, 1-p_right)*(1-p_right)

print("Q2:")
print(q2)

# Q3
ans_tot <- 17

wrongs <- c(0:max_wrong-1)
q3 <- sum(dbinom(wrongs, ans_tot, 1-p_right)) + dbinom(max_wrong-1, ans_tot-1, 1-p_right)*(1-p_right)

print("Q3:")
print(q3)
