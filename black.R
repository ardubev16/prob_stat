#!/usr/bin/env Rscript

c <- 7/(1-exp(-7))

print("Q1:")
print(c)

X <- function(Y) { (Y-1)/2 }

x <- X(1.59)

f <- function(x) { ifelse(x>0 && x<1, c*exp(-7*x), 0) }

print("Q2:")
print(f(x))

function(x) { (x+1)*(x+2)*(x+3)*(x+4)/6 }
