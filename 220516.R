#!/usr/bin/env Rscript

# Dati =====================================================
len <- 1683.901908
mean <- 50
sd <- 2.03
n <- 34
q3_p <- 0.73018

S_mean <- n*mean
S_sd <- sqrt(n)*sd
# ==========================================================

# Functions ================================================
print_q <- function(n, q) {
    library(MASS)
    cat(sprintf("Q%d:\n", n))
    cat(sprintf("    %.7g\t%s\n", q, fractions(q)))
}

E_d <- function(X, x) {
    sum(x*X(x))
}
E2_d <- function(X, x) {
    sum(x^2*X(x))
}
E_c <- function(X, lower, upper) {
    integrand <- function(x) { x*X(x) }
    integrate(integrand, lower, upper)$value
}
# ==========================================================

version <- function() {
# Q1
    q1 <- pnorm(len, S_mean, S_sd)

    print_q(1, q1)

# Q2
    q2 <- 1 - q1

    print_q(2, q2)

# Q3
    q3 <- qnorm(1-q3_p, S_mean, S_sd)

    print_q(3, q3)
}

version()
