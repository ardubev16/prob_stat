#!/usr/bin/env Rscript

# Dati =====================================================
VERSION <- 1
library(pracma)

lower <- 0
upper <- 1.16

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

version1 <- function() {
# Q1
    f_XY_e <- function(x,y) { x^2*y }
    epsilon <- 1/integral2(f_XY_e, lower, upper, lower, upper)$Q
    f_XY <- function(x,y) { epsilon*f_XY_e(x,y) }

    print_q(1, epsilon)

# Q2
    # Foglio di carta
    print_q(2, TRUE)

# Q3
    f_X <- function(x) { epsilon/2*upper^2*x^2 }
    f_Y <- function(y) { epsilon/3*upper^3*y }
    E_X <- E_c(f_X, lower, upper)
    E_Y <- E_c(f_Y, lower, upper)

    print_q(3, E_X+E_Y)

# Q4
    # X e Y sono indipendenti
    print_q(4, E_X*E_Y)
}

version2 <- function() { }

if (VERSION == 1) version1() else version2()
