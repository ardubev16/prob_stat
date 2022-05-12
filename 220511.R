#!/usr/bin/env Rscript

# Dati =====================================================
VERSION <- 1

x1 <- 9
x2 <- 10
E_Y <- 3
q2_x <- 13
q2_z <- 16
q3_z <- 16

lambda_y <- E_Y
# ==========================================================

# Functions ================================================
print_q <- function(n, q) {
    library(MASS)
    cat(sprintf("Q%d:\n", n))
    cat(sprintf("    %.7g\t%s\n", q, fractions(q)))
}

E <- function(X, x) {
    sum(x*X(x))
}
E2 <- function(X, x) {
    sum(x^2*X(x))
}
# ==========================================================

version1 <- function() {
# Q1
    lambda_x <- x2

    print_q(1, lambda_x)

# Q2
    p <- lambda_x/(lambda_x+lambda_y)
    p_XZ <- function(k,n) { dbinom(k, n, p) }

    print_q(2, p_XZ(q2_x, q2_z))
# Q3
    # Valore atteso della v.a. binomiale
    E_XZ <- function(n) { n*p }

    print_q(3, E_XZ(q3_z))
}

version2 <- function() { }

if (VERSION == 1) version1() else version2()
