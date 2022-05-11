#!/usr/bin/env Rscript

# Dati =====================================================
VERSION <- 1

D <- 7          # facce dado
q1_N <- 10      # lanci totali

q1_p <- 1/(D-1)
Rx <- c(0:q1_N-1)
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
    E_XN <- (q1_N-1)*q1_p

    print_q(1, E_XN)

# Q2
    Var_XN <- (q1_N-1)*q1_p*(1-q1_p)
    # Var(X) = E(X^2) - E(X)^2
    E_X2N <- Var_XN + E_XN^2
    
    print_q(2, E_X2N)

# Q3
    function(x) { dgeom(x*(7-1), 1/7) }
}

version2 <- function() { }

if (VERSION == 1) version1() else version2()
