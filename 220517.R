#!/usr/bin/env Rscript

# Dati =====================================================
n_X <- 5
n_Y <- 8
p <- 0.623
q1_min <- 1
q1_max <- 7
q2_S <- 7
q2_X <- 5
q3_S <- 8
q3_Y_min <- 4
q3_Y_max <- 6

n_S <- n_X + n_Y
q3_Y_range <- c(q3_Y_min:q3_Y_max)
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
    p_X <- function(x) { dbinom(x, n_X, p) }
    p_Y <- function(y) { dbinom(y, n_Y, p) }
    p_S <- function(s) { dbinom(s, n_S, p) }
    F_S <- function(s) { pbinom(s, n_S, p) }
# Q1
    q1 <- F_S(q1_max-1) - F_S(q1_min-1)

    print_q(1, q1)

# Q2
    q2 <- p_X(q2_X)*p_Y(q2_S-q2_X)/p_S(q2_S)

    print_q(2, q2)

# Q3
    q3 <- sum(p_Y(q3_Y_range)*p_X(q3_S-q3_Y_range))/p_S(q3_S)

    print_q(3, q3)

}

version()
