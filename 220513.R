#!/usr/bin/env Rscript

# Dati =====================================================
k <- 0.65
x_upper <- 1.754116
q1_x <- 0.54
q3_x <- q1_x
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
    f_X <- function(x) { k*x }
    q1 <- integrate(f_X, 0, q1_x)$value

    print_q(1, q1)

# Q2
    # Foglio di carta
    print_q(2, FALSE)

# Q3
    E_X <- E_c(f_X, 0, x_upper)
    E_Yx <- q3_x/2

    print_q(3, E_X-E_Yx)

# Q4
    # E(E(Y|X)) = E(Y)
    f_Y <- function(y){ k*x_upper }
    E_YX <- E_c(f_Y, 0, x_upper)

    print_q(4, E_YX)
}

version()
