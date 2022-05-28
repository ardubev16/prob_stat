#!/usr/bin/env Rscript

# Dati =====================================================
q2_arr <- c(12, 13, 6, 3, 7, 14, 13, 9, 7, 30, 6, 21, 13)
q3_mean <- 10
q3_n <- 13
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
Var_d <- function(X, x) {
    E2_d(X,x) - E_d(X,x)^2
}
E_d_vv <- function(XY, x, y) {
    res <- 0
    for (xs in x)
        res <- res + xs*sum(y*XY(xs,y))
    res
}

E_c <- function(X, lower, upper) {
    integrand <- function(x) { x*X(x) }
    integrate(integrand, lower, upper)$value
}
# ==========================================================

version <- function() {
# Q1
    print_q(1, FALSE)
# Q2
    var_S <- var(q2_arr)

    print_q(2, var_S)
# Q3
    print_q(3, 0)

}

version()
