#!/usr/bin/env Rscript

# Dati =====================================================
m <- 8
Xs <- c(5,4,6,6,5,6,7,4,5,6,5,5,4,6,4)
q3_alpha <- 0.90
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
    theta <- 1/m*mean(Xs)

    print_q(1, theta)
# Q2
    print_q(2, TRUE)
# Q3
    extr <- function(z) { theta - sqrt(theta*(1-theta)/(m*length(Xs)))*z }
    extr_lower <- extr(qnorm((1+q3_alpha)/2))
    extr_upper <- extr(qnorm((1-q3_alpha)/2))

    print_q(3, c(extr_lower, extr_upper))
}

version()
