#!/usr/bin/env Rscript

# Dati =====================================================
Xs <- c(1.51,4.8,0.39,4.16,1.55,0.74,0.53,1.99,6,1.65,1.71,2.2,1.59)
q3_alpha <- 0.92
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
    theta <- mean(Xs)^-1

    print_q(1, theta)
# Q2
    mean_x <- mean(Xs)

    print_q(2, mean_x)
# Q3
    extr <- function(z) { theta*(1 + z/sqrt(length(Xs))) }
    extr_lower <- extr(qnorm((1-q3_alpha)/2))
    extr_upper <- extr(qnorm((1+q3_alpha)/2))

    print_q(3, c(extr_lower, extr_upper))
}

version()
