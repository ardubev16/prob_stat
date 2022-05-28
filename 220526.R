#!/usr/bin/env Rscript

# Dati =====================================================
w <- 0.4
T1_E <- function(w) { (2*w+7*w+4*w)/13 }
T2_E <- function(w) { (2*w+4*w)/6 }
T1_Var <- function(v) { (2^2*v+7^2*v+4^2*v)/13^2 }
T2_Var <- function(v) { (2^2*v+4^2*v)/6^2 }

mean_X <- w
var_X <- w^2
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
    dist_1 <- T1_E(mean_X) - mean_X
    dist_2 <- T2_E(mean_X) - mean_X

    print_q(1, c(dist_1, dist_2))
# Q2
    Var_1 <- T1_Var(var_X)
    Var_2 <- T2_Var(var_X)

    print_q(2, c(Var_1, Var_2))
# Q3
    mse_1 <- Var_1 + dist_1^2
    mse_2 <- Var_2 + dist_2^2

    print_q(3, c(mse_1, mse_2))
}

version()
