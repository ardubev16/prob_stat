#!/usr/bin/env Rscript

# Dati =====================================================
lambda_X <- 8
lambda_Y <- 9
sigma_W <- 1.1
q1_xy <- 27
q2_x <- 20.38
q2_xy <- 27
q3_w <- 2.9
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
    E_XY <- lambda_X + lambda_Y
    q1 <- E_XY/q1_xy

    print_q(1, q1)
# Q2
    E_XXY <- q2_xy*(lambda_X/(lambda_X+lambda_Y))
    q2 <- E_XXY/ceiling(q2_x)

    print_q(2, q2)
# Q3
    p_W_approx <- 1-sigma_W^2/q3_w^2

    print_q(3, p_W_approx)
# Q4
    p_W_real <- 2*pnorm(q3_w, 0, sigma_W) - 1
    err <- abs(p_W_approx-p_W_real)

    print_q(4, err)
}

version()
