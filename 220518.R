#!/usr/bin/env Rscript

# Dati =====================================================
lambda_X <- 1.3
lambda_Y <- 1.6
q1_u <- 0.535
q1_x <- 0.106
q2_u <- 0.106
q2_x <- 0.535
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
    Fl_X <- function(x) { pexp(x, lambda_X, lower.tail=FALSE) }
    Fl_Y <- function(y) { pexp(y, lambda_Y, lower.tail=FALSE) }
# Q1
    print(function(x) { pexp(x, 1.3+1.6) })

# Q2
    p_UX <- function(u, x) { Fl_X(max(u,x))*Fl_Y(u)/Fl_X(x) }

    print_q(2, p_UX(q1_u, q1_x))

# Q3
    print_q(3, p_UX(q2_u, q2_x))

# Q4
    # X e Y sono stoc. ind.
    E_XY <- 1/lambda_X*1/lambda_Y

    print_q(4, E_XY)
}

version()
