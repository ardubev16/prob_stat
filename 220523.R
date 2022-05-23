#!/usr/bin/env Rscript

# Dati =====================================================
ml_sodio <- 1
mu <- 300
q1_ml_sodio <- 10
q1_molecole <- 3000
q3_molecole <- 3600
q3_prob <- 0.87
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
    n_mu <- mu*q1_ml_sodio
    n_var <- mu*q1_ml_sodio

    p_approx <- pnorm(((q1_molecole+0.5) - n_mu)/sqrt(n_var), lower.tail=FALSE)
    
    print_q(1, p_approx)

# Q2
    p_real <- ppois(q1_molecole, n_mu, lower.tail=FALSE)
    err <- abs(p_approx - p_real)

    print_q(2, err)
# Q3
    found <- FALSE
    n_ml <- 1
    quintile <- qnorm(1-q3_prob)
    while (!found) {
        val <- (q3_molecole-0.5 - n_ml*mu)/sqrt(n_ml*mu)
        if (val <= quintile) {
            found <- TRUE
            res <- n_ml
        }
        n_ml <- n_ml + 1
    }

    print_q(3, res)
}

version()
