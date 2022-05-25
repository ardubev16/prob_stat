#!/usr/bin/env Rscript

# Dati =====================================================
mu <- 6.374
sigma <- 0.605
higher_bound <- 7.1844123
lower_bound <- 5.4715727
q2_n <- 60
q2_peso_tot <- 385.1580597
q3_n <- 60
q3_buone <- 52
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
    p_cattiva <- pnorm(higher_bound, mu, sigma, lower.tail=FALSE) + pnorm(lower_bound, mu, sigma)

    print_q(1, p_cattiva)
# Q2
    mu_n <- q2_n*mu
    sigma_n <- sqrt(q2_n)*sigma
    q2 <- pnorm(q2_peso_tot, mu_n, sigma_n, lower.tail=FALSE)

    print_q(2, q2)
# Q3
    q3 <- pbinom(q3_buone, q3_n, 1-p_cattiva)

    print_q(3, q3)
}

version()
